package Plugins::ShairTunes2W::Plugin;

use strict;
use warnings;

use Plugins::ShairTunes2W::AIRPLAY;
use Plugins::ShairTunes2W::Utils;

use base qw(Slim::Plugin::OPMLBased);

use File::Spec::Functions;
use FindBin qw($Bin);
use lib catdir($Bin, 'Plugins', 'ShairTunes2W', 'lib');

use Slim::Utils::Log;
use Slim::Utils::Prefs;
use Slim::Utils::Misc;
use Slim::Utils::Network;
use Slim::Networking::Async;
use Slim::Networking::Async::Socket;
use Slim::Networking::Async::Socket::HTTP;

use Config;
use Digest::MD5 qw(md5 md5_hex);
use MIME::Base64;
use File::Spec;
use File::Which;
use File::Copy;
use POSIX qw(ceil :errno_h);
use Data::Dumper;

#use IO::Socket::INET6;
use IO::Socket::INET;
use Net::SDP;
use IO::Handle;

# create log categogy before loading other modules
my $log = Slim::Utils::Log->addLogCategory(
    {
        'category'     => 'plugin.shairtunes',
        'defaultLevel' => 'INFO',
        'description'  => getDisplayName(),
		'helper'	   => '',	
    }
);

my $prefs  = preferences( 'plugin.shairtunes' );
my $sprefs = preferences('server');

$prefs->init({ 
	squeezelite => 0, 
	loglevel => '',
	bufferThreshold => 32,
	latency => 1500,
	http_latency => 2000,
	useFLAC => 1,
});

my $shairtunes_helper;
my $DACPfqdn = "_dacp._tcp.local";
my $mDNSsock;
my $mDNShelper;

my $airport_pem = _airport_pem();
my $rsa;

my $samplingRate = 44100;  
  
my %clients     = (); # ( [ LMSclient ]      => [mDNSPID],       ...)
my %sockets     = (); # ( [ LMSclient ]      => [masterINETsock], ...)
my %players     = (); # ( [ masterINETsock ] => [LMSclient],      ...)
my %connections = (); # ( [ slaveINETSock ]  => ('socket' => [MasterINETSock], 'player' => [LMSclient],
					  #							 'decoder_ipc' => [INETSockIn], 'decoder_pid' => [helper],
					  #							 'poweroff', 'iport' => [image proxy]
					  #							 'cover_fh' => [INETSockCover], 'cover' => [jpeg blob] },		
					  #							 'DACPid' => [DACP ID], 'activeRemote' => [remote ID], 
					  #						     'remote' => [IP::port of remote]	
					  #							 'volume' => [player volume set by AirPlay]
					  #							 'host' => IP of iXXX device
my %covers		= (); # ( [ coverINETSock ]	 =>	 [jpeg blob]


sub logFile {
	my $id = shift;
	return catdir(Slim::Utils::OSDetect::dirsFor('log'), "shairtunes2-$id.log");
}


sub volumeChange {
	my $request = shift;
	my $client = $request->client();
	my $volume = $client->volume();
	
	# is there a player registered ?
	my ($slave) = grep { $connections{$_}->{player} == $client } keys %connections;
	return 0 if !defined $slave;
	
	my $conn = $connections{$slave};
			
	# nothing to do if there is no remote or volume change is the result of AirPlay request		
	return 0 if !defined $conn->{remote} || $conn->{volume} == $volume;
	$log->info("Volume new:$volume current:$conn->{volume}");

	if (!$volume) {
		$volume = -144;
	} else {
		$volume = 30 * ($volume - 100) / 99;
	}
	
	my $http = Slim::Networking::SimpleAsyncHTTP->new(
					sub { 
						my $reponse = shift;
						$log->info( "Volume response: $reponse" );
					},
					sub { 
						my $error = shift;
						$log->error( "Volume error: ", Dumper($error) );
					} );
					
	my $url = "http://$conn->{remote}/ctrl-int/1/setproperty?dmcp.device-volume=$volume";
		
	$http->get($url, 'Active-Remote' => $conn->{activeRemote} );	
}


sub getAirTunesMetaData {
	my $class  = shift;
	my $url = shift;
	my $slave;
	
	foreach my $s (keys %connections) {
		if (!defined $connections{$s}->{url}) {
			$log->debug($s, $connections{$s});
		}	
	}
	
	($slave) = grep { $connections{$_}->{url} eq $url } keys %connections;
	
	return  { artist   => "ShairTunes Artist",
			  title    => "ShairTunes Title",
			  album    => "ShairTunes Album",
			  #bitrate  => 44100 . " Hz",
			  cover    => "",
			  icon	   => "",	
			  type     => 'ShairTunes Stream',
			  duration => undef,
			} if !defined $slave;
	
	return $connections{$slave}->{metaData};
}

sub sendAction {
	my $class = shift;
	my $client = shift;
	my $action = shift;
	
	foreach my $master (keys %connections) {
		my $conn = $connections{$master};
		$log->debug("Player matching: p: $conn->{player}, c: $client, m: ", $conn->{player}->master);
	}
	
	my ($slave) = grep { $connections{$_}->{player}->master == $client } keys %connections;
	return 0 if !defined $slave || !defined $connections{$slave}->{remote};
	
	my $conn = $connections{$slave};
	
	my $http = Slim::Networking::SimpleAsyncHTTP->new(
					sub { 
						my $reponse = shift;
						$log->debug( "Action response: $reponse" );
					},
					sub { 
						my $error = shift;
						$log->error( "Action error: ", Dumper($error) );
					} );

	my $command;
	my $doAction = 0;
			
	if ($action eq 'rew') { $command = 'previtem'; }
	elsif ($action eq 'stop') { $command = 'nextitem'; }
	elsif ($action eq 'play') { $command = 'play'; }
	elsif ($action eq 'pause') { $command = 'pause'; }
	
	return 1 if !defined $command;
	
	my $url = "http://$conn->{remote}/ctrl-int/1/$command";
	$log->debug("Sending action: $url");
	
	$http->get($url, 'Active-Remote' => $conn->{activeRemote} );			
	
	return $doAction;
}	

	  					  
sub initPlugin {
    my $class = shift;
	my $version = $class->_pluginDataFor( 'version' );

    $log->info( "Initialising $version on " . $Config{'archname'} );
	
	eval {require Crypt::OpenSSL::RSA};
    if ($@) {
		my $lib = catdir(Slim::Utils::PluginManager->allPlugins->{'ShairTunes2W'}->{'basedir'}, "lib");
		move(catdir($lib, "_Crypt"), catdir($lib, "Crypt"));
		$log->warn("cannot find system OpenSSL::RSA, trying local version");
		
		require Crypt::OpenSSL::RSA;
	}
	
	$rsa = Crypt::OpenSSL::RSA->new_private_key( $airport_pem )	|| do { $log->error( "RSA private key import failed" ); return; };
		
	if ( $version ne $prefs->get('version') ) {
		$log->info("version change");
		$prefs->set('version', $version);
		
		if ($version eq '0.80.1') {
			$prefs->set("bufferThreshold", 32);
			$prefs->set("latency", 1500);
			$prefs->set("http_latency", 2000);
			$prefs->set("usesync", 0);
		}
	}
	
	if ( main::WEBUI ) {
		require Plugins::ShairTunes2W::Settings;
		Plugins::ShairTunes2W::Settings->new;
	}
		
	# this must be done in 2 steps as helperBinary *must* be called for 1st plugin run	
	$shairtunes_helper = Plugins::ShairTunes2W::Utils::helperBinary();	
	$shairtunes_helper = Plugins::ShairTunes2W::Utils::helperPath( $prefs->get('helper') || $shairtunes_helper );
	
	if ( !$shairtunes_helper || !-x $shairtunes_helper ) {
        $log->error( "I'm sorry your platform \"" . $Config{archname}
                      . "\" is unsupported or nobody has compiled a binary for it! Can't work." );
     }
	
	$log->info("selected helper: $shairtunes_helper");

    revoke_publishPlayers();

    # Subscribe to player connect/disconnect messages
    Slim::Control::Request::subscribe( \&playerSubscriptionChange,
        [ ['client'], [ 'new', 'reconnect', 'disconnect' ] ] );
		
	Slim::Formats::RemoteMetadata->registerProvider(
		match => qr/airplay\:/,
		func  => \&getAirTunesMetaData,
	);		
	
    $mDNSsock = new IO::Socket::INET(
        #LocalAddr 	=> '0.0.0.0',
        ReuseAddr 	=> 1,
		#ReusePort 	=> 1,
		Proto     	=> 'udp',
    );
	
	Slim::Utils::Network::blocking( $mDNSsock, 0 );
	Slim::Networking::Select::addRead( $mDNSsock, \&mDNSlistener );

    return 1;
}

sub getDisplayName {
    return ( 'PLUGIN_SHAIRTUNES2' );
}

sub republishPlayers {
	# first stop all existing players	
	foreach my $client (keys %clients) {
		next if (!defined($clients{$client}));
				
		my $player = $players{$sockets{$client}};
		next if !$player;
		removePlayer($player);
	}	
	
	%clients = %sockets = %players = %connections = ();
	
	# then re-publish all authorized
	foreach my $client (Slim::Player::Client::clients()) {
		next if !($prefs->get($client->id) // 1);
		next if $client->modelName() =~ /SqueezeLite/ && !$client->firmware && !$prefs->get('squeezelite');
		addPlayer($client);
	}
}

sub shutdownPlugin {
    revoke_publishPlayers();
	
	Slim::Networking::Select::removeRead( $mDNSsock );
	close($mDNSsock) if defined $mDNSsock;
	
	stop_mDNS();
	
	my $lib = catdir(Slim::Utils::PluginManager->allPlugins->{'ShairTunes2W'}->{'basedir'}, "lib");
	move(catdir($lib, "Crypt"), catdir($lib, "_Crypt"));
		
    return;
}

sub stop_mDNS {
	my $helper = Plugins::ShairTunes2W::Utils::helperBinary();
	$log->info("Killing all processes $mDNShelper");
	
	my $os = Slim::Utils::OSDetect::details();
		
	if ($os->{'os'} eq 'Windows') {
		system("taskkill /F /IM $mDNShelper /T");
	} else {	
		system("killall $mDNShelper");
	}	
}	

sub playerSubscriptionChange {
    my $request = shift;
    my $client  = $request->client;

    my $reqstr     = $request->getRequestString();
    my $clientname = $client->name();

    $log->debug( "request=$reqstr client=$clientname" );
	
	return if !($prefs->get($client->id) // 1);
	return if $client->modelName() =~ /SqueezeLite/ && !$client->firmware && !$prefs->get('squeezelite');
		
    if ( ( $reqstr eq "client new" ) || ( $reqstr eq "client reconnect" ) ) {
		addPlayer($client);
    } elsif ( $reqstr eq "client disconnect" ) {
		removePlayer($client);
	}	
}

sub addPlayer {
	my $client = shift;
	my $name = $client->name;
    
    $sockets{$client} = createListenPort(1);
    $players{ $sockets{$client} } = $client;
	
    if ( $sockets{$client} ) {
         # Add us to the select loop so we get notified
        Slim::Networking::Select::addRead( $sockets{$client}, \&handleSocketConnect );

        $clients{$client} = publishPlayer( $name, "", $sockets{$client}->sockport() );
		$log->info("create client $client with proc $clients{$client}");
    } else {
        $log->error( "could not create ShairTunes socket for $name" );
        delete $sockets{$client};
    }
}

sub removePlayer {	
	my $client = shift;
	my $name = $client->name;
	
    $log->info( "publisher for $name PID $clients{$client} will be terminated." );
    $clients{$client}->die();
    Slim::Networking::Select::removeRead( $sockets{$client} );
	my ($slave) = grep { $connections{$_}->{player} eq $client } keys %connections;
	if (defined $slave) {
		my $conn = $connections{$slave};
			
		$log->debug("Cleaning connection: $conn->{self}");
		cleanHelper( $conn );
		Slim::Networking::Select::removeRead( $conn->{self} );
		Slim::Networking::Select::removeRead( $conn->{cover_fh} );
		close $conn->{self};
		close $conn->{cover_fh};
		delete $connections{$slave};
	}	
	delete $players{ $sockets{$client} };
	close $sockets{$client};
	delete $sockets{$client};
	delete $clients{$client};
 }

sub createListenPort {
	my $max = shift || 1;
    my $listen;

=comment	
    $listen = new IO::Socket::INET6(
        Listen    => $max || 1,
        Domain    => AF_INET6,
        ReuseAddr => 1,
        Proto     => 'tcp',
    );
=cut	

	$listen = new IO::Socket::INET(
        Listen    => $max,
	    ReuseAddr => 1,
		Proto     => 'tcp',
    );

    if ( !$listen ) {
        $log->error( "Socket creation failed!: $!" );
    }

    return $listen;
}

sub revoke_publishPlayers {

	foreach my $client (keys %clients) {
		next if (!defined($clients{$client}));
		$log->info( "Stop old publish players services:", $clients{$client}->pid()  );
		$clients{$client}->die();
    }
}

sub publishPlayer {
    my ( $apname, $password, $port ) = @_;

    my $pw_clause = ( length $password ) ? "pw=true" : "pw=false";
    my @hw_addr = +( map( ord, split( //, md5( $apname ) ) ) )[ 0 .. 5 ];
	my $id =  join( '', map { sprintf "%02X", $_ } @hw_addr ) . "\@$apname";
	
	my $proc;
	my $path;
	my @params = ( $port, 
				"tp=UDP",                "sm=false",
                "sv=false",              "ek=1",
                "et=0,1",		         "md=0,1,2",
                "cn=0,1",                "ch=2",
                "ss=16",                 "sr=44100",
                $pw_clause,              "vn=3",
                "txtvers=1",			 "am=shairtunes2" );
	
	if ( $path = which('avahi-publish-service') ) {
		$log->info( "start avahi-publish-service \"$apname\"" );
        eval { $proc = Proc::Background->new( $path, $id, "_raop._tcp", $port, @params ); };
		$mDNShelper = $path;
		return $proc unless ($@);
        $log->error( "start avahi-publish-service failed" );
	}	
	$log->info( "avahi-publish-player not in path" ) if (!$@);
    
	if ( $path = which('dns-sd') ) {
        $log->info( "start dns-sd \"$apname\"" );
		eval { $proc = Proc::Background->new( $path, '-R', $id, "_raop._tcp", ".", @params ); };
		$mDNShelper = $path;
		return $proc unless ($@);
        $log->error( "start dns-sd failed" );
    }
	$log->info( "dns-sd not in path" ) if (!$@);
        
    if ( $path = which('mDNSPublish') ) {
        $log->info( "start mDNSPublish \"$apname\"" );
        eval { $proc = Proc::Background->new($path, $id, "_raop._tcp", @params ); };
		$mDNShelper = $path;
		return $proc unless ($@);
        $log->error( "start mDNSPublish failed" );
    }
    $log->info( "mDNSPublish not in path" ) if (!$@);
        
    $log->info("using built-in helper: $shairtunes_helper");
	
	eval { $proc = Proc::Background->new( $shairtunes_helper, "-dns", "host", Slim::Utils::Network::serverAddr(), $id, "_raop._tcp", @params ); };
	$mDNShelper = $shairtunes_helper;
	return $proc unless ($@);
	$log->error( "start $shairtunes_helper failed" ) if (!$@);
	
    return undef;
}

sub handleSocketConnect {
    my $socket = shift;
    my $player = $players{$socket};

    my $new = $socket->accept;
	
	$log->info( "New connection from: ", $new->peerhost);
	
    # set socket to unblocking mode => 0
    Slim::Utils::Network::blocking( $new, 0 );
    $connections{$new} = { self => $new, 
						   socket => $socket, player => $player,
						   metaData => { artist   => "ShairTunes Artist",
										 title    => "ShairTunes Title",
										 album    => "ShairTunes Album",
										 bitrate  => $samplingRate . " Hz",
										 cover    => "",
										 duration => 0,
										 position => 0,
										 offset   => 0,
										}, 
							cover => '', cover_fh => createListenPort(5),	
							remote => undef, DACPid => 'none',
							volume => -1,
						};
	
    # Add us to the select loop so we get notified
    Slim::Networking::Select::addRead( $new, \&handleSocketRead );
		
	# Add us to the select loop so we get notified
    Slim::Networking::Select::addRead( $connections{$new}->{cover_fh}, \&handleCoverConnect );
	
	#subscribe to volume changes
	Slim::Control::Request::subscribe( \&volumeChange, [['mixer'], ['volume']], $player );
}

sub handleCoverConnect {
    my $socket = shift;
    
    my $new = $socket->accept;
	Slim::Utils::Network::blocking( $new, 0 );
	
	my ($slave) = grep { $connections{$_}->{cover_fh} == $socket } keys %connections;
	$covers{$new} = $connections{$slave}->{cover};
				
    $log->info( "New cover proxy connection from " . $new->peerhost );
	
	Slim::Networking::Select::addRead( $new, \&handleCoverRequest);
}

sub handleCoverRequest {
	my $socket = shift;
	
	while (my $line = _readline($socket)) {
		$log->info("Image proxy request: $line");
	}
	
	return if !defined $covers{$socket};
	
	my $resp = HTTP::Response->new( 200 );
	$resp->protocol('HTTP/1.1');
	$resp->user_agent('ShairTunes2');
	$resp->content_type('image/jpeg');
    $resp->header( 'Content-Length' => length $covers{$socket} );
	$resp->header( 'Connection' => 'close');
	$resp->content( $covers{$socket} );
	
	my $data = $resp->as_string();
	my $sent = 0;
	while ($sent < length $data) {
		my $bytes = send ($socket, substr($data, $sent), 0);
		last if !defined $bytes && $! != EAGAIN && $! != EWOULDBLOCK;
		next if !$bytes;
		$log->debug("sent $bytes");
		$sent += $bytes;
	}
			
	Slim::Networking::Select::removeRead( $socket );
	
	close $socket;
	delete $covers{$socket};

	$log->info("Coverart sent $sent over ", length $data);
}

sub handleHelperOut {
    my $socket = shift;
	
	my ($slave) = grep { $connections{$_}->{decoder_ipc} == $socket } keys %connections;
	my $line = _readline($socket);
	
	if (!defined $line) {
		$log->error("helper crash");
		cleanHelper($connections{$slave}) if defined $slave;
		Slim::Networking::Select::removeRead( $socket ) if !defined $slave;
		return;
	}
	
	$log->info("From helper: ", $line);
	
	if ($line =~ /play/) {
		$connections{$slave}->{player}->execute( ['play'] );
	} elsif ($line =~ /flushed/) {
		$connections{$slave}->{metaData}->{offset} = 0;
		$connections{$slave}->{player}->execute( ['stop'] );
	}	
}	

sub handleSocketRead {
    my $socket = shift;

    my $conn = $connections{$socket};
	my $contentLength = 0;
    my $buffer        = "";
    my $bytesToRead = 1024;
	
	# read header
    while ( 1 ) {
		my $ret = sysread( $socket, my $incoming, $bytesToRead );
		next if !defined $ret && ($! == EAGAIN || $! == EWOULDBLOCK);
		#next if !defined $ret;
		$log->error( "Reading socket failed!: $!" ) if !defined $ret;
        last if !$ret;    # ERROR or EOF
		
        $buffer .= $incoming;

        last if ( $incoming =~ /\r\n\r\n/ );
    }
	
	if (!$buffer) {
        $log->info( "Closed: " . $socket );
		
		Slim::Networking::Select::removeRead( $socket );
		Slim::Networking::Select::removeRead( $conn->{cover_fh} );
		close $socket;
		close $conn->{cover_fh};
		delete $connections{$socket};
		
		return;
	}
	
	$log->debug( "header: $buffer" );
		
    my ( $header, $contentBody ) = split( /\r\n\r\n/, $buffer, 2 );

    # get body length
    if ( $header =~ /Content-Length:\s(\d+)/ ) {
        $contentLength = $1;
        $log->debug( "Content Length is: " . $contentLength );
    } 

    $log->debug( "ContentBody length already received: " . length( $contentBody ) );

    $bytesToRead = $contentLength - length( $contentBody );
    while ( $bytesToRead > 0 ) {
        $log->debug( "Content not yet completely received. Waiting..." );
        ### In the next loop just read whats missing.
        my $ret = read( $socket, my $incoming, $bytesToRead );
        next if !defined $ret && ($! == EAGAIN || $! == EWOULDBLOCK);
		#next if !defined $ret;
        $log->error( "Reading socket failed!: $!" ) if !defined $ret;
        last if !$ret;    # ERROR or EOF
			
        $contentBody .= $incoming;
        $bytesToRead = $contentLength - length( $contentBody );
    }

    ### START: Not yet updated.
    $conn->{req} = HTTP::Request->parse( $header );
    $conn->{req}->content( $contentBody );

    conn_handle_request( $socket, $conn );
    ### END: Not yet updated.
}


sub waitChildConnect {   
	my $socket = shift;
	my $sel = IO::Select->new();
	
	$sel->add($socket);	
	
	return undef if !$sel->can_read(5);
		
	my $sock = $socket->accept;
	IO::Handle::blocking($sock, 0);
	$sel->remove($socket);
					
	return $sock;
}

# a readline that returns undef if nothing pending, otherwise reads up to '\n'
sub _readline {
	my $socket = shift;
	
	return undef if !defined $socket;

	my $bytes = sysread($socket, my $c, 1);
	return undef if !$bytes;
	
	my $line = $c;
	return $line if $c =~ /\n\z/;
	
	while (1) {
		$bytes = sysread($socket, $c, 1);
		next if !defined $bytes && ($! == EAGAIN || $! == EWOULDBLOCK);
		$line .= $c if $bytes;
		last if !$bytes || $c =~ /\n\z/;
	}	
	
	return $line;
}

sub cleanHelper {
	my $conn = shift;

	if (defined $conn->{decoder_ipc} && $conn->{decoder_ipc}->connected) { 
        $conn->{player}->execute( ['stop'] );
		
        # read all left output
        my $dfh = $conn->{decoder_ipc};
        while ( my $out = _readline($dfh) ) { $log->info( "Decoder output: " . $out ); }

        Slim::Networking::Select::removeRead( $conn->{decoder_ipc} );
		close $conn->{decoder_ipc};
				
        if ( $conn->{poweredoff} ) {
            $conn->{player}->execute( [ 'power', 0 ] );
        }
		
		$conn->{decoder_pid}->die;
		
		# disconnect from volume 
		Slim::Control::Request::unsubscribe( \&volumeChange, $conn->{player} );
	}	
}

sub notifyUpdate {
	my $client = shift;
	my $metaData = shift;
	
	my $master = $client->master;
	$master->playingSong->pluginData( wmaMeta => {
										cover  => $metaData->{cover},
										icon   => $metaData->{icon},
										artist => $metaData->{artist},
										title  => $metaData->{title},
											} );
											
	$master->currentPlaylistUpdateTime( Time::HiRes::time() );
	Slim::Control::Request::notifyFromArray( $master, ['newmetadata'] );  											
	$master->execute( ['play'] );
}											

sub conn_handle_request {
    my ( $socket, $conn ) = @_;

    my $req  = $conn->{req};
    my $resp = HTTP::Response->new( 200 );
	
    $resp->request( $req );
    $resp->protocol( $req->protocol );

    $resp->header( 'CSeq',              $req->header( 'CSeq' ) );
    $resp->header( 'Audio-Jack-Status', 'connected; type=analog' );
	#$resp->header( 'Server',           "AirTunes/105.1" );

    if ( my $chall = $req->header( 'Apple-Challenge' ) ) {
        my $data = decode_base64( $chall );
        my $ip   = $socket->sockhost;
        if ( $ip =~ /((\d+\.){3}\d+)$/ ) {    # IPv4
            $data .= join '', map { chr } split( /\./, $1 );
        }
        else {
            $data .= Plugins::ShairTunes2W::Utils::ip6bin( $ip );
        }

        my @hw_addr = +( map( ord, split( //, md5( $conn->{player}->name() ) ) ) )[ 0 .. 5 ];

        $data .= join '', map { chr } @hw_addr;
        $data .= chr( 0 ) x ( 0x20 - length( $data ) );

        $rsa->use_pkcs1_padding;              # this isn't hashed before signing
        my $signature = encode_base64 $rsa->private_encrypt( $data ), '';
		$signature =~ s/=*$//;
        $resp->header( 'Apple-Response', $signature );
    }

    if ( defined $conn->{password} && length $conn->{password} ) {
        if ( !Plugins::ShairTunes2W::Utils::digest_ok( $req, $conn ) ) {
            my $nonce = md5_hex( map { rand } 1 .. 20 );
            $conn->{nonce} = $nonce;
            my $apname = $conn->{player}->name();
            $resp->header( 'WWW-Authenticate', "Digest realm=\"$apname\", nonce=\"$nonce\"" );
            $resp->code( 401 );
            $req->method( 'DENIED' );
        }
    }

    for ( $req->method ) {
        $log->debug( "got command / method: $_" );

        /^OPTIONS$/ && do {
            $resp->header( 'Public',
                'ANNOUNCE, SETUP, RECORD, PAUSE, FLUSH, TEARDOWN, OPTIONS, GET_PARAMETER, SET_PARAMETER' );

            if ( !defined $conn->{remote} ) {
				$conn->{DACPid} = $req->header( 'DACP-ID' );
				$conn->{activeRemote} = $req->header( 'Active-Remote' );
				my $mDNSdata = AnyEvent::DNS::dns_pack { rd => 1, qd => [[$DACPfqdn, "ptr"]] };	
				send $mDNSsock, $mDNSdata, 0, sockaddr_in(5353, Socket::inet_aton('224.0.0.251'));
				
				$log->info("DACP-ID: $conn->{DACPid}, Active Remote: $conn->{activeRemote}");
				$log->debug("Send mDNS data: $DACPfqdn, ", Dumper($mDNSdata));
			}	

            last;
        };

        /^ANNOUNCE$/ && do {
            my $sdp   = Net::SDP->new( $req->content );
            my $audio = $sdp->media_desc_of_type( 'audio' );

            do { $log->error( "no AESIV" ); return; } unless my $aesiv = decode_base64( $audio->attribute( 'aesiv' ) );
            do { $log->error( "no AESKEY" ); return; }
              unless my $rsaaeskey = decode_base64( $audio->attribute( 'rsaaeskey' ) );
            $rsa->use_pkcs1_oaep_padding;
            my $aeskey = $rsa->decrypt( $rsaaeskey ) || do { $log->error( "RSA decrypt failed" ); return; };

            $conn->{aesiv}  = $aesiv;
            $conn->{aeskey} = $aeskey;
            $conn->{fmtp}   = $audio->attribute( 'fmtp' );
			# there is a bug in some client like AirAudio who puts the target IN addr in the o= fiekd of the SDP session
			$conn->{host}   = $sdp->session_origin_address();
			if ($conn->{host} eq Slim::Utils::Network::serverAddr()) {
				$conn->{host}   = $socket->peerhost();
				$log->info("suspicious peer in SDP, using socket $conn->{host}");
			}	
						
			last;
        };

        /^SETUP$/ && do {
            my $transport = $req->header( 'Transport' );
            $transport =~ s/;control_port=(\d+)//;
            my $cport = $1;
            $transport =~ s/;timing_port=(\d+)//;
            my $tport = $1;
            $resp->header( 'Session', 'DEADBEEF' );

            my %socket_params = (   Listen    => 1,
						ReuseAddr => 1,
						LocalHost => 'localhost',
						Proto     => 'tcp'
					);	

			my $h_ipc  = new IO::Socket::INET(%socket_params);
			IO::Handle::blocking($h_ipc, 0);
			
			my @fmtp = split( / /, $conn->{fmtp} );
					
			my @dec_args = (
				host  => $conn->{host},
				socket => $h_ipc->sockport, 
                iv    => unpack( 'H*', $conn->{aesiv} ),
                key   => unpack( 'H*', $conn->{aeskey} ),
                fmtp  => $conn->{fmtp},
                cport => $cport,
                tport => $tport,
				latencies => $prefs->get('latency') . ':' . $prefs->get('http_latency'),
				codec => $prefs->get('useFLAC') ? "flac" : "wav",
                );

			if (my $loglevel = $prefs->get('loglevel')) {
				my $id = $conn->{player}->id;
				$id =~ s/:/-/g;
				push @dec_args, ("log", logFile($id), "dbg", $loglevel);
			}	
			
			push (@dec_args, "drift") if $prefs->get('drift');
			
			$log->info( "decode command: ", Dumper(@dec_args));
						    
			$h_ipc->accept;
			
			my $helper_pid = Proc::Background->new( $shairtunes_helper, @dec_args );
			my $helper_ipc = waitChildConnect($h_ipc);
			$h_ipc->close();

			my $sel = IO::Select->new();
			$sel->add($helper_ipc);
			my %helper_ports = ( cport => '', hport => '', port => '', tport => '' );

			while ( (my $reader) = $sel->can_read(5) ) {
				while ( defined( my $line = _readline($reader) ) ) {
					if ( $line =~ /^([cht]?port):\s*(\d+)/ ) {
                           $helper_ports{$1} = $2;
                           next;
                    }
                    $log->error( "Helper output: " . $line );
                }
                last if ( !grep { !$helper_ports{$_} } keys %helper_ports );
            }
			
            $sel->remove( $helper_ipc );
                   
			$conn->{decoder_pid}  = $helper_pid;
            $conn->{decoder_ipc}  = $helper_ipc;
            $conn->{iport}		  = $conn->{cover_fh}->sockport;	
			
			$log->info(
                "launched decoder: $helper_pid on ports: $helper_ports{port}/$helper_ports{cport}/$helper_ports{tport}/$helper_ports{hport}, http port: $conn->{iport}"
            );
					
			my $host = Slim::Utils::Network::serverAddr();
			$conn->{url}  = "airplay://$host:$helper_ports{hport}/" . md5_hex($conn) . "_stream." . ($prefs->get('useFLAC') ? "flc" : "wav");
								
			# Add out to the select loop so we get notified of play after flush (pause)
			Slim::Networking::Select::addRead( $helper_ipc, \&handleHelperOut );

			#$resp->header( 'Transport', $req->header( 'Transport' ) . ";server_port=$helper_ports{port}" );
			$resp->header( 'Transport', "RTP/AVP/UDP;unicast;mode=record;control_port=$helper_ports{cport};timing_port=$helper_ports{tport};server_port=$helper_ports{port}" );
						
            last;
        };

        /^RECORD$/ && do {

			my $client = $conn->{player};
			
			# save current playlist
			$client->pluginData(playlist => {
					playlist 	=> [ @{$client->playlist} ],
					shufflelist => [ @{$client->shufflelist} ],
					index   	=> Slim::Player::Source::streamingSongIndex($client),
					shuffle  	=> $sprefs->client($client)->get('shuffle'),
					repeat		=> $sprefs->client($client)->get('repeat'),
			} );	
			
			$log->info( "Playing url: $conn->{url}" );
			
            $conn->{poweredoff} = !$conn->{player}->power;
			$conn->{metaData}->{offset} = 0;
			$conn->{metaData}->{type}   = 'ShairTunes Stream';
			$conn->{player}->execute( [ 'playlist', 'play', $conn->{url} ] );
			$client->currentPlaylistUpdateTime( Time::HiRes::time() );
            Slim::Control::Request::notifyFromArray( $conn->{player}, ['newmetadata'] );
			
			$resp->header( 'Audio-Latency', '44100' );

            last;
        };
		
        /^FLUSH$/ && do {

            my $dfh = $conn->{decoder_ipc};
			my ($seqno, $rtptime) = $req->header('RTP-Info') =~ m|seq=([^;]+);rtptime=([^;]+)|i;
			$log->info("Flush up to $seqno, $rtptime");
            send ($dfh, "flush $seqno $rtptime\n", 0);
            
            last;
        };
		
        /^TEARDOWN$/ && do {
			my $client = $conn->{player};
            
			$resp->header( 'Connection', 'close' );
					
			cleanHelper($conn);
			
			# restore playlist
			my $playlist = $client->pluginData('playlist');
			
			@{$client->playlist} = @{$playlist->{playlist}};
			@{$client->shufflelist} = @{$playlist->{shufflelist}};
		
			$sprefs->client($client)->set('shuffle', $playlist->{shuffle});
			$sprefs->client($client)->set('repear', $playlist->{repeat});
		
			$client->controller()->resetSongqueue($playlist->{index});
			$client->currentPlaylistUpdateTime(Time::HiRes::time());

			Slim::Control::Request::notifyFromArray($client, ['playlist', 'stop']);	
			Slim::Control::Request::notifyFromArray($client, ['playlist', 'sync']);
			
			last;
        };
		
        /^SET_PARAMETER$/ && do {
            if ( $req->header( 'Content-Type' ) eq "text/parameters" ) {
                my @lines = split( /[\r\n]+/, $req->content );
                $log->debug( "SET_PARAMETER req: " . $req->content );
                my %content = map { /^(\S+): (.+)/; ( lc $1, $2 ) } @lines;
                my $cfh = $conn->{decoder_ipc};
                if ( exists $content{volume} ) {
                    my $volume = $content{volume};
                    my $percent = int( 100 + ( $volume * 99 / 30) + 0.5 );
					$percent = 0 if $percent < 0;
					
                   	# synchronize volume of other players, 		
					if ($prefs->get('syncVolume')) {
						my $client = $conn->{player};
						
						my @otherclients = grep { $_->name ne $client->name and $_->power } $client->syncGroupActiveMembers();
						foreach my $otherclient ( @otherclients ) {
							my $volume;
							
							if ( $client->volume) {
								$volume = ceil( $otherclient->volume * $percent / $client->volume );
							} else {
								$volume = $percent; 
							}
							
							$otherclient->execute( [ 'mixer', 'volume', $volume ] );
							$log->info("Changing volume on ", $otherclient->name, " volume:$volume");
						}
					}	
					
					$log->info( "sending-> vol: " . $percent );
					
					$conn->{player}->execute( [ 'mixer', 'volume', $percent ] );
					$conn->{volume} = $percent;
	            }
                elsif ( exists $content{progress} ) {
					my $metaData = $conn->{metaData};
                    my ( $start, $curr, $end ) = split( /\//, $content{progress} );
                    my $positionRealTime = ( $curr - $start ) / $samplingRate;
                    my $durationRealTime = ( $end - $start ) / $samplingRate;

					# this is likely a bridge, so duration shall be set to 0 (live stream)
					if ($conn->{player}->model =~ /squeezelite/ && !$conn->{player}->firmware) {
						$metaData->{duration} = 0;
					} else {
						$metaData->{duration} = $durationRealTime;
					}	
					
					$metaData->{position} = $positionRealTime;
					
					notifyUpdate ($conn->{player}, $metaData);
                    $conn->{player}->execute( ['play'] );

                    $log->debug( "Duration: " . $durationRealTime . "; Position: " . $positionRealTime );
                }
                else {
                    $log->error( "unable to perform content for req SET_PARAMETER text/parameters: " . $req->content );
                }
            }
            elsif ( $req->header( 'Content-Type' ) eq "application/x-dmap-tagged" ) {
				my $metaData = $conn->{metaData};
                my %dmapData = Plugins::ShairTunes2W::Utils::getDmapData( $req->content );
                $metaData->{artist} = $dmapData{artist};
                $metaData->{album}  = $dmapData{album};
                $metaData->{title}  = $dmapData{title};

                $log->debug( "DMAP DATA found. Length: " . length( $req->content ) . " " . Dumper( \%dmapData ) );
                
                notifyUpdate($conn->{player}, $metaData);
				$metaData->{offset} = $conn->{player}->songElapsedSeconds();
			}
            elsif ( $req->header( 'Content-Type' ) eq "image/jpeg" ) {
				my $metaData = $conn->{metaData};
                                
                my $host = Slim::Utils::Network::serverAddr();
				my $url  = "http://$host:$conn->{iport}/" . md5_hex($req->content) . "/cover.jpg";
				$metaData->{cover} = $url;
				$metaData->{icon} = $url;
				$conn->{cover} = $req->content;
				      								
				$log->debug( "IMAGE DATA received, sending to: ", $url );
				
				notifyUpdate($conn->{player}, $metaData);
			}
			elsif ( $req->header( 'Content-Type' ) eq "image/none" ) {
				my $metaData = $conn->{metaData};
                                
            	$metaData->{cover} = '';
				$metaData->{icon} = '';
				
				notifyUpdate($conn->{player}, $metaData);
			}
			
            else {
                $log->error( "Unknown content-type: \"" . $req->header( 'Content-Type' ) . "\"" );
            }
            last;
        };
		
        /^GET_PARAMETER$/ && do {
            my @lines = split /[\r\n]+/, $req->content;
            $log->debug( "GET_PARAMETER req: " . $req->content );

            my %content = map { /^(\S+): (.+)/; ( lc $1, $2 ) } @lines;

            last;

        };
		
		
        /^DENIED$/ && last;
        $log->error( "Got unknown method: $_" );
    }

    $log->debug("\n\nPLAYER_MESSAGE_START: \n" .$resp->as_string("\r\n"). "\nPLAYER_MESSAGE_END\n\n");

    print $socket $resp->as_string( "\r\n" );
    $socket->flush;

}

sub mDNSlistener {

	recv $mDNSsock, my $buf, 4096, 0;
		
	my $res = AnyEvent::DNS::dns_unpack $buf;
	#$log->debug("DNSListener: ", Dumper($res));
	 
	my @answers = (@{$res->{an}}, @{$res->{ar}});
	
	foreach my $socket (keys %connections) {
		my $conn = $connections{$socket};
		next if defined $conn->{remote};
		
		my $DACPid = $conn->{DACPid};
		
		my ($service) = grep { $_->[1] eq 'srv' && $_->[0] =~ /$DACPid/ } @answers;
		return if !defined $service;
		
		my ($addr) = grep { $_->[1] eq 'a' && $_->[0] eq $service->[6] } @answers;
		return if !defined $addr;
		
		$conn->{remote} = "$addr->[3]:$service->[5]";
		$log->info("Found remote: $DACPid, $conn->{remote}");
	}	
}


sub _airport_pem {
    return q|-----BEGIN RSA PRIVATE KEY-----
MIIEpQIBAAKCAQEA59dE8qLieItsH1WgjrcFRKj6eUWqi+bGLOX1HL3U3GhC/j0Qg90u3sG/1CUt
wC5vOYvfDmFI6oSFXi5ELabWJmT2dKHzBJKa3k9ok+8t9ucRqMd6DZHJ2YCCLlDRKSKv6kDqnw4U
wPdpOMXziC/AMj3Z/lUVX1G7WSHCAWKf1zNS1eLvqr+boEjXuBOitnZ/bDzPHrTOZz0Dew0uowxf
/+sG+NCK3eQJVxqcaJ/vEHKIVd2M+5qL71yJQ+87X6oV3eaYvt3zWZYD6z5vYTcrtij2VZ9Zmni/
UAaHqn9JdsBWLUEpVviYnhimNVvYFZeCXg/IdTQ+x4IRdiXNv5hEewIDAQABAoIBAQDl8Axy9XfW
BLmkzkEiqoSwF0PsmVrPzH9KsnwLGH+QZlvjWd8SWYGN7u1507HvhF5N3drJoVU3O14nDY4TFQAa
LlJ9VM35AApXaLyY1ERrN7u9ALKd2LUwYhM7Km539O4yUFYikE2nIPscEsA5ltpxOgUGCY7b7ez5
NtD6nL1ZKauw7aNXmVAvmJTcuPxWmoktF3gDJKK2wxZuNGcJE0uFQEG4Z3BrWP7yoNuSK3dii2jm
lpPHr0O/KnPQtzI3eguhe0TwUem/eYSdyzMyVx/YpwkzwtYL3sR5k0o9rKQLtvLzfAqdBxBurciz
aaA/L0HIgAmOit1GJA2saMxTVPNhAoGBAPfgv1oeZxgxmotiCcMXFEQEWflzhWYTsXrhUIuz5jFu
a39GLS99ZEErhLdrwj8rDDViRVJ5skOp9zFvlYAHs0xh92ji1E7V/ysnKBfsMrPkk5KSKPrnjndM
oPdevWnVkgJ5jxFuNgxkOLMuG9i53B4yMvDTCRiIPMQ++N2iLDaRAoGBAO9v//mU8eVkQaoANf0Z
oMjW8CN4xwWA2cSEIHkd9AfFkftuv8oyLDCG3ZAf0vrhrrtkrfa7ef+AUb69DNggq4mHQAYBp7L+
k5DKzJrKuO0r+R0YbY9pZD1+/g9dVt91d6LQNepUE/yY2PP5CNoFmjedpLHMOPFdVgqDzDFxU8hL
AoGBANDrr7xAJbqBjHVwIzQ4To9pb4BNeqDndk5Qe7fT3+/H1njGaC0/rXE0Qb7q5ySgnsCb3DvA
cJyRM9SJ7OKlGt0FMSdJD5KG0XPIpAVNwgpXXH5MDJg09KHeh0kXo+QA6viFBi21y340NonnEfdf
54PX4ZGS/Xac1UK+pLkBB+zRAoGAf0AY3H3qKS2lMEI4bzEFoHeK3G895pDaK3TFBVmD7fV0Zhov
17fegFPMwOII8MisYm9ZfT2Z0s5Ro3s5rkt+nvLAdfC/PYPKzTLalpGSwomSNYJcB9HNMlmhkGzc
1JnLYT4iyUyx6pcZBmCd8bD0iwY/FzcgNDaUmbX9+XDvRA0CgYEAkE7pIPlE71qvfJQgoA9em0gI
LAuE4Pu13aKiJnfft7hIjbK+5kyb3TysZvoyDnb3HOKvInK7vXbKuU4ISgxB2bB3HcYzQMGsz1qJ
2gG0N5hvJpzwwhbhXqFKA4zaaSrw622wDniAK5MlIE0tIAKKP4yxNGjoD2QYjhBGuhvkWKY=
-----END RSA PRIVATE KEY-----|;
}

1;