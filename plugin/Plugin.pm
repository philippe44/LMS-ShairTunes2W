package Plugins::ShairTunes2W::Plugin;

use strict;
use warnings;

use base qw(Slim::Plugin::OPMLBased);

use Config;
use File::Spec;
use File::Spec::Functions;
use File::Basename qw(basename);
use Encode qw(encode);

use Digest::MD5 qw(md5 md5_hex);
use MIME::Base64;
use File::Which;
use File::Copy;
use POSIX qw(ceil :errno_h);

use IO::Socket::INET;
use IO::Handle;

use Data::Dumper;

use Slim::Utils::Log;
use Slim::Utils::Prefs;
use Slim::Utils::Misc;
use Slim::Utils::Network;
use Slim::Networking::Async;
use Slim::Networking::Async::Socket;
use Slim::Networking::Async::Socket::HTTP;

use Plugins::ShairTunes2W::AIRPLAY;
use Plugins::ShairTunes2W::Utils;

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
	latency => 1000,
	http_latency => 2000,
	codec => 'flc',
	http_fill => 1,
	port_base => '',
});

my $shairtunes_helper;
my $DACPfqdn = "_dacp._tcp.local";
my $mDNSsock;
my $mDNShelper;

my $rsa;
my $airport_pem = _airport_pem();

my $samplingRate = 44100;  
  
my %daemons   = (); # ( [ clientId ]       => [mDNSPID],       ...)
my %listeners = (); # ( [ clientId ]       => [masterINETsock], ...)
my %covers    = (); # ( [ coverINETSock ]  => {'cover' => [blob], 'coverType' => [content-type]}
my %sessions  = (); # ( [ slaveINETSock ]  => ('socket' => [MasterINETSock], 'player' => [client],
                    #                          'url' => [URL where LMS get the audio], 
                    #                          'decoder_ipc' => [INETSockIn], 'decoder_pid' => [helper],
                    #                          'poweroff', 'iport' => [image proxy]
                    #                          'cover_fh' => [INETSockCover], 'cover' => [blob] }, 'coverType' => [content-type]		
                    #                          'DACPid' => [DACP ID], 'activeRemote' => [remote ID], 
                    #                          'remote' => [IP::port of remote]	
                    #                          'volume' => [player volume set by AirPlay]
                    #						   'host' => IP of iXXX device
# migrate existing prefs to new structure, bump prefs version by one tick
$prefs->migrate(2, sub {
	if ($prefs->get('useFLAC')) {
		$prefs->set('codec', 'flc');
	} else {
		$prefs->set('codec', 'wav');
	}	
	$prefs->remove('useFLAC');
});

sub logFile {
	my $id = shift;
	return catdir(Slim::Utils::OSDetect::dirsFor('log'), "shairtunes2-$id.log");
}

sub volumeChange {
	my $request = shift;
	my $client = $request->client();
	my $volume = $client->volume();
	
	# is there a player registered ?
	my ($slave) = grep { $sessions{$_}->{player} == $client } keys %sessions;
	return 0 if !defined $slave;
	
	my $session = $sessions{$slave};
			
	# nothing to do if there is no remote or volume change is the result of AirPlay request		
	return 0 if !defined $session->{remote} || $session->{volume} == $volume;
	$log->info("Volume new:$volume current:$session->{volume}");

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
					
	my $url = "http://$session->{remote}/ctrl-int/1/setproperty?dmcp.device-volume=$volume";
		
	$http->get($url, 'Active-Remote' => $session->{activeRemote} );	
}

sub sendAction {
	my $class = shift;
	my $client = shift;
	my $action = shift;
	
	foreach my $master (keys %sessions) {
		my $session = $sessions{$master};
		$log->debug("Player matching: p: $session->{player}, c: $client, m: ", $session->{player}->master);
	}
	
	my ($slave) = grep { $sessions{$_}->{player}->master == $client } keys %sessions;
	return 0 if !defined $slave || !defined $sessions{$slave}->{remote};
	
	my $session = $sessions{$slave};
	
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
	
	my $url = "http://$session->{remote}/ctrl-int/1/$command";
	$log->debug("Sending action: $url");
	
	$http->get($url, 'Active-Remote' => $session->{activeRemote} );			
	
	return $doAction;
}	

sub initPlugin {
    my $class = shift;
	$class->SUPER::initPlugin();
	
	my $version = $class->_pluginDataFor( 'version' );

    $log->info( "Initializing $version on " . $Config{'archname'} );
	$log->info( "Using INC", Dumper(\@INC) );
	
	my $basedir = Slim::Utils::PluginManager->allPlugins->{'ShairTunes2W'}->{'basedir'};
	my $perlmajorversion = $Config{'version'};
	$perlmajorversion =~ s/\.\d+$//;

	my $arch = $Config::Config{'archname'};		
	
	# align arch names with LMS' expectations (see Slim::Utils::PluginManager)
	$arch =~ s/^i[3456]86-/i686-/;
	$arch =~ s/gnu-//;
	my $is64bitint = $arch =~ /64int/;
	
	if ( $arch =~ /^arm.*linux/ ) {
		$arch = $arch =~ /gnueabihf/
				? 'arm-linux-gnueabihf-thread-multi'
				: 'arm-linux-gnueabi-thread-multi';
		$arch .= '-64int' if $is64bitint;
	}

	if ( $arch =~ /^(?:ppc|powerpc).*linux/ ) {
		$arch = 'powerpc-linux-thread-multi';
		$arch .= '-64int' if $is64bitint;
	}
	
	# note the '/...pm' instead of "::"
	foreach my $module ( 'Math/BigInt.pm', 'Crypt/PK/RSA.pm', 'Net/SDP.pm' ) {
		eval { require $module };
		
		if ($@) {
			$log->warn("cannot find system $module, using local version [", $INC{$module} || '<n/a>', "]");
			delete $INC{$module};
	
			local @INC = (
				"$basedir/elib", 
				"$basedir/elib/$perlmajorversion",
				"$basedir/elib/$perlmajorversion/$arch",
				"$basedir/elib/$perlmajorversion/$arch/auto",
				@INC
			);
			
			require $module;
		}	
		
		$log->info("$module loaded from $INC{$module}");
	}	
	
	# we need LTM and it might not be loaded on older arm version
	Math::BigInt->import( try => 'LTM, GMP, FastCalc, Pari' );
	$log->info("Using ", Math::BigInt->config->{lib}, " version ", Math::BigInt->config->{version});
	
	$rsa = Crypt::PK::RSA->new( \$airport_pem )	|| do { $log->error( "RSA private key import failed" ); return; };
		
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
		
    $mDNSsock = new IO::Socket::INET(
        LocalAddr 	=> Slim::Utils::Network::serverAddr(),
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
	foreach my $id (keys %daemons) {
		next unless defined $daemons{$id};
		removePlayer($id);
	}	
	
	%daemons = %listeners = %sessions = ();
	
	# then re-publish all authorized
	foreach my $client (Slim::Player::Client::clients()) {
		next unless $prefs->get($client->id) // 1;
		next if ($client->modelName =~ /Bridge/ || $client->model =~/squeezeesp32/ || ($client->model =~ /squeezelite/ && !$client->firmware)) && !$prefs->get('squeezelite');
		addPlayer($client);
	}
}

sub shutdownPlugin {
    revoke_publishPlayers();
	
	Slim::Networking::Select::removeRead( $mDNSsock );
	close($mDNSsock) if defined $mDNSsock;
	
	stop_mDNS();
	
    return;
}

sub stop_mDNS {
	my $helper = Plugins::ShairTunes2W::Utils::helperBinary();
	$log->info("Killing all processes $mDNShelper");
	
	my $os = Slim::Utils::OSDetect::details();
		
	if ($os->{'os'} eq 'Windows') {
		system("taskkill /F /T /IM ". basename($mDNShelper));
	} else {	
		system("pkill -9 -f $mDNShelper");
	}	
}	

sub playerSubscriptionChange {
    my $request = shift;
    my $reqstr = $request->getRequestString();
	my $client = $request->client;
	my $id = $request->clientid;
	
	return unless $prefs->get($id) // 1;
	
	$log->info( "request=$reqstr client=$client ", $id );
		
    if ( ($reqstr eq "client new" || $reqstr eq "client reconnect") &&
		($prefs->get('squeezelite') || ($client->model !~ /squeezeesp32/ && $client->modelName !~ /Bridge/ && ($client->model !~ /squeezelite/ || $client->firmware)))) {
		addPlayer($client);
    } elsif ( $reqstr eq "client disconnect" ) {
		removePlayer($id);
	}	
}

sub addPlayer {
	my $client = shift;
	my $name = $client->name;
	my $id = $client->id;
    
	# cleanup in case of reconnect with unattached players
	removePlayer($id) if $daemons{$id};
	
	my $sock = createListenPort(1);
	
    if ( $sock ) {
	    # Add ourself to the select loop so we get notified
		$listeners{$id} = $sock;
        Slim::Networking::Select::addRead( $sock, \&handleSocketConnect );
		
		# now we can publish ourself
        $daemons{$id} = publishPlayer( $name, "", $sock->sockport() );
		$log->info("create client $client with proc $daemons{$id}");
    } else {
        $log->error( "could not create ShairTunes socket for $name" );
    }
}

sub removePlayer {	
	my $id = shift;
	my $client = Slim::Player::Client::getClient($id);		
	
	return unless $daemons{$id};
	
    $log->info( "publisher for ", $client ? $client->name : 'n/a', " with $id, PID $daemons{$id} will be terminated." );
	
    $daemons{$id}->die();
	delete $daemons{$id};
	
    Slim::Networking::Select::removeRead( $listeners{$id} );
	my ($sock) = grep { $sessions{$_}->{player}->id eq $id } keys %sessions;

	if ($sock) {
		my $session = $sessions{$sock};
			
		$log->debug("Cleaning connection: $session->{self}");
		cleanHelper( $session );
		Slim::Networking::Select::removeRead( $session->{self} );
		Slim::Networking::Select::removeRead( $session->{cover_fh} );
		close $session->{self};
		close $session->{cover_fh};
		delete $sessions{$sock};
	}	
	
	close $listeners{$id};
	delete $listeners{$id};
 }

sub createListenPort {
	my $max = shift || 1;
    my $listen;
	my $range = $prefs->get('port_base') ? ($prefs->get('port_range') || 128) : 1;
	my $offset = rand($range);
	my $count;
	
	do {
		$listen = new IO::Socket::INET(
			LocalAddr	=> Slim::Utils::Network::serverAddr(),
			Listen    	=> $max,
			ReuseAddr	=> 1,
			Proto    	=> 'tcp',
			LocalPort	=> ($prefs->get('port_base') || 0) + (($offset + $count++) % $range),
		);
	} until	($listen || $count >= $range);

	if ($listen) {
		$log->info( "Created listener on port ", $listen->sockport );
	} else {
		$log->error( "Listener creation failed!: $!" );
	}	
	
    return $listen;
}

sub revoke_publishPlayers {
	foreach my $id (keys %daemons) {
		next unless defined $daemons{$id};
		$log->info( "Stop old publish players services:", $daemons{$id}->pid()  );
		$daemons{$id}->die();
    }
}

sub publishPlayer {
    my ( $apname, $password, $port ) = @_;

    my $pw_clause = ( length $password ) ? "pw=true" : "pw=false";
    my @hw_addr = +( map( ord, split( //, md5( encode('utf8', $apname) ) ) ) )[ 0 .. 5 ];
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
		$mDNShelper = $path;
        eval { $proc = Proc::Background->new( $path, $id, "_raop._tcp", $port, @params ); };
		return $proc if $proc;
        $log->warn( "start avahi-publish-service failed" );
	}	
	$log->info( "avahi-publish-player not in path" ) if (!$@);
    
	if ( $path = which('dns-sd') ) {
        $log->info( "start dns-sd \"$apname\"" );
		$mDNShelper = $path;
		eval { $proc = Proc::Background->new( $path, '-R', $id, "_raop._tcp", ".", @params ); };
		return $proc if $proc;
        $log->warn( "start dns-sd failed" );
    }
	$log->info( "dns-sd not in path" ) if (!$@);
        
    if ( $path = which('mDNSPublish') ) {
        $log->info( "start mDNSPublish \"$apname\"" );
		$mDNShelper = $path;
        eval { $proc = Proc::Background->new($path, $id, "_raop._tcp", @params ); };
		return $proc if $proc;
        $log->warn( "start mDNSPublish failed" );
    }
    $log->info( "mDNSPublish not in path" ) if (!$@);
        
    $log->info("using built-in helper: $shairtunes_helper");
	
	$mDNShelper = $shairtunes_helper;
	eval { $proc = Proc::Background->new( $shairtunes_helper, "-mdns", "host", Slim::Utils::Network::serverAddr(), $id, "_raop._tcp", @params ); };
	return $proc if $proc;
	$log->error( "start $shairtunes_helper failed" ) if (!$@);
	
    return undef;
}

sub handleSocketConnect {
    my $socket = shift;
	my ($id) = grep { $listeners{$_} eq $socket } keys %listeners;
	my $player = Slim::Player::Client::getClient($id);

    my $new = $socket->accept;
	
	$log->info( "New connection from: ", $new->peerhost);
	
    # set socket to unblocking mode => 0
    Slim::Utils::Network::blocking( $new, 0 );
    $sessions{$new} = { self => $new, 
						   socket => $socket, player => $player,
						   cover => '', cover_fh => createListenPort(5),	
						   remote => undef, DACPid => 'none',
						   volume => -1,
						};
	
    # Add us to the select loop so we get notified
    Slim::Networking::Select::addRead( $new, \&handleSocketRead );
		
	# Add us to the select loop so we get notified
    Slim::Networking::Select::addRead( $sessions{$new}->{cover_fh}, \&handleCoverConnect );
	
	#subscribe to volume changes
	Slim::Control::Request::subscribe( \&volumeChange, [['mixer'], ['volume']], $player );
}

sub handleCoverConnect {
    my $socket = shift;
    
    my $new = $socket->accept;
	Slim::Utils::Network::blocking( $new, 0 );
	
	my ($slave) = grep { $sessions{$_}->{cover_fh} == $socket } keys %sessions;
	$covers{$new}->{image} = $sessions{$slave}->{cover};
	$covers{$new}->{type} = $sessions{$slave}->{coverType};
				
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
	$resp->content_type($covers{$socket}->{type});
    $resp->header( 'Content-Length' => length $covers{$socket}->{image} );
	$resp->header( 'Connection' => 'close');
	$resp->content( $covers{$socket}->{image} );
	
	my $data = $resp->as_string("\r\n");
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
	
	my ($slave) = grep { $sessions{$_}->{decoder_ipc} == $socket } keys %sessions;
	my $line = _readline($socket);
	
	if (!defined $line) {
		$log->error("helper crash");
		cleanHelper($sessions{$slave}) if defined $slave;
		Slim::Networking::Select::removeRead( $socket ) if !defined $slave;
		return;
	}
	
	$log->info("From helper: ", $line);
	
	if ($line =~ /play/) {
		$sessions{$slave}->{player}->execute( ['play'] );
	} elsif ($line =~ /flushed/) {
		$sessions{$slave}->{player}->execute( ['stop'] );
	}	
}	

sub handleSocketRead {
    my $socket = shift;
                           
    my $session = $sessions{$socket};
	my $contentLength = 0;
    my $buffer        = "";
    my $bytesToRead = 1024;
	
	# read header
    while ( 1 ) {
		my $ret = sysread( $socket, my $incoming, $bytesToRead );
		next if !defined $ret && ($! == EAGAIN || $! == EWOULDBLOCK);
		
		$log->error( "Reading socket failed!: $!" ) if !defined $ret;
        last if !$ret;    # ERROR or EOF
		
        $buffer .= $incoming;

        last if ( $incoming =~ /\r\n\r\n/ );
    }
	
	if (!$buffer) {
        $log->info( "Closed: " . $socket );
		
		Slim::Networking::Select::removeRead( $socket );
		Slim::Networking::Select::removeRead( $session->{cover_fh} );
		close $socket;
		close $session->{cover_fh};
		delete $sessions{$socket};
		
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
		
        $log->error( "Reading socket failed!: $!" ) if !defined $ret;
        last if !$ret;    # ERROR or EOF
			
        $contentBody .= $incoming;
        $bytesToRead = $contentLength - length( $contentBody );
    }

    ### START: Not yet updated.
    $session->{req} = HTTP::Request->parse( $header );
    $session->{req}->content( $contentBody );

    conn_handle_request( $socket, $session );
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
	my $session = shift;

	if (defined $session->{decoder_ipc} && $session->{decoder_ipc}->connected) { 
        $session->{player}->execute( ['stop'] );
		
        # read all left output
        my $dfh = $session->{decoder_ipc};
        while ( my $out = _readline($dfh) ) { $log->info( "Decoder output: " . $out ); }

        Slim::Networking::Select::removeRead( $session->{decoder_ipc} );
		close $session->{decoder_ipc};
				
        if ( $session->{poweredoff} ) {
            $session->{player}->execute( [ 'power', 0 ] );
        }
		
		$session->{decoder_pid}->die;
		
		# disconnect from volume 
		Slim::Control::Request::unsubscribe( \&volumeChange, Slim::Player::Client::getClient($session->{id}) );
	}	
}

sub conn_handle_request {
    my ( $socket, $session ) = @_;

    my $req  = $session->{req};
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

		my @hw_addr = +( map( ord, split( //, md5( encode('utf8', $session->{player}->name()) ) ) ) )[ 0 .. 5 ];

        $data .= join '', map { chr } @hw_addr;
        $data .= chr( 0 ) x ( 0x20 - length( $data ) );
		
        # this isn't hashed before signing
		my $signature = encode_base64(rsa_private_encrypt_v15($rsa, $data), ''); 
		$signature =~ s/=*$//;
        $resp->header( 'Apple-Response', $signature );
    }

    if ( defined $session->{password} && length $session->{password} ) {
        if ( !Plugins::ShairTunes2W::Utils::digest_ok( $req, $session ) ) {
            my $nonce = md5_hex( map { rand } 1 .. 20 );
            $session->{nonce} = $nonce;
            my $apname = $session->{player}->name();
            $resp->header( 'WWW-Authenticate', "Digest realm=\"$apname\", nonce=\"$nonce\"" );
            $resp->code( 401 );
            $req->method( 'DENIED' );
        }
    }
	
	my $client = $session->{player};

    for ( $req->method ) {
		$log->debug( "got command / method: $_" );

        /^OPTIONS$/ && do {
            $resp->header( 'Public',
                'ANNOUNCE, SETUP, RECORD, PAUSE, FLUSH, TEARDOWN, OPTIONS, GET_PARAMETER, SET_PARAMETER' );

            if ( !defined $session->{remote} ) {
				$session->{DACPid} = $req->header( 'DACP-ID' );
				$session->{activeRemote} = $req->header( 'Active-Remote' );
				my $mDNSdata = AnyEvent::DNS::dns_pack { rd => 1, qd => [[$DACPfqdn, "ptr"]] };	
				send $mDNSsock, $mDNSdata, 0, sockaddr_in(5353, Socket::inet_aton('224.0.0.251'));
				
				$log->info("DACP-ID: $session->{DACPid}, Active Remote: $session->{activeRemote}");
				$log->debug("Send mDNS data: $DACPfqdn, ", Dumper($mDNSdata));
			}	

            last;
        };

        /^ANNOUNCE$/ && do {
            my $sdp   = Net::SDP->new( $req->content );
            my $audio = $sdp->media_desc_of_type( 'audio' );
			
			my $aesiv = decode_base64( $audio->attribute( 'aesiv' ) );
			my $rsaaeskey = decode_base64( $audio->attribute( 'rsaaeskey' ) );
			my $aeskey;
			            
			if ($rsaaeskey) {
				$aeskey = $rsa->decrypt( $rsaaeskey, 'oaep', 'SHA1', '' ) || do { $log->error( "RSA decrypt failed" ); return; };
			}	

            $session->{aesiv}  = $aesiv;
            $session->{aeskey} = $aeskey;
            $session->{fmtp}   = $audio->attribute( 'fmtp' );
			# there is a bug in some client like AirAudio who puts the target IN addr in the o= fiekd of the SDP session
			$session->{host}   = $sdp->session_origin_address();
			if ($session->{host} eq Slim::Utils::Network::serverAddr() || 
			    $session->{host} eq Socket::inet_ntoa(INADDR_BROADCAST) || 
				$session->{host} eq Socket::inet_ntoa(INADDR_ANY)) {
				$session->{host} = $socket->peerhost();
				$log->info("suspicious peer in SDP, using socket $session->{host}");
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
						Proto     => 'tcp',
						Blocking  => 0, 
					);	

			my $h_ipc  = new IO::Socket::INET(%socket_params);
			IO::Handle::blocking($h_ipc, 0);
			
			my @fmtp = split( / /, $session->{fmtp} );
					
			my @dec_args = (
				host  => $session->{host},
				socket => $h_ipc->sockport, 
                fmtp  => $session->{fmtp},
                cport => $cport,
                tport => $tport,
				latencies => $prefs->get('latency') . ':' . $prefs->get('http_latency') . ($prefs->get('http_fill') ? ':f' : ''),
				codec => $prefs->get('codec'),
                );
				
			push (@dec_args, "ports", $prefs->get("port_base") . ($prefs->get('port_range') ? (':' . $prefs->get('port_range')) : '')) if $prefs->get("port_base");
			push (@dec_args, ("iv", unpack('H*', $session->{aesiv}), "key", unpack('H*', $session->{aeskey}))) if $session->{aesiv} && $session->{aeskey};
			
			if (my $loglevel = $prefs->get('loglevel')) {
				my $id = $client->id;
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
                   
			$session->{decoder_pid} = $helper_pid;
            $session->{decoder_ipc} = $helper_ipc;
            $session->{iport} = $session->{cover_fh}->sockport;	
			
			$log->info(
                "launched decoder: $helper_pid on ports: $helper_ports{port}/$helper_ports{cport}/$helper_ports{tport}/$helper_ports{hport}, http port: $session->{iport}"
            );
					
			my $host = Slim::Utils::Network::serverAddr();
			$session->{url}  = "airplay://$host:$helper_ports{hport}/" . $session->{decoder_pid}->pid . "_stream." . substr($prefs->get('codec'), 0, 3);
								
			# Add out to the select loop so we get notified of play after flush (pause)
			Slim::Networking::Select::addRead( $helper_ipc, \&handleHelperOut );

			$resp->header( 'Transport', "RTP/AVP/UDP;unicast;mode=record;control_port=$helper_ports{cport};timing_port=$helper_ports{tport};server_port=$helper_ports{port}" );
						
            last;
        };

        /^RECORD$/ && do {
			# save current playlist
			$client->pluginData(playlist => {
					playlist 	=> [ @{$client->playlist} ],
					shufflelist => [ @{$client->shufflelist} ],
					index   	=> Slim::Player::Source::streamingSongIndex($client),
					shuffle  	=> $sprefs->client($client)->get('shuffle'),
					repeat		=> $sprefs->client($client)->get('repeat'),
			} );

			# set empty metadata in master
			$client->master->pluginData(metadata => {
						artist   => "ShairTunes Artist",
						title    => "ShairTunes Title",
						album    => "ShairTunes Album",
						bitrate  => $samplingRate . " Hz",
						type     => 'ShairTunes Stream, ' . $prefs->get('codec'),
			} );			

			# save volume 
			$client->pluginData(volume => $sprefs->client($client)->get('volume'));
			
			$log->info( "Playing url: $session->{url}" );
			
            $session->{poweredoff} = !$session->{player}->power;
			$client->execute( [ 'playlist', 'load', $session->{url} ] );
			
			$resp->header( 'Audio-Latency', '44100' );
			
			my ($seqno, $rtptime) = (0, 0);
			($seqno, $rtptime) = $req->header('RTP-Info') =~ m|seq=([^;]+);rtptime=([^;]+)|i if $req->header('RTP-Info');
			send ($session->{decoder_ipc}, "record $seqno $rtptime\n", 0);
			$log->info("Record from $seqno, $rtptime");

            last;
        };
		
        /^FLUSH$/ && do {

			my ($seqno, $rtptime) = $req->header('RTP-Info') =~ m|seq=([^;]+);rtptime=([^;]+)|i;
			send ($session->{decoder_ipc}, "flush $seqno $rtptime\n", 0);
			$log->info("Flush up to $seqno, $rtptime");
            
            last;
        };
		
        /^TEARDOWN$/ && do {
			$resp->header( 'Connection', 'close' );
					
			cleanHelper($session);
			
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
			
			# restore volume
			$client->execute( [ 'mixer', 'volume', $client->pluginData('volume') ] );
			
			last;
        };
		
        /^SET_PARAMETER$/ && do {
			my $metadata = $client->master->pluginData('metadata');
			
            if ( $req->header( 'Content-Type' ) eq "text/parameters" ) {
                my @lines = split( /[\r\n]+/, $req->content );
                my %content = map { /^(\S+): (.+)/; ( lc $1, $2 ) } @lines;
                my $cfh = $session->{decoder_ipc};
				
				$log->debug( "SET_PARAMETER req: " . $req->content );
				
                if ( exists $content{volume} ) {
                    my $volume = $content{volume};
                    my $percent = int( 100 + ( $volume * 99 / 30) + 0.5 );
					$percent = 0 if $percent < 0;
					
                   	# synchronize volume of other players, 		
					if ($prefs->get('syncVolume') && !$client->controller->isa("Plugins::Groups::StreamingController")) {
						my @otherclients = grep { $_->name ne $client->name and $_->power and !$sprefs->client($_)->get('syncVolume') } $client->syncGroupActiveMembers();
						foreach my $otherclient ( @otherclients ) {
							my $volume;
							
							if ( $client->volume ) {
								$volume = ceil( $otherclient->volume * $percent / $client->volume );
							} else {
								$volume = $percent; 
							}
							
							$otherclient->execute( [ 'mixer', 'volume', $volume ] );
							$log->info("Changing volume on ", $otherclient->name, " volume:$volume");
						}
					}	
					
					$log->info( "sending-> vol: " . $percent );
					
					$client->execute( [ 'mixer', 'volume', $percent ] );
					$session->{volume} = $percent;
	            } elsif ( exists $content{progress} ) {
                    my ( $start, $curr, $end ) = split( /\//, $content{progress} );
                    my $position = ( $curr - $start ) / $samplingRate;
                    my $duration = ( $end - $start ) / $samplingRate;

					# this is likely a bridge, so duration shall be set to 0 (live stream)
					$duration = 0 if $client->modelName =~ /Bridge/ || ($client->model =~ /squeezelite/ && !$client->firmware);
					$metadata->{duration} = $duration;

					# the song might not be valid yet, so wait a bit (can't find a better solution)
					Slim::Utils::Timers::setTimer(undef, time() + 2, sub { 
											my $song = $client->playingSong || $client->streamingSong;		
											$song->duration( $duration );
											$song->startOffset( $position - $client->master->songElapsedSeconds + 1 );
											Slim::Control::Request::notifyFromArray($client, ['newmetadata']);	
										} );
																				
                    $log->debug( "Duration: " . $duration . "; Position: " . $position );
                } else {
                    $log->error( "unable to perform content for req SET_PARAMETER text/parameters: " . $req->content );
                }
            } elsif ( $req->header( 'Content-Type' ) eq "application/x-dmap-tagged" ) {
				my %dmapData = Plugins::ShairTunes2W::Utils::getDmapData( $req->content );
                $metadata->{artist} = $dmapData{artist};
                $metadata->{album}  = $dmapData{album};
                $metadata->{title}  = $dmapData{title};
				
				Slim::Music::Info::setCurrentTitle($session->{url}, $metadata->{title}, $client);

                $log->debug( "DMAP DATA found. Length: " . length( $req->content ) . " " . Dumper( \%dmapData ) );    
			} elsif ( $req->header( 'Content-Type' ) =~ /image\/(.+)/ ) {
				my $ext = $1;
				my $host = Slim::Utils::Network::serverAddr();
				my $url  = "http://$host:$session->{iport}/" . md5_hex($req->content) . "/cover.$ext";
				
				$metadata->{cover} = $url;
				$metadata->{icon} = $url;
				$session->{cover} = $req->content;
				$session->{coverType} = $req->header( 'Content-Type' );
				
				$client->master->currentPlaylistUpdateTime( Time::HiRes::time() );
				Slim::Control::Request::notifyFromArray( $client->master, ['newmetadata'] );  
				      								
				$log->debug( "IMAGE DATA received, sending to: ", $url );
			} elsif ( $req->header( 'Content-Type' ) eq "image/none" ) {
            	$metadata->{cover} = '';
				$metadata->{icon} = '';
				
				$client->master->currentPlaylistUpdateTime( Time::HiRes::time() );
				Slim::Control::Request::notifyFromArray( $client->master, ['newmetadata'] );  
			} else {
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
		 
	my @answers = (@{$res->{an}}, @{$res->{ar}});
	
	foreach my $socket (keys %sessions) {
		my $session = $sessions{$socket};
		next if defined $session->{remote};
		
		my $DACPid = $session->{DACPid};
		
		my ($service) = grep { $_->[1] eq 'srv' && $_->[0] =~ /$DACPid/ } @answers;
		return if !defined $service;
		
		my ($addr) = grep { $_->[1] eq 'a' && $_->[0] eq $service->[6] } @answers;
		return if !defined $addr;
		
		$session->{remote} = "$addr->[3]:$service->[5]";
		$log->info("Found remote: $DACPid, $session->{remote}");
	}	
}

# === PKCS#1 v1.5 "private_encrypt" (NO hash), OpenSSL-compatible ===
# because for sure Apple uses private encrypt that is not supported in CryptX
sub rsa_private_encrypt_v15 {
  my ($key, $data) = @_;

  my $h = $key->key2hash;
  my $n = Math::BigInt->from_hex($h->{N});
  my $d = Math::BigInt->from_hex($h->{d});

  my $len = (length($h->{N}) / 2) - 3 - length($data);
  my $bytes = "\x00\x01" . ("\xFF" x $len) . "\x00" . $data;  # 00 01 FF..FF 00 || data
  
  # why do we need all that shit of hex, pack and unpack ? Well because we can
  # and to_bytes and to_hex are not properly supported in our CryptX. Of course
  $bytes = unpack("H*", $bytes);

  my $m = Math::BigInt->from_hex($bytes);
  # private exponentiation
  my $s = $m->bmodpow($d, $n);
 
  (my $res = $s->as_hex()) =~ s/^0x//i; 
  return pack("H*", $res);
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
