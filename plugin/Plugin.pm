package Plugins::ShairTunes2::Plugin;

use strict;
use warnings;

use Plugins::ShairTunes2::AIRPLAY;
use Plugins::ShairTunes2::Utils;

use base qw(Slim::Plugin::OPMLBased);

use File::Spec::Functions;
use FindBin qw($Bin);
use lib catdir($Bin, 'Plugins', 'ShairTunes2', 'lib');

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
use POSIX qw(:errno_h);
use Data::Dumper;

#use IO::Socket::INET6;
use IO::Socket::INET;
use Crypt::OpenSSL::RSA;
use Net::SDP;
use IPC::Open3;
use IO::Handle;

# create log categogy before loading other modules
my $log = Slim::Utils::Log->addLogCategory(
    {
        'category'     => 'plugin.shairtunes',
        'defaultLevel' => 'INFO',
        'description'  => getDisplayName(),
    }
);

my $prefs         = preferences( 'plugin.shairtunes' );
my $shairtunes_helper;
my $DACPfqdn = "_dacp._tcp.local";
my $mDNSsock;

my $airport_pem = _airport_pem();
my $rsa         = Crypt::OpenSSL::RSA->new_private_key( $airport_pem )
  || do { $log->error( "RSA private key import failed" ); return; };

my $samplingRate = 44100;  
  
my %clients     = (); # ( [ LMSclient ]      => [mDNSPID],       ...)
my %sockets     = (); # ( [ LMSclient ]      => [masterINETsock], ...)
my %players     = (); # ( [ masterINETsock ] => [LMSclient],      ...)
my %connections = (); # ( [ slaveINETSock ]  => ('socket' => [MasterINETSock], 'player' => [LMSclient],
					  #							 'decoder_fh' => [INETSockIn], decoder_fhout' => [INETSockOut], 
					  #							 'decoder_ferr' => [INETSockErr], 'decoder_pid' => [helper],
					  #							 'poweroff', 'iport' => [image proxy]
					  #							 'cover_fh' => [INETSockIn], 'cover' => [jpeg blob]
					  #							 'DACPid' => [DACP ID], 'activeRemote' => [remote ID], 
					  #						     'remote' => [IP::port of remote]	


sub getAirTunesMetaData {
	my $class  = shift;
	my $url = shift;
	my $slave;
	
	($slave) = grep { $connections{$_}->{url} eq $url } keys %connections;
	
	return  { artist   => "ShairTunes Artist",
			  title    => "ShairTunes Title",
			  album    => "ShairTunes Album",
			  bitrate  => 44100 . " Hz",
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
		$log->debug("Player matching: p: $conn->{player}, c: $client");
	}
	
	my ($slave) = grep { $connections{$_}->{player} == $client } keys %connections;
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

    $log->info( "Initialising " . $class->_pluginDataFor( 'version' ) . " on " . $Config{'archname'} );
	
	$shairtunes_helper = Plugins::ShairTunes2::Utils::helperBinary();
	if ( !$shairtunes_helper || !-x $shairtunes_helper ) {
        $log->error( "I'm sorry your platform \"" . $Config{archname}
                      . "\" is unsupported or nobody has compiled a binary for it! Can't work." );
        return 0;        
    }
	
	$log->info($shairtunes_helper);

    revoke_publishPlayer();

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

sub shutdownPlugin {
    revoke_publishPlayer();
	Slim::Networking::Select::removeRead( $mDNSsock );
	close($mDNSsock) if defined $mDNSsock;
    return;
}

sub playerSubscriptionChange {
    my $request = shift;
    my $client  = $request->client;

    my $reqstr     = $request->getRequestString();
    my $clientname = $client->name();

    $log->debug( "request=$reqstr client=$clientname" );
	
	return if ($client->modelName() =~ /SqueezeLite/ && !$client->firmware);

    if ( ( $reqstr eq "client new" ) || ( $reqstr eq "client reconnect" ) ) {
        $sockets{$client} = createListenPort(1);
        $players{ $sockets{$client} } = $client;

        if ( $sockets{$client} ) {

            # Add us to the select loop so we get notified
            Slim::Networking::Select::addRead( $sockets{$client}, \&handleSocketConnect );

            $clients{$client} = publishPlayer( $clientname, "", $sockets{$client}->sockport() );
			$log->info("create client $client with proc $clients{$client}");
        }
        else {
            $log->error( "could not create ShairTunes socket for $clientname" );
            delete $sockets{$client};
        }
    }
    elsif ( $reqstr eq "client disconnect" ) {
        $log->info( "publisher for $clientname PID $clients{$client} will be terminated." );
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
}

sub createListenPort {
	my $max = shift;
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
        Listen    => $max || 1,
        ReuseAddr => 1,
		Proto     => 'tcp',
    );

    if ( !$listen ) {
        $log->error( "Socket creation failed!: $!" );
    }

    return $listen;
}

sub revoke_publishPlayer {

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
		return $proc unless ($@);
        $log->error( "start avahi-publish-service failed" );
	}	
	$log->info( "avahi-publish-player not in path" ) if (!$@);
    
	if ( $path = which('dns-sd') ) {
        $log->info( "start dns-sd \"$apname\"" );
		eval { $proc = Proc::Background->new( $path, '-R', $id, "_raop._tcp", ".", @params ); };
		return $proc unless ($@);
        $log->error( "start dns-sd failed" );
    }
	$log->info( "dns-sd not in path" ) if (!$@);
        
    if ( $path = which('mDNSPublish') ) {
        $log->info( "start mDNSPublish \"$apname\"" );
        eval { $proc = Proc::Background->new($path, $id, "_raop._tcp", @params ); };
		return $proc unless ($@);
        $log->error( "start mDNSPublish failed" );
    }
    $log->info( "mDNSPublish not in path" ) if (!$@);
        
    $log->info("using built-in helper: $shairtunes_helper");
	eval { $proc = Proc::Background->new( $shairtunes_helper, "-dns", $id, "_raop._tcp", @params ); };
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
							cover_fh => createListenPort(5),	
							cover => '', url => '',
							remote => undef, DACPid => 'none',
						};
	
    # Add us to the select loop so we get notified
    Slim::Networking::Select::addRead( $new, \&handleSocketRead );
		
	# Add us to the select loop so we get notified
    Slim::Networking::Select::addRead( $connections{$new}->{cover_fh}, \&handleCoverConnect );
}

sub handleCoverConnect {
    my $socket = shift;
    #my $player = $players{$socket};

    my $new = $socket->accept;
	Slim::Utils::Network::blocking( $new, 0 );
	
	my ($slave) = grep { $connections{$_}->{cover_fh} == $socket } keys %connections;
	$connections{$slave}->{cover_fhd} = $new;
	
    $log->info( "New cover proxy connection from " . $new->peerhost );
	
	Slim::Networking::Select::addRead( $new, \&handleCoverRequest);
}

sub handleCoverRequest {
	my $socket = shift;
	
	while (my $line = _readline($socket)) {
		$log->debug("Image proxy request: $line");
	}
	
	my ($slave) = grep { $connections{$_}->{cover_fhd} == $socket } keys %connections;
	my $resp = HTTP::Response->new( 200 );
	
	$resp->protocol('HTTP/1.1');
	$resp->user_agent('ShairTunes2');
	$resp->content_type('image/jpeg');
    $resp->header( 'Content-Length' => length $connections{$slave}->{cover} );
	$resp->header( 'Connection' => 'close');
	$resp->content( $connections{$slave}->{cover} );
	
	my $data = $resp->as_string();
	my $sent = 0;
	while ($sent < length $data) {
		my $bytes = send ($socket, substr($data, $sent), 0);
		last if !defined $bytes && $! != EAGAIN && $! != EWOULDBLOCK;
		$sent += $bytes;
	}
			
	Slim::Networking::Select::removeRead( $socket );
	$socket->shutdown(1);
	while (1) {
		my $bytes = sysread($socket, my $c, 16);
		next if !defined $bytes && ($! == EAGAIN || $! == EWOULDBLOCK);
		last if !$bytes;
	}	
	
	$log->debug("Coverart sent $sent over ", length $data);
	close $socket;
}

sub handleHelperOut {
    my $socket = shift;
	
	my ($slave) = grep { $connections{$_}->{decoder_fhout} == $socket } keys %connections;
	my $line = _readline($socket);
	
	if (!defined $line) {
		$log->error("helper crash");
		cleanHelper($connections{$slave}) if defined $slave;
		Slim::Networking::Select::removeRead( $socket ) if !defined $slave;
		return;
	}
	
	$log->info("From helper: ", $line);
	
    $connections{$slave}->{player}->execute( ['play'] ) if ($line =~ /play/);
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

sub acceptChildSockets {   
	my @sockets = @_;
	
	foreach my $socket (@sockets)	{
		IO::Handle::blocking($socket, 0);
		$socket->accept;
	}	
}

sub closeSockets {
	my @sockets = @_;
	
	foreach my $socket (@sockets)	{
		$socket->close;
	}	
}	

sub waitChildConnect {   
	my @sockets = @_;
	my %status;
	my $sel = IO::Select->new();
	
	foreach my $socket (@sockets)	{
		$sel->add($socket);	
		$status{$socket} = undef;
	}	
	
	while (1) {
		my @connects = $sel->can_read(5);
		
		last if !@connects;
		
		foreach my $connect (@connects) {
			$status{$connect} = $connect->accept;
			IO::Handle::blocking($status{$connect}, 0);
			$sel->remove($connect);
		}
		
		last if !grep { defined $status{$_} } keys %status;
	}	
	
	my @result;
	
	# can't use values %status or keys because of order randomization;
	foreach my $socket (@sockets)	{
		push @result, $status{$socket};
	}

	return @result;
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

	if (defined $conn->{decoder_fh} && $conn->{decoder_fh}->connected) { 
        close $conn->{decoder_fh};
		$conn->{player}->execute( ['stop'] );

        # read all left errors
        my $derr = $conn->{decoder_fherr};
        while ( my $err = _readline($derr) ) { $log->error( "Decoder error: " . $err ); }

        close $conn->{decoder_fherr};
		Slim::Networking::Select::removeRead( $conn->{decoder_fhout} );
		close $conn->{decoder_fhout};
		
        if ( $conn->{poweredoff} ) {
            $conn->{player}->execute( [ 'power', 0 ] );
        }
		
		$conn->{decoder_pid}->die;
	}	
}

sub notifyUpdate {
	my $client = shift;
	my $metaData = shift;
	
	$client->master->playingSong->pluginData( wmaMeta => {
										cover  => $metaData->{cover},
										icon   => $metaData->{icon},
										artist => $metaData->{artist},
										title  => $metaData->{title},
											} );
											
	$client->currentPlaylistUpdateTime( Time::HiRes::time() );
	Slim::Control::Request::notifyFromArray( $client, ['newmetadata'] );  											
	$client->execute( ['play'] );
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
            $data .= Plugins::ShairTunes2::Utils::ip6bin( $ip );
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
        if ( !Plugins::ShairTunes2::Utils::digest_ok( $req, $conn ) ) {
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

            # OPTIONS is called every 2s so ideal to read all errors on a regular basis
	        my $derr = $conn->{decoder_fherr};
			while ( my $err = _readline($derr) ) { $log->error( "Decoder error: " . $err ); }
			
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

			my $h_in  = new IO::Socket::INET(%socket_params);
			my $h_out = new IO::Socket::INET(%socket_params);
			my $h_err = new IO::Socket::INET(%socket_params);
			
			my @dec_args = (
				'ipv4_only',
				socket => join( ',', $h_in->sockport, $h_out->sockport, $h_err->sockport ),
                iv    => unpack( 'H*', $conn->{aesiv} ),
                key   => unpack( 'H*', $conn->{aeskey} ),
                fmtp  => $conn->{fmtp},
                cport => $cport,
                tport => $tport,
                );
			
			$log->debug( "decode command: ", Dumper(@dec_args));
    
			acceptChildSockets($h_in, $h_out, $h_err);

			my $helper_pid = Proc::Background->new( $shairtunes_helper, @dec_args );

			my $sel = IO::Select->new();
			my ($helper_in, $helper_out, $helper_err) = waitChildConnect($h_in, $h_out, $h_err);

			closeSockets($h_in, $h_out, $h_err);

			$sel->add( $helper_out, $helper_err);
			my %helper_ports = ( cport => '', hport => '', port => '', tport => '' );

			my @ready;
            while ( @ready = $sel->can_read( 5 ) ) {
			    foreach my $reader ( @ready ) {
                    while ( defined( my $line = _readline($reader) ) ) {
                        $log->debug( $line ) and next if ( $line =~ /^shairport_helper: / );
						next if ( $line =~ /^shairport_helper: / );
                        if ( $reader == $helper_err ) {
                            $log->error( "Helper error: " . $line );
							next;
                        }
						if ( $line =~ /^([cht]?port):\s*(\d+)/ ) {
                            $helper_ports{$1} = $2;
                            next;
                        }
                        $log->error( "Helper unknown output: " . $line );
					}
                }
                last if ( !grep { !$helper_ports{$_} } keys %helper_ports );
            }
			
            $sel->remove( $helper_out );
            $sel->remove( $helper_err );
       
			$conn->{decoder_pid}   = $helper_pid;
            $conn->{decoder_fh}    = $helper_in;
            $conn->{decoder_fherr} = $helper_err;
			$conn->{decoder_fhout} = $helper_out;
			$conn->{iport}		   = $conn->{cover_fh}->sockport;	
			
			$log->info(
                "launched decoder: $helper_pid on ports: $helper_ports{port}/$helper_ports{cport}/$helper_ports{tport}/$helper_ports{hport}, http port: $conn->{iport}"
            );
					
			my $host = Slim::Utils::Network::serverAddr();
			$conn->{url}  = "airplay://$host:$helper_ports{hport}/" . md5_hex($conn) . "_stream.wav";
					
			# Add out to the select loop so we get notified of play after flush (pause)
			Slim::Networking::Select::addRead( $helper_out, \&handleHelperOut );

			#$resp->header( 'Transport', $req->header( 'Transport' ) . ";server_port=$helper_ports{port}" );
			$resp->header( 'Transport', "RTP/AVP/UDP;unicast;mode=record;control_port=$helper_ports{cport};timing_port=$helper_ports{tport};server_port=$helper_ports{port}" );
			
			$log->debug( "Playing url: $conn->{url}" );
			
            $conn->{poweredoff} = !$conn->{player}->power;
			$conn->{metaData}->{offset} = 0;
			$conn->{metaData}->{type}   = 'ShairTunes Stream';
			$conn->{player}->execute( [ 'playlist', 'play', $conn->{url} ] );
			
            last;
        };

        /^RECORD$/ && do {

			my $client = $conn->{player};
			$client->currentPlaylistUpdateTime( Time::HiRes::time() );
            Slim::Control::Request::notifyFromArray( $conn->{player}, ['newmetadata'] );
            $conn->{player}->execute( ['play'] );

            last;
        };
		
        /^FLUSH$/ && do {

            my $dfh = $conn->{decoder_fh};
			my ($seqno, $rtptime) = $req->header('RTP-Info') =~ m|seq=([^;]+);rtptime=([^;]+)|i;
			$log->debug("Flush up to $seqno, $rtptime");
            send ($dfh, "flush $seqno $rtptime\n", 0);

			$conn->{metaData}->{offset} = 0;
			$conn->{player}->execute( ['stop'] );
            
            last;
        };
		
        /^TEARDOWN$/ && do {
            $resp->header( 'Connection', 'close' );
			
			cleanHelper($conn);
			
			last;
        };
		
        /^SET_PARAMETER$/ && do {
            if ( $req->header( 'Content-Type' ) eq "text/parameters" ) {
                my @lines = split( /[\r\n]+/, $req->content );
                $log->debug( "SET_PARAMETER req: " . $req->content );
                my %content = map { /^(\S+): (.+)/; ( lc $1, $2 ) } @lines;
                my $cfh = $conn->{decoder_fh};
                if ( exists $content{volume} ) {
                    my $volume = $content{volume};
                    my $percent = 100 + ( $volume * 3.35 );

                    $conn->{player}->execute( [ 'mixer', 'volume', $percent ] );

                    $log->debug( "sending-> vol: " . $percent );
                }
                elsif ( exists $content{progress} ) {
					my $metaData = $conn->{metaData};
                    my ( $start, $curr, $end ) = split( /\//, $content{progress} );
                    my $positionRealTime = ( $curr - $start ) / $samplingRate;
                    my $durationRealTime = ( $end - $start ) / $samplingRate;

                    $metaData->{duration} = $durationRealTime;
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
                my %dmapData = Plugins::ShairTunes2::Utils::getDmapData( $req->content );
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
				my $url  = "http://$host:$conn->{iport}/cover" . md5_hex($req->content) . "/image.jpeg";
				$metaData->{cover} = $url;
				$metaData->{icon} = $url;
				$conn->{cover} = $req->content;
				      								
				$log->debug( "IMAGE DATA received, sending to: ", $url );
				
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

our $VERSION = 0.34;

1;
