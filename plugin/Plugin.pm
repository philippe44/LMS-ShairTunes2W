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

use constant CAN_IMAGEPROXY => ( Slim::Utils::Versions->compareVersions( $::VERSION, '7.8.0' ) >= 0 );

# create log categogy before loading other modules
my $log = Slim::Utils::Log->addLogCategory(
    {
        'category'     => 'plugin.shairtunes',
        'defaultLevel' => 'INFO',
        'description'  => getDisplayName(),
    }
);

my $cover_cache = '';
my $cachedir    = preferences( 'server' )->get( 'cachedir' );
if ( !-d File::Spec->catdir( $cachedir, "shairtunes" ) ) {
    mkdir( File::Spec->catdir( $cachedir, "shairtunes" ) );
}

my $prefs         = preferences( 'plugin.shairtunes' );
my $hairtunes_cli = "";

my $airport_pem = _airport_pem();
my $rsa         = Crypt::OpenSSL::RSA->new_private_key( $airport_pem )
  || do { $log->error( "RSA private key import failed" ); return; };

my $samplingRate = 44100;  
  
my %clients     = (); # ( [ LMSclient ]      => [mDNSPID],       ...)
my %sockets     = (); # ( [ LMSclient ]      => [masterINETsock], ...)
my %players     = (); # ( [ masterINETsock ] => [LMSclient],      ...)
my %connections = (); # ( [ slaveINETSock ]  => ('socket' => [MasterINETSock], 'player' => [LMSclient],
					  #							 'decoder_fh' => [INETSockIn], decoder_fhout' => [INETSockOut], 
					  #							 'decoder_ferr' => [INETSockErr], 'decoder_pid' => [helper]

sub getAirTunesMetaData {
	my $class  = shift;
	my $client = shift;
	
	my ($slave) = grep { $connections{$_}->{player} == $client } keys %connections;
	
	return undef if !defined $slave;
	
	return $connections{$slave}->{metaData};
}

sub initPlugin {
    my $class = shift;

    $log->info( "Initialising " . $class->_pluginDataFor( 'version' ) . " on " . $Config{'archname'} );
	
	my $shairtunes_helper = Plugins::ShairTunes2::Utils::helperBinary();
	$log->error($shairtunes_helper);

    revoke_publishPlayer();

    # Subscribe to player connect/disconnect messages
    Slim::Control::Request::subscribe( \&playerSubscriptionChange,
        [ ['client'], [ 'new', 'reconnect', 'disconnect' ] ] );

    if ( CAN_IMAGEPROXY ) {
        require Slim::Web::ImageProxy;
        Slim::Web::ImageProxy->import();
        Slim::Web::ImageProxy->registerHandler(
            match => qr/shairtunes:image:/,
            func  => \&_getcover,
        );
    }
    else {
        $log->error( "Imageproxy not supported! Covers disabled!" );
    }

    return 1;
}

sub getDisplayName {
    return ( 'PLUGIN_SHAIRTUNES2' );
}

sub shutdownPlugin {
    revoke_publishPlayer();
    return;
}

sub _getcover {
    my ( $url, $spec, $cb ) = @_;

    # $url is aforementioned image URL
    # $spec we don't need (yet)
    # $cb is the callback to be called with the URL

    my ( $track_id ) = $url =~ m|shairtunes:image:(.*?)$|i;

    my $imagefilepath = File::Spec->catdir( $cachedir, 'shairtunes', $track_id . "_cover.jpg" );

    $log->debug( "_getcover called for $imagefilepath" );

    # now return the URLified file path
    return Slim::Utils::Misc::fileURLFromPath( $imagefilepath );
}

sub playerSubscriptionChange {
    my $request = shift;
    my $client  = $request->client;

    my $reqstr     = $request->getRequestString();
    my $clientname = $client->name();

    $log->debug( "request=$reqstr client=$clientname" );
	
	return if ($client->modelName() =~ /Squeeze2Raop/);

    if ( ( $reqstr eq "client new" ) || ( $reqstr eq "client reconnect" ) ) {
        $sockets{$client} = createListenPort();
        $players{ $sockets{$client} } = $client;

        if ( $sockets{$client} ) {

            # Add us to the select loop so we get notified
            Slim::Networking::Select::addRead( $sockets{$client}, \&handleSocketConnect );

            $clients{$client} = publishPlayer( $clientname, "", $sockets{$client}->sockport() );
			$log->error("create client $client with proc $clients{$client}");
        }
        else {
            $log->error( "could not create ShairTunes socket for $clientname" );
            delete $sockets{$client};
        }
    }
    elsif ( $reqstr eq "client disconnect" ) {
        $log->debug( "publisher for $clientname PID $clients{$client} will be terminated." );
        $clients{$client}->die();
        Slim::Networking::Select::removeRead( $sockets{$client} );
    }
}

sub createListenPort {
    my $listen;

=comment	
    $listen = new IO::Socket::INET6(
        Listen    => 1,
        Domain    => AF_INET6,
        ReuseAddr => 1,
        Proto     => 'tcp',
    );
=cut	

    $listen = new IO::Socket::INET(
        Listen    => 1,
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
        
    my $shairtunes_helper = Plugins::ShairTunes2::Utils::helperBinary();
	$log->error("SHAIRTUNES2 helper: $shairtunes_helper");
	eval { $proc = Proc::Background->new( $shairtunes_helper, "-dns", $id, "_raop._tcp", @params ); };
	return $proc unless ($@);
	$log->error( "start $shairtunes_helper failed" ) if (!$@);
	
    return undef;
}

sub handleSocketConnect {
    my $socket = shift;
    my $player = $players{$socket};

    my $new = $socket->accept;
    $log->info( "New connection from " . $new->peerhost );

    # set socket to unblocking mode => 0
    Slim::Utils::Network::blocking( $new, 0 );
    $connections{$new} = { socket => $socket, player => $player,
						   metaData => { artist   => "ShairTunes Artist",
										 title    => "ShairTunes Title",
										 album    => "ShairTunes Album",
										 bitrate  => $samplingRate . " Hz",
										 cover    => "",
										 duration => 0,
										 position => 0,
										 offset   => 0,
										}, 
						};
	
    # Add us to the select loop so we get notified
    Slim::Networking::Select::addRead( $new, \&handleSocketRead );
}

sub handleHelperOut {
    my $socket = shift;
	
	my ($slave) = grep { $connections{$_}->{decoder_fhout} == $socket } keys %connections;
	my $line = _readline($socket);
	
	if (!defined $line) {
		$log->error("helper crash");
		cleanHelper($connections{$slave}) if defined $slave;
		return;
	}
	
	$log->debug("From helper: ", $line);
	
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
        $log->debug( "Closed: " . $socket );
		
		Slim::Networking::Select::removeRead( $socket );
		close $socket;
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
		$log->error("SIGN: $ip, $signature");
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

            my $shairtunes_helper = Plugins::ShairTunes2::Utils::helperBinary();

            if ( !$shairtunes_helper || !-x $shairtunes_helper ) {
                $log->error( "I'm sorry your platform \""
                      . $Config{archname}
                      . "\" is unsupported or nobody has compiled a binary for it! Can't work." );
                last;
            }

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
                        $log->error( $line ) and next if ( $line =~ /^shairport_helper: / );
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
       
			$log->info(
                "launched decoder: $helper_pid on ports: $helper_ports{port}/$helper_ports{cport}/$helper_ports{tport}/$helper_ports{hport}"
            );

			$conn->{decoder_pid}   = $helper_pid;
            $conn->{decoder_fh}    = $helper_in;
            $conn->{decoder_fherr} = $helper_err;
			$conn->{decoder_fhout} = $helper_out;
			
			# Add out to the select loop so we get notified of play after flush (pause)
			Slim::Networking::Select::addRead( $helper_out, \&handleHelperOut );

			#$resp->header( 'Transport', $req->header( 'Transport' ) . ";server_port=$helper_ports{port}" );
			$resp->header( 'Transport', "RTP/AVP/UDP;unicast;mode=record;control_port=$helper_ports{cport};timing_port=$helper_ports{tport};server_port=$helper_ports{port}" );
			
            my $host         = Slim::Utils::Network::serverAddr();
            my $url          = "airplay://$host:$helper_ports{hport}/stream.wav";
			my $client       = $conn->{player};
            my @otherclients = grep { $_->name ne $client->name and $_->power } $client->syncGroupActiveMembers();
            foreach my $otherclient ( @otherclients ) {
                $log->info( 'turning off: ' . $otherclient->name );
                $otherclient->display->showBriefly(
                    { line => [ 'AirPlay streaming to ' . $client->name . ':', 'Turning this player off' ] } );
                $otherclient->execute( [ 'power', 0 ] );
            }
            $conn->{poweredoff} = !$client->power;
			$conn->{metaData}->{offset} = 0;
			$conn->{player}->execute( [ 'playlist', 'play', $url ] );
			
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

            # this is pause at airplay - but only stop also flushed the buffer at the player
            # so if you press skip you won't hear the old song
            # also double FLUSH won't result in play again (like on skip)
            # 
			my $dfh = $conn->{decoder_fh};
            send ($dfh, "flush\n", 0);

			$conn->{metaData}->{offset} = 0;
			$log->debug(Dumper($conn->{metaData}));
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

                    my $client = $conn->{player};
					$client->currentPlaylistUpdateTime( Time::HiRes::time() );
                    Slim::Control::Request::notifyFromArray( $client, ['newmetadata'] );
                    $client->execute( ['play'] );

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

                my $hashkey       = Plugins::ShairTunes2::Utils::imagekeyfrommeta( $metaData );
                my $imagefilepath = File::Spec->catdir( $cachedir, 'shairtunes', $hashkey . "_cover.jpg" );
                my $imageurl      = "/imageproxy/shairtunes:image:" . $hashkey . "/cover.jpg";
                if ( length $cover_cache ) {
                    open( my $imgFH, '>' . $imagefilepath );
                    binmode( $imgFH );
                    print $imgFH $cover_cache;
                    close( $imgFH );

                    $log->debug( "IMAGE DATA COVER_CACHE found. " . $imagefilepath . " " . $imageurl );

                    $metaData->{cover}        = $imageurl;
                    $cover_cache                    = '';
                    $metaData->{waitforcover} = 0;
                }
                elsif ( -e $imagefilepath ) {
                    $metaData->{cover}        = $imageurl;
                    $metaData->{waitforcover} = 0;
                }
                else {
                    $metaData->{waitforcover} = 1;
                    $metaData->{cover}        = '';
                }

                my $client = $conn->{player};
				$client->currentPlaylistUpdateTime( Time::HiRes::time() );
				$metaData->{offset} = $client->songElapsedSeconds();
				$log->debug(Dumper($conn->{metaData}));
                Slim::Control::Request::notifyFromArray( $client, ['newmetadata'] );
            }
            elsif ( $req->header( 'Content-Type' ) eq "image/jpeg" ) {
				my $metaData = $conn->{metaData};
                if ( $metaData->{waitforcover} && length $metaData->{title} ) {
                    $cover_cache = '';
                    $metaData->{waitforcover} = 0;

                    my $hashkey       = Plugins::ShairTunes2::Utils::imagekeyfrommeta( $metaData );
                    my $imagefilepath = File::Spec->catdir( $cachedir, 'shairtunes', $hashkey . "_cover.jpg" );
                    my $imageurl      = "/imageproxy/shairtunes:image:" . $hashkey . "/cover.jpg";

                    open( my $imgFH, '>' . $imagefilepath );
                    binmode( $imgFH );
                    print $imgFH $req->content;
                    close( $imgFH );

                    $log->debug( "IMAGE DATA found. " . $imagefilepath . " " . $imageurl );

                    $metaData->{cover} = $imageurl;

                    my $client = $conn->{player};
					$client->currentPlaylistUpdateTime( Time::HiRes::time() );
                    Slim::Control::Request::notifyFromArray( $client, ['newmetadata'] );
                }
                else {
                    $log->debug( "IMAGE DATA CACHED" );
                    $cover_cache = $req->content;
                }
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

    $log->error("\n\nPLAYER_MESSAGE_START: \n" .$resp->as_string("\r\n"). "\nPLAYER_MESSAGE_END\n\n");

    print $socket $resp->as_string( "\r\n" );
    $socket->flush;

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

package Tie::Restore;

our $VERSION = 0.11;

sub TIESCALAR { $_[1] }
sub TIEARRAY  { $_[1] }
sub TIEHASH   { $_[1] }
sub TIEHANDLE { $_[1] }

1;
