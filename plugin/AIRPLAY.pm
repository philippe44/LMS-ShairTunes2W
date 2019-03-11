package Plugins::ShairTunes2W::AIRPLAY;

use strict;
use warnings;

use base qw(Slim::Player::Protocols::HTTP);
use List::Util qw(min);
use Slim::Utils::Strings qw(string);
use Slim::Utils::Misc;
use Slim::Utils::Log;
use Slim::Utils::Prefs;

Slim::Player::ProtocolHandlers->registerHandler( 'airplay', __PACKAGE__ );

my $log   = logger( 'plugin.shairtunes' );
my $prefs = preferences( 'plugin.shairtunes' );

sub isRemote { 1 }
sub canSeek { 0 }
sub canHandleTranscode { 0 }
sub isAudioURL { 1 }
sub bufferThreshold { $prefs->get('bufferThreshold') // 255; }

sub canDoAction {
    my ( $class, $client, $url, $action ) = @_;
    
	$log->info( "Action=$action" );

    return Plugins::ShairTunes2W::Plugin->sendAction($client, $action);
}

sub scanUrl {
    my ( $class, $url, $args ) = @_;

	$args->{cb}->( $args->{song}->currentTrack() );
}

sub canDirectStream {
    my ( $class, $client, $url ) = @_;
	
	return 0 if $client->isSynced(1);
	
    $log->debug( "canDirectStream $url" );

    $url =~ s{^airplay://}{http://};
	
    return $class->SUPER::canDirectStream($client, $url);
}

# To support remote streaming (synced players, slimp3/SB1), we need to subclass Protocols::HTTP
sub new {
    my $class  = shift;
    my $args   = shift;
    
    my $client = $args->{client};
    my $song   = $args->{song};
	my $url    = $args->{url};
	
	my $rate;
	
	$prefs->get('codec') =~ m|([^:]+):*(\d*)|i;
	if ($1 eq 'wav') {
		$rate = 1_411_200;
	} elsif ($1 eq 'flc') {
		$rate = 705_000;
	} elsif ($1 eq 'mp3') {
		$rate = $2 * 1000 || 128_000;
	}	
	
	$url =~ s/airplay/http/;
	
	my $sock = $class->SUPER::new( {
		url     => $url,
        song    => $song,
        client  => $client,
		bitrate => $rate,
    } ) || return;
	
	$log->debug("new: $url");

	${*$sock}{contentType} = substr($prefs->get('codec'), 0, 3);
	
    return $sock;
}

sub contentType {
    my $self = shift;

    return ${*$self}{'contentType'};
}

sub getMetadataFor {
	my ( $class, $client ) = @_;
	my $metadata = $client->master->pluginData('metadata');
	
	return $metadata || {};
}


1;
