package Plugins::ShairTunes2W::AIRPLAY;

use strict;
use warnings;

use base qw(Slim::Player::Protocols::HTTP);
use List::Util qw(min);
use Slim::Utils::Strings qw(string);
use Slim::Utils::Misc;
use Slim::Utils::Log;
use Slim::Utils::Prefs;
use Slim::Utils::Errno;

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
    # we can't set $track->audio_offset otherwise LMS will *always* skip the WAV header
	# in the GET request. This is by design because if we claim to have an audio_offset
	# the it mean we have a header and should use a processor to handle it.
	$args->{cb}->($args->{song}->currentTrack);
}

sub canDirectStreamSong {
    my ($class, $client, $song) = @_;
    my $directUrl = $class->SUPER::canDirectStreamSong($client, $song);  
    return $song->stripHeader ? undef : $directUrl;
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
		$rate = $2 * 1000 || 192_000;
	} elsif ($1 eq 'aac') {
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

    my $codec = substr($prefs->get('codec'), 0, 3);
	${*$sock}{contentType} = $codec;
	
    return $sock;
}

sub sysread{
    my $self = $_[0];   
    return $self->SUPER::sysread($_[1], $_[2], $_[3]) unless ${*$self}{__PACKAGE__ . '_skip'};
        
    # skip what we need until done or EOF
    my $bytes = $self->SUPER::sysread($_[1], min(${*$self}{__PACKAGE__ . '_skip'}, $_[2]), $_[3]);
    return $bytes if defined $bytes && !$bytes;

    ${*$self}{__PACKAGE__ . '_skip'} -= $bytes if $bytes;
    $_[1] = '';
    $! = EINTR;
    return undef;
}    

sub contentType {
    my $self = shift;
    return ${*$self}{'contentType'};
}

sub response {
    my $self = shift;
    my $args = shift;
	my $song = $args->{song};
    $self->SUPER::response($args, @_);
	# when asked to strip header, it means we are doing wav->pcm, but LMS can't remove 
	# the header for us, so need to handle it manually
    ${*$self}{__PACKAGE__ . '_skip'} = 44 if $song->stripHeader;
}

sub getMetadataFor {
	my ( $class, $client ) = @_;
	my $metadata = $client->master->pluginData('metadata');
	return $metadata || {};
}


1;
