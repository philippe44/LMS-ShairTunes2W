package Plugins::ShairTunes2W::Settings;
use base qw(Slim::Web::Settings);

use strict;

use List::Util qw(min max);

use Slim::Utils::Prefs;
use Slim::Utils::Log;

my $log = logger('plugin.shairtunes');

sub name {
	return 'PLUGIN_SHAIRTUNES2';
}

sub page {
	return 'plugins/ShairTunes2W/settings/basic.html';
}

my @bool  = qw(squeezelite drift syncVolume http_fill);

sub prefs {
	return (preferences('plugin.shairtunes'), (qw(bufferThreshold codec loglevel latency http_latency), @bool) );
}

my $prefs = preferences('plugin.shairtunes');

sub handler {
	my ($class, $client, $params, $callback, @args) = @_;
	
	$params->{logdir} = Plugins::ShairTunes2W::Plugin::logFile("[mac]");
		
	# get all LMS players and filter out squeezelite if needed
	my @players =  sort { $a->{name} cmp $b->{name} } map { { id => $_->id, name => $_->name, model => $_->model, FW => $_->revision } } Slim::Player::Client::clients();
	
	if ($params->{republish}) {
		Plugins::ShairTunes2W::Plugin::revoke_publishPlayers();
		Plugins::ShairTunes2W::Plugin::republishPlayers();
	} elsif ($params->{wipe}) {
		Plugins::ShairTunes2W::Plugin::revoke_publishPlayers();
		Plugins::ShairTunes2W::Plugin::stop_mDNS;		
		Plugins::ShairTunes2W::Plugin::republishPlayers();
	} elsif ($params->{saveSettings}) {
		@players = grep { $_->{model} ne 'squeezelite' || $_->{FW} } @players if !$params->{pref_squeezelite};
		foreach my $player (@players) {
			my $enabled = $params->{'enabled.'.$player->{id}} // 0;
			my $client = Slim::Player::Client::getClient($player->{id});
			Plugins::ShairTunes2W::Plugin::removePlayer($client) if $prefs->get($player->{id}) && !$enabled;
			Plugins::ShairTunes2W::Plugin::addPlayer($client) if !$prefs->get($player->{id}) && $enabled;
			$prefs->set($player->{id}, $enabled);
			$player->{enabled} = $enabled;
		}
		
		# checkboxes must have a value but HTML page returns undef if they are unchecked
		for my $param (@bool) {
			$params->{"pref_$param"} ||= 0;
		}
		
		# special case for squeezelite so that the get below is set
		$prefs->set('squeezelite', $params->{pref_squeezelite});
		
		$params->{pref_codec} = $params->{codec_name} || 'flc';
		if ($params->{pref_codec} eq 'flc') {
			$params->{pref_codec} .= ":$params->{codec_level}" if defined $params->{codec_level} && $params->{codec_level} ne '';
		} elsif ($params->{pref_codec} eq 'mp3') {
			$params->{pref_codec} .= ":$params->{codec_bitrate}" if $params->{codec_bitrate};
		}	
								
		$params->{pref_bufferThreshold} = min($params->{pref_bufferThreshold}, 255);
		$Plugins::ShairTunes2W::Utils::shairtunes_helper = Plugins::ShairTunes2W::Utils::helperPath( $params->{binary} || Plugins::ShairTunes2W::Utils::helperBinary() );
		Plugins::ShairTunes2W::Utils::checkHelper( $params->{binary} );
		$prefs->set( 'helper', $params->{binary} );
	} 
	
	@players = grep { $_->{model} ne 'squeezelite' || $_->{FW} } @players if !$prefs->get('squeezelite');
	foreach my $player (@players) {
		$player->{enabled} = $prefs->get($player->{id}) // 1;
	}	

	@{$params->{players}} = @players;
	
	$params->{'binary'} = $prefs->get('helper') || Plugins::ShairTunes2W::Utils::helperBinary();
	$params->{'binaries'} = [ '', Plugins::ShairTunes2W::Utils::helperBinaries() ];
	
	$callback->($client, $params, $class->SUPER::handler($client, $params), @args);
}

sub beforeRender {
	my ($class, $params, $client) = @_;
		
	$prefs->get('codec') =~ m|([^:]+):*(\d*)|i;
	
	$params->{codec_name} = $1;
	$params->{codec_level} = $2 if defined $2 && $1 eq 'flc';
	$params->{codec_bitrate} = $2 if $2 && $1 eq 'mp3';
}
	
1;
