package Plugins::ShairTunes2W::Settings;
use base qw(Slim::Web::Settings);

use strict;

use List::Util qw(min max);
use Data::Dumper;

use Slim::Utils::Prefs;
use Slim::Utils::Log;

my $log = logger('plugin.shairtunes');

sub name {
	return 'PLUGIN_SHAIRTUNES2';
}

sub page {
	return 'plugins/ShairTunes2W/settings/basic.html';
}

my @bool  = qw(squeezelite useFLAC syncVolume);

sub prefs {
	return (preferences('plugin.shairtunes'), (qw(bufferThreshold loglevel latency http_latency), @bool) );
}

my $prefs = preferences('plugin.shairtunes');

sub handler {
	my ($class, $client, $params, $callback, @args) = @_;
	
	$params->{logdir} = Plugins::ShairTunes2W::Plugin::logFile("[mac]");
	
	# get all LMS players and filter out squeezelite if needed
	my @players =  map { { mac => $_->id, name => $_->name, model => $_->model, FW => $_->revision } } Slim::Player::Client::clients();
		
	if ($params->{saveSettings}) {
		@players = grep { $_->{model} ne 'squeezelite' || $_->{FW} } @players if !$params->{pref_squeezelite};
		foreach my $player (@players) {
			my $enabled = $params->{'enabled.'.$player->{mac}} // 0;
			$prefs->set($player->{mac}, $enabled);
			$player->{enabled} = $enabled;
		}
		
		# checkboxes must have a value but HTML page returns undef if they are unchecked
		for my $param (@bool) {
			$params->{"pref_$param"} ||= 0;
		}
		
		$params->{pref_bufferThreshold} = min($params->{pref_bufferThreshold}, 255);
		$Plugins::ShairTunes2W::Utils::shairtunes_helper = Plugins::ShairTunes2W::Utils::helperPath( $params->{binary} || Plugins::ShairTunes2W::Utils::helperBinary() );
		$prefs->set('helper', $params->{binary});
	} else {
		@players = grep { $_->{model} ne 'squeezelite' || $_->{FW} } @players if !$prefs->get('squeezelite');
		foreach my $player (@players) {
			$player->{enabled} = $prefs->get($player->{mac}) // 1;
		}	
	}	

	@{$params->{players}} = @players;
	
	$params->{'binary'} = $prefs->get('helper') || Plugins::ShairTunes2W::Utils::helperBinary();
	$params->{'binaries'} = [ '', Plugins::ShairTunes2W::Utils::helperBinaries() ];
	
	$callback->($client, $params, $class->SUPER::handler($client, $params), @args);
}

	
1;
