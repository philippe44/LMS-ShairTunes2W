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

my @bool  = qw(squeezelite useFLAC drift syncVolume);

sub prefs {
	return (preferences('plugin.shairtunes'), (qw(bufferThreshold loglevel latency http_latency), @bool) );
}

my $prefs = preferences('plugin.shairtunes');

sub handler {
	my ($class, $client, $params, $callback, @args) = @_;
	
	$params->{logdir} = Plugins::ShairTunes2W::Plugin::logFile("[mac]");
	
	# get all LMS players and filter out squeezelite if needed
	my @players =  map { { id => $_->id, name => $_->name, model => $_->model, FW => $_->revision } } Slim::Player::Client::clients();
		
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
		
		$params->{pref_bufferThreshold} = min($params->{pref_bufferThreshold}, 255);
		$Plugins::ShairTunes2W::Utils::shairtunes_helper = Plugins::ShairTunes2W::Utils::helperPath( $params->{binary} || Plugins::ShairTunes2W::Utils::helperBinary() );
		$prefs->set('helper', $params->{binary});
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

	
1;
