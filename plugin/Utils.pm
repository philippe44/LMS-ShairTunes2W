package Plugins::ShairTunes2W::Utils;

use strict;
use warnings;
use Encode;

use Config;

use Data::Dumper;
use Digest::MD5 qw(md5 md5_hex);
use File::Spec::Functions;

use Slim::Utils::Unicode;
use Slim::Utils::Log;

my $log   = logger('plugin.shairtunes');

my %dmapData = (
    artist => '',
    title  => '',
    album  => '',
);

my %dmaptypes        = getDmapTypes();
my %dmaptypeToUnpack = (
    '1'  => 'c',
    '3'  => 'n',
    '5'  => 'N',
    '7'  => 'Q',
    '9'  => 'a*',    # utf-8 encoded
    '10' => 'N',
    '11' => 'nn',
    '42' => 'a*',    # this is a local invention - 9 is
                     # getting handled as utf-8, but for
                     # dpap.picturedata that would be
                     # bad m'kay
);

sub helperBinary {
    my $bin;
	my $os = Slim::Utils::OSDetect::details();
	
	$log->debug(Dumper($os));
	
	if ($os->{'os'} eq 'Linux') {

		if ($os->{'osArch'} =~ /x86_64/) {
			$bin = "shairport_helper-x64-linux";
        } elsif ($os->{'binArch'} =~ /i386/) {
		    $bin = "shairport_helper-i386-linux";
		}
		
		if ($os->{'binArch'} =~ /armhf/) {
			$bin = "shairport_helper-armv6hf";
		} elsif ($os->{'binArch'} =~ /arm/) {
			$bin = "shairport_helper-armv5te";
		}	
		
	}
	
	if ($os->{'os'} eq 'Darwin') {
		$bin = "shairport_helper-osx";
	}
	
	if ($os->{'os'} eq 'Windows') {
		$bin = "shairport_helper-win.exe";
	}	
	
	if ($os->{'os'} eq 'Unix') {
	
		if ($os->{'osName'} eq 'solaris') {
			$bin = "shairport_helper-i86pc-solaris";
		}	
		
	}	
	
	if ($os->{'os'} ne 'Windows') {
		my $exec = catdir(Slim::Utils::PluginManager->allPlugins->{'ShairTunes2W'}->{'basedir'}, 'Bin', $bin);
		$exec = Slim::Utils::OSDetect::getOS->decodeExternalHelperPath($exec);
			
		if (!((stat($exec))[2] & 0100)) {
			$log->warn('executable not having \'x\' permission, correcting');
			chmod (0555, $exec);
		}	
	}	
	
	my $shairtunes_helper = Slim::Utils::Misc::findbin($bin) || do {
		$log->warn("$bin not found");
		return undef;
	};

	$shairtunes_helper = Slim::Utils::OSDetect::getOS->decodeExternalHelperPath($shairtunes_helper);
			
	if (!-e $shairtunes_helper) {
		$log->warn("$shairtunes_helper not executable");
		return undef;
	}
	
	return $shairtunes_helper;
}

sub ip6bin {
    my $ip = shift;

    $ip =~ /((.*)::)?(.+)/;
    my @left  = split /:/, $2;
    my @right = split /:/, $3;
    my @mid;
    my $pad = 8 - ( $#left + $#right + 2 );
    if ( $pad > 0 ) {
        @mid = ( 0 ) x $pad;
    }
    pack( 'S>*', map { hex } ( @left, @mid, @right ) );
}

sub imagekeyfrommeta {
    my ( $airTunesMetaData ) = @_;

    return
      uc md5_hex(
        encode( 'utf-8', $airTunesMetaData->{artist} . $airTunesMetaData->{album} . $airTunesMetaData->{title} ) );
}

sub digest_ok {
    my ( $req, $conn ) = @_;
    my $authz = $req->header( 'Authorization' );
    return 0 unless $authz =~ s/^Digest\s+//i;
    return 0 unless length $conn->{nonce};
    my @authz = split /,\s*/, $authz;
    my %authz = map { /(.+)="(.+)"/; ( $1, $2 ) } @authz;

    # not a standard digest - uses capital hex digits, in conflict with the RFC
    my $digest =
      uc md5_hex(
            uc( md5_hex( $authz{username} . ':' . $authz{realm} . ':' . $conn->{password} ) ) . ':'
          . $authz{nonce} . ':'
          . uc( md5_hex( $req->method . ':' . $authz{uri} ) ) );

    return $digest eq $authz{response};
}

sub getDmapData {
    my $buf = shift;

    while ( length( $buf ) ) {
        my ( $tag, $len ) = unpack( "A4N", $buf );

        my $data = substr( $buf, 8, $len );
        my $type = $dmaptypes{$tag}{TYPE};
        my $name = $dmaptypes{$tag}{NAME};
        substr( $buf, 0, 8 + $len ) = '';

        # skip unknown types
        next if !$type;

        if ( $type == 12 ) {
            $data = getDmapData( $data );
        }

        if ( $tag =~ /asar/ ) {
            $data = unpack( "a*", $data );
            $dmapData{artist} = Slim::Utils::Unicode::utf8decode( $data );
        }
        elsif ( $tag =~ /asal/ ) {
            $data = unpack( "a*", $data );
            $dmapData{album} = Slim::Utils::Unicode::utf8decode( $data );
        }
        elsif ( $tag =~ /minm/ ) {
            $data = unpack( "a*", $data );
            $dmapData{title} = Slim::Utils::Unicode::utf8decode( $data );
        }
    }

    return %dmapData;
}

sub getDmapTypes {
    return (
        'abal' => {
            'ID'   => 'abal',
            'NAME' => 'daap.browsealbumlisting',
            'TYPE' => 12
        },
        'abar' => {
            'ID'   => 'abar',
            'NAME' => 'daap.browseartistlisting',
            'TYPE' => 12
        },
        'abcp' => {
            'ID'   => 'abcp',
            'NAME' => 'daap.browsecomposerlisting',
            'TYPE' => 12
        },
        'abgn' => {
            'ID'   => 'abgn',
            'NAME' => 'daap.browsegenrelisting',
            'TYPE' => 12
        },
        'abpl' => {
            'ID'   => 'abpl',
            'NAME' => 'daap.baseplaylist',
            'TYPE' => 1
        },
        'abro' => {
            'ID'   => 'abro',
            'NAME' => 'daap.databasebrowse',
            'TYPE' => 12
        },
        'adbs' => {
            'ID'   => 'adbs',
            'NAME' => 'daap.databasesongs',
            'TYPE' => 12
        },
        'aeNV' => {
            'ID'   => 'aeNV',
            'NAME' => 'com.apple.itunes.norm-volume',
            'TYPE' => 5
        },
        'aeSP' => {
            'ID'   => 'aeSP',
            'NAME' => 'com.apple.itunes.smart-playlist',
            'TYPE' => 1
        },
        'aply' => {
            'ID'   => 'aply',
            'NAME' => 'daap.databaseplaylists',
            'TYPE' => 12
        },
        'apro' => {
            'ID'   => 'apro',
            'NAME' => 'daap.protocolversion',
            'TYPE' => 11
        },
        'apso' => {
            'ID'   => 'apso',
            'NAME' => 'daap.playlistsongs',
            'TYPE' => 12
        },
        'arif' => {
            'ID'   => 'arif',
            'NAME' => 'daap.resolveinfo',
            'TYPE' => 12
        },
        'arsv' => {
            'ID'   => 'arsv',
            'NAME' => 'daap.resolve',
            'TYPE' => 12
        },
        'asal' => {
            'ID'   => 'asal',
            'NAME' => 'daap.songalbum',
            'TYPE' => 9
        },
        'asar' => {
            'ID'   => 'asar',
            'NAME' => 'daap.songartist',
            'TYPE' => 9
        },
        'asbr' => {
            'ID'   => 'asbr',
            'NAME' => 'daap.songbitrate',
            'TYPE' => 3
        },
        'asbt' => {
            'ID'   => 'asbt',
            'NAME' => 'daap.songbeatsperminute',
            'TYPE' => 3
        },
        'ascm' => {
            'ID'   => 'ascm',
            'NAME' => 'daap.songcomment',
            'TYPE' => 9
        },
        'asco' => {
            'ID'   => 'asco',
            'NAME' => 'daap.songcompilation',
            'TYPE' => 1
        },
        'ascp' => {
            'ID'   => 'ascp',
            'NAME' => 'daap.songcomposer',
            'TYPE' => 9
        },
        'asda' => {
            'ID'   => 'asda',
            'NAME' => 'daap.songdateadded',
            'TYPE' => 10
        },
        'asdb' => {
            'ID'   => 'asdb',
            'NAME' => 'daap.songdisabled',
            'TYPE' => 1
        },
        'asdc' => {
            'ID'   => 'asdc',
            'NAME' => 'daap.songdisccount',
            'TYPE' => 3
        },
        'asdk' => {
            'ID'   => 'asdk',
            'NAME' => 'daap.songdatakind',
            'TYPE' => 1
        },
        'asdm' => {
            'ID'   => 'asdm',
            'NAME' => 'daap.songdatemodified',
            'TYPE' => 10
        },
        'asdn' => {
            'ID'   => 'asdn',
            'NAME' => 'daap.songdiscnumber',
            'TYPE' => 3
        },
        'asdt' => {
            'ID'   => 'asdt',
            'NAME' => 'daap.songdescription',
            'TYPE' => 9
        },
        'aseq' => {
            'ID'   => 'aseq',
            'NAME' => 'daap.songeqpreset',
            'TYPE' => 9
        },
        'asfm' => {
            'ID'   => 'asfm',
            'NAME' => 'daap.songformat',
            'TYPE' => 9
        },
        'asgn' => {
            'ID'   => 'asgn',
            'NAME' => 'daap.songgenre',
            'TYPE' => 9
        },
        'asrv' => {
            'ID'   => 'asrv',
            'NAME' => 'daap.songrelativevolume',
            'TYPE' => 1
        },
        'assp' => {
            'ID'   => 'assp',
            'NAME' => 'daap.songstoptime',
            'TYPE' => 5
        },
        'assr' => {
            'ID'   => 'assr',
            'NAME' => 'daap.songsamplerate',
            'TYPE' => 5
        },
        'asst' => {
            'ID'   => 'asst',
            'NAME' => 'daap.songstarttime',
            'TYPE' => 5
        },
        'assz' => {
            'ID'   => 'assz',
            'NAME' => 'daap.songsize',
            'TYPE' => 5
        },
        'astc' => {
            'ID'   => 'astc',
            'NAME' => 'daap.songtrackcount',
            'TYPE' => 3
        },
        'astm' => {
            'ID'   => 'astm',
            'NAME' => 'daap.songtime',
            'TYPE' => 5
        },
        'astn' => {
            'ID'   => 'astn',
            'NAME' => 'daap.songtracknumber',
            'TYPE' => 3
        },
        'asul' => {
            'ID'   => 'asul',
            'NAME' => 'daap.songdataurl',
            'TYPE' => 9
        },
        'asur' => {
            'ID'   => 'asur',
            'NAME' => 'daap.songuserrating',
            'TYPE' => 1
        },
        'asyr' => {
            'ID'   => 'asyr',
            'NAME' => 'daap.songyear',
            'TYPE' => 3
        },
        'avdb' => {
            'ID'   => 'avdb',
            'NAME' => 'daap.serverdatabases',
            'TYPE' => 12
        },
        'mbcl' => {
            'ID'   => 'mbcl',
            'NAME' => 'dmap.bag',
            'TYPE' => 12
        },
        'mccr' => {
            'ID'   => 'mccr',
            'NAME' => 'dmap.contentcodesresponse',
            'TYPE' => 12
        },
        'mcna' => {
            'ID'   => 'mcna',
            'NAME' => 'dmap.contentcodesname',
            'TYPE' => 9
        },
        'mcnm' => {
            'ID'   => 'mcnm',
            'NAME' => 'dmap.contentcodesnumber',
            'TYPE' => 9
        },
        'mcon' => {
            'ID'   => 'mcon',
            'NAME' => 'dmap.container',
            'TYPE' => 12
        },
        'mctc' => {
            'ID'   => 'mctc',
            'NAME' => 'dmap.containercount',
            'TYPE' => 5
        },
        'mcti' => {
            'ID'   => 'mcti',
            'NAME' => 'dmap.containeritemid',
            'TYPE' => 5
        },
        'mcty' => {
            'ID'   => 'mcty',
            'NAME' => 'dmap.contentcodestype',
            'TYPE' => 3
        },
        'mdcl' => {
            'ID'   => 'mdcl',
            'NAME' => 'dmap.dictionary',
            'TYPE' => 12
        },
        'miid' => {
            'ID'   => 'miid',
            'NAME' => 'dmap.itemid',
            'TYPE' => 5
        },
        'mikd' => {
            'ID'   => 'mikd',
            'NAME' => 'dmap.itemkind',
            'TYPE' => 7
        },
        'mimc' => {
            'ID'   => 'mimc',
            'NAME' => 'dmap.itemcount',
            'TYPE' => 5
        },
        'minm' => {
            'ID'   => 'minm',
            'NAME' => 'dmap.itemname',
            'TYPE' => 9
        },
        'mlcl' => {
            'ID'   => 'mlcl',
            'NAME' => 'dmap.listing',
            'TYPE' => 12
        },
        'mlid' => {
            'ID'   => 'mlid',
            'NAME' => 'dmap.sessionid',
            'TYPE' => 5
        },
        'mlit' => {
            'ID'   => 'mlit',
            'NAME' => 'dmap.listingitem',
            'TYPE' => 12
        },
        'mlog' => {
            'ID'   => 'mlog',
            'NAME' => 'dmap.loginresponse',
            'TYPE' => 12
        },
        'mpco' => {
            'ID'   => 'mpco',
            'NAME' => 'dmap.parentcontainerid',
            'TYPE' => 5
        },
        'mper' => {
            'ID'   => 'mper',
            'NAME' => 'dmap.persistentid',
            'TYPE' => 7
        },
        'mpro' => {
            'ID'   => 'mpro',
            'NAME' => 'dmap.protocolversion',
            'TYPE' => 11
        },
        'mrco' => {
            'ID'   => 'mrco',
            'NAME' => 'dmap.returnedcount',
            'TYPE' => 5
        },
        'msal' => {
            'ID'   => 'msal',
            'NAME' => 'dmap.supportsautologout',
            'TYPE' => 1
        },
        'msau' => {
            'ID'   => 'msau',
            'NAME' => 'dmap.authenticationmethod',
            'TYPE' => 1
        },
        'msbr' => {
            'ID'   => 'msbr',
            'NAME' => 'dmap.supportsbrowse',
            'TYPE' => 1
        },
        'msdc' => {
            'ID'   => 'msdc',
            'NAME' => 'dmap.databasescount',
            'TYPE' => 5
        },
        'msex' => {
            'ID'   => 'msex',
            'NAME' => 'dmap.supportsextensions',
            'TYPE' => 1
        },
        'msix' => {
            'ID'   => 'msix',
            'NAME' => 'dmap.supportsindex',
            'TYPE' => 1
        },
        'mslr' => {
            'ID'   => 'mslr',
            'NAME' => 'dmap.loginrequired',
            'TYPE' => 1
        },
        'mspi' => {
            'ID'   => 'mspi',
            'NAME' => 'dmap.supportspersistentids',
            'TYPE' => 1
        },
        'msqy' => {
            'ID'   => 'msqy',
            'NAME' => 'dmap.supportsquery',
            'TYPE' => 1
        },
        'msrs' => {
            'ID'   => 'msrs',
            'NAME' => 'dmap.supportsresolve',
            'TYPE' => 1
        },
        'msrv' => {
            'ID'   => 'msrv',
            'NAME' => 'dmap.serverinforesponse',
            'TYPE' => 12
        },
        'mstm' => {
            'ID'   => 'mstm',
            'NAME' => 'dmap.timeoutinterval',
            'TYPE' => 5
        },
        'msts' => {
            'ID'   => 'msts',
            'NAME' => 'dmap.statusstring',
            'TYPE' => 9
        },
        'mstt' => {
            'ID'   => 'mstt',
            'NAME' => 'dmap.status',
            'TYPE' => 5
        },
        'msup' => {
            'ID'   => 'msup',
            'NAME' => 'dmap.supportsupdate',
            'TYPE' => 1
        },
        'mtco' => {
            'ID'   => 'mtco',
            'NAME' => 'dmap.specifiedtotalcount',
            'TYPE' => 5
        },
        'mudl' => {
            'ID'   => 'mudl',
            'NAME' => 'dmap.deletedidlisting',
            'TYPE' => 12
        },
        'mupd' => {
            'ID'   => 'mupd',
            'NAME' => 'dmap.updateresponse',
            'TYPE' => 12
        },
        'musr' => {
            'ID'   => 'musr',
            'NAME' => 'dmap.serverrevision',
            'TYPE' => 5
        },
        'muty' => {
            'ID'   => 'muty',
            'NAME' => 'dmap.updatetype',
            'TYPE' => 1
        },
        'pasp' => {
            'ID'   => 'pasp',
            'NAME' => 'dpap.aspectratio',
            'TYPE' => 9
        },
        'pfdt' => {
            'ID'   => 'pfdt',
            'NAME' => 'dpap.picturedata',
            'TYPE' => 42
        },
        'picd' => {
            'ID'   => 'picd',
            'NAME' => 'dpap.creationdate',
            'TYPE' => 5
        },
        'pimf' => {
            'ID'   => 'pimf',
            'NAME' => 'dpap.imagefilename',
            'TYPE' => 9
        },
        'ppro' => {
            'ID'   => 'ppro',
            'NAME' => 'dpap.protocolversion',
            'TYPE' => 11
        }
    );
}

1;
