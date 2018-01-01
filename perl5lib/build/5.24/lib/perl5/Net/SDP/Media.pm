package Net::SDP::Media;

################
#
# Net::SDP - Session Description Protocol (rfc2327)
#
# Nicholas J Humfrey
# njh@cpan.org
#
# See the bottom of this file for the POD documentation. 
#

use strict;
use vars qw/$VERSION %avt_profile_map/;
use Carp;
$VERSION="0.07";



# Static Payload Type map built using data from:
#
# http://www.iana.org/assignments/rtp-parameters
# http://www.iana.org/assignments/media-types/

%avt_profile_map = (
	'0' => 'audio/PCMU/8000/1',
#	'1'	=> reserved
#	'2'	=> reserved
	'3' => 'audio/GSM/8000/1',
	'4' => 'audio/G723/8000/1',
	'5' => 'audio/DVI4/8000/1',
	'6' => 'audio/DVI4/16000/1',
	'7' => 'audio/LPC/8000/1',
	'8' => 'audio/PCMA/8000/1',
	'9' => 'audio/G722/8000/1',
	'10' => 'audio/L16/44100/2',
	'11' => 'audio/L16/44100/1',
	'12' => 'audio/QCELP/8000/1',
	'13' => 'audio/CN/8000/1',
	'14' => 'audio/MPA/90000',
	'15' => 'audio/G728/8000/1',
	'16' => 'audio/DVI4/11025/1',
	'17' => 'audio/DVI4/22050/1',
	'18' => 'audio/G729/8000/1',
#	'19' => reserved,
#	'20' => unassigned,
#	'21' => unassigned,
#	'22' => unassigned,
#	'23' => unassigned,
#	'24' => unassigned,
	'25' => 'video/CelB/90000',
	'26' => 'video/JPEG/90000',
#	'27' => unassigned,
	'28' => 'video/nv/90000',
#	'29' => unassigned,
#	'30' => unassigned,
	'31' => 'video/H261/90000',
	'32' => 'video/MPV/90000',
	'33' => 'video/MP2T/90000',
	'34' => 'video/H263/90000',
);





sub new {
	my $class = shift;
	my $self = {
		'm_media' => 'unknown',
		'm_port' => 0,
		'm_transport' => 'RTP/AVP',
		'm_fmt_list' => [],
		'i' => undef,
		'c_net_type' => 'IN',
		'c_addr_type' => 'IP4',
		'c_address' => '0.0.0.0',
		'c_ttl' => 5,
		'a' => {}
	};
	bless $self, $class;	

	# Initial value provided ?
	my ($m) = @_;
	$self->_parse_m($m) if (defined $m);
	
	return $self;
}

#sub remove {
#    my $self=shift;
#
#	### Delete ourselves from our parent's array    
#    
#    undef $self;
#}



sub _parse_m {
	my $self = shift;
	my ($m) = @_;
	
	($self->{'m_media'},
	 my $port,
	 $self->{'m_transport'},
	 my @formats ) = split(/\s/, $m);

	$self->{'m_fmt_list'} = \@formats;
	
	if (defined $port) {
		($self->{'m_port'}, my $range) = split(/\//, $port);
		if (defined $range and $range ne '' and $range ne '1') {
			carp "Port ranges are not supported by Net::SDP.";
		}
	}
	
	# Success
	return 1;
}

sub _generate_m {
	my $self = shift;

	return	'm='.$self->{'m_media'}.' '.
			$self->{'m_port'}.' '.
			$self->{'m_transport'}.' '.
			join(' ', @{$self->{'m_fmt_list'}})."\n";
}

sub _parse_c {
	my $self = shift;
	my ($c) = @_;
	
	($self->{'c_net_type'}, $self->{'c_addr_type'}, my $address) = split(/\s/, $c);
	($self->{'c_address'}, $self->{'c_ttl'}, my $range) = split(/\//, $address);
	
	if ($self->{'c_net_type'} ne 'IN') {
		carp "Network type is not Internet (IN): ".$self->{'c_net_type'};
	}
	
	if ($self->{'c_addr_type'} ne 'IP4' and $self->{'c_addr_type'} ne 'IP6') {
		carp "Address type is not IP4 or IP6: ".$self->{'c_addr_type'};
	}
	
	if (!defined $self->{'c_ttl'}) {
		$self->{'c_ttl'} = 0;
	}
	
	if (defined $range and $range ne '' and $range ne '1') {
		carp "Address ranges are not supported by Net::SDP.";
	}
	
	# Success
	return 1;
}

sub _generate_c {
	my $self = shift;
	my $c = 'c='.$self->{'c_net_type'}.' '.
			$self->{'c_addr_type'}.' '.
			$self->{'c_address'};
			
	if ($self->{'c_ttl'}) {
		$c .= '/'.$self->{'c_ttl'};
	}

	return "$c\n";
}





sub address {
    my $self=shift;
	my ($address) = @_;
    $self->{'c_address'} = $address if defined $address;
    return $self->{'c_address'};
}

sub address_type {
    my $self=shift;
	my ($addr_type) = @_;
    $self->{'c_addr_type'} = $addr_type if defined $addr_type;
    return $self->{'c_addr_type'};
}


sub port {
    my $self=shift;
	my ($port) = @_;
    $self->{'m_port'} = $port if defined $port;
    return $self->{'m_port'};
}

sub ttl {
    my $self=shift;
	my ($ttl) = @_;
    $self->{'c_ttl'} = $ttl if defined $ttl;
    return $self->{'c_ttl'};
}

sub media_type {
    my $self=shift;
	my ($media) = @_;
    $self->{'m_media'} = $media if defined $media;
    return $self->{'m_media'};
}

sub title {
    my $self=shift;
	my ($title) = @_;
    $self->{'i'} = $title if defined $title;
    return $self->{'i'};
}

sub transport {
    my $self=shift;
	my ($transport) = @_;
    $self->{'m_transport'} = $transport if defined $transport;
    return $self->{'m_transport'};
}

sub network_type {
    my $self=shift;
	my ($net_type) = @_;
    $self->{'c_net_type'} = $net_type if defined $net_type;
    return $self->{'c_net_type'};
}


sub attribute {
    my $self=shift;

    return Net::SDP::_attribute( $self, @_);
}

sub attributes {
    my $self=shift;

    return $self->{'a'};
}

sub add_attribute {
	my $self = shift;
	my ($name, $value) = @_;
	carp "Missing attribute name" unless (defined $name);
	
	my $attrib = $name;
	$attrib .= ":$value" if (defined $value);
	Net::SDP::_add_attribute( $self, 'a', $attrib );
}



sub remove_format_num {
    my $self=shift;
	my ($fmt_num) = @_;
	carp "Missing format number to remove" unless (defined $fmt_num);
	
	foreach my $n ( 0 .. $#{$self->{'m_fmt_list'}}) {
		if ($self->{'m_fmt_list'}->[$n] == $fmt_num) {
			splice( @{$self->{'m_fmt_list'}}, $n, 1 );
			return 1;
		}
	}

	# Failed to delete
	return 0;
}


sub default_format_num {
	my $self=shift;
	my ($fmt_num) = @_;

	if (defined $fmt_num) {

		# Remove it from elsewhere in the list
		$self->remove_format_num( $fmt_num );

		# Put it at the start of the format list
		unshift( @{$self->{'m_fmt_list'}}, $fmt_num );
	}

	return $self->{'m_fmt_list'}->[0];
}

sub default_format {
    my $self=shift;
    my $fmt_list = $self->format_list();
    
	return $fmt_list->{ $self->default_format_num() };
}


sub format_num_list {
    my $self=shift;
	my ($fmt_list) = @_;

	if (defined $fmt_list) {
		carp "Parameter should be an array ref" if (ref $fmt_list ne 'ARRAY');
    	$self->{'m_fmt_list'} = $fmt_list;
    }
    
    return $self->{'m_fmt_list'};
}

sub format_list {
    my $self=shift;
	my $fmt_list = {};

	# Build a list of formats from rtpmap attributes
	my %rtpmap = ();
    foreach( @{$self->{'a'}->{'rtpmap'}} ) {
    	/(\d+)\s(.*)$/;
    	$rtpmap{$1} = $self->{'m_media'}."/$2";
    }

	# Build our payload type map
	foreach (@{$self->{'m_fmt_list'}}) {
		if (exists $rtpmap{$_}) {
			$fmt_list->{$_} = $rtpmap{$_};
		} elsif (exists $avt_profile_map{$_}) {
			$fmt_list->{$_} = $avt_profile_map{$_};
		} else {
			$fmt_list->{$_} = '';
		}
	}

	return $fmt_list;	
}


sub add_format {
    my $self=shift;
	my ($format_num, $mime) = @_;
	carp "Missing format number to add" unless (defined $format_num);
	
	
	# Appened the format number to the list
	# (which means the first one you add is default)
	push( @{$self->{'m_fmt_list'}}, $format_num );


	# Mime type specified ?
	if (!defined $mime) {
		$mime = $avt_profile_map{$format_num};
	}
	
	if (!defined $mime) {
		warn "Payload format $format_num is unknown dynamic type.";
		return;
	}
	
	# Work out rtpmap entry
	my ($mime_media, $mime_format) = ($mime =~ /^([^\/]+)\/(.+)$/);
	if ($mime_media ne $self->{'m_media'}) {
		warn "This Media Description is for ".$self->{'m_media'};
	}
	
	# Appened the rtpmap entry for this format
	push( @{$self->{'a'}->{'rtpmap'}}, "$format_num $mime_format" );
}

sub as_string {
	my $self=shift;
	my $type = $self->{'m_media'};
	$type =~ s/^(.+)/\u\L$1/;
	return "$type Stream";
}

1;

__END__

=pod

=head1 NAME

Net::SDP::Media - Media Description in an SDP file

=head1 SYNOPSIS

  my $audio = $sdp->new_media_desc( 'audio' );

  $audio->address('224.123.234.56');
  $audio->port(5004);
  $audio->attribute('quality', 5);

=head1 DESCRIPTION

This class represents a single Media Description (m=) in an SDP file.
As well as the m= line, it also contains the information about all the 
following lines that make up that media description. When parsing an SDP file,
C<Net::SDP> will create an instance of C<Net::SDP::Media> for each media description.
New media descriptions can be created using the C<new_media_desc()> method in C<Net::SDP>.

=head2 METHODS

=over 4


=item B<title()>

Get or Set the title for this media description. B<[i=]>

Example:

	$audio = $audio->title();
	$audio->title( 'English' );

=item B<address()>

Get or Set the connection address for this media description. B<[c=]>

Example:

	$addr = $audio->address();
	$audio->address( '224.123.234.56' );
  

=item B<address_type()>

Get or Set the connection address type for this media description. 
Default is IP4. B<[c=]>

Example:

	$addr = $audio->address_type();
	$audio->address_type( 'IP6' );
  

=item B<port()>

Get or Set the connection port type for this media description. B<[m=]>

Example:

	$port = $audio->port();
	$audio->port( 5004 );


=item B<ttl()>

Get or Set the Time to Live for packets in this media description. B<[c=]>
To not specify a TTL, then set to 0.

Example:

	$ttl = $audio->ttl();
	$audio->ttl( 127 );


=item B<media_type()>

Get or Set the media type this media description - eg audio, video etc. B<[m=]>

Example:

	$media = $audio->media_type();
	$audio->media_type( 'audio' );


=item B<transport()>

Get or Set the transport method for this media description. Default is RTP/AVP. B<[m=]>

Example:

  $media = $audio->transport();
  $audio->transport( 'UDP' );


=item B<network_type()>

Get or Set the network type for this media description. Default is 'IN' - Internet. B<[c=]>

Example:

	$media = $audio->network_type();
	$audio->network_type( 'IN' );


=item B<attribute( name, [value] )>

Get or Set an attribute for this media description. B<[a=]>

When setting an attribute, if you pass in a scalar, then all attributes with
the same name will be replaced. Alternively an attribute may be set to multiple
values by passing an ARRAYREF. If an attribute does not require it, 
then the value parameter is optional - eg for 'recvonly' attribute.

When getting an attribute that has no value, then '' is returned, 
or if the attribute does not exists then undef is returned.
If the attribute has a single value, then that value is returned, 
or if it has more than one value then an ARRAYREF is returned.

Example:

	$quality = $audio->attribute( 'quality' );
	$video->attribute( 'framerate',  10 );


=item B<attributes()>

Get a HASHREF of all the attributes associated with this media description B<[a=]>

Example:

	$hashref = $audio->attributes();


=item B<add_attribute( name, [value] )>

Add a value for sepecified attribute. This method is intended to be used with attributes
with multiple values - ie rtpmap and fmtp B<[a=]>

Example:

	$audio->add_attribute( 'fmtp', '32 type=mpeg1');


=item B<remove_format_num( num )>

Removes the specified format number from the list of payload IDs/formats 
that are allowed for this media description. B<[m=]>

Example:

	$audio->remove_format_num( 0 );


=item B<default_format_num( num )>

Gets or Set the default format number/payload ID in the list of formats.
The format number doesn't need to already be in the list of formats. B<[m=]>

Example:

	$default = $audio->default_format_num();
	$audio->default_format_num( 0 );


=item B<default_format()>

Returns the default format for this media description as a MIME type.

Example:

	# eg audio/L16/44100/2
	$default = $audio->default_format();


=item B<format_num_list()>

Get or Set an ARRAYREF containing a list of the format numbers/payload IDs
for this media description. B<[m=]>

Example:

	$fmt_list = $audio->format_num_list();
	$audio->format_num_list( [0, 8, 10] );


=item B<format_list()>

Returns a HASHREF containing format numbers as keys and MIME types as values
for all the formats allowed by this media description.


=item B<add_format( format_num, [mime_type] )>

Add a format to the list of formats allowed in this media description.
The first parameter is the format number/payload ID and the second (optional)
parameter is the MIME type for that format.

Example:

	$audio->add_format( 96, 'audio/L16/22500/1' );


=item B<as_string()>

Returns a textual representation/summary of the media description.

=back

=head1 AUTHOR

Nicholas J Humfrey, njh@cpan.org

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004 University of Southampton

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.005 or,
at your option, any later version of Perl 5 you may have available.

=cut
