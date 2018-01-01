package Net::SDP;

################
#
# Net::SDP - Session Description Protocol (rfc2327)
#
# Nicholas J Humfrey
# njh@cpan.org
#
# See the bottom of this file for the POD documentation. 
#
# All parsing and generating of SDP data
# is delt with in this file


use strict;
use vars qw/$VERSION/;

use Net::SDP::Media;
use Net::SDP::Time;
use Sys::Hostname;
use Net::hostent;
use Carp;

$VERSION="0.07";



sub new {
    my $class = shift;
    my ($data) = @_;
    
    my $self = {'v'=>'0',
    			'session'=> {
     				'o_uname' => '',
    				'o_sess_id' => 0,
    				'o_sess_vers' => 0,
    				'o_net_type' => '',
    				'o_addr_type' => '',
    				'o_address' => '',
    				'p' => [],
    				'e' => [],
    				'a' => {}
   				},
   				'media'=>[],
   				'time'=>[]
   	};
    bless $self, $class;
   	
   	
   	# Parse data if we are passed some
   	if (defined $data) {
		unless ($self->parse( $data )) {
			# Failed to parse
			return undef;
		}
    } else {
   		# Use sane defaults
   		$self->{'session'}->{'o_uname'} = $ENV{'USER'} || '-';
   		$self->{'session'}->{'o_sess_id'} = Net::SDP::Time::_ntptime();
   		$self->{'session'}->{'o_sess_vers'} = Net::SDP::Time::_ntptime();
   		$self->{'session'}->{'o_net_type'} = 'IN';
   		$self->{'session'}->{'o_addr_type'} = 'IP4';
    	
   		my $hostname = hostname();
   		if (defined $hostname) {
   			if (my $h = gethost($hostname)) {
   				$self->{'session'}->{'o_address'} = $h->name();
   			}
   		}
   	}	

    return $self;
}

# Try and work out what the source is
sub parse {
	my $self = shift;
	my ($source) = @_;

	if (@_ == 1) {

		if (ref $source eq 'Net::SAP::Packet') {
			# It is a SAP packet
			if ($source->payload_type() ne 'application/sdp') {
				carp "Payload type of Net::SAP::Packet is not application/sdp.";
				return 0;
			}
			return $self->parse_data( $source->payload() );

    	} elsif ($source =~ /^v=0/) {
    		# Looks like start of SDP file
    		return $self->parse_data( $source );
    		
    	} elsif ($source =~ /^\w+:/) {
    		# Looks like a URL
    		return $self->parse_url( $source );
    		
    	} elsif ($source eq '-') {
    		# Parse STDIN
			return $self->parse_stdin();
    		
    	} elsif ($source ne '') {
    		# Assume it is a filename
     		return $self->parse_file( $source );

    	} else {
    		carp "Failed to parse empty string.";
    		return 0;
    	}
    
	} elsif (@_ == 0) {
		return $self->parse_stdin();
		
	} else {
		croak "Too many parameters for parse()";
	}
	
}

sub parse_file {
	my $self = shift;
	my ($filename) = @_;

	open(SDP, $filename) or croak "can't open SDP file ($filename): $!";
	local $/ = undef;  # slurp full file
	my $data = <SDP>;
	close (SDP);
	
	return $self->parse_data( $data );
}

sub parse_url {
	my $self = shift;
	my ($url) = @_;

	eval "use LWP::Simple";
	croak "Couldn't fetch URL because LWP::Simple is unavailable." if ($@);

	my $data = LWP::Simple::get($url) or
	croak "Failed to fetch the URL '$url' with LWP: $!\n";

	return $self->parse_data( $data );
}

sub parse_stdin {
	my $self = shift;

	local $/ = undef;  # slurp STDIN
	my $data = <>;

	return $self->parse_data( $data );
}





sub parse_data {
	my $self = shift;
	my ($data) = @_;
	croak "Missing SDP data parameter.\n" unless (defined $data);
	
	# Undefine defaults
	undef $self->{'v'};
	undef $self->{'session'}->{'s'};
	undef $self->{'session'}->{'o_sess_id'};
	undef $self->{'session'}->{'o_sess_vers'};
	
	
	# Sections of sdp file: 'session', 'media'
	my $section = "session";

	
	# Split the file up into an array of its lines
	my @lines = split(/[\r\n]+/, $data);
	

	while (my $line = shift(@lines)) {
		my ($field, $value) = ($line =~ /^(\w)=(.*?)\s*$/);
		if ($field eq '') {
			carp "Failed to parse line of SDP data: $line\n";
			next;	
		}
		
		# Ignore empty values
		next if ($value eq '');
		
	
		## Session description
		if ($section eq 'session') {
		
			if ($field eq 'v') {

				$self->_parse_v( $value ) || return 0;

			} elsif ($field eq 'm') {
			
				# Move on to the media section
				$section = 'media';
				
			} elsif ($field eq 't') {
			
				my $time = new Net::SDP::Time( $value );
				
				push( @{$self->{'time'}}, $time );

			} elsif ($field eq 'r') {
				
				# Add to last time descriptor
				unless ( $self->{'time'}->[-1] ) {
				  carp "No previous 't' parameter to associate 'r' with: $line\n";
				  next;
				}

				$self->{'time'}->[-1]->_parse_r($value);

			} elsif ($field eq 'o') {

				$self->_parse_o( $value );

			} elsif ($field eq 'p' || $field eq 'e') {
				
				# Phone and email can have more than one value
				push( @{$self->{'session'}->{$field}}, $value );

			} elsif ($field eq 'a' || $field eq 'b') {
			
				# More than one value is allowed
				_add_attribute( $self->{'session'}, $field, $value );
				
			} else {
	
				# Single value
				$self->{'session'}->{$field} = $value;
			}
		}


		## Media description
		if ($section eq 'media') {
		
			if ($field eq 'm') {
				my $media = new Net::SDP::Media( $value );
				
				# Copy accross connection information for easier access
				if (defined $self->{'session'}->{'c'}) {
					$media->_parse_c( $self->{'session'}->{'c'} );
				}
				push( @{$self->{'media'}}, $media );
				
			} elsif ($field =~ /a|b/) {

				# XXXXXX Check array exists? XXXXXX
				_add_attribute( $self->{'media'}->[-1], $field, $value );
				
			} elsif ($field =~ /c/) {

				my $media = $self->{'media'}->[-1];
				$media->_parse_c( $value );
				
			} else {
				$self->{'media'}->[-1]->{$field} = $value;
			}
		
		}
		
	}


	# Ensure we have the required elements
	$self->_validate_self();


	# Success
	return 1;	
}


# Ensure we have the right session elements
sub _validate_self {
	my $self = shift;
	my $session = $self->{'session'};

	# The following elements are required
	if (!defined $self->{'v'}) {
		carp "Invalid SDP file: Missing version field";
		return 1;
	}
	if (!defined $session->{'o_sess_id'}) {
		carp "Invalid SDP file: Missing origin session ID field";
		return 1;
	}
	if (!defined $session->{'o_sess_vers'}) {
		carp "Invalid SDP file: Missing origin version field";
		return 1;
	}
	if (!defined $session->{'s'}) {
		carp "Invalid SDP file: Missing session name field";
		return 1;
	}
		
	
	# We should have a Time Description...
	if (!exists $self->{'time'}->[0]) {
		carp "Invalid SDP file: Session is missing required time discription";
		
		# Make it valid :-/
		$self->{'time'}->[0] = new Net::SDP::Time();
	}
	
	# Everything is ok :)
	return 0;
}

sub generate {
	my $self=shift;
	my $session = $self->{'session'};
	my $sdp = '';

	# The order of the fields must be as stated in the RFC
	$sdp .= $self->_generate_v();
	$sdp .= $self->_generate_o();
	$sdp .= _generate_lines($session, 's', 0 );
	$sdp .= _generate_lines($session, 'i', 1 );
	$sdp .= _generate_lines($session, 'u', 1 );
	$sdp .= _generate_lines($session, 'e', 1 );
	$sdp .= _generate_lines($session, 'p', 1 );
	#c=	- I don't like having c lines here !
	#	The module will put c= lines in the media description
	$sdp .= _generate_lines($session, 'b', 1 );


	# Time Descriptions
	if (scalar(@{$self->{'time'}})==0) {
		# At least one is required
		warn "Missing Time description";
		return undef;
	}
	foreach my $time ( @{$self->{'time'}} ) {
		$sdp .= $time->_generate_t();
		#$sdp .= _generate_lines($time, 'z', 1 );
		$sdp .= $time->_generate_r();
	}

	$sdp .= _generate_lines($session, 'k', 1 );
	$sdp .= _generate_lines($session, 'a', 1 );


	# Media Descriptions
	foreach my $media ( @{$self->{'media'}} ) {
		$sdp .= $media->_generate_m();
		$sdp .= _generate_lines($media, 'i', 1 );
		# 'c' is non-optional because we dont have one 
		# in the session description
		$sdp .= $media->_generate_c();
		$sdp .= _generate_lines($media, 'b', 1 );
		$sdp .= _generate_lines($media, 'k', 1 );
		$sdp .= _generate_lines($media, 'a', 1 );
	}

	# Return the SDP description we just generated
	return $sdp;
}

sub _generate_lines {
	my ($hashref, $field, $optional) = @_;
	my $lines = '';

	if (exists $hashref->{$field} and
	    defined $hashref->{$field}) {
		if (ref $hashref->{$field} eq 'ARRAY') {
			foreach( @{$hashref->{$field}} ) {
				$lines .= "$field=$_\n";
			}
		} elsif (ref $hashref->{$field} eq 'HASH') {
			foreach my $att_field ( sort keys %{$hashref->{$field}} ) {
				my $attrib = $hashref->{$field}->{$att_field};
				if (ref $attrib eq 'ARRAY') {
					foreach my $att_value (@{$attrib}) {
						$lines .= "$field=$att_field:$att_value\n";
					}
				} else {
					$lines .= "$field=$att_field\n";
				}
			}
		} else {
			$lines = $field.'='.$hashref->{$field}."\n";
		}
	} else {
		if (!$optional) {
			warn "Non-optional field '$field' missing";
		}
	}
	
	return $lines;
}


sub _parse_o {
	my $self = shift;
	my $session = $self->{'session'};
	my ($o) = @_;

	($session->{'o_uname'},
	 $session->{'o_sess_id'},
	 $session->{'o_sess_vers'},
	 $session->{'o_net_type'},
	 $session->{'o_addr_type'},
	 $session->{'o_address'}) = split(/\s/, $o);
		
	# Success
	return 1;
}


sub _generate_o {
	my $self = shift;
	return "o=".$self->session_origin()."\n";
}


sub _parse_v {
	my $self = shift;
	$self->{'v'} = shift;
	
	# Check the version number
	if ($self->{'v'} ne '0') {
		carp "Unsupported SDP format version number: ".$self->{'v'};
		return 0;
	}
	
	# Success
	return 1;
}


sub _generate_v {
	my $self = shift;
	return "v=0\n";
}

# hashref - the hash to add the attribute to
# field - the name of the field - ie 'a'
# value - the actual attribute
sub _add_attribute {
	my ($hashref, $field, $value) = @_;
	
	if (!defined $hashref->{$field}) {
		$hashref->{$field} = {};
	}
	
	if ( my($att_field, $att_value) = ($value =~ /^([\w\-\_]+):(.*)$/) ) {
		my $fieldref = $hashref->{$field};
		
		if (!defined $fieldref->{$att_field}) {
			$fieldref->{$att_field} = [];
		}
		
		push( @{$fieldref->{$att_field}}, $att_value );
	
	} else {
		$hashref->{$field}->{$value} = '';
	}
}

sub session_origin {
    my $self=shift;
	my $session = $self->{'session'};
	my ($o) = @_;
	
	$self->_parse_o( $o ) if (defined $o);

	return  $session->{'o_uname'} .' '.
			$session->{'o_sess_id'} .' '.
			$session->{'o_sess_vers'} .' '.
			$session->{'o_net_type'} .' '.
			$session->{'o_addr_type'} .' '.
			$session->{'o_address'};
}

sub session_origin_username {
    my $self=shift;
	my ($uname) = @_;
	$self->{'session'}->{'o_uname'} = $uname if (defined $uname);
	return $self->{'session'}->{'o_uname'};
}

sub session_origin_id {
    my $self=shift;
	my ($id) = @_;
	$self->{'session'}->{'o_sess_id'} = $id if (defined $id);
	return $self->{'session'}->{'o_sess_id'};
}

sub session_origin_version {
	my $self=shift;
	my ($vers) = @_;
    $self->{'session'}->{'o_sess_vers'} = $vers if defined $vers;
	return $self->{'session'}->{'o_sess_vers'};
}

sub session_origin_net_type {
	my $self=shift;
	my ($net_type) = @_;
    $self->{'session'}->{'o_net_type'} = $net_type if defined $net_type;
	return $self->{'session'}->{'o_net_type'};
}

sub session_origin_addr_type {
	my $self=shift;
	my ($addr_type) = @_;
    $self->{'session'}->{'o_addr_type'} = $addr_type if defined $addr_type;
	return $self->{'session'}->{'o_addr_type'};
}

sub session_origin_address {
	my $self=shift;
	my ($addr) = @_;
    $self->{'session'}->{'o_address'} = $addr if defined $addr;
	return $self->{'session'}->{'o_address'};
}



# Returns a unique identifier for this session
#
sub session_identifier {
	my $self=shift;
	my $session = $self->{'session'};

	return	$session->{'o_uname'} . 
			sprintf("%x",$session->{'o_sess_id'}) .
			$session->{'o_net_type'} .
			$session->{'o_addr_type'} .
			$session->{'o_address'};
}


sub session_name {
    my $self=shift;
	my ($s) = @_;
    $self->{'session'}->{'s'} = $s if defined $s;
    return $self->{'session'}->{'s'};
}

sub session_info {
    my $self=shift;
	my ($i) = @_;
    $self->{'session'}->{'i'} = $i if defined $i;
    return $self->{'session'}->{'i'};
}

sub session_uri {
    my $self=shift;
	my ($u) = @_;
    $self->{'session'}->{'u'} = $u if defined $u;
    return $self->{'session'}->{'u'};
}

sub session_email {
    my $self=shift;
	my ($e) = @_;
    my $session = $self->{'session'};
    
	# An ARRAYREF may be passed to set more than one email address
	if (defined $e) {
		if (ref $e eq 'ARRAY') {
			$session->{'e'} = $e;
		} else {
			$session->{'e'} = [ $e ];
		}
	}

    # Multiple emails are allowed, but we just return the first
    if (exists $session->{'e'}->[0]) {
    	return $session->{'e'}->[0];
    }
    return undef;
}

sub session_email_arrayref {
    my $self=shift;
    my $session = $self->{'session'};
    
    if (defined $session->{'e'}) {
        return $session->{'e'};
    }
    return undef;
}

sub session_phone {
    my $self=shift;
	my ($p) = @_;
    my $session = $self->{'session'};
    
	# An ARRAYREF may be passed to set more than one phone number
	if (defined $p) {
		if (ref $p eq 'ARRAY') {
			$session->{'p'} = $p;
		} else {
			$session->{'p'} = [ $p ];
		}
	}

    # Multiple phone numbers are allowed, but we just return the first
    if (exists $session->{'p'}->[0]) {
    	return $session->{'p'}->[0];
    }
    return undef;
}

sub session_phone_arrayref {
    my $self=shift;
    my $session = $self->{'session'};
    
    if (defined $session->{'p'}) {
        return $session->{'p'};
    }
    return undef;
}

sub session_key {
    my $self=shift;
	my ($method, $key) = @_;
	
    $self->{'session'}->{'k'} = $method if defined $method;
    $self->{'session'}->{'k'} .= ":$key" if defined $key;
	
    return ($self->{'session'}->{'k'} =~ /^([\w-]+):?(.*)$/);
}



sub _attribute {
	my ($hashref, $attr_name, $attr_value) = @_;
	carp "Missing attribute name" unless (defined $attr_name);
	
	# Set attribute to value, if value supplied
	# Warning - all other values are lost
	if (defined $attr_value) {
		if (ref $attr_value eq 'ARRAY') {
			$hashref->{'a'}->{$attr_name} = $attr_value;
		} else {
			$hashref->{'a'}->{$attr_name} = [ $attr_value ];
		}
	}
	
	# Return undef if attribute doesn't exist
	if (!exists $hashref->{'a'}->{$attr_name}) {
		return undef;
	}
	
	# Return 1 if attribute exists but has no value
	# Return value if attribute has single value
	# Return arrayref if attribute has more than one value
	my $attrib = $hashref->{'a'}->{$attr_name};
	if (ref $attrib eq 'ARRAY') {
		if (scalar(@{ $attrib }) == 1) {
			return $attrib->[0];
		} else {
			return $attrib;
		}
	} else {
		return '';
	}
}

sub session_attribute {
    my $self=shift;

    return Net::SDP::_attribute( $self->{'session'}, @_);
}

sub session_attributes {
    my $self=shift;

    return $self->{'session'}->{'a'};
}

# Add a session atrribute
sub session_add_attribute {
	my $self = shift;
	my ($name, $value) = @_;
	carp "Missing attribute name" unless (defined $name);
	
	my $attrib = $name;
	$attrib .= ":$value" if (defined $value);
	Net::SDP::_add_attribute( $self->{'session'}, 'a', $attrib );
}

# Delete a session atrribute
sub session_del_attribute {
	my $self = shift;
	my ($name) = @_;
	carp "Missing attribute name" unless (defined $name);

	if ( exists $self->{'session'}->{'a'}->{$name} ) {
		delete $self->{'session'}->{'a'}->{$name};
	}
}





# Returns first media description of specified type
sub media_desc_of_type {
	my $self = shift;
	my ($type) = @_;
	carp "Missing media type parameter" unless (defined $type);
	
    foreach my $media ( @{$self->{'media'}} ) {
    	return $media if ($media->media_type() eq $type);
	}
	
	return undef;
}


# Return all media descriptions
sub media_desc_arrayref {
	my ($self) = @_;
	
	return $self->{'media'};
}

# delete all Net::SDP::Media elements
sub media_desc_delete_all {
	my ($self) = @_;

	$self->{'media'} = [ ];
	
	return 0;
}

# delete a specific ARRAYREF Net::SDP::Media element
sub media_desc_delete {
	my $self = shift;
             my ($num) = @_;
	
	return 1 if ( !defined($num) || !defined($self->{'media'}->[$num]) );

	my $results = [ ];
	for my $loop ( 0...(scalar(@{$self->{'media'}}) - 1) ) {
		next if ( $loop == $num );
		
		push @$results, $self->{'media'}->[$loop];
	}
	$self->{'media'} = $results;

	return 0;
}

# Return $num time description, for backwards compatibility the
# first time description by default if nothing is passed to it
sub time_desc {
	my $self = shift;
	my ($num) = @_;
	
	$num = 0 unless ( defined $num );
	return undef unless ( defined($self->{'time'}->[$num]) );

	## Ensure that one exists ?
	return $self->{'time'}->[$num];
}

# Return all time descriptions
sub time_desc_arrayref {
	my ($self) = @_;

	return $self->{'time'};
}

# delete all Net::SDP::Time elements
sub time_desc_delete_all {
	my ($self) = @_;

	$self->{'time'} = [ ];
	
	return 0;
}

# delete a specific ARRAYREF Net::SDP::Time element
sub time_desc_delete {
	my $self = shift;
	my ($num) = @_;
	
	return 1 unless ( defined $num );
	return 1 unless ( defined $self->{'time'}->[$num] );

	my $results = [ ];
	for my $loop ( 0...(scalar(@{$self->{'time'}}) - 1) ) {
		next if ( $loop == $num );
		
		push @$results, $self->{'time'}->[$loop];
	}
	$self->{'time'} = $results;

	return 0;
}


# Net::SDP::Time factory method
sub new_time_desc {
	my $self = shift;
	
	my $time = new Net::SDP::Time();
	push( @{$self->{'time'}}, $time );

	return $time;
}


# Net::SDP::Media factory method
sub new_media_desc {
	my $self = shift;
	my ($media_type) = @_;
	
	my $media = new Net::SDP::Media();
	$media->media_type( $media_type ) if (defined $media_type);
	push( @{$self->{'media'}}, $media );

	return $media;
}



sub DESTROY {
    my $self=shift;
    
}


1;

__END__

=pod

=head1 NAME

Net::SDP - Session Description Protocol (rfc2327)

=head1 SYNOPSIS

  use Net::SDP;

  my $sdp = Net::SDP->new();
  
  $sdp->parse_file( 'myfile.sdp' );
  
  print "Session name: ".$sdp->session_name()."\n";
  print "Session info: ".$sdp->session_info()."\n";
  print "Session tool: ".$sdp->session_attribute('tool')."\n";
  
  $sdp->session_info( "This is my session" );
  
  print $sdp->generate();


=head1 DESCRIPTION

C<Net::SDP> is an SDP (Session Description Protocol) parser and generator. 
C<Net::SDP> is object oriented and a single instance of C<Net::SDP> represents
a single SDP session description. There are methods to easily get, set and create 
each of the fields in the session description.

The classes C<Net::SDP::Time> and C<Net::SDP::Media> are automatically instantiated 
for each Time Description (t=) and Media Description (m=).

=head2 METHODS

=over 4

=item B<new( [source] )>

Creates a new C<Net::SDP> session description object with default values for 
the required fields.

If the optional paramater C<source> is specified, then it is passed to 
parse(). If parsing fails, than new() will return undef.


=item B<parse( source )>

Parses in an SDP description. This method tries to work our the source of the SDP data
automatically and is ideal for use with command line tools. The source may be 
a path to a file, a URL, a C<Net::SAP::Packet> object, SDP data itself or if undefined
 will be read in on STDIN.
Returns 1 if successful, or 0 on failure.


B<NOTE:> it is faster to pass SDP data straight into the new() method, as it does not 
then initialise the object with default values, this involves doing DNS lookups to
find out the name of the local host.


=item B<parse_file( filepath )>

Parses in an SDP description from the specified file path. Returns 1 if successful, or 0 on failure.


=item B<parse_url( url )>

Parses in an SDP description from the specified URL. LWP (libwww-perl) is required for this method. 
Returns 1 if successful, or 0 on failure.


=item B<parse_stdin()>

Parses in an SDP description from STDIN. Returns 1 if successful, or 0 on failure.


=item B<parse_data( sdpdata )>

Parses the SDP description data passed as parameter. Returns 1 if successful, or 0 on failure.


=item B<generate()>

Generates and returns SDP description. Note that you must have set the compulsory fields and have 
at least one Time Description for the SDP description to be valid.


=item B<session_origin()>

Get or Set the whole of the session origin field. B<[o=]>

Example:

	$origin = $sdp->session_origin();
	$sdp->session_origin( 'njh 3303643609 3303643669 IN IP4 152.78.104.83' );
  

=item B<session_origin_username()>

Get or Set the username subfield of the session origin field. B<[o=]>

Example:

	$username = $sdp->session_origin_username();
	$sdp->session_origin_username( 'njh' );
  

=item B<session_origin_id()>

Get or Set the ID subfield of the session origin field. This should be an NTP timestamp. B<[o=]>

Example:

	$id = $sdp->session_origin_id();
	$sdp->session_origin_id( 3303643609 );
  

=item B<session_origin_version()>

Get or Set the version subfield of the session origin field. This should be an NTP timestamp. B<[o=]>

Example:

	$version = $sdp->session_origin_version();
	$sdp->session_origin_version( 3303643669 );


=item B<session_origin_net_type()>

Get or Set the network type subfield of the session origin field. In most cases this will be 'IN'. B<[o=]>

Example:

	$net_type = $sdp->session_origin_net_type();
	$sdp->session_origin_net_type( 'IN' );


=item B<session_origin_addr_type()>

Get or Set the address type subfield of the session origin field. In most cases this will be 'IP4'. B<[o=]>

Example:

	$addr_type = $sdp->session_origin_addr_type();
	$sdp->session_origin_addr_type( 'IP6' );


=item B<session_origin_address()>

Get or Set the address subfield of the session origin field. 
This may be a fully qualified domain name or global IP address but 
must not be a private IP or localhost. B<[o=]>

Example:

	$address = $sdp->session_origin_address();
	$sdp->session_origin_address( '152.78.104.83' );


=item B<session_identifier()>

Returns a unqiue identifier string of this sessions origin.
It contains all of the sub fields in the origin field apart from the version subfield. B<[o=]>

Example:

	$ident = $sdp->session_identifier();


=item B<session_name()>

Get or Set the session name field. B<[s=]>

Example:

	$name = $sdp->session_name();
	$sdp->session_name( 'My Cool Session' );


=item B<session_info()>

Get or Set the session information/description field. B<[i=]>

Example:

	$name = $sdp->session_info();
	$sdp->session_info( 'Broadcast live from Southampton' );


=item B<session_uri()>

Get or Set the session URI field. Used by WWW clients to get more information about the session. B<[u=]>

Example:

	$name = $sdp->session_uri();
	$sdp->session_uri( 'http://www.surgeradio.co.uk' );


=item B<session_email()>

Get or Set the session email field.
Although uncommon, more than one email address field is valid.
You can set multiple email addresses by passing them in an ARRAYREF.
This method will only return the first email address. B<[e=]>

Example:

	$email = $sdp->session_email();
	$sdp->session_email( 'njh@ecs.soton.ac.uk' );
	$sdp->session_email( ['njh@ecs.soton.ac.uk', 'njh@surgeradio.co.uk'] );

=item B<session_email_arrayref()>

Returns all email addresses as an array reference. Will return an
empty ARRAYREF if no email addresses are available.


=item B<session_phone()>

Get or Set the session telephone number field.
Although uncommon, more than one phone number field is valid.
You can set multiple phone numbers by passing them in an ARRAYREF.
This method will only return the first phone number. B<[p=]>

Example:

	$phone = $sdp->session_phone();
	$sdp->session_phone( '+44 870 357 2287' );
	$sdp->session_phone( ['0870 357 2287', '41287'] );


=item B<session_phone_arrayref()>

Returns all phone numbers as an array reference. Will return an
empty ARRAYREF if no phone numbers are available.


=item B<session_key( method, [key] )>

Get or Set the session encryption key field.
When setting the key parameter is optional - dependant on the method. B<[k=]>

Example:

	($method, $key) = $sdp->session_key();
	$sdp->session_key( 'prompt' );
	$sdp->session_key( 'base64', 'AoItAE8BAQ8DAQOBQwA' );


=item B<session_attribute( name, [value] )>

Get or Set an attribute for this session description. B<[a=]>

When setting an attribute, if you pass in a scalar, then all attributes with
the same name will be replaced. Alternively an attribute may be set to multiple
values by passing an ARRAYREF. If an attribute does not require it, 
then the value parameter is optional - eg for 'recvonly' attribute.

When getting an attribute that has no value, then '' is returned, 
or if the attribute does not exists then undef is returned.
If the attribute has a single value, then that value is returned, 
or if it has more than one value then an ARRAYREF is returned.

Example:

	$tool = $sdp->session_attribute( 'tool' );
	$sdp->session_attribute( 'recvonly' );


=item B<session_attributes()>

Get a HASHREF of all the attributes associated with this session description B<[a=]>

Example:

	$hashref = $sdp->session_attributes();


=item B<session_add_attribute( name, [value] )>

Add a value for sepecified attribute. This method is intended to be used with attributes
with multiple values - eg lang B<[a=]>

Example:

	$audio->session_add_attribute( 'lang', 'en');
	$audio->session_add_attribute( 'lang', 'fr');
	
	
=item B<session_del_attribute( name )>

Deletes all attributes of given name.
Example:

	$audio->session_del_attribute( 'lang' );


=item B<media_desc_of_type( type )>

Returns the first media description (as a C<Net::SDP::Media>) of the specified type.

Example:

	$audio = $sdp->media_desc_of_type( 'audio' );


=item B<media_desc_arrayref( )>

Returns an ARRAYREF of all the media descriptions - C<Net::SDP::Media> objects.


=item B<media_desc_delete_all()>

Deletes all media descriptors.


=item B<media_desc_delete( $num )>

Delete media description with index C<$num>.
Returns 0 if successful or 1 on failure.


=item B<time_desc( [$num] )>

Returns the time description with index number $num. 
Returns the first time description if, $num is undefined.
Return undef if no time description of chosen index is available.
Returns a C<Net::SDP::Time>.


=item B<time_desc_arrayref()>

Returns an ARRAYREF of all the time descriptions - C<Net::SDP::Time> objects.


=item B<time_desc_delete_all()>

Deletes all time descriptors.


=item B<time_desc_delete( $num )>

Delete time description with index C<$num>.
Returns 0 if successful or 1 on failure.


=item B<new_time_desc()>

Creates a new time description for the session and returns a new C<Net::SDP::Time> object.


=item B<new_media_desc( [type] )>

Creates a new media description for the session and returns a new C<Net::SDP::Media> object.
The type parameter is optional, and will set the media type if specified.

Example:

	$time = $sdp->new_media_desc( 'audio' );


=back

=head1 TODO

=over

=item Stricter parsing of SDP, so that it can be used as a validator

=item Add support for Zone Adjustments (z=)

=back

=head1 SEE ALSO

perl(1), L<Net::SAP>

L<http://www.ietf.org/rfc/rfc2327.txt>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-net-sdp@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.  I will be notified, and then you'll automatically
be notified of progress on your bug as I make changes.

=head1 AUTHOR

Nicholas J Humfrey, njh@cpan.org

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004 University of Southampton

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.005 or,
at your option, any later version of Perl 5 you may have available.

=cut
