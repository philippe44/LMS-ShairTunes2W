package Net::SDP::Time;

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
use vars qw/$VERSION/;
use constant NTPOFFSET => 2208988800;
use Carp;

$VERSION="0.07";



sub new {
	my $class = shift;
	my $self = {
		't_start' => '0',
		't_end' => '0',
		'r'	=> [],
	};
	bless $self, $class;	

	# Initial value provided ?
	my ($t) = @_;
	$self->_parse_t($t) if (defined $t);

	return $self;
}

sub _ntptime {
	return time() + NTPOFFSET;
}

#sub remove {
#    my $self=shift;
#
#	### Delete ourselves from our parent's array    
#    
#    undef $self;
#}


sub _parse_t {
	my $self = shift;
	my ($t) = @_;
	
	# we need two positive integers
	# t=<start-time> <end-time>
	if ( $t !~ /^([0-9]+) ([0-9]+)$/ ) {
		warn "Invalid 't' passed: $t";
		return 0;
	}

	($self->{'t_start'}, $self->{'t_end'}) = split(/ /, $t);
	
	# Success
	return 1;
}

sub _generate_t {
	my $self = shift;

	return "t=".$self->{'t_start'}.' '.$self->{'t_end'}."\n";
}

sub _parse_r {
	my $self = shift;
	my ($r) = @_;
	
	if ( $self->is_permanent ) {
		warn "corrupt packet, you cannot have a repeat field for a permanent session";
		return 0;
	}
	
	# we need at least three
	# r=<repeat-interval> <active duration> <offsets from start-time>
	if ( $r !~ /^([0-9]+[dhms]?)( [0-9]+[dhms]?){2,}$/ ) {
		warn "Invalid 'r' passed: $r";
		return 0;
	}

	my @values = split / /, $r;
        if ( $values[0] == 0 ) {
	  warn "you cannot have a repeat interval of zero";
	  return 0;
	}
	
	_repeat_push($self, \@values);
	
	# Success
	return 1;
}

sub _generate_r {
	my $self = shift;
	
	my $result = '';
	foreach my $item ( @{$self->{'r'}} )
	{
		my $element = _rollup_seconds($item->{'interval'}) . ' '
				    . _rollup_seconds($item->{'duration'});

		foreach my $offset ( @{$item->{'offsets'}} ) {
			$element .= ' ' . _rollup_seconds( $offset );
		}
		
		$result .= 'r=' . $element . "\n";
	}

	return $result;
}


sub start_time_ntp {
    my $self=shift;
	my ($start_time) = @_;
	if ( defined $start_time ) {
		$self->{'t_start'} = $start_time;
		# you cannot have a permanent session with repeat interval
		$self->repeat_delete_all if ( $start_time == 0 );
	}
	return $self->{'t_start'};
}

sub end_time_ntp {
    my $self=shift;
	my ($end_time) = @_;
    $self->{'t_end'} = $end_time if defined $end_time;
	return $self->{'t_end'};
}

sub start_time_unix {
    my $self=shift;
	my ($start_time) = @_;
	$self->start_time_ntp( $start_time+NTPOFFSET ) if (defined $start_time);
    return 0 if ($self->start_time_ntp()==0);
    return $self->start_time_ntp() - NTPOFFSET;
}

sub end_time_unix {
    my $self=shift;
	my ($end_time) = @_;
	$self->end_time_ntp( $end_time+NTPOFFSET ) if (defined $end_time);
    return 0 if ($self->end_time_ntp()==0);
    return $self->end_time_ntp() - NTPOFFSET;
}

sub start_time {
    my $self=shift;
    return "Permanent" if ($self->is_permanent());
    return scalar(localtime($self->start_time_unix()))
}

sub end_time {
    my $self=shift;
    return "Permanent" if ($self->is_permanent());
	return "Unbounded" if ($self->end_time_ntp()==0);
    return scalar(localtime($self->end_time_unix()))
}

sub is_permanent {
    my $self=shift;
    
    if ($self->start_time_ntp()==0)
			{ return 1; }
	else	{ return 0; }
}

sub make_permanent {
    my $self=shift;
	$self->{'t_start'} = 0;
	$self->{'t_end'} = 0;

    # you cannot have a permanent session with repeat intervals
    $self->repeat_delete_all;
}

sub is_unbounded {
    my $self=shift;
    
    if ($self->end_time_ntp()==0)
			{ return 1; }
	else	{ return 0; }
}

sub make_unbounded {
    my $self=shift;
	$self->{'t_end'} = 0;

    # you cannot have a permanent session with repeat intervals
    $self->repeat_delete_all();
}

sub as_string {
    my $self=shift;

	# Permanent
	if ( $self->is_permanent() ) {
   		return "Broadcasts permanently.";
   	}


    # Repeat elements present
    if ( @{$self->{'r'}} ) {
		my $text;
		
		if ( $self->end_time_ntp() == 0 ) {
			$text = 'Broadcasts ';
		}
		else {
			$text = 'Until ' . $self->end_time() . ', broadcasts ';
		}

		my @repeatSlots = ();
		foreach my $repeat ( @{$self->{'r'}} ) {
			my $interval = _summariseTime($repeat->{interval});
			
			my %results = (
				interval	=> $interval,
				times		=> [ ]
			);
			
			my @abbr = qw( Sun Mon Tue Wed Thu Fri Sat );
			# the results output depends on the interval
			foreach my $offset ( @{$repeat->{offsets}} ) {
				my @startTime = localtime($self->start_time_unix() + $offset);
				my @endTime = localtime($self->start_time_unix() + $offset + $repeat->{duration} );
				
				my $time;
				# weekly: display which day
				if ( $repeat->{interval} == 604800 ) {
					$time = 'every ' . $abbr[$startTime[6]] . ', from ';
					$time .= _buildHourlyTime(\@startTime, \@endTime);
				}
				# daily: display which hour
				elsif ( $repeat->{interval} == 86400 ) {
					$time = _buildHourlyTime(\@startTime, \@endTime);
				}
				# hourly: display minutely times
				elsif ( $repeat->{interval} == 3600 ) {
					$time = 'from ' . $startTime[1] . 'mins until ' . $endTime[1] . 'mins past';
				}
				# we fall back to the best we can do which is a more direct
				# textual description of the 'r' field
				# anyone being caught here might want to consider improving
				# the above common cases and/or adding their common cases
				# that I could not think of
				else {
					my $friendlierOffset = _rollup_seconds($offset);
					$friendlierOffset .= 's'
					if ( $friendlierOffset =~ /^[0-9]+$/ );
					my $friendlierDuration = _rollup_seconds($repeat->{duration});
					$friendlierDuration .= 's'
					if ( $friendlierDuration =~ /^[0-9]+$/ );
					
					$time =  " starting $friendlierOffset past the interval and lasting $friendlierDuration";
				}
			
				push @{$results{times}}, $time;
			}
		
			push @repeatSlots, \%results;
		}
      
		while ( my $repeater = shift @repeatSlots ) {
			if ( $repeater->{interval} !~ /^[0-9]+$/ ) {
				$text .= 'every ' . $repeater->{interval} . ' ';
			} else {
				my $friendlierInterval = _rollup_seconds($repeater->{interval});
				$friendlierInterval .= 's' if ( $friendlierInterval =~ /^[0-9]+$/ );
				$text .= "every $friendlierInterval interval ";
			}
			
			while ( my $repeaterTimes = shift @{$repeater->{times}} ) {
				$text .= $repeaterTimes;
				
				if ( scalar(@{$repeater->{times}}) > 1 ) {
					$text .= ', ';
				}
				elsif ( scalar(@{$repeater->{times}}) == 1 ) {
					$text .= ' and ';
				}
			}
			
			if ( scalar(@repeatSlots) > 1 ) {
				$text .= ', again';
			}
			elsif ( scalar(@repeatSlots) == 1 ) {
				$text .= ', and again ';
			}
		}
		
		$text .= ' starting ' . $self->start_time() . '.';
		
		return $text;
    }
    
    # no repeat elements so nice and simple
    else {
		if ( $self->start_time_ntp() == 0 ) {
			return 'Broadcasts until ' . $self->end_time().'.' ;
		}
		else {
			return 'Broadcasts from ' . $self->start_time() . ' until ' . $self->end_time().'.' ;
		}
	}
}

sub _buildHourlyTime {
	my $startTime = shift;
	my $endTime = shift;
	
	my @times = ( $startTime->[2], $startTime->[1], $endTime->[2], $endTime->[1] );
	
	foreach my $item ( 0..(scalar(@times) - 1 ) ) {
		$times[$item] = ( length($times[$item]) == 1 )
			? '0' . $times[$item]
			: $times[$item];
	}
	
	return $times[0] . ':' . $times[1] . ' until ' . $times[2] . ':' . $times[3];
}

sub _summariseTime {
	my $value = shift;
	
	# we can only do from minutes to weeks as after that how many
	# days are there in a month, what about a year, etc etc?
	if ( ( $value % 604800 ) == 0 ) {
		$value /= 604800;
		$value = ( $value == 1 ) ? 'week' : $value . ' weeks';
	}
	elsif ( ( $value % 86400 ) == 0 ) {
		$value /= 86400;
		$value = ( $value == 1 ) ? 'day' : $value . ' days';
	}
	elsif ( ( $value % 3600 ) == 0 ) {
		$value /= 3600;
		$value = ( $value == 1 ) ? 'hour' : $value . ' hours';
	}
	elsif ( ( $value % 60 ) == 0 ) {
		$value /= 60;
		$value = ( $value == 1 ) ? 'minute' : $value . ' minutes';
	}
	else {
		$value = ( $value == 1 ) ? 'second' : $value . ' seconds';
	}
	
	return $value;
}

sub repeat_add {
    my $self=shift;
	my ($interval, $duration, $offsets) = @_;
	carp "Missing interval parameter" unless (defined $interval);
	carp "Missing duration parameter" unless (defined $duration);
	carp "Missing offsets parameter" unless (defined $offsets);
	carp "Interval parameter cannot be zero" if ( $interval =~ /^\d+$/ and $interval == 0 );
	
    if ( $self->is_permanent ) {
		carp "repeat_add failed, you cannot have a repeat field for a permanent session";
		return;
    }
    
    # Make it is hashref if only one offset passed
	$offsets = [ $offsets ] if ( ref($offsets) ne 'ARRAY' );
    
	my @values = ( $interval, $duration, ( @$offsets ) );
	_repeat_push($self, \@values);

	return $self->{'r'}->[-1];
}

sub repeat_delete {
	my $self=shift;
	my ($num) = @_;
	
	return 1 if ( !defined($num) || !defined($self->{'r'}->[$num]) );
	
	my $results = [ ];
	for my $loop ( 0...(scalar(@{$self->{'r'}}) - 1) ) {
		next if ( $loop == $num );
		
		push @$results, $self->{'r'}->[$loop];
	}
	$self->{'r'} = $results;
	
	return 0;
}

sub repeat_delete_all {
    my $self=shift;

    $self->{'r'} = [ ];
    
    return 0;
}

sub repeat_desc {
	my $self=shift;
	
	my $num = shift;
	
	$num = 0 if ( !defined($num) );
	
	return undef if ( !defined($self->{'r'}->[$num]) );
	
	return $self->{'r'}->[$num];
}

sub repeat_desc_arrayref {
	my $self=shift;
	
	if ( defined($self->{'r'}) ) {
	  return $self->{'r'};
	}
	return undef;
}

sub _rollup_seconds {
	my $value = shift;
	
	if ( $value !~ /^[0-9]+[dhms]?$/ ) {
		carp "Invalid value parsed to _rollup_seconds: $value";
		return;
	}
	
	# if its already partially rolled up we should unroll it all first
	$value = _rollout_seconds($value)
	if ( $value !~ /^[0-9]+[dhms]$/ );
	
	# if its zero do nothing with it
	return 0 if ( $value == 0 );
	
	# try reducing to days
	if ( ( $value % 86400 ) == 0 ) {
		$value = ( $value / 86400 ) . 'd';
	}
	# try reducing to hours
	elsif ( ( $value % 3600 ) == 0 ) {
		$value = ( $value / 3600 ) . 'h';
	}
	# and finally try to minutes
	elsif ( ( $value % 60 ) == 0 ) {
		$value = ( $value / 60 ) . 'm';
	}
	
	return $value;
}

sub _rollout_seconds {
  my $value = shift;

	if ( $value !~ /^[0-9]+[dhms]?$/ ) {
		carp "Invalid value parsed to _rollout_seconds: $value";
		return;
	}
	
	# test for a NOOP
	if ( $value =~ /^[0-9]+s?$/ ) {
		$value = substr($value, 0, -1) if ( substr($value, -1, 1) eq 's' );
		return int($value);
	}
	
	my $unit = substr($value, -1, 1);
	$value = substr($value, 0, -1);
	
	if ( $unit eq 'd' ) {
		$value *= 86400;
	}
	elsif ( $unit eq 'h' ) {
		$value *= 3600;
	}
	# must be 'm' (minutes)
	else {
		$value *= 60;
	}
	
	return int($value);
}

sub _repeat_push {
	my $self=shift;
	
	my $values = shift;
	
	foreach my $item ( 0...(scalar(@$values) - 1) ) {
		$values->[$item] = _rollout_seconds($values->[$item]);
	}
	
	my $rProcessed = {
		interval	=> shift @$values,
		duration	=> shift @$values,
		offsets		=> [ ]
	};
	foreach my $offset ( @$values ) {
		push @{$rProcessed->{'offsets'}}, $offset;
	}
	
	push @{$self->{'r'}}, $rProcessed;
}

1;

__END__

=pod

=head1 NAME

Net::SDP::Time - Time Description in an SDP file

=head1 SYNOPSIS

  my $time = $sdp->new_time_desc();

  print "Session started: ".$time->start_time();
  
  $time->make_permanent();

=head1 DESCRIPTION

This class represents a single Time Description (t=) in an SDP file.
When parsing an SDP file, Net::SDP will create an instance of Net::SDP::Time
for each time description. New time descriptions can be created using the 
new_time_desc() method in Net::SDP.

=head2 METHODS

=over 4


=item B<start_time_ntp()>

Get or Set the Start Time as decimal representation of Network Time Protocol (NTP) 
time values in seconds. B<[t=]>

Example:

	$start_ntp = $time->start_time_ntp();
	$time->start_time_ntp( 3303564104 );


=item B<end_time_ntp()>

Get or Set the End Time as decimal representation of Network Time Protocol (NTP) 
time values in seconds. B<[t=]>

Example:

	$end_ntp = $time->end_time_ntp();
	$time->end_time_ntp( 3303567704 );


=item B<start_time_unix()>

Get or Set the Start Time as decimal Unix time stamp. B<[t=]>

Example:

	$start = $time->start_time_unix();
	$time->start_time_unix( time() );


=item B<end_time_unix()>

Get or Set the End Time as decimal Unix time stamp. B<[t=]>

Example:

	$end = $time->end_time_unix();
	$time->end_time_unix( time()+3600 );


=item B<start_time()>

Get a textual representation of the Start Time. B<[t=]>

Example:

	print "Session starts: ".$time->start_time();


=item B<end_time()>

Get a textual representation of the End Time. B<[t=]>

Example:

	print "Session ends: ".$time->end_time();


=item B<is_permanent()>

Returns true if the session is permanent.
Returns false if the session has a start or end time. B<[t=]>


=item B<make_permanent()>

Makes the session permanent - no start or end time. B<[t=]>


=item B<is_unbounded()>

Returns true if the session has no end time.
Returns false if the session has an end time. B<[t=]>


=item B<make_unbounded()>

Makes the session unbounded - no end time. B<[t=]>


=item B<repeat_add($interval, $duration, $offset)>

For a session that shows every day (86400, 1440m, 24h or 1d) for one hour 
 (3600, 60m or 1h) with an origin time from $time->start_time_ntp().  This 
 session then repeats that day six hours (21600, or 360m or 6h) later.

	my ( $interval, $duration ) = ( 86400, 3600 );
	my $offsets = [ 0, 21600 ]; # ARRAYREF
	$time->repeat_add($interval, $duration, $offsets);

This produces an 'r' field of: r=1d 1h 0 6h

RFC2327 (http://www.ietf.org/rfc/rfc2327.txt), page 14 titled
"Times, Repeat Times and Time Zones" should be consulted for clarification.

N.B. $offsets can be a single value (typically 0) regular SCALAR instead but 
	then you obviously only get a single value

B<[r=]>


=item B<repeat_delete($num)>

Delete repeat field element $num.  
Returns 0 on sucess, 1 if its a bad request. B<[r=]>


=item B<repeat_delete_all()>

Deletes any exising repeat fields. B<[r=]>


=item B<repeat_desc($num)>

Returns repeat array element $num.  

	my $repeat = $time->repeat_desc(2);

If element does not exist 'undef' is returned, if nothing is passed to the 
function it defaults to the first element.  The format of the 'reply' is 
(all values are in seconds):

	$repeat = { 
	    interval  => <interval>, 
	    duration  => <duration>,
	    offsets  => [ <offset1>, <offset2> ... ]
	};

=item B<repeat_desc_arrayref()>

Returns all the repeat elements in an ARRAYREF.


=item B<as_string()>

Returns a textual representation/summary of the time description.

Example:

	'Tue Aug  2 11:13:28 2004 until Tue Aug  2 13:15:00 2004'

=back

=head1 AUTHOR

Nicholas J Humfrey, njh@cpan.org

Alexander Clouter, alex@digriz.org.uk

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004 University of Southampton

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.005 or,
at your option, any later version of Perl 5 you may have available.

=cut
