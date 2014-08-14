=head1 NAME

Algorithm::QuineMcCluskey - solve Quine-McCluskey set-cover problems

=cut

package Algorithm::QuineMcCluskey;

use strict;
use warnings;

use Algorithm::QuineMcCluskey::Util qw(
	bin columns diffpos hdist stl uniqels
);
use Moose;
use Moose::Util::TypeConstraints;
use Carp qw(carp croak);
use Data::Dumper;
use List::Compare::Functional qw(:main is_LequivalentR);
use List::Util qw(sum min);
use Tie::Cycle;

#
# Required attributes to create the object.
#
has 'dontcares'	=> (
	isa => 'ArrayRef[Int]', is => 'rw', required => 0,
	predicate => 'has_dontcares'
	);
has 'minterms'	=> (
	isa => 'ArrayRef[Int]', is => 'rw', required => 0,
	predicate => 'has_minterms'
	);
has 'maxterms'	=> (
	isa => 'ArrayRef[Int]', is => 'rw', required => 0,
	predicate => 'has_maxterms'
	);
has 'width'	=> (
	isa => 'Int', is => 'rw', required => 1
);

#
# Optional attributes.
#
has 'title'	=> (
	isa => 'Str', is => 'rw', required => 0,
	predicate => 'has_title'
);
has 'dc'	=> (
	isa => 'Str', is => 'rw',
	default => '-'
);
has 'minonly'	=> (
	isa => 'Bool', is => 'rw',
	default => 1
);
has 'sortterms'	=> (
	isa => 'Bool', is => 'rw',
	default => 1
);
has 'vars'	=> (
	isa => 'ArrayRef[Str]', is => 'rw', required => 0,
	default => sub{['A' .. 'Z']}
);

#
# Internal attributes. No using them at object creation.
#
#CHANGES: add init_arg => undef, and change to ro and add reader/writer accessor methods

#
# The terms' bitstring fields.
#
has 'dc_bits'	=> (
	isa => 'ArrayRef[Str]', is => 'rw', required => 0,
	init_arg => undef,
	predicate => 'has_dc_bits'
	);
has 'min_bits'	=> (
	isa => 'ArrayRef[Str]', is => 'rw', required => 0,
	init_arg => undef,
	predicate => 'has_min_bits'
	);
has 'max_bits'	=> (
	isa => 'ArrayRef[Str]', is => 'rw', required => 0,
	init_arg => undef,
	predicate => 'has_max_bits'
	);
has 'ess'	=> (
	isa => 'HashRef', is => 'ro', required => 0,
	init_arg => undef,
	reader => 'get_essentials',
	writer => '_set_essentials',
	predicate => 'has_essentials',
	clearer => 'clear_essentials'
	);
has 'primes'	=> (
	isa => 'HashRef', is => 'ro', required => 0,
	init_arg => undef,
	reader => 'get_primes',
	writer => '_set_primes',
	predicate => 'has_primes',
	clearer => 'clear_primes'
	);
has 'covers'	=> (
	isa => 'ArrayRef[Int]', is => 'ro', required => 0,
	init_arg => undef,
	reader => 'get_covers',
	writer => '_set_covers',
	predicate => 'has_covers',
	clearer => 'clear_covers'
);

=head1 VERSION

This document describes version 0.01 released 24 June 2006.

=cut

our $VERSION = 0.02;

=head1 SYNOPSIS

	use Algorithm::QuineMcCluskey;

	# Five-bit, 12-minterm Boolean expression test with don't-cares
	my $q = new Algorithm::QuineMcCluskey(
		width => 5,
		minterms => [ qw(0 5 7 8 10 11 15 17 18 23 26 27) ],
		dontcares => [ qw(2 16 19 21 24 25) ]
	);
	my @result = $q->solve;
	# @result is (
	# 	"(B'CE) + (C'E') + (AC') + (A'BDE)"
	# );

=head1 DESCRIPTION

NOTE: This module's API is NOT STABLE; the next version should support
multiple-output problems and will add more object-oriented features, but in
doing so will change the API. Upgrade at your own risk.

This module feebly stabs at providing solutions to Quine-McCluskey set-cover
problems, which are used in electrical engineering/computer science to find
minimal hardware implementations for a given input-output mapping. Since this
problem is NP-complete, and since this implementation uses no heuristics, it is
not expected to be useful for real-world problems.

The module is used in an object-oriented fashion; all necessary arguments can
be (and currently must be) provided to the constructor. Unless only a certain
step of is required, the whole algorithm is set off by calling solve() on an
Algorithm::QuineMcCluskey object; this method returns a list of boolean
expressions (as strings) representing valid solutions for the given inputs (see
the C<SYNOPSIS>).

=cut

#
# Sub and method definitions.
#

=head1 METHODS

=over 4

=item new

Default constructor

=cut

sub BUILD
{
	my $self = shift;
	my $w = $self->width;

	#
	# Catch errors.
	#
	croak "Mixing minterms and maxterms not allowed"
		if ($self->has_minterms and $self->has_maxterms);
	croak "Must supply either minterms or maxterms"
		unless ($self->has_minterms or $self->has_maxterms);

	#
	# Convert terms to strings of bits as needed.
	#
	if ($self->has_minterms)
	{
		my @bitstrings = map {
			substr(unpack("B32", pack("N", $_)), -$w)
		} @{$self->minterms};

		$self->min_bits(\@bitstrings);
	}
	if ($self->has_maxterms)
	{
		my @bitstrings = map {
			substr(unpack("B32", pack("N", $_)), -$w)
		} @{$self->minterms};

		$self->max_bits(\@bitstrings);
	}
	if ($self->has_dontcares)
	{
		my @bitstrings = map {
			substr(unpack("B32", pack("N", $_)), -$w)
		} @{$self->minterms};

		$self->dc_bits(\@bitstrings);
	}

	$self->title("$w-variable truth table") unless ($self->has_title);

	return $self;
}

sub allterms
{
	my $self = shift;
	my @terms;

	push @terms, @{ $self->min_bits } if ($self->has_min_bits);
	push @terms, @{ $self->max_bits } if ($self->has_max_bits);
	push @terms, @{ $self->dc_bits } if ($self->has_dc_bits);
	return @terms;
}

sub minmax_terms
{
	my $self = shift;
	my @terms;

	push @terms, @{ $self->min_bits } if ($self->has_min_bits);
	push @terms, @{ $self->max_bits } if ($self->has_max_bits);
	return @terms;
}

=item maskmatch

Returns true if a mask matches a minterm, false otherwise.

=cut

sub maskmatch
{
	my $self = shift;
	my ($mask, $term) = @_;
	my $dc = $self->dc;

	(my $mask0 = $mask) =~ s/$dc/0/g;
	(my $mask1 = $mask) =~ s/$dc/1/g;

	((bin $mask0 & bin $term) == bin $mask0) &&
		((bin $mask1 & bin $term) == bin $term)
}

=item maskmatches

Returns the elements that match a mask, selected from an array

=cut

sub maskmatches
{
	my $self = shift;
	my $m = shift;
	grep { $self->maskmatch($m, $_) } @_;
}

=item remel

Remove a value from an arrayref if it matches a mask

=cut

sub remel
{
	my $self = shift;
	my ($el, $a) = @_;
	my $pos = firstidx { $self->maskmatch($el, $_) } @$a;
	splice(@$a, $pos, 1) if $pos >= 0;
	$a;
}

=item find_primes

Finding prime essentials

=cut

sub find_primes
{
	my $self = shift;
	my @bits;
	my %implicant;

	#
	# Entering the sub message.
	#
	carp "find_primes:\n";

	# Separate into bins based on number of 1's
	for ($self->allterms())
	{
		my $l = sum stl $_;
		carp "    $_ converted to $l";
		push  @{$bits[0][ $l ]}, $_;
	}

	#
	# Dump here.
	#
	carp "    Dump of \@bits:\n";
	carp Dumper(\@bits);

	for my $level (0 .. $self->width)
	{
		#
		# Skip if we haven't generated such data
		#
		last unless ref $bits[$level];

		# Find pairs with Hamming distance of 1
		for my $low (0 .. $#{ $bits[$level] })
		{
			#
			# These nested for-loops get all permutations
			# of adjacent sets
			#
			for my $lv (@{ $bits[$level][$low] })
			{
				#
				# Initialize the implicant as unused; Skip
				# ahead if we don't have this data.
				# FIXME: explain
				#
				$implicant{$lv} //= 0;
				next unless ref $bits[$level][$low + 1];

				for my $hv (@{ $bits[$level][$low + 1] })
				{
					#
					# Initialize the implicant.
					#
					$implicant{$hv} //= 0;

					if (hdist($lv, $hv) == 1)
					{
						my $new = $lv;	# or $hv
						substr($new, diffpos($lv, $hv), 1) = $self->dc;
						#
						# Save new implicant to next
						# level, then mark the two
						# values as used.
						#
						push @{ $bits[$level + 1][$low + 1] }, $new;
						@{$implicant{$lv,$hv}} = (1, 1);
					}
				}
			}
		}
	}

	carp "    Dump after processing of \@bits:\n";
	carp Dumper(\@bits);

	my %p = map { $_ => [ $self->maskmatches($_, $self->minmax_terms()) ] }
		grep { !$implicant{$_} } keys %implicant;

	#
	# Carp the primes.
	#
	carp "    Setting attribute primes with:\n";
	carp Dumper(\%p);

	$self->_set_primes( \%p );
	return $self->get_primes;
}


=item row_dom

Row-dominance

=cut

sub row_dom
{
	my $self = shift;
	my $primes = shift || \%{$self->get_primes};

	$primes = { map {
		my $o = $_;
		(sum map {
			is_LsubsetR([ $primes->{$o} => $primes->{$_} ])
				&& !is_LequivalentR([ $primes->{$o} => $primes->{$_} ])
			} grep { $_ ne $o } keys %$primes)
		? () : ( $_ => $primes->{$_} )
	} keys %$primes };
	%$primes;
}

=item col_dom

Column-dominance

=cut

sub col_dom {
	my $self = shift;
	my $primes = shift || \%{$self->get_primes};

	my %cols = columns $primes, $self->minmax_terms();
	for my $col1 (keys %cols) {
		for my $col2 (keys %cols) {
			next if $col1 eq $col2;
			
			# If col1 is a non-empty proper subset of col2,
			# remove col2
			if (@{ $cols{$col1} }
					and is_LsubsetR		([ $cols{$col1} => $cols{$col2} ])
					and !is_LequivalentR	([ $cols{$col1} => $cols{$col2} ]))
			{
				remel $col2, $primes->{$_} for keys %$primes;
			}
		}
	}
	%$primes;
}

=item find_essentials

Finding essential prime implicants

=cut

sub find_essentials
{
	my $self = shift;

	my $primes = @_ ? shift : \%{$self->get_primes};
	my @terms = @_ ? @{ shift() } : ($self->minmax_terms());

	my @kp = keys %$primes;
	my %essentials;

	$self->clear_essentials;

carp "find_essentials:\n";
carp "    Keys of \$primes are [", join(", ", @kp), "]\n    Dump:\n";

#
# Find out what each is holding.
#
foreach my $kp (@kp)
{
	my @pt = @{ $primes->{$kp}};
	carp "    $kp => [", join(", ", @pt), "]\n";
}

	for my $term (@terms)
	{
carp "    For term '$term', ";
		#my $ess = ( map { @$_ == 1 ? @$_ : undef } [ grep {
		#	grep { $_ eq $term } @{ $primes->{$_} }
		#} keys %$primes ] )[0];

		#CHANGES: Move "keys %$primes" out of the loop - it's a constant list.

		#my $ess = ( map { @$_ == 1 ? @$_ : undef } [
		#	grep {
		#	grep { $_ eq $term } @{ $primes->{$_} } } @kp ] )[0];

		#CHANGES: Separate out the list from the double grep to see
		#         what's going on internally.
		my @tp = grep {
			grep { $_ eq $term } @{ $primes->{$_} } } @kp;

		my $ess = ( map { @$_ == 1 ? @$_ : undef }[@tp] )[0];
carp "    term/prime list is (", join(", ", @tp), "), ";

		# TODO: It would be nice to track the terms that make this essential
		# CHANGES: set up essentials in local hash, to be set in the object at the end.
carp "    'essential' found is ", (defined $ess)? $ess: "undef", "\n";
		$essentials{$ess}++ if $ess;
	}

	$self->_set_essentials(\%essentials);
	return $self;
}

=item purge_essentials

Delete essential primes from table

=cut

sub purge_essentials
{
	my $self = shift;
	my %ess = @_ ? %{ shift() } : %{$self->get_essentials};
	my $primes = shift || \%{$self->get_primes};

	# Delete columns associated with this term
	for my $col (keys %$primes) {
		remel $_, $primes->{$col} for keys %ess;
	}
	delete ${$primes}{$_} for keys %ess;
	return $self;
}

=item to_boolean

Generating Boolean expressions

=cut

sub to_boolean {
	my $self = shift;
	my @boolean;

	# Group separators (grouping character pairs)
	my @gs = ('(', ')');
	# Group joiner, element joiner, match condition
	my ($gj, $ej, $cond) = $self->has_min_bits ? (' + ', '', 1) : ('', ' + ', 0);
	tie my $var, 'Tie::Cycle', [ @{$self->vars}[0 .. $self->width - 1] ];

	push @boolean,
		join $gj, map { $gs[0] . (
			join $ej, map {
				my $var = $var;	# Activate cycle even if not used
				$_ eq $self->dc ? () : $var . ($_ == $cond ? '' : "'")
			} stl $_) . $gs[1]
		} @$_
		for ($self->get_covers);

	return @boolean;
}

=item solve

Main solution sub (wraps recurse_solve())

=cut

sub solve {
	my $self = shift;
	$self->find_primes unless ($self->has_primes);
	$self->_set_covers($self->recurse_solve($self->get_primes));
	$self->to_boolean();
}

=item recurse_solve

Recursive divide-and-conquer solver

=cut

sub recurse_solve
{
	no warnings 'closure';

	my $self = shift;
	my %primes = %{ $_[0] };
	my @prefix;
	my @covers;

	# begin (slightly) optimized block : do not touch without good reason
	my %ess = $self->find_essentials(\%primes);
	$self->purge_essentials(\%ess, \%primes);
	push @prefix, grep { $ess{$_} } keys %ess;

	$self->row_dom(\%primes);
	$self->col_dom(\%primes);

	while (!is_LequivalentR([
			[ keys %ess ] => [ %ess = $self->find_essentials(\%primes) ]
			])) {
		$self->purge_essentials(\%ess, \%primes);
		push @prefix, grep { $ess{$_} } keys %ess;
		$self->row_dom(\%primes);
		$self->col_dom(\%primes);
	}
	# end optimized block
	unless (keys %primes) {
		return [ reverse sort @prefix ];
	}
	# Find the term with the fewest implicant covers
	# Columns actually in %primes
	my @t = grep {
		my $o = $_;
		sum map { sum map { $_ eq $o } @$_ } values %primes
	} ($self->minmax_terms());

	# Flip table so terms are keys
	my %ic = columns \%primes, @t;
	my $term = (sort { @{ $ic{$a} } <=> @{ $ic{$b} } } keys %ic)[0];
	# Rows of %primes that contain $term
	my @ta = grep { sum map { $_ eq $term } @{ $primes{$_} } } keys %primes;
	
	# For each such cover, recursively solve the table with that column removed
	# and add the result(s) to the covers table after adding back the removed
	# term
	for my $ta (@ta) {
		my %reduced = map {
			$_ => [ grep { $_ ne $term } @{ $primes{$_} } ]
		} keys %primes;
		# Use this prime implicant -- delete its row and columns
		remel $ta, $reduced{$_} for keys %reduced;
		delete $reduced{$ta};
		# Remove empty rows (necessary?)
		%reduced = map { $_ => $reduced{$_} } grep { @{ $reduced{$_} } } keys %reduced;
		
		my @c = $self->recurse_solve(\%reduced);
		my @results = $self->sortterms
			? @c
				? map { [ reverse sort (@prefix, $ta, @$_) ] } @c
				: [ reverse sort (@prefix, $ta) ]
			: @c
				? map { [ @prefix, $ta, @$_ ] } @c
				: [ @prefix, $ta ];
		push @covers, @results;
	}

	# Weed out expensive solutions
	sub cost { sum map { /$self->dc/ ? 0 : 1 } stl join '', @{ shift() } }
	my $mincost = min map { cost $_ } @covers;
	@covers = grep { cost($_) == $mincost } @covers if $self->minonly;

	# Return our covers table to be treated similarly one level up
	# FIXME: How to best ensure non-duplicated answers?
	return uniqels @covers;
}

1;
__END__

=back

=head1 BUGS

Probably. The tests aren't complete enough, and the documentation is far from
complete. Features missing include multiple-output support, which is
in-progress but will require at least some rewriting to keep the code minimally
ugly.

Please report any bugs or feature requests to C<bug-algorithm-quinemccluskey at
rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Algorithm-QuineMcCluskey>.  I
will be notified, and then you'll automatically be notified of progress on your
bug as I make changes.

=head1 SUPPORT

Feel free to contact me at the email address below if you have any questions,
comments, suggestions, or complaints with regard to this module.

You can find documentation for this module with the perldoc command.

    perldoc Algorithm::QuineMcCluskey

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Algorithm-QuineMcCluskey>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Algorithm-QuineMcCluskey>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Algorithm-QuineMcCluskey>

=item * Search CPAN

L<http://search.cpan.org/dist/Algorithm-QuineMcCluskey>

=back


=head1 AUTHOR

Darren M. Kulp C<< <darren@kulp.ch> >>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2006 by Darren Kulp

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut

