=head1 NAME

Algorithm::QuineMcCluskey - solve Quine-McCluskey set-cover problems

=cut

package Algorithm::QuineMcCluskey;

use strict;
use warnings;
use 5.008003;

use Moose;
use namespace::autoclean;

use Moose::Util::TypeConstraints;
use Carp qw(carp croak);
use Data::Dumper;

use Algorithm::QuineMcCluskey::Util qw(bin columns diffpos hdist stl uniqels);
use List::Compare::Functional qw(:main is_LequivalentR);
use List::MoreUtils qw(firstidx);
use List::Util qw(sum min);
use Tie::Cycle;

#
# Required attributes to create the object.
#
# 1. 'width' is absolutely required (handled via Moose).
#
# 2. if 'characteristic' is present, 'dontcares', 'minterms', and
#    'maxterms' cannot be used.
#
# 3. if either 'minterms' or 'maxterms' is used (but not both),
#    then 'characteristic' can't be used.
#
# 4. 'dontcares' are used with either 'minterms' or 'maxterms', but
#    cannot be used by itself.
#
has 'dontcares'	=> (
	isa => 'ArrayRef[Int]', is => 'ro', required => 0,
	predicate => 'has_dontcares'
);
has 'minterms'	=> (
	isa => 'ArrayRef[Int]', is => 'ro', required => 0,
	predicate => 'has_minterms'
);
has 'maxterms'	=> (
	isa => 'ArrayRef[Int]', is => 'ro', required => 0,
	predicate => 'has_maxterms'
);
has 'characteristic' => (
	isa => 'String', is => 'ro', required => 0,
	predicate => 'has_characteristic'
);
has 'width'	=> (
	isa => 'Int', is => 'ro', required => 1
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
	isa => 'ArrayRef[Str]', is => 'ro', required => 0,
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
		minterms => [ 0, 5, 7, 8, 10, 11, 15, 17, 18, 23, 26, 27 ],
		dontcares => [ 2, 16, 19, 21, 24, 25 ]
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
	my @terms;

	#
	# Catch errors.
	#
	croak "Mixing minterms and maxterms not allowed"
		if ($self->has_minterms and $self->has_maxterms);

	if ($self->has_characteristic)
	{
		croak "No other terms necessary when using the characteristic attribute"
			if ($self->has_minterms or $self->has_maxterms or $self->has_dontcares);
	}
	else
	{
		croak "Must supply either minterms or maxterms"
			unless ($self->has_minterms or $self->has_maxterms);
	}

	#
	# Convert terms to strings of bits as needed.
	#
	if ($self->has_characteristic)
	{
		croak "Not yet implemented";
		# Set minterms here.
	}
	if ($self->has_minterms)
	{
		@terms = @{$self->minterms};

		my @bitstrings = map {
			substr(unpack("B32", pack("N", $_)), -$w)
		} @terms;

		$self->min_bits(\@bitstrings);
	}
	if ($self->has_maxterms)
	{
		@terms = @{$self->maxterms};

		my @bitstrings = map {
			substr(unpack("B32", pack("N", $_)), -$w)
		} @terms;

		$self->max_bits(\@bitstrings);
	}

	if ($self->has_dontcares)
	{
		my @dontcares = @{$self->dontcares};

		my @intersect = get_intersection([\@dontcares, \@terms]);
		if (scalar @intersect != 0)
		{
			croak "Term(s) ", join(", ", @intersect),
				" are in both the don't-care list and the term list.";
		}

		my @bitstrings = map {
			substr(unpack("B32", pack("N", $_)), -$w)
		} @dontcares;

		$self->dc_bits(\@bitstrings);
	}

	$self->title("$w-variable truth table") unless ($self->has_title);

	return $self;
}

sub complement
{
	my $self = shift;
	my %paramset;

	my $termtype = ($self->has_minterms)? "minterms": "maxterms";

	my $highterm = (1 << $self->width) - 1;
	my @allterms = (0 .. $highterm);
	my @terms = @{$self->{$termtype}};

	$paramset{$termtype} = [get_complement(\@terms, \@allterms)];

	$paramset{dontcares} = $self->dontcares if ($self->has_dontcares);

	return $self->makenew(%paramset);
}

#
# Return a new object (via complement()) that is the dual of this object's state.
#
sub dual
{
	my $self = shift;
	my %paramset;

	my $termtype = ($self->has_minterms)? "minterms": "maxterms";

	my $highterm = (1 << $self->width) - 1;
	my @allterms = (0 .. $highterm);
	my @terms = map { $highterm - $_} @{$self->{$termtype} };

	$paramset{$termtype} = [get_complement(\@terms, \@allterms)];

	if ($self->has_dontcares)
	{
		@terms = map { $highterm - $_} @{$self->dontcares};
		$paramset{dontcares} = [@terms];
	}

	return $self->makenew(%paramset);
}

sub makenew
{
	my $self = shift;
	my %paramset = @_;

	for my $k (qw(dc title minonly sortterms vars))
	{
		$paramset{$k} = $self->{$k} unless (exists $paramset{$k});
	}

	return Algorithm::QuineMcCluskey->new(%paramset);
}

sub all_bit_terms
{
	my $self = shift;
	my @terms;

	push @terms, @{ $self->min_bits } if ($self->has_min_bits);
	push @terms, @{ $self->max_bits } if ($self->has_max_bits);
	push @terms, @{ $self->dc_bits } if ($self->has_dc_bits);
	return @terms;
}

sub minmax_bit_terms
{
	my $self = shift;
	my @terms;

	push @terms, @{ $self->min_bits } if ($self->has_min_bits);
	push @terms, @{ $self->max_bits } if ($self->has_max_bits);
	return @terms;
}

=item maskmatcher

Returns the terms that match a mask.

=cut

sub maskmatcher
{
	my $self = shift;
	my $m = shift;
	my $dc = $self->dc;

	(my $mask0 = $m) =~ s/$dc/0/g;
	(my $mask1 = $m) =~ s/$dc/1/g;
	$mask0 = bin $mask0;
	$mask1 = bin $mask1;

	grep { (($mask0 & bin $_) == $mask0) && (($mask1 & bin $_) == bin $_) } @_;
}

=item remel

Remove a value from an arrayref if it matches a mask

=cut

sub remel
{
	my $self = shift;
	my ($el, $a) = @_;
carp "remel: ", Data::Dumper->Dump([$el, $a], ['el', 'a']);

	my $pos = firstidx { $self->maskmatcher($el, $_) } @$a;
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

	#
	# Separate into bins based on number of 1's (the weight).
	#
	for ($self->all_bit_terms())
	{
		my $l = sum stl $_;
		carp "    $_ : $l bits set.";
		push  @{$bits[0][ $l ]}, $_;
	}

	#
	# Dump here.
	#
	carp "    Dump of \@bits:", Data::Dumper->Dump([\@bits], [qw(bits)]);

	#
	# Now for each level, we look for terms that be absorbed into
	# simpler product terms (for example, _ab_c + ab_c can be simplified
	# to b_c).
	#
	# Level 0 consists of the fundemental
	# product terms; level 1 consists of pairs of fundemental terms
	# that have a variable in common; level 2 consists of pairs of pairs
	# that have a variable in common; and so on until we're out of
	# levels (number of variables) or cannot find any more products
	# with terms in common.
	#
	for my $level (0 .. $self->width)
	{
		#
		# Skip if we haven't generated data for this level.
		#
		last unless ref $bits[$level];

		#
		# Find pairs with Hamming distance of 1 (i.e., a weight
		# difference of 1).
		#
		for my $low (0 .. $#{ $bits[$level] })
		{
			#
			# These nested for-loops get all permutations
			# of adjacent sets.
			#
			for my $lv (@{ $bits[$level][$low] })
			{
				#
				# Initialize the implicant as unused.
				#
				$implicant{$lv} //= 0;

				#
				# Skip ahead if there are no terms at
				# this level.
				#
				next unless ref $bits[$level][$low + 1];

				for my $hv (@{ $bits[$level][$low + 1] })
				{
					#
					# Initialize the implicant.
					#
					$implicant{$hv} //= 0;

					#
					# If there are matching terms, save
					# the new implicant at the next 'level',
					# creating it if it doesn't exist.
					#
					if (hdist($lv, $hv) == 1)
					{
						my $new = $lv;	# or $hv
						substr($new, diffpos($lv, $hv), 1) = $self->dc;

						#
						# Save the new implicant to the
						# next level, then mark the two
						# values as used.
						#
						push @{ $bits[$level + 1][$low + 1] }, $new;
						$implicant{$lv} = 1;
						$implicant{$hv} = 1;
					}
				}
			}
		}
	}

	carp "Dump of implicants: ", Data::Dumper->Dump([\%implicant], [qw(implicant)]);

	#
	# For each unmarked (value == 0) implicant, match it against the
	# minterms (or maxterms). The resulting hash of arrays is our
	# set of prime implicants.
	#
	my %p = map { $_ => [ $self->maskmatcher($_, $self->minmax_bit_terms()) ] }
		grep { !$implicant{$_} } keys %implicant;

	#
	# Carp the primes.
	#
	carp "    Setting attribute primes with:\n", Data::Dumper->Dump([\%p], [qw(p)]);

	$self->_set_primes( \%p );
	return $self->get_primes;
}


=item row_dom

Row-dominance

=cut

sub row_dom
{
	my $self = shift;
	my $primes = shift;

	carp "row_dom: primes hash before processing: ", Data::Dumper->Dump([$primes], [qw(primes)]);

	$primes = { map {
		my $o = $_;
		(sum map {
			is_LsubsetR([ $primes->{$o} => $primes->{$_} ])
				&& !is_LequivalentR([ $primes->{$o} => $primes->{$_} ])
			} grep { $_ ne $o } keys %$primes)
		? () : ( $_ => $primes->{$_} )
	} keys %$primes };

	carp "row_dom: primes hash after processing: ", Data::Dumper->Dump([$primes], [qw(primes)]);

	return $primes;
}

=item col_dom

Column-dominance

=cut

sub col_dom
{
	my $self = shift;
	my $primes = shift;

	carp "col_dom: primes hash before processing: ", Data::Dumper->Dump([$primes], [qw(primes)]);

	my %cols = columns $primes, $self->minmax_bit_terms();

	for my $col1 (keys %cols)
	{
		for my $col2 (keys %cols)
		{
			next if $col1 eq $col2;

			#
			# If col1 is a non-empty proper subset of col2,
			# remove col2
			#
			if (@{ $cols{$col1} }
				and is_LsubsetR([ $cols{$col1} => $cols{$col2} ])
				and !is_LequivalentR([ $cols{$col1} => $cols{$col2} ]))
			{
				$self->remel($col2, $primes->{$_}) for keys %$primes;
				carp "$col1 is a proper subset of $col2 (<-- removed)";
			}
		}
	}

	carp "col_dom: primes hash after processing: ", Data::Dumper->Dump([$primes], [qw(primes)]);

	return $primes;
}

=item find_essentials

Finding essential prime implicants. Called from the solve(), but may also
be called by itself for testing purposes.

=cut

sub find_essentials
{
	my $self = shift;

	my $primes = @_ ? shift : \%{$self->get_primes};
	my @terms = @_ ? @{ shift() } : ($self->minmax_bit_terms());

	my @kp = keys %$primes;
	my %essentials;

	$self->clear_essentials;

carp "find_essentials:\n";

	for my $term (@terms)
	{
		my @tp = grep {
			grep { $_ eq $term } @{ $primes->{$_} } } @kp;

carp "    For term '$term', term/prime list is (", join(", ", @tp), "), ";

		# TODO: It would be nice to track the terms that make this essential
		if (scalar @tp == 1)
		{
			$essentials{$tp[0]}++;
		}
	}

carp "    Setting essentials with: ", Data::Dumper->Dump([\%essentials], [qw(essentials)]);

	$self->_set_essentials(\%essentials);
	return %essentials;
}

=item purge_essentials

Delete essential primes from table

=cut

sub purge_essentials
{
	my $self = shift;
	my %ess = %{ shift() };
	my $primes = shift;

carp "purge_essentials(ess, primes): ", Data::Dumper->Dump([\%ess, $primes], ['%ess', '$primes']);

	# Delete columns associated with this term
	for my $col (keys %$primes)
	{
		$self->remel($_, $primes->{$col}) for keys %ess;
	}

	delete ${$primes}{$_} for keys %ess;

carp "    After: purge_essentials(ess, primes): ", Data::Dumper->Dump([\%ess, $primes], ['%ess', '$primes']);
	return $self;
}

=item to_boolean

Generating Boolean expressions

=cut

sub to_boolean
{
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

sub solve
{
	my $self = shift;
	$self->find_primes unless ($self->has_primes);

	my $p = $self->get_primes;

	carp "solve with primes: ", Data::Dumper->Dump([$p], ['p']);

	$self->_set_covers($self->recurse_solve($p));
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

carp "recurse_solve:";

	# begin (slightly) optimized block : do not touch without good reason
	my %ess = $self->find_essentials(\%primes);

	carp " ", Data::Dumper->Dump([\%primes, \%ess], ['%primes', '%ess']);

	$self->purge_essentials(\%ess, \%primes);
	push @prefix, grep { $ess{$_} } keys %ess;

carp "    prefix: [", join(", ", @prefix), "]\n";

	$self->row_dom(\%primes);
	$self->col_dom(\%primes);
my $lercount  = 0;
	while (!is_LequivalentR([
			[ keys %ess ] => [ %ess = $self->find_essentials(\%primes) ]
			]))
	{
		$self->purge_essentials(\%ess, \%primes);
		push @prefix, grep { $ess{$_} } keys %ess;
		$self->row_dom(\%primes);
		$self->col_dom(\%primes);
$lercount++;
	}
	# end optimized block

carp "    After $lercount 'optimized' block runs:";
carp "    prefix: [", join(", ", reverse sort @prefix), "]\n";


	return [ reverse sort @prefix ] unless (keys %primes);

	# Find the term with the fewest implicant covers
	# Columns actually in %primes
	my @t = grep {
		my $o = $_;
		sum map { sum map { $_ eq $o } @$_ } values %primes
	} ($self->minmax_bit_terms());

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
		$self->remel($ta, $reduced{$_}) for keys %reduced;
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

