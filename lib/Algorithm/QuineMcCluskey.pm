=head1 NAME

Algorithm::QuineMcCluskey - solve Quine-McCluskey set-cover problems

=cut

package Algorithm::QuineMcCluskey;

use strict;
use warnings;
use 5.008003;

use Moose;
use namespace::autoclean;

use Carp qw(croak);

use Algorithm::QuineMcCluskey::Util qw(columns countels diffpos hdist
	maskmatcher remels matchcount stl uniqels);
use Algorithm::QuineMcCluskey::Format qw(tableform);
use List::Compare::Functional qw(get_intersection is_LequivalentR is_LsubsetR);
use List::Util qw(sum any);
use Tie::Cycle;

#
# Vaguely consistent Smart-Comment rules:
# 3 pound signs for the code in BUILD(), find_primes() and find_essentials().
#
# 4 pound signs for code that manipulates prime/essentials/covers hashes:
#      col_dom(), row_dom(), and purge_essentials().
#
# 5 pound signs for the solve() and recursive_solve() code.
#
use Smart::Comments ('#####');

#
# Required attributes to create the object.
#
# 1. 'width' is absolutely required (handled via Moose).
#
# 2. If 'columnstring' is provided, 'minterms', 'maxterms', and
#    'dontcares' can't be used.
#
# 3. Either 'minterms' or 'maxterms' is used, but not both.
#
# 4. 'dontcares' are used with either 'minterms' or 'maxterms', but
#    cannot be used by itself.
#
has 'width'	=> (
	isa => 'Int', is => 'ro', required => 1
);

has 'minterms'	=> (
	isa => 'ArrayRef[Int]', is => 'rw', required => 0,
	predicate => 'has_minterms'
);
has 'maxterms'	=> (
	isa => 'ArrayRef[Int]', is => 'rw', required => 0,
	predicate => 'has_maxterms'
);
has 'dontcares'	=> (
	isa => 'ArrayRef[Int]', is => 'rw', required => 0,
	predicate => 'has_dontcares'
);
has 'columnstring'	=> (
	isa => 'Str', is => 'ro', required => 0,
	predicate => 'has_columnstring',
	lazy => 1,
	builder => 'to_columnstring'
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
has 'vars'	=> (
	isa => 'ArrayRef[Str]', is => 'rw', required => 0,
	default => sub{['A' .. 'Z']}
);

#
# Change behavior.
#
has ['minonly', 'sortterms'] => (
	isa => 'Bool', is => 'rw',
	default => 1
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

	if ($self->has_columnstring)
	{
		croak "No other terms necessary when using the columnstring attribute"
			if ($self->has_minterms or $self->has_maxterms or $self->has_dontcares);

		my $cl = length $self->columnstring;
		my $wl = 1 << $self->width;

		croak "Columnstring length is off by ", $wl - $cl unless ($wl == $cl);
	}
	else
	{
		croak "Must supply either minterms or maxterms"
			unless ($self->has_minterms or $self->has_maxterms);
	}

	#
	# Do we really need to check if they've set the
	# don't-care character to '0' or '1'? Oh well...
	#
	croak "Don't-care must be a single character" if (length $self->dc != 1);
	croak "The don't-care character can not be '0' or '1'" if ($self->dc =~ qr([01]));

	#
	# And make sure we have enough variable names.
	#
	croak "Not enough variable names for your width" if (scalar @{$self->vars} < $self->width);

	if ($self->has_columnstring)
	{
		my($min_ref, $max_ref, $dc_ref) = $self->break_columnstring();

		### min_ref: $min_ref
		### max_ref: $max_ref
		### don't cars: $dc_ref

		$self->minterms($min_ref) if (scalar @{$min_ref} );
		$self->dontcares($dc_ref) if (scalar @{$dc_ref} );
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

#
# Return a string made up of the function column. Position 0 in the string is
# the 0th row of the column, and so on.
#
sub to_columnstring
{
	my $self = shift;
	my ($dfltbit, $setbit) = ($self->has_min_bits)? qw(0 1): qw(1 0);
	my @bitlist = ($dfltbit) x (1 << $self->width);

	my @terms;

	push @terms, @{$self->minterms} if ($self->has_minterms);
	push @terms, @{$self->maxterms} if ($self->has_maxterms);

	map {$bitlist[$_] = $setbit} @terms;

	if ($self->has_dontcares)
	{
		map {$bitlist[$_] = $self->dc} (@{ $self->dontcares});
	}

	return join "", @bitlist;
}

#
# Take a column string and return array refs usable as parameters for
# minterm, maxterm, and don't-care attributes.
#
sub break_columnstring
{
	my $self = shift;
	my @bitlist = stl $self->columnstring;
	my $x = 0;

	my(@maxterms, @minterms, @dontcares);

	for (@bitlist)
	{
		push @minterms, $x if ($_ eq '1');
		push @maxterms, $x if ($_ eq '0');
		push @dontcares, $x if ($_ eq $self->dc);
		$x++;
	}

	return (\@minterms, \@maxterms, \@dontcares);
}

#
# Build another Quine-McCluskey object that's the complement
# of the existing object.
#
sub complement
{
	my $self = shift;
	my $cstring = $self->columnstring();

	my $comp = $cstring =~ tr/01/10/r;
	my $title = "Complement of '" . $self->title() . "'";

	return Algorithm::QuineMcCluskey->new(columnstring => $comp,
		width => $self->width,
		dc => $self->dc,
		vars => $self->vars,
		title => $title);
}

#
# Build another Quine-McCluskey object that's the dual
# of the existing object.
#
sub dual
{
	my $self = shift;
	my $cstring = $self->columnstring();

	my $dual = reverse $cstring =~ tr/01/10/r;
	my $title = "Dual of '" . $self->title() . "'";

	return Algorithm::QuineMcCluskey->new(columnstring => $dual,
		width => $self->width,
		dc => $self->dc,
		vars => $self->vars,
		title => $title);
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

=item find_primes

Finding prime essentials

=cut

sub find_primes
{
	my $self = shift;
	my @bits;
	my %implicant;

	#
	# Separate into bins based on number of 1's (the weight).
	#
	for ($self->all_bit_terms())
	{
		push  @{$bits[0][ matchcount($_, '1') ]}, $_;
	}

	#
	### find_primes() group the bit terms by bit count: @bits
	#

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

	#
	### find_primes() implicant hash (we use the unmarked [i.e., 0] ones: %implicant
	#

	#
	# For each unmarked (value == 0) implicant, match it against the
	# minterms (or maxterms). The resulting hash of arrays is our
	# set of prime implicants.
	#
	my %p = map { $_ => [ maskmatcher($_, $self->dc, $self->minmax_bit_terms()) ] }
		grep { !$implicant{$_} } keys %implicant;

	#
	### find_primes() -- attributes primes: %p
	#

	$self->_set_primes( \%p );
	return $self;
}

=item row_dom

Row-dominance

=cut

sub row_dom
{
	my $self = shift;
	my $primes = shift;

	return $primes if (scalar keys %$primes == 0);

	#### row_dom() primes hash before processing: $primes

	%$primes = ( map {
		my $o = $_;
		(sum map {
			is_LsubsetR([ $primes->{$o} => $primes->{$_} ])
				&& !is_LequivalentR([ $primes->{$o} => $primes->{$_} ])
			} grep { $_ ne $o } keys %$primes)
		? () : ( $_ => $primes->{$_} )
	} keys %$primes );

	#### row_dom() primes hash after processing: $primes

	return $self;
}

=item col_dom

Column-dominance

=cut

sub col_dom
{
	my $self = shift;
	my $primes = shift;

	return $self if (scalar keys %$primes == 0);

	#### col_dom() primes hash before processing: $primes

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
				remels($col2, $self->dc, $primes);
			}
		}
	}

	#### col_dom() primes hash after processing: $primes

	return $self;
}

=item find_essentials

Find the essential prime implicants.

Called from the solve(), but may also be called by itself for testing
purposes.

=cut

sub find_essentials
{
	my $self = shift;

	my $primes = shift;
	my @terms = $self->minmax_bit_terms();

	my @kp = keys %$primes;
	my %essentials;

	$self->clear_essentials;

	for my $term (@terms)
	{
		my @tp = grep {
				grep { $_ eq $term } @{ $primes->{$_} }
			} @kp;

		#
		### Examining term: $term
		### Prime list for term is: @tp
		#

		# TODO: It would be nice to track the terms that make
		# this essential
		if (scalar @tp == 1)
		{
			$essentials{$tp[0]}++;
		}
	}

	### find_essentials() found: %essentials

	$self->_set_essentials(\%essentials);
	return $self;
}

=item purge_essentials

Given a table (hash form) of prime implicants, delete the essential
prime implicants from the table (row-wise and column-wise), leaving
those implicants that must be chosen for the remaining elements of
the boolean function.

=cut

sub purge_essentials
{
	my $self = shift;
	my %ess = %{ shift() };
	my $primes = shift;

	return $self if (scalar keys %ess == 0 or scalar keys %$primes == 0);

	#### purge_essentials() called with essentials hash: %ess
	#### purge_essentials() called with primes hash ref: $primes

	#
	# Delete the columns associated with each essential prime implicant.
	#
	for my $el (keys %ess)
	{
		remels($el, $self->dc, $primes);
	}

	#
	# Now delete the rows.
	#
	delete ${$primes}{$_} for keys %ess;

	#### purge_essentials() returns having set primes to: $primes

	return $self;
}

=item to_boolean

Generating Boolean expressions

=cut

sub to_boolean
{
	my $self = shift;
	my @terms = @_;
	my @boolean;

	#
	### to_boolean() called with: @terms
	#
	# Group separators (grouping character pairs)
	#
	my @gs = ('(', ')');

	#
	# Group joiner string.
	#
	my $gj = $self->has_min_bits ? ' + ': '';

	push @boolean,
		join $gj,
			map { $gs[0] . $self->to_boolean_term($_) . $gs[1] } @$_
		for (@terms);

	### to_boolean() returns: @boolean

	return @boolean;
}

#
# Convert an individual term or prime implicant to a boolean variable string.
#
sub to_boolean_term
{
	my $self = shift;
	my $term = $_[0];

	#
	# Element joiner and match condition
	#
	my ($ej, $cond) = $self->has_min_bits ? ('', 1) : (' + ', 0);
	tie my $var, 'Tie::Cycle', [ @{$self->vars}[0 .. $self->width - 1] ];

	my $varstring = join $ej, map {
			my $var = $var;	# Activate cycle even if not used
			$_ eq $self->dc ? () : $var . ($_ == $cond ? '' : "'")
		} stl $term;

	return $varstring;
}

=item solve

Main solution sub (wraps recurse_solve())

=cut

sub solve
{
	my $self = shift;
	$self->find_primes unless ($self->has_primes);

	my $p = $self->get_primes;

	$self->_set_covers($self->recurse_solve($p));

	return $self->to_boolean($self->get_covers);
}

=item recurse_solve

Recursive divide-and-conquer solver

=cut

sub recurse_solve
{
	my $self = shift;
	my %primes = %{ $_[0] };
	my @prefix;
	my @covers;
	my @essentials_keys;

	#
	##### recurse_solve() called with: "\n" . tableform(\%primes, $self->width)
	#
	$self->find_essentials(\%primes);
	my %ess = %{ $self->get_essentials() };

	#
	##### Begin prefix/essentials loop.
	#
	do
	{
		##### recurse_solve() essentials: %ess

		$self->purge_essentials(\%ess, \%primes);
		@essentials_keys = keys %ess;
		push @prefix, grep { $ess{$_} > 0} @essentials_keys;

		##### recurse_solve() \@prefix now: @prefix

		#$self->row_dom(\%primes);
		$self->col_dom(\%primes);
		$self->find_essentials(\%primes);
		%ess = %{ $self->get_essentials() };

	} while (!is_LequivalentR([
			[ @essentials_keys ] => [ keys %ess ]
			]));

	#
	##### recurse_solve() Primes after loop: "\n" . tableform(\%primes, $self->width)
	#

	return [ reverse sort @prefix ] unless (keys %primes);

	#
	# Find the term with the fewest implicant covers.
	# Columns actually in %primes
	#
	my @t = grep {
		my $o = $_;
		any { countels( $o, $_ ) } values %primes
	} ($self->minmax_bit_terms());

	#
	##### Primes table is: %primes
	##### Flipping table so terms are keys using: @t
	#
	my %ic = columns \%primes, @t;

	#
	##### Resulting table is: %ic
	#
	my $term = (sort { @{ $ic{$a} } <=> @{ $ic{$b} } } keys %ic)[0];

	# Rows of %primes that contain $term
	my @ta = grep { countels($term, $primes{$_}) } keys %primes;

	# For each such cover, recursively solve the table with that column
	# removed and add the result(s) to the covers table after adding
	# back the removed term.
	#
	##### Term used to filter primes list is: $term
	##### keys are: @ta
	#
	for my $ta (@ta)
	{
		my @results;

		#
		##### For ta: $ta
		#
		my %reduced = map {
			$_ => [ grep { $_ ne $term } @{ $primes{$_} } ]
		} keys %primes;

		# Use this prime implicant -- delete its row and columns
		remels($ta, $self->dc, \%reduced);
		delete $reduced{$ta};

		# Remove empty rows (necessary?)
		%reduced = map {
			$_ => $reduced{$_} } grep { @{ $reduced{$_} }
		} keys %reduced;

		if (keys %reduced)
		{
			my @c = $self->recurse_solve(\%reduced);

			#
			##### recurse_solve() returns (to recurse_solve()): @c
			#
			@results = $self->sortterms
				? @c
					? map { [ reverse sort (@prefix, $ta, @$_) ] } @c
					: [ reverse sort (@prefix, $ta) ]
				: @c
					? map { [ @prefix, $ta, @$_ ] } @c
					: [ @prefix, $ta ];
		}
		else
		{
			@results = $self->sortterms
				? [ reverse sort (@prefix, $ta) ]
				: [ @prefix, $ta ];
		}
		push @covers, @results;
	}

	#
	##### Covers is: @covers
	##### before weeding out expensive solutions.
	#
	if ($self->minonly)
	{
		my $mincost = 1 << $self->width;
		my @weededcovers;

		for my $c (@covers)
		{
			my $cost = matchcount(join('', @$c), "[01]");

			next if ($cost > $mincost);

			if ($cost < $mincost)
			{
				$mincost = $cost;
				@weededcovers = ();
			}
			push @weededcovers, $c;
		}
		@covers = @weededcovers;
	}

	#
	##### Covers is: @covers
	##### after the weeding out.
	#

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

