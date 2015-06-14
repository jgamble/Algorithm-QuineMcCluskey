=head1 NAME

Algorithm::QuineMcCluskey - solve Quine-McCluskey set-cover problems

=cut

package Algorithm::QuineMcCluskey;

use strict;
use warnings;
use 5.010001;

use Moose;
use namespace::autoclean;

use Carp qw(croak);

use Algorithm::QuineMcCluskey::Util qw(:all);
use List::Compare::Functional qw(get_complement get_intersection is_LequivalentR);
use Tie::Cycle;

#
# Vaguely consistent Smart-Comment rules:
# 3 pound signs for the code in BUILD() and generate_*() functions.
#
# 4 pound signs for code that manipulates prime/essentials/covers hashes:
#      row_dominance().
#
# 5 pound signs for the solve() and recurse_solve() code, and the remels() calls.
#
# The ::Format package is only needed for Smart Comments -- comment or uncomment
# in concert with Smart::Comments as needed.
#
#use Algorithm::QuineMcCluskey::Format qw(arrayarray hasharray tableform);
#use Smart::Comments ('###', '####', '#####');

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
has ['minonly'] => (
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

#
# The calculated prime implicants.
#
has 'primes'	=> (
	isa => 'HashRef', is => 'ro', required => 0,
	init_arg => undef,
	reader => 'get_primes',
	writer => '_set_primes',
	predicate => 'has_primes',
	clearer => 'clear_primes',
	lazy => 1,
	builder => 'generate_primes'
);
has 'essentials'	=> (
	isa => 'HashRef', is => 'ro', required => 0,
	init_arg => undef,
	reader => 'get_essentials',
	writer => '_set_essentials',
	predicate => 'has_essentials',
	clearer => 'clear_essentials',
	lazy => 1,
	builder => 'generate_essentials'
);

#
# The terms that cover the primes needed to solve the
# truth table.
#
has 'covers'	=> (
	isa => 'ArrayRef[ArrayRef[Str]]', is => 'ro', required => 0,
	init_arg => undef,
	reader => 'get_covers',
	writer => '_set_covers',
	predicate => 'has_covers',
	clearer => 'clear_covers',
	lazy => 1,
	builder => 'generate_covers'
);

our $VERSION = 0.04;

=head1 SYNOPSIS

    use Algorithm::QuineMcCluskey;

    #
    # Five-bit, 12-minterm Boolean expression test with don't-cares
    #
    my $q = Algorithm::QuineMcCluskey->new(
        width => 5,
        minterms => [ 0, 5, 7, 8, 10, 11, 15, 17, 18, 23, 26, 27 ],
        dontcares => [ 2, 16, 19, 21, 24, 25 ]
    );

    my $result = $q->solve();

or

    my $q = Algorithm::QuineMcCluskey->new(
	width => 5,
        columnstring => '10-0010110110001-11-0-01--110000'
    );

In either case $result will be C<"(AC') + (A'BDE) + (B'CE) + (C'E')">.

=head1 DESCRIPTION

This module minimizes
L<Boolean expressions|https://en.wikipedia.org/wiki/Boolean_algebra> using the
L<Quine-McCluskey algorithm|https://en.wikipedia.org/wiki/Quine%E2%80%93McCluskey_algorithm>.


=cut

#
# Sub and method definitions.
#

=head2 Object Methods

=head3 new([<attribute> => value, ...])

Creates the QuineMcCluskey object. The attributes are:

=over 4

=item 'width'

The number of variables (columns) in the Boolean expression.

This is a required attribute.

=item 'minterms'

An array reference of terms representing the 1-values of the
Boolean expression.

=item 'maxterms'

An array reference of terms representing the 0-values of the
Boolean expression. This will also indicate that you want the
expression in product-of-sum form, instead of the default
sum-of-product form.

=item 'dontcares'

An array reference of terms representing the don't-care-values of the
Boolean expression. These represent inputs that simply shouldn't happen
(e.g., numbers 11 through 15 in a base 10 system), and therefore don't
matter to the result.

=item 'columnstring'

Present the entire list of values of the boolean expression as a single
string. The values are ordered from left to right in the string. For example,
a simple two-variable AND equation would have a string "0001".

=item 'dc'

I<Default value: '-'>

Change the representation of the don't-care character. The don't-care character
is used both in the columnstring, and internally as a place holder for
eliminated variables in the equation. Some of those internals
may be examined via other methods.

=item 'title'

A title for the problem you are solving.

=item 'vars'

I<Default value: ['A' .. 'Z']>

The variable names used to form the equation. The names will be taken from
the leftmost first:

    my $f1 = Algorithm::QuineMcCluskey->new(
        width => 4,
        maxterms => [1 .. 11, 13, 15],
	vars => ['w' .. 'z']
    );

The names do not have to be single characters, e.g.:

	vars => ['a0', 'a1', 'b0', 'b1']

=back

=head3 solve()

Returns a string of the Boolean equation.

=head3 complement()

Returns a new object that's the complement of the existing object:

    my $qc = $q->complement();
    print $qc->solve(), "\n";

Prints C<"(ABC) + (A'B'D'E) + (BD'E) + (CE')">.

=head3 dual()

Returns a new object that's the dual of the existing object:

    my $qd = $q->dual();
    print $qd->solve(), "\n";

Prints C<"(ABCE') + (A'B'C') + (B'DE') + (C'E)">.

=head3 to_columnstring()

Return a string made up of the function column. Position 0 in the string is
the 0th row of the column, and so on.

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
		### don't cares: $dc_ref

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
	my @bitlist = split(//, $self->columnstring);
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

sub complement_terms
{
	my $self = shift;
	my @bitlist = (0 .. (1 << $self->width) - 1);
	my @termlist;

	@termlist = @{$self->dontcares} if ($self->has_dontcares);

	if ($self->has_minterms)
	{
		push @termlist, @{ $self->minterms };
	}
	else
	{
		push @termlist, @{ $self->maxterms };
	}

	return get_complement([\@termlist, \@bitlist]);
}

#
# Build another Quine-McCluskey object that's the complement
# of the existing object.
#
sub complement
{
	my $self = shift;
	my %term;


	$term{dontcares} = [@{$self->dontcares}] if ($self->has_dontcares);

	if ($self->has_minterms)
	{
		$term{minterms} = [$self->complement_terms()];
	}
	else
	{
		$term{maxterms} = [$self->complement_terms()];
	}

	my $title = "Complement of '" . $self->title() . "'";

	return Algorithm::QuineMcCluskey->new(
		title => $title,
		width => $self->width,
		dc => $self->dc,
		vars => $self->vars,
		%term
	);
}

#
# Build another Quine-McCluskey object that's the dual
# of the existing object.
#
sub dual
{
	my $self = shift;
	my $last = (1 << $self->width) - 1;
	my %term;

	$term{dontcares} = [@{$self->dontcares}] if ($self->has_dontcares);

	my @dualterms = sort map {$last - $_} $self->complement_terms();

	if ($self->has_minterms)
	{
		$term{minterms} = [@dualterms];
	}
	else
	{
		$term{maxterms} = [@dualterms];
	}

	my $title = "Dual of '" . $self->title() . "'";

	return Algorithm::QuineMcCluskey->new(
		title => $title,
		width => $self->width,
		dc => $self->dc,
		vars => $self->vars,
		%term
	);
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

sub generate_primes
{
	my $self = shift;
	my @bits;
	my %implicant;

	#
	# Separate into bins based on number of 1's (the weight).
	#
	for ($self->all_bit_terms())
	{
		push @{$bits[0][ matchcount($_, '1') ]}, $_;
	}

	#
	### generate_primes() group the bit terms
	### by bit count: $bits[0]
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
	### generate_primes() implicant hash (we use the unmarked entries
	### [i.e., prime => 0] ) : %implicant
	#

	#
	# For each unmarked (value == 0) implicant, match it against the
	# minterms (or maxterms). The resulting hash of arrays is our
	# set of prime implicants.
	#
	my %p = map { $_ => [ maskedmatch($_, $self->minmax_bit_terms()) ] }
		grep { !$implicant{$_} } keys %implicant;

	#
	### generate_primes() -- prime implicants: hasharray(\%p)
	#
	return \%p;
}

sub generate_covers
{
	my $self = shift;
	my @c = $self->recurse_solve($self->get_primes, 0);

	### generate_covers() -- recurse_solve() returned: arrayarray(\@c)

	return \@c;
}

sub generate_essentials
{
	my $self = shift;

	my $p = $self->get_primes;
	my %e = find_essentials($p, $self->minmax_bit_terms());

	### generate_essentials() -- essentials: hasharray(\%e)

	return \%e;
}

=head3 to_boolean()

Generating Boolean expressions

=cut

sub to_boolean
{
	my $self = shift;
	my @terms = @_;

	#
	### to_boolean() called with: arrayarray(\@terms)
	#
	# Group separators (grouping character pairs)
	#
	my @gs = ('(', ')');

	#
	# Group joiner string.
	#
	my $gj = $self->has_min_bits ? ' + ': '';

	return
		join $gj,
			map { $gs[0] . $self->to_boolean_term($_) . $gs[1] } @$_
		for (@terms);
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
		} split(//, $term);

	return $varstring;
}

sub solve
{
	my $self = shift;
	my $c = $self->get_covers();

	return $self->to_boolean($c->[0]);
}

#
# recurse_solve
#
# Recursive divide-and-conquer solver
#
# "To reduce the complexity of the prime implicant chart:
#
# 1. Select all the essential prime impliciants. If these PIs cover all
# minterms, stop; otherwise go the second step.
#
# 2. Apply Rules 1 and 2 to eliminate redundant rows and columns from
# the PI chart of non-essential PIs.  When the chart is thus reduced,
# some PIs will become essential (i.e., some columns will have a single
# 'x'. Go back to step 1."
#
# Introduction To Logic Design, by Sajjan G. Shiva page 129.
#
sub recurse_solve
{
	my $self = shift;
	my %primes = %{ $_[0] };
	my $level = $_[1];
	my @prefix;
	my @covers;
	my @essentials_keys;

	#
	##### recurse_solve() level: $level
	##### recurse_solve() called with: "\n" . tableform(\%primes, $self->width)
	#
	
	my %ess = find_essentials(\%primes, $self->minmax_bit_terms());

	#
	##### Begin prefix/essentials loop.
	#
	do
	{
		##### recurse_solve() essentials: %ess

		#
		# REMOVE LATER: the sort op isn't necessary
		# to the algorithm, but it makes debugging easier.
		#
		@essentials_keys = sort keys %ess;

		#
		# Remove the essential prime implicants from
		# the prime implicants table.
		#
		##### Purging prime hash of: "[" . join(", ", @essentials_keys) . "]"
		#
		purge_elements(\%primes, @essentials_keys);
		push @prefix, grep { $ess{$_} > 0} @essentials_keys;

		##### recurse_solve() @prefix now: "[" . join(", ", sort @prefix) . "]"

		#
		# Now eliminate dominated rows and columns.
		#
		# Rule 1: A row dominated by another row can be eliminated.
		# Rule 2: A column that dominated another column can be eliminated.
		#
		my @rows = row_dominance(\%primes, 1);
		#### row_dominance called with primes: "\n" . tableform(\%primes, $self->width)
		#### row_dominance returns for removal: "[" . join(", ", @rows) . "]"
		delete $primes{$_} for (@rows);

		my %cols = columns(\%primes, $self->minmax_bit_terms());
		my @cols = row_dominance(\%cols, 0);
		#### row_dominance called with primes (rotated): "\n" . tableform(\%cols, $self->width)
		#### row_dominance returns for removal: "[" . join(", ", @cols) . "]"
		remels($_, \%primes) for (@cols);

		%ess = find_essentials(\%primes, $self->minmax_bit_terms());

		##### recurse_solve() essentials after purge/dom: %ess

	} until (is_LequivalentR([
			[ @essentials_keys ] => [ keys %ess ]
			]));

	return [ reverse sort @prefix ] unless (keys %primes);

	#
	##### recurse_solve() Primes after loop: "\n" . tableform(\%primes, $self->width)
	#

	#
	# Find the term that has the least number of prime implicants
	# covering it. Then having found it, make a list of those
	# prime implicants, and use that list to figure out the best
	# set to cover the rest of the terms.
	#
	my $term = least_covered(\%primes, $self->minmax_bit_terms());
	my @ta = grep { countels($term, $primes{$_}) } keys %primes;

	#
	##### Least-covered term returned is: $term
	##### Prime implicants that cover term are: "[" . join(", ", @ta) . "]"
	#
	# Make a copy of the section of the prime implicants
	# table that don't cover that term.
	#
	my %r = map {
		$_ => [ grep { $_ ne $term } @{ $primes{$_} } ]
	} keys %primes;

	#
	# For each such cover, recursively solve the table with that column
	# removed and add the result(s) to the covers table after adding
	# back the removed term.
	#
	for my $ta (@ta)
	{
		my (@c, @results);
		my %reduced = %r;

		#
		# Use this prime implicant -- delete its row and columns
		#
		##### Purging reduced hash of: $ta
		#
		purge_elements(\%reduced, $ta);

		if (keys %reduced and scalar(@c = $self->recurse_solve(\%reduced, $level + 1)))
		{
			#
			##### recurse_solve() at level: $level
			##### returned (in loop): arrayarray(\@c)
			#
			@results = map { [ reverse sort (@prefix, $ta, @$_) ] } @c;
		}
		else
		{
			@results = [ reverse sort (@prefix, $ta) ]
		}

		push @covers, @results;

		#
		##### Covers now at: arrayarray(\@covers)
		#
	}

	#
	##### Weed out the expensive solutions.
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
	##### Covers is: arrayarray(\@covers)
	##### after the weeding out.
	#

	# Return our covers table to be treated similarly one level up
	# FIXME: How to best ensure non-duplicated answers?
	return uniqels @covers;
}

1;
__END__


=head1 AUTHOR

Darren M. Kulp C<< <darren@kulp.ch> >>


=cut

