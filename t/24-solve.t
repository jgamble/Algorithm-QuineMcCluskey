#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;

#
# Testing code starts here
#

use Test::More tests => 1;

my($q, @eqn, @expected);

$q = Algorithm::QuineMcCluskey->new(
	title	=> "Example 3.18 from Introduction to Logic Design, by Sajjan G. Shiva, page 131.",
	width => 5,
	minterms => [ 0, 1, 2, 5, 14, 16, 18, 24, 26, 30 ],
	dontcares => [3, 13, 28],
);

#
#    (AC'E') + (A'B'C') + (A'B'D'E) + (BCDE')
# or (AC'E') + (A'B'D'E) + (BCDE') + (B'C'E')
#
@expected = (
	q/(AC'E') + (A'B'C') + (A'B'D'E) + (BCDE')/
);

@eqn = $q->solve;
is_deeply(\@eqn, \@expected, $q->title);

