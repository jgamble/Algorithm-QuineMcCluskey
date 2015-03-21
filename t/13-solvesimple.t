#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;

#
# Testing code starts here
#

use Test::More tests => 1;

my($q, @eqn, @expected);

$q = Algorithm::QuineMcCluskey->new(
	title	=> "Example 3.16 from Introduction to Logic Design, by Sajjan G. Shiva",
	width => 4,
	minterms => [ 0, 2, 5, 6, 7, 8, 10, 12, 13, 14, 15 ],
);

@expected = (
	q/(AD') + (BD) + (B'D') + (CD')/
);

@eqn = $q->solve;
is_deeply(\@eqn, \@expected, $q->title);

