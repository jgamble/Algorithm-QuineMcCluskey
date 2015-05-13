#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;

#
# Testing code starts here
#

use Test::More tests => 1;

my($q, @eqn, @expected);

$q = Algorithm::QuineMcCluskey->new(
	title	=> "Example 3.17 from Introduction to Logic Design, by Sajjan G. Shiva, page 130.",
	width => 3,
	minterms => [ 0, 1, 3, 4, 6, 7 ],
);

@expected = (
	q/(A'B') + (AC') + (BC)/
);

@eqn = $q->solve;
is_deeply(\@eqn, \@expected, $q->title);

