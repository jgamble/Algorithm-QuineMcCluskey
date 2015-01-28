#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;

#
# Testing code starts here
#

use Test::More tests => 1;

my($q, @eqn, @expected);

$q = Algorithm::QuineMcCluskey->new(
	title	=> "First column of a four bit binary to 2-4-2-1 converter",
	width => 4,
	minterms => [ 5 .. 9 ],
	dontcares => [ 10 .. 15 ],
	vars => ['w' .. 'z'],
);

@expected = (
	q/(w) + (xy) + (xz)/
);

@eqn = $q->solve;
is_deeply(\@eqn, \@expected, $q->title);

