#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;

#
# Testing code starts here
#

use Test::More tests => 3;

my($q, @eqn, @expected);

$q = Algorithm::QuineMcCluskey->new(
	title	=> 'Four-bit, 8-minterm Boolean expression test',
	width => 4,
	minterms => [ 1, 3, 7, 11, 12, 13, 14, 15 ]
);

@expected = (
	q/(AB) + (A'B'D) + (CD)/
);

@eqn = $q->solve;
is_deeply(\@eqn, \@expected, $q->title);

$q = Algorithm::QuineMcCluskey->new(
	title	=> 'a xor c xor d test',
	width => 4,
	minterms => [ 1, 2, 5, 6, 8, 11, 12, 15 ]
);

@expected = (
	q/(ACD) + (AC'D') + (A'CD') + (A'C'D)/
);

@eqn = $q->solve;
is_deeply(\@eqn, \@expected, $q->title);


$q = Algorithm::QuineMcCluskey->new(
	title	=> "Five-bit, 12-minterm Boolean expression test with don't-cares",
	width => 5,
	minterms => [ 0, 5, 7, 8, 10, 11, 15, 17, 18, 23, 26, 27 ],
	dontcares => [ 2, 16, 19, 21, 24, 25 ]
);

@expected = (
	q/(AC') + (A'BDE) + (B'CE) + (C'E')/
);

@eqn = $q->solve;
is_deeply(\@eqn, \@expected, $q->title);

