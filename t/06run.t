#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;

#
# Testing code starts here
#

use Test::More tests => 1;

my $q = Algorithm::QuineMcCluskey->new(
	title	=> "Five-bit, 12-minterm Boolean expression test with don't-cares",
	width => 5,
	minterms => [ qw(0 5 7 8 10 11 15 17 18 23 26 27) ],
	dontcares => [ qw(2 16 19 21 24 25) ]
};

my @val1 => [
	q/(B'CE) + (C'E') + (AC') + (A'BDE)/
];

@r01 = $q->solve;
is_deeply(\@r01, \@val1, "getting Boolean expression");

