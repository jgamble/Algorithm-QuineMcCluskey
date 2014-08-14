#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;

#
# Testing code starts here
#

use Test::More tests => 1;

my $q = Algorithm::QuineMcCluskey->new(
	title	=> 'Four-bit, 8-minterm Boolean expression test',
	width => 4,
	minterms => [ qw(1 3 7 11 12 13 14 15) ]
);

my @val1	=> [
	q/(AB) + (A'B'D) + (CD)/
];

my @r01 = $q->solve;
is_deeply(\@r01, \@val1, "getting Boolean expression");

