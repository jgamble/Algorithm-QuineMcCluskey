#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;


#
# Testing code starts here
#

use Test::More skip_all => 'Some day we'll be able to do this.';

my $q = Algorithm::QuineMcCluskey->new(
	title	=> "Rock (01) Paper (10) Scissors (11)  'Winner' table.",
	width => 4,
	dc => "x",
	vars => [qw(a1 a0 b1 b0)],
	funcs => [qw(c1 c0)],
	columns => [
		{
			minterms => [ 6, 9, 11, 14 ],
			dcterms => [0..4, 8, 12],
		},
		{
			minterms => [ 7, 11, 13, 14 ],
			dcterms => [0..4, 8, 12],
		}
	]
);

$q->find_primes;

