#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;

#
# Testing code starts here
#

use Test::More tests => 1;

my $q = Algorithm::QuineMcCluskey->new(
	title => 'Five-bit, 19-minterm problem',
	width => 5,
	minterms => [ 0, 2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 21, 23,
			26, 28, 29, 30, 31 ],
	dc => "x"
);


my %val1 = {
	'111xx' => 1,
	'xx1x1' => 2
};

my %r01 = $q->find_primes;

my %r02 = $q->find_essentials;

is_deeply(\%r02, \%val1, "finding essential prime implicants");


