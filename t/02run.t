#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;


#
# Testing code starts here
#

use Test::More tests => 3;

my $q = Algorithm::QuineMcCluskey->new(
	title	=> "Simple 4-minterm problem",
	width => 3,
	minterms => [ 0, 3, 5, 7 ],
	dc => "x"
);

my %expected_primes = (
	'000' => [ '000' ],
	'x11' => [ '011', '111' ],
	'1x1' => [ '101', '111' ],
);


my %val2 = (
	'000' => 1,
	'x11' => 1,
	'1x1' => 1,
);

#
# 'finding prime implicants',
#
$q->find_primes;
my $hashref = $q->get_primes;
is_deeply($hashref, \%expected_primes, "finding prime implicants");

#
# 'finding essential prime implicants',
#
$q->find_essentials;
$hashref = $q->get_essentials;
is_deeply($hashref, \%val2, "finding essential prime implicants");

my $bitstring = $q->bitstring;
ok($bitstring eq "10010101", "Unexpected bitstring.");
