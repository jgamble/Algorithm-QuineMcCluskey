#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;


#
# Testing code starts here
#

use Test::More tests => 5;

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


my %expected_ess = (
	'000' => 1,
	'x11' => 1,
	'1x1' => 1,
);

$q->find_primes;
my $hashref = $q->get_primes;
is_deeply($hashref, \%expected_primes, $q->title);

$q->find_essentials;
$hashref = $q->get_essentials;
is_deeply($hashref, \%expected_ess, $q->title);


$q = Algorithm::QuineMcCluskey->new(
	title	=> "Simple 6-minterm problem",
	width => 4,
	minterms => [ 3, 5, 7, 8, 9, 10 ],
	dc => "x"
);

%expected_primes = (
	'01x1' => [ '0101', '0111' ],
	'100x' => [ '1000', '1001' ],
	'0x11' => [ '0011', '0111' ],
	'10x0' => [ '1000', '1010' ]
);


%expected_ess = (
	'01x1' => 1,
	'100x' => 1,
	'0x11' => 1,
	'10x0' => 1
);


$q->find_primes;
$hashref = $q->get_primes;
is_deeply($hashref, \%expected_primes, $q->title);

$q->find_essentials;
$hashref = $q->get_essentials;
is_deeply($hashref, \%expected_ess, $q->title);


$q = Algorithm::QuineMcCluskey->new(
	title => 'Five-bit, 19-minterm problem',
	width => 5,
	minterms => [ 0, 2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 21, 23,
			26, 28, 29, 30, 31 ],
	dc => "x"
);


%expected_ess = (
	'111xx' => 1,
	'xx1x1' => 2
);

$q->find_primes;
$q->find_essentials;
$hashref = $q->get_essentials;
is_deeply($hashref, \%expected_ess, $q->title);

