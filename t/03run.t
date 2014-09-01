#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;


#
# Testing code starts here
#

use Test::More tests => 3;

my $q = Algorithm::QuineMcCluskey->new(
	title	=> "Simple 6-minterm problem",
	width => 4,
	minterms => [ 3, 5, 7, 8, 9, 10 ],
	dc => "x"
);

my %val1 = (
	'01x1' => [ '0101', '0111' ],
	'100x' => [ '1000', '1001' ],
	'0x11' => [ '0011', '0111' ],
	'10x0' => [ '1000', '1010' ]
);


my %val2 = (
	'01x1' => 1,
	'100x' => 1,
	'0x11' => 1,
	'10x0' => 1
);

my %val3 = (
	'01x1' => [ '0101', '0111' ],
	'100x' => [ '1000', '1001' ],
	'0x11' => [ '0011', '0111' ],
	'10x0' => [ '1000', '1010' ]
);

#
# 'finding prime implicants',
# 'read field after finding prime implicants',
#
$q->find_primes;

my $r02 = $q->get_primes;
is_deeply($r02, \%val1, "read field after finding prime implicants");

#
# 'finding essential prime implicants',
#
$q->find_essentials;
my $r03 = $q->get_essentials;
is_deeply($r03, \%val2, "finding essential prime implicants");

#
# 'column dominance',
#
my $r05 = $q->col_dom($r02);
is_deeply($r05, \%val3, "purging essential prime implicants");

