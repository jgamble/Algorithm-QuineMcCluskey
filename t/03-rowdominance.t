#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey::Util qw(row_dominance);


#
# Testing code starts here
#

use Test::More tests => 3;


#
# Example 3.17 from Introduction to Logic Design, p. 130.
#
my %primes = (
	'00x' => [ '000', '001' ],
	'x00' => [ '000', '100' ],
	'0x1' => [ '001', '011' ],
	'1x0' => [ '100', '110' ],
	'x11' => [ '011', '111' ],
	'11x' => [ '011', '111' ],
);

my @expected_rows = qw();	# I.e., no rows dominate others.
my @rows = row_dominance(\%primes);

is_deeply(\@rows, \@expected_rows, "Dominated rows 1");


%primes = (
	'000x' => [ '0000', '0001' ],
	'x00x' => [ '0000', '0001', '1000' ],
	'101x' => [ '0010', '0011', '1011' ],
	'10xx' => [ '0011', '1000', '1001', '1011' ],
	'1xxx' => [ '0011', '1000', '1001', '1011' ],
);

@expected_rows = qw(x00x);
@rows = row_dominance(\%primes);
is_deeply(\@rows, \@expected_rows, "Dominated rows 3");

%primes = (
	'x101x' => [ '01011', '11011' ],
	'0x111' => [ '01111' ],
	'01x11' => [ '01011', '01111' ],
	'1x0xx' => [ '10001', '11011' ],
	'10xx1' => [ '10001' ],
);

@expected_rows = qw(01x11 1x0xx);
@rows = sort(row_dominance(\%primes));
is_deeply(\@rows, \@expected_rows, "Dominated rows 2");

