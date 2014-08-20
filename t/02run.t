#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;
use Carp;


#
# Testing code starts here
#

use Test::More tests => 2;
use Data::Dumper;

my $q = Algorithm::QuineMcCluskey->new(
	title	=> "Simple 4-minterm problem",
	width => 3,
	minterms => [ 0, 3, 5, 7 ],
	dc => "x"
);

my %val1 = (
	'000' => [ '000' ],
	'x11' => [ '011', '111' ],
	'1x1' => [ '101', '111' ],
);


my %val2 = (
	'000' => 1,
);

#
# 'finding prime implicants',
#
$q->find_primes;
my $hashref = $q->get_primes;
is_deeply($hashref, \%val1, "finding prime implicants");

#
# 'finding essential prime implicants',
#
$q->find_essentials;
$hashref = $q->get_essentials;
carp Dumper($hashref);
is_deeply($hashref, \%val2, "finding essential prime implicants");

