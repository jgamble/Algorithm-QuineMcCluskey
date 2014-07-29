#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;

#
# Test object creation.
#

use Test::More tests => 1;

my $q = Algorithm::QuineMcCluskey->new(
	title => "Null Test",
	width => 1,
	minterms => [ 1 ]
);

isa_ok($q, "Algorithm::QuineMcCluskey");

