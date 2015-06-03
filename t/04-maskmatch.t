#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey::Util qw(maskmatcher);

#
# Testing code starts here
#

use Test::More tests => 4;

my $matched;
my @b = qw(00000 01010 01011 11010 10111 11011);

#
# Check with default don't-care character.
#
$matched = "[" . join(", ", maskmatcher('--0-0', '-', @b)) . "]";
is($matched, "[00000, 01010, 11010]", "Match on --0-0");

$matched = "[" . join(", ", maskmatcher('-01-1', '-', @b)) . "]";
is($matched, "[10111]", "Match on -01-1");

#
# Repeat using a RE character as the don't-care.
#
$matched = "[" . join(", ", maskmatcher('..0.0', '.', @b)) . "]";
is($matched, "[00000, 01010, 11010]", "Match on ..0.0");

$matched = "[" . join(", ", maskmatcher('.01.1', '.', @b)) . "]";
is($matched, "[10111]", "Match on .01.1");
