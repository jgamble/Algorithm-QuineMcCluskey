#!/usr/bin/perl -w
use strict;
use Algorithm::QuineMcCluskey;

################################################################################
# Beware: this test uses a rather unconventional style of testing that was
# designed to make tests easy to add but instead mostly looks odd while trying
# to be too general.
################################################################################
# Test definitions start here
################################################################################

my @tests = (
	{
		name	=> "Null test - not executed",
		ctor	=> undef,
		calls	=> undef
	},
	{
		name	=> "Simple 6-minterm problem",
		ctor	=> {
			call	=> 'new Algorithm::QuineMcCluskey(@_)',
			args	=> '
				width => 4,
				minterms => [ qw(3 5 7 8 9 10) ],
				dc => "x"
			',
		},
		calls	=> [
			{
				type	=> 'method',
				value	=> \&Algorithm::QuineMcCluskey::find_primes,
				message	=> 'finding prime implicants',
				result	=> {
					type	=> 'HASH',
					value	=> {
						'01x1' => [ '0101', '0111' ],
						'100x' => [ '1000', '1001' ],
						'0x11' => [ '0011', '0111' ],
						'10x0' => [ '1000', '1010' ]
					}
				}
			},
			{
				type	=> 'field',
				value	=> 'primes',
				message	=> 'read field after finding prime implicants',
				result	=> {
					type	=> 'HASH',
					value	=> {
						'01x1' => [ '0101', '0111' ],
						'100x' => [ '1000', '1001' ],
						'0x11' => [ '0011', '0111' ],
						'10x0' => [ '1000', '1010' ]
					}
				}
			},
			{
				type	=> 'method',
				value	=> \&Algorithm::QuineMcCluskey::find_essentials,
				message	=> 'finding essential prime implicants',
				result	=> {
					type	=> 'HASH',
					value	=> {
						'01x1' => 1,
						'100x' => 1,
						'0x11' => 1,
						'10x0' => 1
					}
				}
			},
			{
				type	=> 'method',
				value	=> \&Algorithm::QuineMcCluskey::purge_essentials,
				message	=> 'purging essential prime implicants',
				result	=> {
					type	=> 'HASH',
					value	=> {
						'01x1' => 1,
						'100x' => 1,
						'0x11' => 1,
						'10x0' => 1
					}
				}
			},
			{
				type	=> 'method',
				value	=> \&Algorithm::QuineMcCluskey::col_dom,
				message	=> 'column dominance',
				result	=> {
					type	=> 'HASH',
					# TODO: Make this more interesting
					value	=> { }
				}
			},
		]
	},
	{
		name	=> 'Five-bit, 19-minterm problem',
		ctor	=> {
			call	=> 'new Algorithm::QuineMcCluskey(@_)',
			args	=> '
				width => 5,
				minterms => [ qw(0 2 4 5 6 7 8 9 10 11 13 15 21 23 26 28 29 30 31) ],
				dc => "x"
			',
		},
		calls	=> [
			{
				type	=> 'method',
				value	=> \&Algorithm::QuineMcCluskey::find_primes,
				message	=> 'finding prime implicants'
			},
			{
				type	=> 'method',
				value	=> \&Algorithm::QuineMcCluskey::find_essentials,
				message	=> 'finding essential prime implicants',
				result	=> {
					type	=> 'HASH',
					value	=> {
						'111xx' => 1,
						'xx1x1' => 2
					}
				}
			}
		]
	},
	{
		name	=> 'Four-bit, 8-minterm Boolean expression test',
		ctor	=> {
			call	=> 'new Algorithm::QuineMcCluskey(@_)',
			args	=> '
				width => 4,
				minterms => [ qw(1 3 7 11 12 13 14 15) ]
			',
		},
		calls	=> [
			{
				type	=> 'method',
				value	=> \&Algorithm::QuineMcCluskey::solve,
				message	=> 'getting Boolean expression',
				result	=> {
					type	=> 'ARRAY',
					value	=> [
						q/(AB) + (A'B'D) + (CD)/
					]
				}
			}
		]
	},
	{
		name	=> 'Five-bit, 12-minterm Boolean expression test with don\'t-cares',
		ctor	=> {
			call	=> 'new Algorithm::QuineMcCluskey(@_)',
			args	=> '
				width => 5,
				minterms => [ qw(0 5 7 8 10 11 15 17 18 23 26 27) ],
				dontcares => [ qw(2 16 19 21 24 25) ]
			',
		},
		calls	=> [
			{
				type	=> 'method',
				value	=> \&Algorithm::QuineMcCluskey::solve,
				message	=> 'getting Boolean expression',
				result	=> {
					type	=> 'ARRAY',
					value	=> [
						q/(B'CE) + (C'E') + (AC') + (A'BDE)/
					]
				}
			}
		]
	}
);

################################################################################
# Testing code starts here
################################################################################

use Test::More no_plan => 1;

for my $test (@tests) {
	my $args = $test->{ctor}{args};
	# FIXME: Don't use strings for arguments unless necessary
	(my $ctor = $test->{ctor}{call} || '') =~ s/(\@_)/$args/;
	my $o = eval $ctor;
	for my $call (@{ $test->{calls} }) {
		my $value = $call->{value};
		my $args = $call->{args} || '';
		my $return;

		if ($call->{type} eq 'method') {
			local $_ = $call->{result}{type};
			if (defined $_) {
				$return =
					/^HASH$/
						? { $o->$value(eval $args) } :
					/^ARRAY$/
						? [ $o->$value(eval $args) ] :
					/^SCALAR$/ || /^HASHREF$/ || /^ARRAYREF$/
						? $o->$value(eval $args) :
					'ERROR! Unknown datatype!';
			} else { $o->$value(eval $args); }
			# FIXME: What about testing functions that return undef?
		} elsif ($call->{type} eq 'field') {
			# FIXME: What about other field access methods
			$return = $o->{$call->{value}};
		}
		# TODO: Add support for different kinds of tests
		is_deeply($return, $call->{result}{value},
			$test->{name} . ' - ' . $call->{message})
			if defined $call->{result}{value};
	}
}


