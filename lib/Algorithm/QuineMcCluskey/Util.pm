=head1 NAME

Algorithm::QuineMcCluskey::Util - provide utility functions to
Algorithm::QuineMcCluskey

=cut

package Algorithm::QuineMcCluskey::Util;

use strict;
use warnings;

use Data::Dumper;
use List::MoreUtils qw(pairwise indexes firstidx);
use List::Util qw(any sum);

use base qw(Exporter);
our @EXPORT = qw(
	columns countels diffpos diffposes find_essentials hdist maskmatcher
	purge_elements remels matchcount stl uniqels
);
our @EXPORT_OK = qw(
	columns countels diffpos diffposes find_essentials hdist maskmatcher
	purge_elements remels matchcount stl uniqels
);

=head1 VERSION

This document describes version 0.01 released 24 June 2006.

=cut

our $VERSION = 0.01;

=head1 DESCRIPTION

This module provides various utilities designed for (but not limited to) use in
Algorithm::QuineMcCluskey.

=cut

=head1 FUNCTIONS

=over 4

=item matchcount

Returns the count of a search string Y found in the source string X.

E.g.:
      my $str = "d10d11d1d"; 
      matchcount($str, "d");     # returns 4
      matchcount($str, "d1");    # returns 3

To search for only the string without a regular expression accidentally
interfering, enclose the search string between '\Q' and '\E'. E.g.:

      # We don't know what's in $looking, so de-magic it.
      my $str = "d10d11d1d"; 
      matchcount($str, '\E' . $looking . '\Q]);

=cut

sub matchcount
{
	my($x, $y) = @_;

	return scalar(() = $x=~ m/$y/g);
}

=item maskmatcher

Returns the terms that match a mask.

=cut

sub maskmatcher
{
	my($m, $dc, @terms) = @_;
	my @t;

	#
	# Make two patterns based on the don't-care characters
	# in the mask ("quoted" in case the don't-care character
	# happens to be a metacharacter).
	#
	(my $mask0 = $m) =~ s/\Q$dc\E/0/g;
	(my $mask1 = $m) =~ s/\Q$dc\E/1/g;
	$mask0 = oct "0b" . $mask0;
	$mask1 = oct "0b" . $mask1;

	for my $x (@terms)
	{
		my $b = oct "0b" . $x;
		push @t, $x if ((($mask0 & $b) == $mask0) && (($mask1 & $b) == $b));
	}

	return @t;
}

=item find_essentials

Find the essential prime implicants.

=cut

sub find_essentials
{
	my $primes = shift;
	my(@terms) = @_;

	my @kp = keys %$primes;
	my %essentials;

	for my $term (@terms)
	{
		my @tp = grep {
				grep { $_ eq $term } @{ $primes->{$_} }
			} @kp;

		#
		# TODO: It would be nice to track the terms that make
		# this essential
		if (scalar @tp == 1)
		{
			$essentials{$tp[0]}++;
		}
	}

	return %essentials;
}


=item purge_elements

Given a table (hash form) of prime implicants, delete the list of elements
(usually essential prime implicants) from the table (row-wise and column-wise),
leaving behind implicants that must be chosen for the remaining elements of
the boolean function.

=cut

sub purge_elements
{
	my($primes, $dc, @ess) = @_;
	my $count = 0;

	return $count if (scalar @ess == 0 or scalar keys %$primes == 0);

	#
	# Delete the rows of each element,
	# then delete the columns associated with each element.
	#
	delete ${$primes}{$_} for @ess;

	for my $el (@ess)
	{
		$count += remels($el, $dc, $primes);
	}

	return $count;
}


=item remels

Given a value and a reference to a hash of arrayrefs, remove the value
from the individual arrayrefs if the value matches the masks.

Returns the number of removals made.

=cut

sub remels
{
	my ($el, $dc, $href) = @_;
	my $rems = 0;

	for my $k (keys %$href)
	{
		my @pos = indexes { maskmatcher($el, $dc, $_) } @{$href->{$k}};
		for my $pos (reverse @pos)
		{
			splice(@{$href->{$k}}, $pos, 1);
			$rems++;
		}
	}

	return $rems;
}

sub countels
{
	my($el, $aref) = @_;

	return 0 unless (@$aref);
	return sum map { $_ eq $el } @$aref;
}

=item uniqels

Returns unique elements of an arrayref; usable for deep structures

=cut

sub uniqels
{
    my %h;
    map { $h{Dumper($_)}++ == 0 ? $_ : () } @_;
}

=item columns

Rotates 90 degrees a hashtable of the type used for %primes

=cut

sub columns
{
	my ($r, @c) = @_;
	map {
		my $o = $_;
		$o => [ grep {
			any { $_ eq $o } @{ $r->{$_} }
		} keys %$r ]
	} @c
}

=item diffpos

Find the location of the first difference between two strings

=cut

sub diffpos { firstidx { $_ } diffposes(@_)}

=item hdist

Hamming distance

=cut

sub hdist { sum diffposes(@_)}

=item diffposes

Return pairwise the 'un-sameness' of two strings

=cut

sub diffposes { pairwise { $a ne $b } @{[ stl(shift)]}, @{[ stl(shift)]} }

=item stl

Splits a string into a list of its chars

=cut

sub stl { split //, shift }

=back

=head1 TODO

Documentation. Most of the subs are very simple, but they still could use a bit
more explanation.

=head1 SEE ALSO

L<Algorithm::QuineMcCluskey>

=head1 AUTHOR

Darren M. Kulp C<< <darren@kulp.ch> >>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2006 by Darren Kulp

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut

1;

__END__

