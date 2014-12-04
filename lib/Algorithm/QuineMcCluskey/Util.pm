=head1 NAME

Algorithm::QuineMcCluskey::Util - provide utility functions to
Algorithm::QuineMcCluskey

=cut

package Algorithm::QuineMcCluskey::Util;

use strict;
use warnings;

use Data::Dumper;
use List::MoreUtils qw(pairwise firstidx);
use List::Util qw(sum);

use base qw(Exporter);
our @EXPORT = qw(
	columns diffpos diffposes hdist maskmatcher remels
	strsearchcount stl uniqels
);
our @EXPORT_OK = qw(
	columns diffpos diffposes hdist maskmatcher remels
	strsearchcount stl uniqels
);

=head1 VERSION

This document describes version 0.01 released 24 June 2006.

=cut

our $VERSION = 0.01;

=head1 DESCRIPTION

This module provides various utilities designed for (but not limited to) use in
Algorithm::QuineMcCluskey.

=cut

################################################################################
# Sub declarations
################################################################################
sub maskmatcher ($$@);
sub remels ($$$);
sub uniqels (@);
sub columns ($@);
sub diffpos ($$);
sub bin ($);
sub diffposes;
sub strsearchcount ($$);
sub stl ($);

=head1 FUNCTIONS

=over 4

=item strsearchcount

Returns the count of a search string Y found in the source string X.
Pattern matching is turned off.

E.g.:
      my $str = "d10d11d1d"; 
      strsearchcount($str, "d");     # returns 4
      strsearchcount($str, "d1");    # returns 3

To search for only the string without a regular expression accidentally
interfering, enclose the search string between '\Q' and '\E'. E.g.:

      # We don't know what's in $looking, so de-magic it.
      my $str = "d10d11d1d"; 
      strsearchcount($str, '\E' . $looking . '\Q]);

=cut

sub strsearchcount($$)
{
	my($x, $y) = @_;

	return scalar(() = $x=~ m/$y/g);
}

=item maskmatcher

Returns the terms that match a mask.

=cut

sub maskmatcher ($$@)
{
	my($m, $dc, @terms) = @_;
	my @t;

	#
	# Make two patterns based on the don't-care
	# characters in the mask ("quoted" in case
	# the don't-care character happens to be a
	# metacharacter).
	#
	(my $mask0 = $m) =~ s/\Q$dc\E/0/g;
	(my $mask1 = $m) =~ s/\Q$dc\E/1/g;
	$mask0 = bin $mask0;
	$mask1 = bin $mask1;

	for my $x (@terms)
	{
		my $b = bin $x;
		push @t, $x if ((($mask0 & $b) == $mask0) && (($mask1 & $b) == $b));
	}

	return @t;
}

=item remels

Given a value and a reference to a hash of arrayrefs, remove the value
from the individual arrayrefs if the value matches the masks.

Returns the number of removals made.

=cut

sub remels ($$$)
{
	my ($el, $dc, $href) = @_;
	my $rems = 0;

	for my $k (keys %$href)
	{
		my $pos = firstidx { maskmatcher($el, $dc, $_) } @{$href->{$k}};
		if ($pos >= 0)
		{
			splice(@{$href->{$k}}, $pos, 1);
			$rems++;
		}
	}

	return $rems;
}

=item uniqels

Returns unique elements of an arrayref; usable for deep structures

=cut

sub uniqels (@) {
    my %h;
    map { $h{Dumper($_)}++ == 0 ? $_ : () } @_;
}

=item columns

Rotates 90 degrees a hashtable of the type used for %::primes

=cut

sub columns ($@) {
	my ($r, @c) = @_;
	map {
		my $o = $_;
		$o => [ grep {
			sum map {
				$_ eq $o ? 1 : 0
			} @{ $r->{$_} }
		} keys %$r ]
	} @c
}

=item bin

Wrap oct() to provide easy conversion of a binary string to a number

=cut

sub bin ($) { oct "0b" . shift }

=item diffpos

Find the location of the first difference between two strings

=cut

sub diffpos ($$) { firstidx { $_ } diffposes @_ }

=item hdist

Hamming distance

=cut

sub hdist { sum diffposes @_ }

=item diffposes

Return pairwise the 'un-sameness' of two strings

=cut

sub diffposes { pairwise { $a ne $b } @{[ stl shift ]}, @{[ stl shift ]} }

=item stl

Splits a string into a list of its chars

=cut

sub stl ($) { split //, shift }

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

