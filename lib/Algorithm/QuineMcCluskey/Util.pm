=head1 NAME

Algorithm::QuineMcCluskey::Util - provide utility functions to
Algorithm::QuineMcCluskey

=cut

package Algorithm::QuineMcCluskey::Util;

use strict;
use warnings;

use Data::Dumper;
use List::MoreUtils qw(pairwise firstidx);
use List::Util qw(sum min);

use base qw(Exporter);
our @EXPORT = qw(
	bin columns diffpos diffposes hdist maskmatch maskmatches remel stl tobit
	uniqels
);
our @EXPORT_OK = qw(
	bin columns diffpos diffposes hdist maskmatch maskmatches remel stl tobit
	uniqels
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
sub uniqels (@);
sub columns ($@);
sub diffpos ($$);
sub tobit ($$);
sub bin ($);
sub remel ($$);
sub diffposes;
sub stl ($);

=head1 FUNCTIONS

=over 4

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

=item tobit

Convert a number to an n-wide string of bits representing it

=cut

sub tobit ($$) { substr(unpack("B32", pack("N", shift)), -shift) }

=item maskmatch

Returns true if a mask matches a minterm, false otherwise.

=cut

sub maskmatch {
	my ($mask, $term) = @_;
	(my $mask0 = $mask) =~ s/$::dc/0/g;
	(my $mask1 = $mask) =~ s/$::dc/1/g;
	((bin $mask0 & bin $term) == bin $mask0) &&
		((bin $mask1 & bin $term) == bin $term)
}

=item maskmatches

Returns the elements that match a mask, selected from an array

=cut

sub maskmatches ($@) { my $m = shift; grep { maskmatch($m, $_) } @_ }

=item remel

Remove a value from an arrayref if it matches a mask

=cut

sub remel ($$) {
	my ($el, $a) = @_;
	my $pos = firstidx { maskmatch($el, $_) } @$a;
	splice(@$a, $pos, 1) if $pos >= 0;
	$a;
}

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

