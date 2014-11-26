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
	bin columns diffpos diffposes hdist maskmatcher remel_hoa stl uniqels
);
our @EXPORT_OK = qw(
	bin columns diffpos diffposes hdist maskmatcher remel_hoa stl uniqels
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
sub remel_hoa ($$$);
sub uniqels (@);
sub columns ($@);
sub diffpos ($$);
sub bin ($);
sub diffposes;
sub stl ($);

=head1 FUNCTIONS

=over 4


=item maskmatcher

Returns the terms that match a mask.

=cut

sub maskmatcher ($$@)
{
	my($m, $dc, @terms) = @_;
	my @t;

	(my $mask0 = $m) =~ s/$dc/0/g;
	(my $mask1 = $m) =~ s/$dc/1/g;
	$mask0 = bin $mask0;
	$mask1 = bin $mask1;

	for my $x (@terms)
	{
		my $b = bin $x;
		push @t, $x if ((($mask0 & $b) == $mask0) && (($mask1 & $b) == $b));
	}

	return @t;
}

=item remel_hoa

Given a value and a reference to a hash of arrayrefs, remove the value
from the individual arrayrefs if the value matches the masks.

Returns the number of removals made.

=cut

sub remel_hoa ($$$)
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

