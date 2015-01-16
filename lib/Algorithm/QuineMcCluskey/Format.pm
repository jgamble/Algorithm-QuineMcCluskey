=head1 NAME

Algorithm::QuineMcCluskey::Format - provide formatting functions to
Algorithm::QuineMcCluskey

=cut

package Algorithm::QuineMcCluskey::Format;

use strict;
use warnings;
use Exporter;
use vars qw(@ISA @EXPORT_OK);
use List::MoreUtils qw(uniq firstidx);

@ISA = qw(Exporter);

our @EXPORT_OK = qw(elem_hash tableform);

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


=item elem_hash

Returns string form of primes structure.

=cut

sub elem_hash
{
	my ($hr, $width) = @_;
	my $fmt = "%" . ($width+2) . "s";
	my @output;
	my @rows = sort keys %$hr;

	for my $r (@rows)
	{
		push @output, "$r: [" . join(", ", @{ $hr->{$r} }) . "]\n";
	}

	return join("\n", @output);
}

sub tableform
{
	my ($hr, $width) = @_;
	my $fmt = "%" . ($width+2) . "s";
	my @output;
	my @rows = sort keys %$hr;

	my @columns = sort(uniq(map{ @{ $hr->{$_} } } @rows));
	push @output, join("", map{sprintf($fmt, $_)} ' ', @columns);

	for my $r (@rows)
	{
		my @present = map {my $v = $_; (firstidx{$v eq $_} @{ $hr->{$r} }) } @columns;

		my @marks = map{ sprintf($fmt, ($_ == -1)? '.': 'x') } @present;

		push @output, join("", sprintf($fmt, $r), @marks);
	}

	return join("\n", @output);
}

=back

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

