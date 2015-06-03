#!/usr/bin/perl -w
use strict;
use Carp;
use Algorithm::QuineMcCluskey;
use Algorithm::QuineMcCluskey::Format qw(tableform);
use Getopt::Long;

my ($width, @mterms);
my ($which_m, $prime_imp);
my (@dontcares);

GetOptions(
	"width=i" => \$width,
	"m" => \$which_m,
	"pi" => \$prime_imp,
	"dc=i{1,}" => \@dontcares
);

croak "Must supply a width!" unless (defined $width);

my $termkey = (defined $which_m)? "maxterms": "minterms";

my $q = Algorithm::QuineMcCluskey->new(
	width => $width,
	$termkey => [ @ARGV ],
	dontcares => [@dontcares]
);

if (defined $prime_imp)
{
	print tableform($q->get_primes, $q->width), "\n";
}
else
{
	print $q->solve;
}

exit(0);

