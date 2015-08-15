#!/usr/bin/perl -w
use strict;
use Carp;
use Algorithm::QuineMcCluskey;
use Algorithm::QuineMcCluskey::Format qw(chart hasharray);
use Getopt::Long;

my ($width, @mterms);
my ($which_m, $prime_imp, $ess, $covers);
my (@dontcares);

GetOptions(
	"width=i" => \$width,
	"m" => \$which_m,
	"pi" => \$prime_imp,
	"ess" => \$ess,
	"co" => \$covers,
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
	my $p = $q->get_primes;
	print chart($p, $q->width), "\n";
}
elsif (defined $ess)
{
	my $e = $q->get_essentials;
	print join(", ", @{$e}), "\n";
}
elsif (defined $covers)
{
	my $c = $q->get_covers();
	for my $idx (0 .. $#{$c})
	{
		my @cvs = @{$c->[$idx]};
		print "'", join("', '",  sort @cvs), "' => ";
		print $q->to_boolean(\@cvs), "\n";
	}
}
else
{
	print $q->solve;
}

exit(0);

