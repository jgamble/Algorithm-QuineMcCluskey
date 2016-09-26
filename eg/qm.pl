#!/usr/bin/perl -w
use strict;
use Carp;
use Algorithm::QuineMcCluskey;
use Algorithm::QuineMcCluskey::Format qw(chart hasharray);
use Getopt::Long;

my($term_opt, $primes_opt, $ess_opt, $covers_opt);
my($width, @mterms);
my(@dontcares);

GetOptions(
	"width=i" => \$width,
	"m" => \$term_opt,
	"pi" => \$primes_opt,
	"ess" => \$ess_opt,
	"co" => \$covers_opt,
	"dc=i{1,}" => \@dontcares
);

if (scalar @ARGV == 0)
{
	carp "qm.pl --width=i, [--m --co --ess --pi ] [--dc=<terms>] <terms>\n";
}

croak "Must supply a width!" unless (defined $width);

my $termkey = (defined $term_opt)? "maxterms": "minterms";

my $q = Algorithm::QuineMcCluskey->new(
	width => $width,
	$termkey => [ @ARGV ],
	dontcares => [@dontcares]
);

if (defined $primes_opt)
{
	my $p = $q->get_primes;
	print chart($p, $q->width), "\n";
}
elsif (defined $ess_opt)
{
	my $e = $q->get_essentials;
	print join(", ", @{$e}), "\n";
}
elsif (defined $covers_opt)
{
	my $c = $q->get_covers();
	for my $idx (0 .. $#{$c})
	{
		my @cvs = sort @{$c->[$idx]};
		print "'", join("', '",  @cvs), "' => ";
		print $q->to_boolean(\@cvs), "\n";
	}
}
else
{
	print $q->solve;
}

exit(0);

