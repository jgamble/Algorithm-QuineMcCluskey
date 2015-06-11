use 5.010;
use strict;
use warnings FATAL =>'all';
use Module::Build;

my $builder = Module::Build->new(
	module_name => 'Algorithm::QuineMcCluskey',

	dist_abstract => 'Solve sets of Boolean terms with the Quine-McCluskey algorithm',
	dist_author => [q{Darren Kulp <darren@kulp.ch>},
			q{John M. Gamble <jgamble@cpan.org>}],
	dist_version => '0.02',

	requires => {
		perl => '5.10.1',
		Moose => 0,
		Carp => 0,
		'Data::Dumper' => 0,
		'List::Compare::Functional' => 0,
		'List::Util' => 0,
		'Tie::Cycle' => 0,
	},

	configure_requires => {
		'Module::Build' => '0',
	},

	build_requires => {
		'Test::More' => 0,
	},

	license => 'perl',
	create_license => 1,
	create_readme => 0,
	create_makefile_pl => 'traditional',
	dynamic_config => 0,

	meta_merge => {
		keywords => [qw(quine-mccluskey)],

		resources => {
			repository => 'git://github.com/jgamble/Algorithm-QuineMcCluskey',
#			repository => {
#				url => 'git://github.com/jgamble/Algorithm-QuineMcCluskey',
#				web => 'https://github.com/jgamble/Algorithm-QuineMcCluskey',
#				type => 'git',
#			},
		},
	},
);

$builder->create_build_script;
