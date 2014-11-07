use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Algorithm::QuineMcCluskey',
    AUTHOR              => 'Darren Kulp <darren@kulp.ch>',
    VERSION_FROM        => 'lib/Algorithm/QuineMcCluskey.pm',
    ABSTRACT_FROM       => 'lib/Algorithm/QuineMcCluskey.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Moose' => 0,
        'Carp' => 0,
		'Data::Dumper' => 0,
        'List::Compare::Functional' => 0,
        'List::MoreUtils' => 0,
        'List::Util' => 0,
        'Test::More' => 0,
        'Tie::Cycle' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Algorithm-QuineMcCluskey-*' },
);
