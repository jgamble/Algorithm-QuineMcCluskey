# Algorithm-QuineMcCluskey
## version 1.00

This module creates objects designed to solve boolean expressions via the
Quine-McCluskey algorithm. Its effectiveness is dependent on the size of
problem; the number of minterms that can be set goes up exponentially with
the number of variables. It is likely that eight varibles may be the practical
maximum. However, speed and memory have improved, so upper limit of what is
practical to solve should be tested first.

## INSTALLATION

To install this module, run the following commands:

```shell
perl Build.PL
./Build
./Build test
./Build install
```

## SUPPORT AND DOCUMENTATION

Depending upon your system, you can view documentation using the 'perldoc' or 'man' command. Online,
you can also look for information at:

### MetaCPAN

        [https://metacpan.org/release/Algorithm-QuineMcCluskey]

Helpful links to supporting web sites and documentation are listed on the page.

## COPYRIGHT AND LICENCE

Copyright (C) 2006 Darren Kulp

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.
