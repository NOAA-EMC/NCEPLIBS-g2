![Status](https://github.com/NOAA-EMC/NCEPLIBS-sp/workflows/Build%20and%20Test/badge.svg)

# NCEPLIBS G2 Library

This library contains
Fortran 90 decoder/encoder routines for GRIB edition 2. This is part of the
[NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For more detailed documentation see
https://noaa-emc.github.io/NCEPLIBS-g2/. For the NCEP WMO GRIB2
Documentation see
https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/.

To submit bug reports, feature requests, or other code-related issues
including installation and usage questions, please create a [GitHub
issue](https://github.com/NOAA-EMC/NCEPLIBS-g2/issues). For general
NCEPLIBS inquiries, contact [Edward
Hartnett](mailto:edward.hartnett@noaa.gov) (secondary point of contact
[Alex Richert](mailto:alexander.richert@noaa.gov)).

## Authors

Stephen Gilbert, NCEP/EMC Developers

Code Manager : [Hang Lei](mailto:hang.lei@noaa.gov), [Ed
Hartnett](mailto:edward.hartnett@noaa.gov)

## Prerequisites

The following libraries are required:

This package requires the following third party libraries:
- [Jasper](http://www.ece.uvic.ca/~mdadams/jasper/)
- [libpng](http://www.libpng.org/pub/png/libpng.html)
- [libz](http://www.gzip.org/zlib/)

This package requires the folling NCEPLIBS libraries:
- [NCEPLIBS-bacio](https://github.com/NOAA-EMC/NCEPLIBS-bacio)
- [NCEPLIBS-w3emc](https://github.com/NOAA-EMC/NCEPLIBS-w3emc) (optional)

## Building

```
cmake .. -DCMAKE_INSTALL_PREFIX=myg2 -DCMAKE_PREFIX_PATH="${PNG_LIBDIR};${PNG_INC};${JASPER_LIBDIR};${JASPER_INC}"

make
make install

```

## References

```
Kumar, V. Krishna, Gilbert, Stephen A., [GRIB2 conversion and its
usage at NCEP](docs/GRIB2_conversion_and_its_usage_at_NCEP.pdf), 14-18
November 2005, 10th Workshop on Meteorological Operational Systems
ECMWF User Orientation, retrieved on July 27, 2021 from
https://www.yumpu.com/en/document/view/11925806/grib2-conversion-and-its-usage-at-ncep.

```
## Disclaimer

```
The United States Department of Commerce (DOC) GitHub project code is
provided on an "as is" basis and the user assumes responsibility for
its use. DOC has relinquished control of the information and no longer
has responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and
logo of a DOC bureau, shall not be used in any manner to imply
endorsement of any commercial product or activity by DOC or the United
States Government.

