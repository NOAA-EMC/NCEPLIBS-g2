![Status](https://github.com/NOAA-EMC/NCEPLIBS-g2/workflows/developer/badge.svg)

# NCEPLIBS-g2 Library

The NCEPLIBS-g2 library contains Fortran code to read and write GRIB
edition 2 files, and itilities to manipulate GRIB2 files.

GRIdded Binary or General Regularly-distributed Information in Binary
form (GRIB) is a data format for meteorological and forecast data,
standardized by the World Meteorological Organization (WMO). GRIB
edition 2 (GRIB2) was approved by the WMO is 2003.

This library is part of the
[NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For more detailed documentation on this library see the [NCEPLIBS-g2
documentation](https://noaa-emc.github.io/NCEPLIBS-g2/). For more
information about GRIB2, see the [NCEP WMO GRIB2
Documentation](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/).

Starting with NCEPLIBS-g2-4.0.0, this library depends on the
[NCEPLIBS-g2c](https://github.com/NOAA-EMC/NCEPLIBS-g2c) library.

Starting with NCEPLIBS-g2-4.0.0, this library includes the utilities
formerly released in
[NCEPLIBS-grib_util](https://github.com/NOAA-EMC/NCEPLIBS-grib_util). The
NCEPLIBS-grib_util repository is deprecated.

To submit bug reports, feature requests, or other code-related issues
including installation and usage questions, please create a [GitHub
issue](https://github.com/NOAA-EMC/NCEPLIBS-g2/issues). For general
NCEPLIBS inquiries, contact [Edward
Hartnett](mailto:edward.hartnett@noaa.gov) (secondary point of contact
[Alex Richert](mailto:alexander.richert@noaa.gov)).

## The Utilities

Utility | Purpose
--------|--------
cnvgrib | Convert between GRIB1 and GRIB2.
copygb | Copy all or part of a GRIB1 file.
copygb2 | Copy all or part of a GRIB2 file.
degrib2 | Inventory a GRIB2 file.
grb2index | Create an index from a GRIB1 file.
grbindex | Create an index from a GRIB2 file.
tocgrib | Copy some GRIB2 fields to a new GRIB1 file.
tocgrib2 | Copy some GRIB2 fields to a new GRIB2 file.
tocgrib2super | Copy some GRIB2 fields to a new GRIB2 file with super WMO header.

## Related NCEPLIBS Projects

Repository | Notes
-----------|------
[NCEPLIBS-ip](https://github.com/NOAA-EMC/NCEPLIBS-ip) | Interpolation.
[NCEPLIBS-w3emc](https://github.com/NOAA-EMC/NCEPLIBS-w3emc) | Coders/decoders for GRIB1
[NCEPLIBS-g2c](https://github.com/NOAA-EMC/NCEPLIBS-g2c) | C implementation of the GRIB 2 functions
[NCEPLIBS-g2tmpl](https://github.com/NOAA-EMC/NCEPLIBS-g2tmpl) | Utilities for GRIB2 templates

## Authors

Harry Glahn, Kyle Gerheiser, Stephen Gilbert, Brent Gordon, Edward
Hartnett, Mark Iredell, Hang Lei, Alyson Stahl, Boi Vuong, and other
NOAA scientists and developers.

Code Manager: [Hang Lei](mailto:hang.lei@noaa.gov), [Edward
Hartnett](mailto:edward.hartnett@noaa.gov)

## Prerequisites

The following libraries are required:

This package requires the following third party libraries:
- [Jasper](http://www.ece.uvic.ca/~mdadams/jasper/)
- [libpng](http://www.libpng.org/pub/png/libpng.html)
- [zlib](http://www.zlib.net/)

This package requires the following NCEPLIBS libraries:
- [NCEPLIBS-g2c](https://github.com/NOAA-EMC/NCEPLIBS-g2c)
- [NCEPLIBS-ip](https://github.com/NOAA-EMC/NCEPLIBS-ip)
- [NCEPLIBS-bacio](https://github.com/NOAA-EMC/NCEPLIBS-bacio)
- [NCEPLIBS-w3emc](https://github.com/NOAA-EMC/NCEPLIBS-w3emc) (optional)

## Building

Download release tarball and go to root directory, or `git clone https://github.com/NOAA-EMC/NCEPLIBS-g2c`.

```
cmake -S NCEPLIBS-g2c -B NCEPLIBS-g2c/build -DCMAKE_INSTALL_PREFIX=myg2 -DCMAKE_PREFIX_PATH="${PNG_LIBDIR};${PNG_INC};${JASPER_LIBDIR};${JASPER_INC}"
cmake --build NCEPLIBS-g2c/build
ctest --test-dir NCEPLIBS-g2c/build # run unit tests
cmake --install NCEPLIBS-g2c/build
```

See [documentation](https://noaa-emc.github.io/NCEPLIBS-g2c) for a list of CMake options.

NCEPLIBS-g2 is also available through Spack as '[g2](https://github.com/spack/spack-packages/blob/develop/repos/spack_repo/builtin/packages/g2)'.

## References

Hartnett, E., Lei, H., Richert, A., Stahl, A., [A New API for
NOAA's GRIB2
Libraries](https://www.researchgate.net/publication/386906653_A_New_API_for_NOAA's_GRIB2_Libraries),
American Geophysical Union (AGU) 2024.

Hartnett, E., Ator, J, Lei, H., Richert, A., Woollen, J., King, A.,
Hartnett, A., [NCEPLIBS GRIB and BUFR Libraries: Maintaining and
Modernizing NOAA's Libraries for WMO Data
Formats](https://www.researchgate.net/publication/376390180_NCEPLIBS_GRIB_and_BUFR_Libraries_Maintaining_and_Modernizing_NOAA's_Libraries_for_WMO_Data_Formats),
American Geophysical Union (AGU) 2023. (See also
[poster](https://www.researchgate.net/publication/376582005_Poster_-_IN51B-0416_NCEPLIBS_GRIB_and_BUFR_Libraries_Maintaining_and_Modernizing_NOAA's_Libraries_for_WMO_Data_Formats)).

Hartnett, E., Lei, H., Curtis, B, Gerheiser K., [Presentation -
Improving Documentation, Testing, Process, and Code for Legacy NOAA
GRIB2 C Fortran
Libraries](https://www.researchgate.net/publication/360757566_Presentation_-_Improving_Documentation_Testing_Process_and_Code_for_Legacy_NOAA_GRIB2_C_Fortran_Libraries),
NCAR Improving Scientific Software, April 2022.  .

Kumar, V. Krishna, Gilbert, Stephen A., [GRIB2 conversion and its
usage at NCEP](docs/GRIB2_conversion_and_its_usage_at_NCEP.pdf), 14-18
November 2005, 10th Workshop on Meteorological Operational Systems
ECMWF User Orientation.

## Disclaimer

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

Generative AI tools are used to assist with developing this code.
The code has been reviewed, edited, and validated by NWS staff.
