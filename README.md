![Status](https://github.com/NOAA-EMC/NCEPLIBS-sp/workflows/Build%20and%20Test/badge.svg)

# NCEPLIBS G2 Library

Utilities for coding/decoding GRIB2 messages. This library contains
Fortran 90 decoder/encoder routines for GRIB edition 2, as well as
indexing/searching utility routines. This is part of the
[NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For more detailed documentation see
https://noaa-emc.github.io/NCEPLIBS-g2/. For the NCEP WMO GRIB2
Documentation see
https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/.

For a C implementation of the GRIB 2 functions, see project
[NCEPLIBS-g2c](https://github.com/NOAA-EMC/NCEPLIBS-g2c).

### Users

The NCEPLIBS-g2 library is used by:
 - [UPP](https://github.com/NOAA-EMC/UPP)
 - [UFS_UTILS](https://github.com/NOAA-EMC/UFS_UTILS)
 - [The WAVEWATCH III Framework](https://github.com/NOAA-EMC/WW3)
 - [global-workflow](https://github.com/NOAA-EMC/global-workflow)
 - [HWRF](https://github.com/NCAR/HWRFdev)
 - [NOAA Meteorological Development Laboratory (MDL)](https://github.com/NOAA-MDL)

## Authors

Stephen Gilbert, Mark Iredell, Boi Vuong, other NCEP/EMC Developers

Code Manager : Hang Lei, Edward Hartnett

## Prerequisites

- [libjasper.a](http://www.ece.uvic.ca/~mdadams/jasper/) - This
  library is a C implementation of the JPEG-2000 Part-1 standard
  (i.e., ISO/IEC 15444-1). Tested version: jasper-1.900.1. More
  information about JPEG2000 can be found at
  http://www.jpeg.org/JPEG2000.html.

- [libpng.a](http://www.libpng.org/pub/png/libpng.html) - This library
  is a C implementation of the Portable Network Graphics PNG image
  compression format. Tested version: libpng-1.2.44. More information
  about PNG can be found at http://www.libpng.org/pub/png/.

- [NCEPLIBS-bacio](https://github.com/NOAA-EMC/NCEPLIBS-bacio) - This library
  does binary file I/O.

## Building

Download the tarball from the release page and unpack it, and cd into
the main directory of the library. Then run the following commands,
substituting your directory locations for the CMAKE_INSTALL_PREFIX
(where the NCEPLIBS-g2 library will be installed), and the
CMAKE_PREFIX_PATH (where the build will look for dependencies):

<pre>
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/usr/local/NCEPLIBS-g2 -DCMAKE_PREFIX_PATH=/usr/local/jasper-2.0.22 ..
make
make test
make install
</pre>

## References

Kumar, V. Krishna, Gilbert, Stephen A., [GRIB2 conversion and its
usage at NCEP](docs/GRIB2_conversion_and_its_usage_at_NCEP.pdf), 14-18
November 2005, 10th Workshop on Meteorological Operational Systems
ECMWF User Orientation, retrieved on July 27, 2021 from
https://www.yumpu.com/en/document/view/11925806/grib2-conversion-and-its-usage-at-ncep.

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
