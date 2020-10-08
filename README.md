![Status](https://github.com/NOAA-EMC/NCEPLIBS-sp/workflows/Build%20and%20Test/badge.svg)

### NCEPLIBS G2 Library

Utilities for coding/decoding GRIB2 messages. This library contains
Fortran 90 decoder/encoder routines for GRIB edition 2, as well as
indexing/searching utility routines. This is part of the
[NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For more detailed documentation see (grib2.doc).

#### Authors

NCEP/EMC Developers

Code Manager : Boi Vuong

#### Prerequisites

Compilers: GNU | Intel | Clang | AppleClang 

#### Installing
```
Download G2 Code from GitHub.com
git clone -b g2_v3.2.0 --recursive https://github.com/NOAA-EMC/NCEPLIBS-g2.git
cd NCEPLIBS-g2
```
#### Create a directory where to build G2 library
```
mkdir build
cd build
```
#### Load the following modules 
```
module load ips/18.0.1.163
module load impi/18.0.1
module load cmake/3.16.3
module load jasper/1.900.1
module load libpng/1.2.44
module load zlib/1.2.11

export CC=icc
export CXX=icpc
export FC=ifort

If the chosen compiler is not the default compiler on the system,
set the environment variables: export CC=..., export CXX=..., 
export FC=..., before invoking cmake.

Note: Windows systems is not supported at this time.

```
#### Run cmake
```
cmake .. -DCMAKE_INSTALL_PREFIX=myg2 -DCMAKE_PREFIX_PATH="${PNG_LIBDIR};${PNG_INC};${JASPER_LIBDIR};${JASPER_INC}"

If -DCMAKE_INSTALL_PREFIX= is omitted, the libraries will be installed in directory 
install underneath the build directory.

make
make install

```

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
