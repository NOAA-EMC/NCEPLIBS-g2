#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# Ed Hartnett, 12/25/21
# Alyson Stahl, 1/29/25

set -e
echo ""
echo "*** Running cnvgrib test"

# Show all options
../utils/cnvgrib -h

# Run with zero arguments
../utils/cnvgrib && exit 2

# Run with incorrect number of arguments
../utils/cnvgrib -g21 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 && exit 2

# Run with no -gxx option
../utils/cnvgrib -p0 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000.grib1 && exit 2

# Invalid master table version
../utils/cnvgrib -g21 -mastertable_ver_0 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000.grib1 && exit 2

# Invalid input file
../utils/cnvgrib -g21 - test_gdaswave.t00z.wcoast.0p16.f000.grib1 && exit 3

# Invalid conversion option
../utils/cnvgrib -g11 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000.grib1 && exit 5

# Convert test file to GRIB1.
../utils/cnvgrib -g21 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000.grib1

# Convert GRIB1 output back to GRIB2, create index, and compare to expected output
# First 120 bytes contain differences, so ignore them.

# Simple packing
../utils/cnvgrib -g12 -p0 test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.g12_simple.grib2
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000.g12_simple.grib2 test_gdaswave.t00z.wcoast.0p16.f000.g12_simple.grib2.idx
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000.g12_simple.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000.g12_simple.grib2.idx

# Complex Packing
../utils/cnvgrib -g12 -p2 test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex.grib2
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex.grib2 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex.grib2.idx
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000.g12_complex.grib2.idx

# Complex Packing w/ primary missing values
../utils/cnvgrib -g12 -p2 -m test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex_m.grib2
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex_m.grib2 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex_m.grib2.idx
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex_m.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000.g12_complex_m.grib2.idx

# Complex Packing w/ no explicit missing values included
../utils/cnvgrib -g12 -p2 -m0 test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex_m0.grib2
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex_m0.grib2 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex_m0.grib2.idx
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex_m0.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000.g12_complex_m0.grib2.idx

# Complex Packing w/ 1st order diffs
../utils/cnvgrib -g12 -p31 test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex1.grib2
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex1.grib2 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex1.grib2.idx
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex1.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000.g12_complex1.grib2.idx

# Complex Packing w/ 2nd order diffs
../utils/cnvgrib -g12 -p32 test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex2.grib2
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex2.grib2 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex2.grib2.idx
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000.g12_complex2.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000.g12_complex2.grib2.idx

# JPEG Packing
../utils/cnvgrib -g12 -p40 test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.g12_jpeg.grib2
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000.g12_jpeg.grib2 test_gdaswave.t00z.wcoast.0p16.f000.g12_jpeg.grib2.idx
cmp -i 121 test_gdaswave.t00z.wcoast.0p16.f000.g12_jpeg.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000.g12_jpeg.grib2.idx

# PNG Packing
../utils/cnvgrib -g12 -p41 test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.g12_png.grib2
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000.g12_png.grib2 test_gdaswave.t00z.wcoast.0p16.f000.g12_png.grib2.idx
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000.g12_png.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000.g12_png.grib2.idx

echo "*** SUCCESS!"
exit 0
