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

# Convert GRIB1 output back to GRIB2.
../utils/cnvgrib -g12 test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.grib2

# Create an index of a GRIB2 file.
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000.grib2.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2.idx

# Convert test file to another GRIB2 file.
../utils/cnvgrib -g22 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000_2.grib2

# Create an index of the new GRIB2 file.
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000_2.grib2 test_gdaswave.t00z.wcoast.0p16.f000_2.grib2.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000_2.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000_2.grib2.idx

# Convert test file to another GRIB2 file with simple packing.
../utils/cnvgrib -g22 -p0 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000_simple.grib2

# Create an index of the new GRIB2 file.
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000_simple.grib2 test_gdaswave.t00z.wcoast.0p16.f000_simple.grib2.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000_simple.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000_simple.grib2.idx

# Convert test file to another GRIB2 file with complex packing.
../utils/cnvgrib -g22 -p2 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000_complex.grib2

# Create an index of the new GRIB2 file.
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000_complex.grib2 test_gdaswave.t00z.wcoast.0p16.f000_complex.grib2.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000_complex.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000_complex.grib2.idx

# Convert test file to another GRIB2 file with complex packing (1st order diffs).
../utils/cnvgrib -g22 -p31 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000_complex1.grib2

# Create an index of the new GRIB2 file.
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000_complex1.grib2 test_gdaswave.t00z.wcoast.0p16.f000_complex1.grib2.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000_complex1.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000_complex1.grib2.idx

# Convert test file to another GRIB2 file with complex packing (2nd order diffs).
../utils/cnvgrib -g22 -p32 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000_complex2.grib2

# Create an index of the new GRIB2 file.
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000_complex2.grib2 test_gdaswave.t00z.wcoast.0p16.f000_complex2.grib2.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000_complex2.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000_complex2.grib2.idx

# Convert test file to another GRIB2 file with png packing.
../utils/cnvgrib -g22 -p41 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000_png.grib2

# Create an index of the new GRIB2 file.
../utils/grb2index 1 test_gdaswave.t00z.wcoast.0p16.f000_png.grib2 test_gdaswave.t00z.wcoast.0p16.f000_png.grib2.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000_png.grib2.idx data/ref_gdaswave.t00z.wcoast.0p16.f000_png.grib2.idx

echo "*** SUCCESS!"
exit 0
