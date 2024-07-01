#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# This tests the grb2index function on a very large file downloaded
# via FTP.
#
# Ed Hartnett, 6/28/24

set -e
echo ""
echo "*** Running grb2index tests on large file"

# Create an index of a GRIB2 file.
../utils/grb2index 2 ../tests/data/fv3lam.t00z.prslev.f000.grib2 test_fv3lam.t00z.prslev.f000.grib2.grb2index

# Check against expected output. First 140 bytes contain differences,
# so ignore them.
#cmp -i 140 test_gdaswave.grb2index.idx data/ref_gdaswave.grb2index.idx

echo "*** SUCCESS!"
exit 0
