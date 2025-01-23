#!/bin/sh
# This is a test script for the NCEPLIBS-g2 project.
#
# This tests the copygb2 utility.
#
# Ed Hartnett, 1/23/25

set -e
echo ""
echo "*** Running copygb2 test"

# Copy GRIB2 file.
../utils/copygb2 -x data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave_2.grib2

echo "*** SUCCESS!"
exit 0
