#!/bin/sh
# This is a test script for the NCEPLIBS-g2 project.
#
# This tests the copygb2 utility.
#
# Ed Hartnett, 1/23/25

set -e
echo ""
echo "*** Running copygb2 test"

# Invalid option.
../utils/copygb2 - && exit 1

# Incorrect number of arguments.
../utils/copygb2 -g data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 run_copygb2_tests_wcoast.grib2 && exit 1

# File missing.
../utils/copygb2 -x data/missing.grib2 && exit 1

# Wrong number of arguments.
../utils/copygb2 -x data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave_2.grib2 data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave_2.grib2 && exit 1


# Use -g option.
#../utils/copygb2 -g kpdtn data/ref_gdaswave.t00z.wcoast.0p16.f000.grib2 run_copygb2_tests_wcoast.grib2


echo "*** SUCCESS!"
exit 0
