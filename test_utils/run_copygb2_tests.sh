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

echo "*** SUCCESS!"
exit 0
