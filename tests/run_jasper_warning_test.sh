#!/bin/sh
# This is a test script for the NCEPLIBS-g2 project.
#
# This test checks that the output of test_jpcpack_4 has not
# changed. This is a test that involves jasper, and jasper sometimes
# starts issuing warnings. This test ensures that jasper warnings are
# detected automatically, since we are not running our tests
# manually (of course). 
#
# Ed Hartnett, 9/15/23

set -e
echo ""
echo "*** Running jasper warning test"
set -x

# Run the test and capture output.
pwd
ls -l
#ls -l test_jpcpack_4
./test_jpcpack_4 &> test_jpcpack_4_output.txt
cat test_jpcpack_4_output.txt

# Check against expected output.
diff -w test_jpcpack_4_output.txt data/ref_test_jpcpack_4_output.txt

set -x
echo "*** SUCCESS!"
exit 0
