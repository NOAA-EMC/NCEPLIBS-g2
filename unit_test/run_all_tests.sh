#!/bin/sh

for test_name in cnvgrib_and_cnvgrib21_gfs cnvgrib_prod_and_new copygb2_prod_and_new; do
    output_dir=${test_name}_output
    rm -rf $output_dir
    echo -n "Running test ${test_name}..."
    mkdir $output_dir
    ./test_${test_name}_wcoss.sh &>${output_dir}/${test_name}.log
    err=$?
    echo " done"
    if [ $err -ne 0 ]; then
        >&2 echo "WARNING: ONE OR MORE ERRORS WERE REPORTED!"
    fi
    mv data output_g1 output_g2 $output_dir/ 
    echo "  --> Please view $output_dir/$test_name.log"
done

