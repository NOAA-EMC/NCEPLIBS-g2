#!/bin/sh

export version=3.1.0

mkdir -p ../include/g2_v${version}_4
mkdir -p ../include/g2_v${version}_d

make -f makefile_4_wcoss
make -f makefile_d_wcoss

#
#  Build unit_test
#
echo " "
echo " "

cd ../unit_test
mkdir -p ../exec
make -f Makefile_wcoss
