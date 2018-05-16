#!/bin/sh

export version=v3.1.0

mkdir -p ../include/g2_${version}_4
mkdir -p ../include/g2_${version}_d

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
