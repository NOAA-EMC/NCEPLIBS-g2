#!/bin/sh
set -eux
#-----------------------------------------------------
cwd=`pwd`
source ../../../machine-setup.sh > /dev/null 2>&1
module use ../../modulefiles
module load jasper/v1.900.1
module load png/v1.2.44
module load z/v1.2.6
module load w3nco/v2.0.6

export LIBNAME=g2
export VER=v3.1.0
export FC=ifort
export CC=icc

if [ $target = wcoss_cray ]; then
  export FC=ftn
  export CC=cc
  module load PrgEnv-intel
  module load craype-sandybridge
  flagOpt="-axCore-AVX2"
elif [ $target = "wcoss" ]; then
  module load ics
  flagOpt=""
elif [ $target = "theia" ]; then
  module load intel/14.0.2
  flagOpt=""
elif [ $target = "gaea" ]; then
  flagOpt=""
elif [ $target = "jet" ]; then
  flagOpt=""
  module load intel/12.1.4
fi

#
#     Build g2 library
#

mkdir -p ../../libs/${LIBNAME}_${VER}

export LIB=../../libs/${LIBNAME}_${VER}/libg2_${VER}_4.a
export MODDIR=../../incmod/g2_${VER}_4
export flagFlt=""
rm -rf $MODDIR $LIB
mkdir -p $MODDIR
make

export LIB=../../libs/${LIBNAME}_${VER}/libg2_${VER}_d.a
export MODDIR=../../incmod/g2_${VER}_d
export flagFlt="-r8 -i4"
rm -rf $MODDIR $LIB
mkdir -p $MODDIR
make

#
#     Create modulefile
#
cd ../../
lwd=`pwd`
cd $cwd
mkdir -p $lwd/modulefiles/$LIBNAME
cat modulefile.template | sed s:_CWD_:$lwd:g > $lwd/modulefiles/$LIBNAME/$VER

exit
