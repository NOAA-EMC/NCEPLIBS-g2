#!/bin/bash
###################################################
## This scripts uses to compile grib utility cnvgrib 
## for testing W3NCO, G2 or W3EMC library
## on DELL or CRAY or Hera
###################################################
set -x
SHELL=/bin/sh

set +x

echo ""
echo "========================================"
echo "=    Compile CNVGRIB  and DEGRIB2      ="
echo "========================================"
echo ""
echo ""

export dir=`pwd`
set -x

mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)
if [ $mac = v -o $mac = m  ] ; then   # For Dell
   machine=dell
   module load jasper/1.900.29
   module load libpng/1.2.59
   module load zlib/1.2.11
   module load bacio/2.0.2
#
#  If the G2 v3.2.0, W3EMC v2.4.0 and W3NCO v2.2.0 libraries are not available
#  You can set these variables W3NCO_LIB4, W3EMC_LIB4, G2_INC4, and G2_LIB4
#  to the path of your G2, W3EMC and W3NCO libraries
#
module use -a /gpfs/dell1/nco/ops/nwtest/modulefiles/compiler_prod/ips/18.0.1
module load g2/3.2.0
module load w3nco/2.2.0
#
#  These varibales are for W3EMC v2.4.0 library for testing on $machine
#  You can set these variables to the path of your W3EMC library
#
export W3EMC_LIB4=/gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/w240/myw3emc/lib/libw3emc_v2.4.0_4.a
export W3EMC_INC4=/gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/w240/myw3emc/w3emc_v2.4.0_4

  module list
elif [ $mac = l -o $mac = s ] ; then   #    wcoss_c (i.e. luna and surge)
  machine=cray
  module load intel/18.1.163
#
#  If the G2 v3.2.0, W3EMC v2.4.0 and W3NCO v2.2.0 libraries are not available
#  You can set these variables W3NCO_LIB4, W3EMC_LIB4, G2_INC4, and G2_LIB4
#  to the path of your G2, W3EMC and W3NCO libraries
#
module use -a /gpfs/hps/nco/ops/nwtest/lib/modulefiles
# (Note: These modules below are in nwtest for testing)
module load ip-intel/3.0.2
module load bacio-intel/2.0.3
module load w3nco-intel/2.2.0
module load g2-intel/3.2.0
module use -a /usrx/local/nceplibs/NCEPLIBS/modulefiles
module load sp-intel/2.0.3
#
#  These varibales are for W3EMC v2.4.0 library for testing on $machine
#  You can set these variables to the path of your W3EMC library 
#
export W3EMC_LIB4=/gpfs/hps3/emc/global/noscrub/Boi.Vuong/n/NCEPLIBS-w3emc/build/myw3emc/lib/libw3emc_v2.4.0_4.a
export W3EMC_INC4=/gpfs/hps3/emc/global/noscrub/Boi.Vuong/n/NCEPLIBS-w3emc/build/myw3emc/include_4

elif [ $mac2 = hf ] ; then
  machine=hera
fi

export CC=icc
export CXX=icpc
export FC=ifort
module list
module list > build_cnvgrib_degrib2_${machine}.log 2>&1

make -f makefile_cnvgrib_${machine}
make -f makefile_degrib2_${machine}
