#!/bin/sh
#                            CNVGRIB 
#  This script uses to test the utility cnvgrib.
#  The cnvgrib will correct the forecast hour > F252 when it converts
#  from GRIB2 to GRIB1 for contnuous accumulation precipitation (ACPCP) and APCP
# 
#  Then, the WGRIB use to display content (inventory) of grib1 file for comparison.
#  The input are GRIB2 file.   The GRIB2 file can be in any model (i.e., GFS, NAM, HRRR, RTMA, ...)
#

export ver=1.1.1
export cyc=00
export fhr=264

mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)
if [ $mac = v -o $mac = m  ] ; then   # For Dell
   machine=dell
   echo " "
   echo " You are on WCOSS :  ${machine}"
elif [ $mac = l -o $mac = s ] ; then   #    wcoss_c (i.e. luna and surge)
   machine=cray
   echo " "
   echo " You are on WCOSS :  ${machine}"
elif [ $mac2 = hf ] ; then
   machine=hera
   echo " You are on RDHPCS :  ${machine}"
else
   echo " "
   echo " Your machine is $machine NOT found "
   echo " The script $0 can not continue.  Aborted ! "
   echo " "
   echo " Your machine must be CRAY (SURGE/LUNA)"
   echo " or DELL (MARS/VENUS) or HERA "
   echo " "
   exit
fi

#  
# If you want change temporary directories,
# you can change variable "dir"
#
#  Setup working directories
#
export dir=` pwd `
export data=$dir/data
output_g1=$dir/output_g1
output_g2=$dir/output_g2
mkdir -p $data $output_g1 $output_g2

if [ "$machine" = "dell" ]; then
    module unload grib_util
#   module use -a /usrx/local/nceplibs/dev/NCEPLIBS/modulefiles
    module load grib_util/${ver}
    input_file=/usrx/local/nceplibs/dev/lib/fv3gfs
elif [ "$machine" = "cray" ]; then
    module unload gcc
    module unload grib_util
    module load gcc/6.3.1
#   module use -a /usrx/local/nceplibs/NCEPLIBS/modulefiles
    module load grib_util/${ver}
    input_file=/usrx/local/nceplibs/gfs_data
elif [ "$machine" = "hera" ]; then
    module unload grib_util
fi

#
# These two executable (cnvgrib and degrib2) files (below)
#
cnvgrib_test=$dir/sorc/cnvgrib
degrib2_test=$dir/sorc/degrib2
echo " "
module list
echo " "

#
#  Clean up temp directory before test starts
#
if [ "$(ls -A $output_g1)" ]; then
   echo "Cleaning $output_g1"
   rm $output_g1/*
fi
if [ "$(ls -A $output_g2)" ]; then
   echo "Cleaning $output_g2"
   rm $output_g2/*
fi
if [ "$(ls -A $data)" ]; then
   echo "Cleaning $data"
   rm $data/*
fi

#
#  Find out if working directory exists or not  
#
if [ ! -d  $data ] ; then
    echo " "
    echo " Your working directory $data NOT found "
    echo " "
    exit 1
fi

for fhr in 012 264
do
if [ -f $input_file/gfs.t${cyc}z.pgrb2.0p25.f${fhr} ] ; then
   cp $input_file/gfs.t${cyc}z.pgrb2.0p25.f${fhr} $dir/data
else
   echo " " 
   echo " " 
   echo "GRIB2 File $input_file/gfs.t${cyc}z.pgrb2.0p25.f${fhr} Does Not Exist." 
   echo " " 
   echo " No input GRIB2 file to continue " 
   echo " " 
   echo " "
   exit 1
fi

filelist=` ls -1  $dir/data `
err=0
export count=0

for file in $filelist
do
export count=` expr $count + 1 `
#
# Step 1: CNVGRIB converts from GRIB2 to GRIB1
#
echo ""
echo "${count}. - Testing NEW cnvgrib with $file file at forecast hour $fhr "
echo ""
echo "Please wait ... CNVGRIB is converting from GRIB2 to GRIB1 "
echo ""
# set -x
$cnvgrib_test -g21 $data/$file  $output_g1/$file.grib2.cnvgrib.g1_test
# $CNVGRIB -g21 $data/$file  $output_g1/$file.grib2.cnvgrib.g1_prod
set +x
echo

echo "Run wgrib on GRIB1 file "
# set -x
${WGRIB:?} -s $output_g1/$file.grib2.cnvgrib.g1_test  > $output_g1/$file.grib2.cnvgrib.g1_test.wgrib
# ${WGRIB:?} -s $output_g1/$file.grib2.cnvgrib.g1_prod  > $output_g1/$file.grib2.cnvgrib.g1_prod.wgrib
# diff $output_g1/$file.grib2.cnvgrib.g1_test.wgrib $output_g1/$file.grib2.cnvgrib.g1_prod.wgrib

${WGRIB:?} -s $output_g1/$file.grib2.cnvgrib.g1_test |grep ACPCP > $output_g1/$file.grib2.cnvgrib.g1_test_acpcp.wgrib
# ${WGRIB:?} -s $output_g1/$file.grib2.cnvgrib.g1_prod |grep ACPCP > $output_g1/$file.grib2.cnvgrib.g1_prod_acpcp.wgrib
echo " "

cat $output_g1/$file.grib2.cnvgrib.g1_test_acpcp.wgrib
# cat $output_g1/$file.grib2.cnvgrib.g1_prod_acpcp.wgrib

echo " ACPCP:kpds5=63:kpds6=1:kpds7=0:TR=4:P1=0:P2=8:TimeU=1:sfc:0-${fhr}hr acc:NAve=0  ---> CORRECTED for forecast hour ${fhr}hr"
echo " "
echo "  --- PASS  ---- "
echo "  CNVGRIB coded correctly for total continuous precipitation 6-hour bucket in grib1 file at forecast hour ${fhr}. "
echo " "
done
done
