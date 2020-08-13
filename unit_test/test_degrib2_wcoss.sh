#!/bin/sh
#                            DEGRIB2 
#  This script uses to test utility degrib2.  The degrib2 shows the content of GRIB2 file
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
    module load EnvVars/1.0.2
    module load ips/18.0.1.163
    module load prod_util/1.1.3
    module load prod_envir/1.0.3
    module unload grib_util
#   module use -a /usrx/local/nceplibs/dev/NCEPLIBS/modulefiles
    module load grib_util/${ver}
    input_file=/usrx/local/nceplibs/dev/lib/fv3gfs
elif [ "$machine" = "Cray" ]; then
    module unload grib_util
#   module use -a /usrx/local/nceplibs/NCEPLIBS/modulefiles
    module load grib_util/${ver}
    input_file=/usrx/local/nceplibs/gfs_data
elif [ "$machine" = "hera" ]; then
    module unload grib_util
fi

#
# These two executable (cnvgrib and degrib2) files (below)
# have comipled with G2 v3.2.0 and W3NCO v2.2.0 and W3EMC v2.4.0
#
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
# Step 1: DEGRIB2 generates inventory file for $file file.
#
echo ""
echo "${count}. - Testing NEW degrib2 with $file file at forecast hour $fhr "
echo ""
echo "Please wait ... DEGRIB2 is generating the inventory for $file file "
echo " "
# set -x
$degrib2_test $data/$file >  $output_g2/${file}_test.inv 
set +x
echo
#
# You can see the content of grib2 file 
# vi  $output_g2/${file}_test.inv
#
echo " "
grep "Total Number of Fields Found"  $output_g2/${file}_test.inv 
echo " "

export numrec=` $WGRIB2 -s $data/$file | wc -l `
echo " "
echo "  --- PASS  ---- "
echo "  Total Number of records in ${file} file is : $numrec "
echo " "
done
done
