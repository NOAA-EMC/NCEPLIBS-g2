#!/bin/sh

######################################################################
#
# This script uses to build G2 Library on WCOSS (CRAY and IBM) 
#
######################################################################
######################################################################

target=$1
export version=v3.0.0
export COMP=$1
export dir=` pwd `
export machine_type=$(hostname | cut -c1-1)
 
if [ $# -ne 1 ]; then
 set +x
 echo " "
 echo " "
 echo "  ################################################"
 echo "  #                                              #"
 echo "  #   Usage:                                     #"
 echo "  #                                              #"
 echo "  #         $0   wcoss          #"
 echo "  #      or                                      #"
 echo "  #                                              #"
 echo "  #         $0   intel          #"
 echo "  #                (craype-sandybridge)          #"
 echo "  #                                              #"
 echo "  #      or                                      #"
 echo "  #                                              #"
 echo "  #         $0   cray           #"
 echo "  #                  (craype-haswell)            #"
 echo "  #                                              #"
 echo "  ################################################"
 echo " "
 echo " "
 exit
fi
#
#    Checking for machine before installation start  ###
#
if [ $machine_type = t -o $machine_type = g ] ; then # For WCOSS
    machine=wcoss
elif  [ $machine_type = l -o $machine_type = s ] ; then # For CRAY
    machine=cray
else
   machine=other
   set +x
   echo "  "
   echo "  Your machine must be on WCOSS (Luna/Surge or Tide/Gyre) "
   echo "  "
   echo "  The G2 library only build on WCOSS or CRAY machine. Usage: "
   echo "  "
   echo "           $0   wcoss      "
   echo "  "
   echo "        or                 "
   echo "  "
   echo "           $0   intel      "
   echo "                   (craype-sandybridge)          "
   echo "  "
   echo "        or                 "
   echo "  "
   echo "           $0   cray       "
   echo "                  (craype-haswell)               "
   echo "  "
   echo "  "
   exit
fi

#
# Checking for argument and make sure you are on right machine to install G2 Library
#
if [ $target = "wcoss" ] && [ $machine != "wcoss" ] ; then
   set +x
   echo "  "
   echo "  Your machine is not WCOSS (TIDE or GYRE) "
   echo "  "
   echo "  The G2 library only builds on Tide/Gyre or Luna/Surge machine. Usage: "
   echo "  "
   echo "           $0   wcoss      "
   echo "  "
   echo "        or                 "
   echo "  "
   echo "           $0   intel      "
   echo "                  (craype-sandybridge)           "
   echo "  "
   echo "        or                 "
   echo "  "
   echo "           $0   cray       "
   echo "                  (craype-haswell)               "
   echo "  "
   echo "  "
   exit
elif [ $target = "intel" ] || [ $target = "cray"  ] && [ $machine != "cray" ] ; then
   set +x
   echo "  "
   echo "  Your machine is not CRAY (Luna or Surge) "
   echo "  "
   echo "  The G2 library only builds on Tide/Gyre or Luna/Surge machine. Usage: "
   echo "  "
   echo "           $0   wcoss      "
   echo "  "
   echo "        or                 "
   echo "  "
   echo "           $0   intel      "
   echo "                  (craype-sandybridge)          "
   echo "  "
   echo "        or                 "
   echo "  "
   echo "           $0   cray       "
   echo "                   (craype-haswell)             "
   echo "  "
   echo "  "
   exit
elif [ $target != "wcoss" ] && [ $target != "intel" ] && [ $target != "cray" ]; then
   set +x
   echo "  "
   echo "  "
   echo "  "
   echo "  $1  is invalid argument. Usage: "
   echo "  "
   echo "           $0   wcoss      "
   echo "  "
   echo "        or                 "
   echo "  "
   echo "           $0   intel      "
   echo "  "
   echo "        or                 "
   echo "  "
   echo "           $0   cray       "
   echo "  "
   echo "  "
   exit
fi

#
# Checking for G2 source code directory
#
cd $dir/src
export chkfile=$dir/src/gribmod.f

if  [  ! -f $chkfile ] ; then
    echo " "
    echo " "
    echo " #####  G2 library source code NOT found  #####"
    echo " "
    echo " The G2 installation can not continue.  Aborted ! "
    echo " "
    echo " "
    echo " Please change to g2/${version} on Tide or CRAY "
    echo "  Usage: "
    echo "  "
    echo "           $0   wcoss      "
    echo "  "
    echo "        or                 "
    echo "  "
    echo "           $0   intel      "
    echo "  "
    echo "        or                 "
    echo "  "
    echo "           $0   cray       "
    echo "  "
    echo "  "
    exit
fi

if [ $target = wcoss ]  ; then
   . /usrx/local/Modules/3.2.10/init/sh
   module load jasper/v1.900.1
   module load png/v1.2.44
   module load z/v1.2.6
   module load w3nco/v2.0.6
   module load ics
   echo " "
   module list 2>compile-g2-$target.log
   module list
   echo " "
   echo " PLEASE WAIT FOR WRITING to LOG file "
   echo " "
   echo " "
   ./makeg2lib_$target.sh &>>compile-g2-$target.log

elif [ $target = intel ] ; then
   module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
   module unload craype-haswell
   module load craype-sandybridge
   module load jasper-gnu-sandybridge/1.900.1
   module load png-intel-sandybridge/1.2.49
   module load zlib-intel-sandybridge/1.2.7
   module load PrgEnv-intel
   module load w3nco-intel/2.0.6
   echo " "
   module list 2>compile-g2-$target.log
   module list
   echo " "
   echo " PLEASE WAIT FOR WRITING to LOG file "
   echo " "
   echo " "
   ./makeg2lib_$target.sh &>>compile-g2-$target.log

elif [ $target = cray ] ; then
   module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
   module unload PrgEnv-intel
   module load   PrgEnv-cray
   module unload craype-sandybridge
   module load craype-haswell
   module unload jasper-gnu-sandybridge/1.900.1
   module load  jasper-gnu-haswell/1.900.1
   module unload png-intel-sandybridge/1.2.49
   module load   png-intel-haswell/1.2.49
   module unload zlib-intel-sandybridge/1.2.7
   module load  zlib-intel-haswell/1.2.7
   module load w3nco-cray-haswell/2.0.6
   echo " "
   module list 2>compile-g2-$target.log
   module list
   echo " "
   echo " PLEASE WAIT FOR WRITING to LOG file "
   echo " "
   echo " "
   ./makeg2lib_$target.sh &>>compile-g2-$target.log

else
   set +x
   echo "  "
   echo "  "
   echo "  Do not know how to build library on $1 "
   echo "  "
   echo "  Usage: "
   echo "  "
   echo "           $0   wcoss      "
   echo "  "
   echo "        or                 "
   echo "  "
   echo "           $0   intel      "
   echo "  "
   echo "        or                 "
   echo "  "
   echo "           $0   cray       "
   echo "  "
   echo "  "
   exit
fi

echo " "
echo " Installation completed. Please refer to log file for details."
echo " "
echo " "
