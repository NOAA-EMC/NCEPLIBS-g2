#!/bin/bash

 (( $# == 0 )) && {
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [[local]install[only]]"
   exit 1
 }

 sys=${1,,}
 [[ $sys == wcoss || $sys == dell || $sys == cray ||\
    $sys == theia || $sys == intel_general || $sys == gnu_general ]] || {
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [[local]install[only]]"
   exit 1
 }
 debg=false
 inst=false
 skip=false
 local=false
 (( $# > 1 )) && {
   [[ ${2,,} == build ]] && debg=false
   [[ ${2,,} == debug ]] && debg=true
   [[ ${2,,} == install ]] && inst=true
   [[ ${2,,} == localinstall ]] && { local=true; inst=true; }
   [[ ${2,,} == installonly ]] && { inst=true; skip=true; }
   [[ ${2,,} == localinstallonly ]] && { local=true; inst=true; skip=true; }
 }
 (( $# > 2 )) && {
   [[ ${3,,} == build ]] && debg=false
   [[ ${3,,} == debug ]] && debg=true
   [[ ${3,,} == install ]] && inst=true
   [[ ${3,,} == localinstall ]] && { local=true; inst=true; }
   [[ ${3,,} == installonly ]] && { inst=true; skip=true; }
   [[ ${3,,} == localinstallonly ]] && { local=true; inst=true; skip=true; }
 }
 if [[ ${sys} == "intel_general" ]]; then
   sys6=${sys:6}
   source ./Conf/G2_${sys:0:5}_${sys6^}.sh
 elif [[ ${sys} == "gnu_general" ]]; then
   sys4=${sys:4}
   source ./Conf/G2_${sys:0:3}_${sys4^}.sh
 else
   source ./Conf/G2_intel_${sys^}.sh
 fi
 [[ -z $G2_VER || -z $G2_LIB4 ]] && {
   echo "??? G2: module/environment not set."
   exit 1
 }

 source ./Conf/Collect_info.sh
 source ./Conf/Gen_cfunction.sh

set -x
 g2Lib4=$(basename $G2_LIB4)
 g2Libd=$(basename $G2_LIBd)
 g2Inc4=$(basename $G2_INC4)
 g2Incd=$(basename $G2_INCd)

#################
 cd src
#################

 $skip || {
#-------------------------------------------------------------------
# Start building libraries
#
 echo
 echo "   ... build (i4/r4) g2 library ..."
 echo
   make clean LIB=$g2Lib4 MOD=$g2Inc4
   mkdir -p $g2Inc4
   FFLAGS4="$I4R4 $FFLAGS ${MODPATH}$g2Inc4"
   collect_info g2 4 OneLine4 LibInfo4
   g2Info4=g2_info_and_log4.txt
   $debg && make debug FFLAGS="$FFLAGS4" LIB=$g2Lib4 &> $g2Info4 \
         || make build FFLAGS="$FFLAGS4" LIB=$g2Lib4 &> $g2Info4
   make message MSGSRC="$(gen_cfunction $g2Info4 OneLine4 LibInfo4)" LIB=$g2Lib4

 echo
 echo "   ... build (i4/r8) g2 library ..."
 echo
   make clean LIB=$g2Libd MOD=$g2Incd
   mkdir -p $g2Incd
   FFLAGSd="$I4R8 $FFLAGS ${MODPATH}$g2Incd"
   collect_info g2 d OneLined LibInfod
   g2Infod=g2_info_and_logd.txt
   $debg && make debug FFLAGS="$FFLAGSd" LIB=$g2Libd &> $g2Infod \
         || make build FFLAGS="$FFLAGSd" LIB=$g2Libd &> $g2Infod
   make message MSGSRC="$(gen_cfunction $g2Infod OneLined LibInfod)" LIB=$g2Libd
 }

 $inst && {
#
#     Install libraries and source files
#
   $local && {
              LIB_DIR4=..
              LIB_DIRd=..
             } || {
                   LIB_DIR4=$(dirname ${G2_LIB4})
                   LIB_DIRd=$(dirname ${G2_LIBd})
                  }
   [ -d $LIB_DIR4 ] || mkdir -p $LIB_DIR4
   [ -d $LIB_DIRd ] || mkdir -p $LIB_DIRd
   INCP_DIR4=$(dirname $G2_INC4)
   [ -d $G2_INC4 ] && rm -rf $G2_INC4 || mkdir -p $INCP_DIR4
   INCP_DIRd=$(dirname $G2_INCd)
   [ -d $G2_INCd ] && rm -rf $G2_INCd || mkdir -p $INCP_DIRd
   SRC_DIR=$G2_SRC
   $local && SRC_DIR=
   [ -d $SRC_DIR ] || mkdir -p $SRC_DIR
   make clean LIB=
   make install LIB=$g2Lib4 MOD=$g2Inc4 \
                LIB_DIR=$LIB_DIR4 INC_DIR=$INCP_DIR4 SRC_DIR=
   make install LIB=$g2Libd MOD=$g2Incd \
                LIB_DIR=$LIB_DIRd INC_DIR=$INCP_DIRd SRC_DIR=$SRC_DIR
 }
