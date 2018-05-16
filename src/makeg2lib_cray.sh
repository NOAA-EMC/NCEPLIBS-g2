#!/bin/sh

export version=v3.1.0

case ${COMP:?} in
  intel)
    export FC=${1:-ftn}
    export CC=${2:-cc}
    export CPP=${3:-cpp -p} 
    export flagOpt="-O3 -axCore-AVX2 -g"
    export flag64flt="-r8"
    export flagFort="-assume noold_ldout_format -module"
  ;;
  cray)
    export FC=${1:-ftn}
    export CC=${2:-cc}
    export CPP=${3:-CC}
    export flagOpt="-O2 -G2"
    export flag64flt="-s real64"
    export flagFort="-J"
  ;;
  *)
    >&2 echo "Don't know how to build lib under $COMP compiler"
    exit 1
  ;;
esac

export LIB=../${COMP}/libg2_${version}_4.a
export MODDIR=../${COMP}/include/g2_${version}_4
mkdir -p $(dirname $LIB) $MODDIR
make -f makefile_4_cray

export LIB=../${COMP}/libg2_${version}_d.a
export MODDIR=../${COMP}/include/g2_${version}_d
mkdir -p $(dirname $LIB) $MODDIR
make -f makefile_d_cray
