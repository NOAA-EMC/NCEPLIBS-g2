# *** manually set environments (for intel compiler) of g2 ***

 : ${USERMODE:=false}  # user mode (USERMODE) is closed by default
                       # set env var USERMODE to "true" to active it
 ${USERMODE} && {
    echo "Environment set by user"
# On theia/cray, user can load environment
#   module load intel/18.0.1.163
# Or set environment on specific platform
    intel_version=2018.1.163
    intel_topdir=/apps/intel/compilers_and_libraries_$intel_version
    source $intel_topdir/linux/bin/compilervars.sh intel64
 }

 ANCHORDIR=..
 export COMP=ips
 export G2_VER=v3.1.0
 export G2_SRC=
 export G2_INC4=$ANCHORDIR/include/g2_${G2_VER}_4
 export G2_INCd=$ANCHORDIR/include/g2_${G2_VER}_d
 export G2_LIB4=$ANCHORDIR/libg2_${G2_VER}_4.a
 export G2_LIBd=$ANCHORDIR/libg2_${G2_VER}_d.a

#JPZlib=/nwprod2/lib                                #WCOSS
 JPZlib=/scratch3/NCEPDEV/nwprod/lib                #THEIA
 export JASPER_VER=v1.900.1
 export JASPER_INC=$JPZlib/jasper/v1.900.1/include
 export PNG_VER=v1.2.44
#export PNG_INC=$JPZlib/png/v1.2.44/include         #WCOSS
 export PNG_INC=$JPZlib/png/v1.2.44/src/include     #THEIA
 export Z_VER=v1.2.6
 export Z_INC=$JPZlib/z/v1.2.6/include

 export CC=icc
 export FC=ifort
 export CPP=cpp
 export OMPCC="$CC -qopenmp"
 export OMPFC="$FC -qopenmp"
 export MPICC=mpiicc
 export MPIFC=mpiifort

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -fPIC"
 export FFLAGS="-O3 -fPIC"
 export FPPCPP="-cpp"
 export FREEFORM="-free"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -fPIC"
 export MPIFFLAGS="-O3 -xHOST -traceback -fPIC"
 export MODPATH="-module "
 export I4R4="-integer-size 32 -real-size 32"
 export I4R8="-integer-size 32 -real-size 64"
 export I8R8="-integer-size 64 -real-size 64"

 export CPPDEFS=""
 export CFLAGSDEFS="-I${PNG_INC} -I${JASPER_INC} -I${Z_INC} -DUNDERSCORE -DLINUX -DUSE_JPEG2000 -DUSE_PNG -D__64BIT__"
 export FFLAGSDEFS=""

 export USECC="YES"
 export USEFC="YES"
 export DEPS="JASPER $JASPER_VER, LIBPNG $PNG_VER, ZLIB $Z_VER"
