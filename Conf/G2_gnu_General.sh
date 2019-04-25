# *** manually set environments (for gnu compiler) of g2 ***

# !!! module environment (*THEIA*) !!!
 module load gcc/6.2.0

 ANCHORDIR=..
 export COMP=gnu
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

 export CC=gcc
 export FC=gfortran
 export CPP=cpp
 export OMPCC="$CC -fopenmp"
 export OMPFC="$FC -fopenmp"
 export MPICC=mpigcc
 export MPIFC=mpigfortran

 INCS="-I${PNG_INC} -I${JASPER_INC} -I${Z_INC}"

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -DUNDERSCORE -DLINUX ${INCS} -D__64BIT__ -fPIC"
 export FFLAGS="-O3 -fPIC"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export MPIFFLAGS="-O3 -fPIC"
 export MODPATH="-J"
 export I4R4=""
 export I4R8="-fdefault-real-8"
 export I8R8="-fdefault-integer-8 -fdefault-real-8"

 export CPPDEFS=""
 export CFLAGSDEFS=""
 export FFLAGSDEFS="-fno-range-check"

 export USECC="YES"
 export USEFC="YES"
 export DEPS="JASPER $JASPER_VER, LIBPNG $PNG_VER, ZLIB $Z_VER"
