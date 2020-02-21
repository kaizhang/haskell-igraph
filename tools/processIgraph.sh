wd=`pwd`
cd $1

# Configuration
cat <<EOT > include/config.h
/* functions */
#define HAVE_EXPM1 1
#define HAVE_FABSL 1
#define HAVE_FINITE 1
#define HAVE_FMIN 1
#define HAVE_FTRUNCATE 1
#define HAVE_ISNAN 1
#define HAVE_LOG1P 1
#define HAVE_LOG2 1
#define HAVE_RINT 1
#define HAVE_RINTF 1
#define HAVE_ROUND 1
#define HAVE_SNPRINTF 1
#undef HAVE_ISFINITE

/* libraries */
#define HAVE_MEMORY_H 1
#define HAVE_STDINT_H 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1

#define IGRAPH_F77_SAVE static IGRAPH_THREAD_LOCAL
#define IGRAPH_THREAD_LOCAL 

#define INTERNAL_ARPACK 1
#define INTERNAL_BLAS 1
#define INTERNAL_F2C 1
#define INTERNAL_GLPK 1
#define INTERNAL_LAPACK 1

#define LT_OBJDIR ".libs/"
#define PACKAGE "igraph"
#define PACKAGE_BUGREPORT "igraph@igraph.org"
#define PACKAGE_NAME "igraph"
#define PACKAGE_STRING "igraph 0.8.0"
#define PACKAGE_TARNAME "igraph"
#define PACKAGE_URL ""
#define PACKAGE_VERSION "0.8.0"
#define STDC_HEADERS 1
#define VERSION "0.8.0"
#undef YYTEXT_POINTER
EOT

mv src/*.h include
mv src/*.pmt include

for dir in f2c bliss prpack lapack cs cliquer plfit
do
    mkdir include/$dir
    mv src/$dir/*.h include/$dir
    mv src/$dir/*.hh include/$dir
    mv src/$dir/*.c src
    mv src/$dir/*.cpp src
    mv src/$dir/*.cc src
done

cat <<EOT > include/arith.h
#define IEEE_8087
#define Arith_Kind_ASL 1
#define Long int
#define Intcast (int)(long)
#define Double_Align
#define X64_bit_pointers
#define NANCHECK
#define QNaN0 0x0
#define QNaN1 0xfff80000
EOT

cd $wd
mkdir -p $2/include
mkdir -p $2/src
mv $1/include/* $2/include
mv $1/src/*.c $2/src
mv $1/src/*.cpp $2/src
mv $1/src/*.cc $2/src

ls -d1 $2/src/*.c
ls -d1 $2/src/*.cpp
ls -d1 $2/src/*.cc

#define HAVE_STDARG_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STPCPY 1
#define HAVE_STPCPY_SIGNATURE 1
#define HAVE_STRCASECMP 1
#define HAVE_STRDUP 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_TIMES_H 1
#define HAVE_TIME_H 1
#define HAVE_UNISTD_H 1
#define HAVE_GMP 1