AC_INIT([BIOME4], [5.0.0], [jed.kaplan@arve-research.com])

AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR(.)

AM_INIT_AUTOMAKE([foreign])

LT_INIT
AC_CONFIG_MACRO_DIR([m4])

AC_PREFIX_DEFAULT(["`pwd`"])

# ------------------------------------------------------------------------
# check for netCDF (this is required for the program to run)

PKG_CHECK_MODULES([NETCDF], [netcdf-fortran])

PKG_CHECK_VAR([NETCDF_CC],      [netcdf-fortran], [ccompiler])
PKG_CHECK_VAR([NETCDF_FC],      [netcdf-fortran], [fcompiler])
PKG_CHECK_VAR([NETCDF_FCFLAGS], [netcdf-fortran], [includedir])

# ------------------------------------------------------------------------
# Check for the C and Fortran compilers. NOTE that this must come AFTER the netcdf check. 
# The netcdf libraries could have been compiled with, for example, Intel C and ifort;
# we have to specifically check if the same compiler used to compile the
# netcdf library exists.
# ------------------------------------------------------------------------

AC_PROG_CC( $NETCDF_CC )
AC_PROG_FC( $NETCDF_FC )
AC_PROG_F77( $NETCDF_FC )
AC_LANG([Fortran])
AC_FC_SRCEXT(f90)

# ----

# get the real name of the compiler

FC_REALNAME=$($NETCDF_FC -show | awk '{print $1}')

# set the linker flags

LDFLAGS="$NETCDF_LIBS $LDFLAGS"

CPPFLAGS="-Dgfortran $CPPFLAGS"

# -----------------------------------------------------------------------
# set a debugging mode - apparently this is not good usage of autotools
# but this is not production/distribution software and so being able to
# easily switch on and off a debugging mode will be useful for students 

test -z "$SED" && SED=sed

AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
                  [whether to include debug symbols (default is no)])],
  [enable_debug=$enableval],
  [enable_debug=no]
)

if test "x$enable_debug" = xyes; then

  echo "setting debugging flags for $FC_REALNAME"

  dnl strip optimization flags from FCFLAGS

  changequote({,})

  FCFLAGS=`echo "$FCFLAGS" | $SED -e 's/-O[0-9s]*//g'`

  changequote([,])
  
  if test "x$FC_REALNAME" = xifort; then
    
    CPPFLAGS="$CPPFLAGS -Difort"
    FCFLAGS="$FCFLAGS -debug -fpe0 -traceback -g -check -check noarg_temp_created -warn all"

  elif test "x$FC_REALNAME" = xgfortran; then
  
    CPPFLAGS="$CPPFLAGS -Dgfortran"
    FCFLAGS="$FCFLAGS -ffree-line-length-none -finit-local-zero -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow -g -fbacktrace -Wall -pedantic"

  else 

    dnl  generic debug flags

    FCFLAGS="$FCFLAGS -g -O0"
  fi
  
else

# set the preprocessor and compiler flags

  if test "x$FC_REALNAME" = xifort; then
    
    CPPFLAGS="-Difort $CPPFLAGS"
  
  elif test "x$FC_REALNAME" = xgfortran; then
  
    FCFLAGS="-O2 -ftree-vectorize -march=native -fno-math-errno -fPIC -ffree-line-length-none -fopenmp"
  
  fi
  
fi

FCFLAGS="$FCFLAGS -I$NETCDF_FCFLAGS"
FFLAGS="$FCFLAGS"

# -----------------------------------------------------------------------

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

echo "summary of build options"
echo "C compiler:             $NETCDF_CC"
echo "Fortran compiler:       $NETCDF_FC"
echo "preprocessor flags:     $CPPFLAGS"
echo "Fortran compiler flags: $FCFLAGS"
echo "Linker flags:           $LDFLAGS"
