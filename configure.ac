AC_INIT(config.pl.in)

if test -z "$IN_GIMP"; then
   echo
   echo "*** WOAH there! ***"
   echo Please do NOT call configure directly, rather run
   echo perl Makefile.PL
   echo as you would do with any other perl extension...
   echo "This time I'll be doing it for you!"
   echo
   perl Makefile.PL
   exit
fi

AC_PREFIX_DEFAULT($prefix)dnl from Makefile.PL

AC_CONFIG_HEADER(config.h)

AC_ARG_WITH(cflags,  [  --with-cflags=FLAGS     Set CFLAGS to FLAGS (optional)],
   CFLAGS="$with_cflags")

AC_ARG_WITH(ldflags, [  --with-ldflags=FLAGS    Set LDFLAGS to FLAGS (optional)],
   LDFLAGS="$with_ldflags")

AC_ARG_WITH(includes,[  --with-includes=DIR     Additionally search for includes in dir DIR (optional)],
   CPPFLAGS="$CPPFLAGS -I$with_includes")

AC_ARG_WITH(libs,    [  --with-libs=DIR         Additionally search for libraries in dir in DIR (optional)],
   LIBS="$LIBS -L$with_libs")

AC_PATH_PROGS(GIMP,gimp)

AM_PATH_GIMP_1_4(1.3.15,, [AC_MSG_ERROR([** unable to find gimp, make sure it's in your path (version 1.3.15+ required!)])])

AM_PATH_GLIB_2_0(2.0.0,, [AC_MSG_ERROR([** unable to find glib, make sure it's in your path (version 2.0+ required!)])])

AM_PATH_GTK_2_0(2.0.0,, [AC_MSG_ERROR([** unable to find gtk+, make sure it's in your path (version 2.0+ required!)])])

ac_gimp_save_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $GIMP_CFLAGS"
AC_EGREP_CPP(DIVIDE_MODE,[#include <libgimp/gimp.h>],AC_DEFINE(HAVE_DIVIDE_MODE))
AC_CHECK_HEADERS(libgimp/gimpmodule.h libintl.h)
CPPFLAGS="$ac_gimp_save_CPPFLAGS"

AC_CHECK_HEADERS(unistd.h)

CONFIG_H="config.h"

sinclude(configure.frag)

INTLLIBS=
MSGFMT=
AC_SUBST(INTLLIBS)
AC_SUBST(MSGFMT)

AC_OUTPUT(config.pl)

echo "now invoking perl to complete the configuration..."
exec $PERL $MAKEFILE_PL --writemakefile PREFIX="$prefix"

