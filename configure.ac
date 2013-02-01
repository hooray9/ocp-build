AC_INIT(typerex,1.99.4-beta)
CONFIGURE_ARGS=$*

AC_COPYRIGHT(Copyright 2012 OcamlPro SAS)
AC_CONFIG_MACRO_DIR([m4])


AC_PROG_OCAML
AC_PROG_CAMLP4
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_CC

AC_ARG_ENABLE(version-check,
 [  --disable-version-check            do not check OCaml version],
 [VERSION_CHECK="$enableval"],
 [VERSION_CHECK="yes"])

AX_SPLIT_VERSION(OCAMLVERSION)
OCAML_MAJOR_VERSION=$AX_MAJOR_VERSION
OCAML_MINOR_VERSION=$AX_MINOR_VERSION
OCAML_POINT_VERSION=$AX_POINT_VERSION

# Check that OCaml version is greater or equal to 4.00.0
if test "$VERSION_CHECK" = "yes" ; then
   AX_COMPARE_VERSION( [$OCAMLVERSION], [lt], [3.12.1],
     AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported]))
fi

if test "x$prefix" = "xNONE"; then
#  prefix=$ac_default_prefix
  prefix="`eval dirname $OCAMLLIB`"
  libdir=$OCAMLLIB
  base="`eval basename $prefix`"
  echo base=$base
  if test "x$base" = "xlib"; then
    prefix="`eval dirname $prefix`"
  fi
else
  libdir=${libdir}/ocaml
fi
test "x$exec_prefix" = xNONE && exec_prefix='${prefix}'

libdir=${libdir}/typerex

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
mandir="`eval echo ${mandir}`"
libdir="`eval echo ${libdir}`"
libdir="`eval echo ${libdir}`"
datarootdir="`eval echo ${datarootdir}`"
datarootdir="`eval echo ${datarootdir}`"
datadir=${datarootdir}/typerex

AC_SUBST(OCAML_MAJOR_VERSION)
AC_SUBST(OCAML_MINOR_VERSION)
AC_SUBST(OCAML_POINT_VERSION)
AC_SUBST(CONFIGURE_ARGS)
AC_SUBST(VERSION)
AC_CONFIG_FILES(
  Makefile.config \
  typerex.ocp \
  packages/opam/ocp-build.opam
)
AC_OUTPUT

echo Creating file ocp-build.root
touch ocp-build.root

echo
echo Executables will be installed in ${bindir}
echo Libraries will be installed in ${libdir}
echo Data files will be installed in ${datadir}
echo Manual pages will be installed in ${mandir}
