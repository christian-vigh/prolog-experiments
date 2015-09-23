#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([PrologDoc], [1.0RC2], [prologdoc-devel@lists.sourceforge.net])

# paths

AC_ARG_WITH(pldir,
	[  --with-pldir=path       Set installation directory for prolog modules ],
	[ PLDIR="$withval"], [PLDIR="$libdir/prologdoc/prolog"])
AC_SUBST(PLDIR)

AC_ARG_WITH(cssdir,
	[  --with-cssdir=path      Set installation directory for CSS style sheets ],
	[ CSSDIR="$withval"], [CSSDIR="$libdir/prologdoc/css"])
AC_SUBST(CSSDIR)


# Checks for programs.

AC_PROG_INSTALL

AC_SUBST(SWIPL)
AC_ARG_WITH(swipl,
	[  --with-swipl=path       Set location of SWI-Prolog executable ],
	[ SWIPL="$withval"], [SWIPL=])
if test -z "$SWIPL"; then
AC_PATH_PROGS([SWIPL], swipl pl)
fi
if test -z "$SWIPL"; then AC_MSG_ERROR([SWI-Prolog binary not found]); fi

AC_SUBST(PERL)
AC_ARG_WITH(perl,
	[  --with-perl=path        Set location of Perl executable ],
        [ PERL="$withval"], [PERL=])
if test -z "$PERL"; then
AC_PATH_PROG([PERL], perl)
fi
if test -z "$PERL"; then AC_MSG_ERROR([Perl binary not found]); fi


AC_CONFIG_FILES([pldoc Makefile])
AC_OUTPUT
