#!/bin/sh
#
# IPhreeqcCOM package build script
#
# $Id: build.sh 4133 2010-02-24 05:50:31Z charlton $
#
# Package maintainers: if the original source is not distributed as a
# (possibly compressed) tarball, set the value of ${src_orig_pkg_name},
# and redefine the unpack() helper function appropriately.
# Also, if the Makefile rule to run the test suite is not "test", change
# the definition of ${test_rule} below.

# echo everything
set -x

# find out where the build script is located
tdir=`echo "$0" | sed 's%[\\/][^\\/][^\\/]*$%%'`
test "x$tdir" = "x$0" && tdir=.
scriptdir=`cd $tdir; pwd`
# find src directory.
# If scriptdir ends in SPECS, then topdir is $scriptdir/..
# If scriptdir ends in CYGWIN-PATCHES, then topdir is $scriptdir/../..
# Otherwise, we assume that topdir = scriptdir
topdir1=`echo ${scriptdir} | sed 's%/SPECS$%%'`
topdir2=`echo ${scriptdir} | sed 's%/CYGWIN-PATCHES$%%'`
if [ "x$topdir1" != "x$scriptdir" ] ; then # SPECS
  topdir=`cd ${scriptdir}/..; pwd`
else
  if [ "x$topdir2" != "x$scriptdir" ] ; then # CYGWIN-PATCHES
    topdir=`cd ${scriptdir}/../..; pwd`
  else
    topdir=`cd ${scriptdir}; pwd`
  fi
fi

tscriptname=`basename $0 .sh`
export PKG=`echo $tscriptname | sed -e 's/\-[^\-]*\-[^\-]*$//'`
export VER=`echo $tscriptname | sed -e "s/${PKG}\-//" -e 's/\-[^\-]*$//'`
export REL=`echo $tscriptname | sed -e "s/${PKG}\-${VER}\-//"`
export MAJOR=`echo $VER | sed -e 's/\.[^.]*//g'`
export MINOR=`echo $VER | sed -e 's/[^\.]*\.//' -e 's/\.[^\.]*//'`
export BASEPKG=${PKG}-${VER}-${REL}
export FULLPKG=${BASEPKG}
export DIFF_IGNORE="-x *.aps -x *.ncb -x *.opt -x *.dep -x *.mak -x *.chm"

# determine correct decompression option and tarball filename
export src_orig_pkg_name=
if [ -e "${src_orig_pkg_name}" ] ; then
  export opt_decomp=? # Make sure tar punts if unpack() is not redefined
elif [ -e ${BASEPKG}.tar.gz ] ; then
  export opt_decomp=z
  export src_orig_pkg_name=${BASEPKG}.tar.gz
else
  echo Cannot find original package.
  exit 1
fi

export src_orig_pkg=${topdir}/${src_orig_pkg_name}

# determine correct names for generated files
export src_pkg_name=${FULLPKG}-src.tar.bz2
export src_patch_name=${FULLPKG}.patch
export bin_pkg_name=${FULLPKG}.tar.bz2

export src_pkg=${topdir}/${src_pkg_name}
export src_patch=${topdir}/${src_patch_name}
export bin_pkg=${topdir}/${bin_pkg_name}
export srcdir=${topdir}/${BASEPKG}
export objdir=${srcdir}/.build
export instdir=${srcdir}/.inst
export srcinstdir=${srcdir}/.sinst
export checkfile=${topdir}/${FULLPKG}.check

prefix=/usr
sysconfdir=/etc
localstatedir=/var
if [ -z "$MY_CFLAGS" ]; then
  MY_CFLAGS="-O2"
fi
if [ -z "$MY_LDFLAGS" ]; then
  MY_LDFLAGS=
fi

export install_docs="\
	ABOUT-NLS \
	ANNOUNCE \
	AUTHORS \
	BUG-REPORTS \
	CHANGES \
	CONTRIBUTORS \
	COPYING \
	COPYRIGHT \
	CREDITS \
	CHANGELOG \
	ChangeLog* \
	FAQ \
	HOW-TO-CONTRIBUTE \
	INSTALL \
	KNOWNBUG \
	LEGAL \
	LICENSE \
	NEWS \
	NOTES \
	PROGLIST \
	README \
	RELEASE_NOTES \
	THANKS \
	TODO \
"
export install_docs="`for i in ${install_docs}; do echo $i; done | sort -u`"
export test_rule=test
if [ -z "$SIG" ]; then
  export SIG=0	# set to 1 to turn on signing by default
fi

# helper function
# unpacks the original package source archive into ./${BASEPKG}/
# change this if the original package was not tarred
# or if it doesn't unpack to a correct directory
unpack() {
  tar xv${opt_decomp}f "$1"
}

mkdirs() {
  (cd "${topdir}" && \
  rm -fr "${objdir}" "${instdir}" "${srcinstdir}" && \
  mkdir -p "${objdir}" && \
  mkdir -p "${instdir}" && \
  mkdir -p "${srcinstdir}" )
}
prep() {
  (cd "${topdir}" && \
  unpack "${src_orig_pkg}" && \
  cd "${topdir}" && \
  if [ -f "${src_patch}" ] ; then \
    patch -p0 --binary < "${src_patch}" ;\
  fi && \
  mkdirs )
}
conf() {
  (cd "${objdir}" && \
  CFLAGS="${MY_CFLAGS}" LDFLAGS="${MY_LDFLAGS}" \
# copy links to ${objdir} for building
  find "${srcdir}" -mindepth 1 -maxdepth 1 ! -name .build ! -name .inst ! -name .sinst -exec cp -al {} . \; )
}
reconf() {
  (cd ${topdir} && \
  rm -fr ${objdir} && \
  mkdir -p ${objdir} && \
  conf )
}
build() {
  (
# build IPhreeqcCOM.chm
# hhc can't handle directory names that begin with a period
  cd "${topdir}" && \
  mv "${srcdir}/.build" "${srcdir}/_build" && \
  cd "${srcdir}/_build/help" && \
  make && \
  cd "${topdir}" && \
  mv "${srcdir}/_build" "${srcdir}/.build" && \
# build IPhreeqcCOM.dll
  cd "${topdir}" && \
  cd "${objdir}" && \
  MsBuild.exe IPhreeqcCOM.sln /t:IPhreeqcCOM /p:Configuration=Release /p:Platform=Win32 && \
# build IPhreeqcCOM.dll
  cd "${topdir}" && \
  cd "${objdir}" && \
  MsBuild.exe IPhreeqcCOM.sln /t:IPhreeqcCOM /p:Configuration=Release /p:Platform=x64 && \
# build IPhreeqcCOM.msi
  MsBuild.exe IPhreeqcCOM.sln /t:msi /p:Configuration=Release /p:Platform=Win32 /p:TargetName=${FULLPKG} /p:Major=${MAJOR} /p:Minor=${MINOR} /p:Build=${REL} && \
# build IPhreeqcCOMx64.msi
  MsBuild.exe IPhreeqcCOM.sln /t:msi /p:Configuration=Release /p:Platform=x64 /p:TargetName=${FULLPKG}-x64 /p:Major=${MAJOR} /p:Minor=${MINOR} /p:Build=${REL} )
}
check() {
  (cd ${objdir} && \
  make ${test_rule} | tee ${checkfile} 2>&1 )
}
clean() {
  (cd ${objdir} && \
  make clean )
}
install() {
  (rm -fr ${instdir}/* && \
# MSI file
  /usr/bin/install -m 755 "${objdir}/msi/bin/Release/${FULLPKG}.msi" "${instdir}/${FULLPKG}.msi" && \
# x64 MSI file
  /usr/bin/install -m 755 "${objdir}/msi/bin/x64/Release/${FULLPKG}-x64.msi" "${instdir}/${FULLPKG}-x64.msi" && \
# Build log  
  if [ -f "${topdir}/all-${REL}.log" ] ; then \
    /usr/bin/install -m 755 "${topdir}/all-${REL}.log" "${instdir}/all-${REL}.log" ;\
  fi && \
  if [ -x /usr/bin/md5sum ]; then \
    cd "${instdir}" && \
    find . -type f ! -name md5sum | sed 's/^/\"/' | sed 's/$/\"/' | xargs md5sum > md5sum ; \
  fi )
}
strip() {
  (cd "${instdir}" && \
  echo 'SKIPPING find . -name "*.dll" -or -name "*.exe" | xargs strip 2>&1' ; \
  true )
}
list() {
  (cd ${instdir} && \
  find . -name "*" ! -type d | sed 's%^\.%  %' ; \
  true )
}
depend() {
  (cd ${instdir} && \
  find ${instdir} -name "*.exe" -o -name "*.dll" | xargs cygcheck | \
  sed -e '/\.exe/d' -e 's,\\,/,g' | sort -bu | xargs -n1 cygpath -u \
  | xargs cygcheck -f | sed 's%^%  %' | sort -u ; \
  true )
}
pkg() {
  (cd "${instdir}" && \
  tar cvjf "${bin_pkg}" * )
}
mkpatch() {
  (cd "${srcdir}" && \
  find . -name "autom4te.cache" | xargs rm -rf ; \
  unpack "${src_orig_pkg}" && \
  cd "${srcdir}" && \
  mv "${BASEPKG}" ../${BASEPKG}-orig && \
  cd "${topdir}" && \
  diff -urN -x '.build' -x '.inst' -x '.sinst' \
    ${DIFF_IGNORE} \
    ${BASEPKG}-orig ${BASEPKG} > \
    ${srcinstdir}/${src_patch_name} ; \
  rm -rf ${BASEPKG}-orig )
}
# Note: maintainer-only functionality
acceptpatch() {
  cp --backup=numbered ${srcinstdir}/${src_patch_name} ${topdir}
}
spkg() {
  (mkpatch && \
  if [ "${SIG}" -eq 1 ] ; then \
    name=${srcinstdir}/${src_patch_name} text="PATCH" sigfile ; \
  fi && \
  cp "${src_orig_pkg}" "${srcinstdir}/${src_orig_pkg_name}" && \
  if [ "${SIG}" -eq 1 ] ; then \
    name=${src_orig_pkg} text="ORIG_PKG" sigfile ; \
  fi && \
  cp dist.sh "${srcinstdir}/" && \
  cp Makefile "${srcinstdir}/" && \
  if [ -e ${src_orig_pkg}.sig ] ; then \
    cp ${src_orig_pkg}.sig ${srcinstdir}/ ; \
  fi && \
  cp $0 "${srcinstdir}/`basename $0`" && \
  name=$0 text="SCRIPT" sigfile && \
  if [ "${SIG}" -eq 1 ] ; then \
    cp $0.sig "${srcinstdir}/" ; \
  fi && \
  cd "${srcinstdir}" && \
  tar cvjf "${src_pkg}" * )
}
finish() {
  rm -rf `echo $phast_ser | sed "s^/.*^^"` && \
  rm -rf "${srcdir}"
}
sigfile() {
  if [ \( "${SIG}" -eq 1 \) -a \( -e $name \) -a \( \( ! -e $name.sig \) -o \( $name -nt $name.sig \) \) ]; then \
    if [ -x /usr/bin/gpg ]; then \
      echo "$text signature need to be updated"; \
      rm -f $name.sig; \
      /usr/bin/gpg --detach-sign $name; \
    else \
      echo "You need the gnupg package installed in order to make signatures."; \
    fi; \
  fi
}
checksig() {
  if [ -x /usr/bin/gpg ]; then \
    if [ -e ${src_orig_pkg}.sig ]; then \
      echo "ORIGINAL PACKAGE signature follows:"; \
      /usr/bin/gpg --verify ${src_orig_pkg}.sig ${src_orig_pkg}; \
    else \
      echo "ORIGINAL PACKAGE signature missing."; \
    fi; \
    if [ -e $0.sig ]; then \
      echo "SCRIPT signature follows:"; \
      /usr/bin/gpg --verify $0.sig $0; \
    else \
      echo "SCRIPT signature missing."; \
    fi; \
    if [ -e ${src_patch}.sig ]; then \
      echo "PATCH signature follows:"; \
      /usr/bin/gpg --verify ${src_patch}.sig ${src_patch}; \
    else \
      echo "PATCH signature missing."; \
    fi; \
  else
    echo "You need the gnupg package installed in order to check signatures." ; \
  fi
}
while test -n "$1" ; do
  case $1 in
    prep)		prep ; STATUS=$? ;;
    mkdirs)		mkdirs ; STATUS=$? ;;
    conf)		conf ; STATUS=$? ;;
    configure)		conf ; STATUS=$? ;;
    reconf)		reconf ; STATUS=$? ;;
    build)		build ; STATUS=$? ;;
    make)		build ; STATUS=$? ;;
    check)		check ; STATUS=$? ;;
    test)		check ; STATUS=$? ;;
    clean)		clean ; STATUS=$? ;;
    install)		install ; STATUS=$? ;;
    list)		list ; STATUS=$? ;;
    depend)		depend ; STATUS=$? ;;
    strip)		strip ; STATUS=$? ;;
    package)		pkg ; STATUS=$? ;;
    pkg)		pkg ; STATUS=$? ;;
    mkpatch)		mkpatch ; STATUS=$? ;;
    acceptpatch)	acceptpatch ; STATUS=$? ;;
    src-package)	spkg ; STATUS=$? ;;
    spkg)		spkg ; STATUS=$? ;;
    finish)		finish ; STATUS=$? ;;
    checksig)		checksig ; STATUS=$? ;;
    first)		mkdirs && spkg && finish ; STATUS=$? ;;
    all)		checksig && prep && conf && build && install && \
			strip && pkg && spkg && finish ; \
			STATUS=$? ;;
    *) echo "Error: bad arguments" ; exit 1 ;;
  esac
  ( exit ${STATUS} ) || exit ${STATUS}
  shift
done

