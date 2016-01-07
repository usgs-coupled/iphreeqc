#!/bin/sh
#-------------------------------------------------------------------------------
# sed files
#-------------------------------------------------------------------------------
alias svn='svn --config-dir ~/.svn-jenkins'
if [ -z "$DATE" ]; then
 DATE=$(date "+%x")
fi
if [ -z "$VER" ]; then
 IPHREEQC_SOURCE_FILE=`ssh -q charlton@parkplace "cd ftp/iphreeqc; ls -t iphreeqc-*-*[0-9][0-9][0-9][0-9].tar.gz | awk '{if (NR == 1) {print}}'"`
 VER=`echo $IPHREEQC_SOURCE_FILE | sed "s/^iphreeqc-//" | sed "s/-.*tar.gz$//"`
 ver_major=`echo $VER | cut -d '.' -f 1`
 ver_minor=`echo $VER | cut -d '.' -f 2`
 ver_patch=`echo $VER | cut -d '.' -f 3`
 ver_patch=$((ver_patch+1))
 VER="${ver_major}.${ver_minor}.${ver_patch}" 
fi
HEAD=`svn st -v configure.ac | awk '{print $1}')`
if [ "$REL" = 'HEAD' ]; then
 REL="$HEAD"
fi
svn export --force "http://internalbrr.cr.usgs.gov/svn_GW/phreeqc3/trunk/HTMLversion/phreeqc3.chm" "doc/phreeqc3.chm"
if [ "$HEAD" != "$REL" ]; then
 svn export "-r$REL" --force "http://internalbrr.cr.usgs.gov/svn_GW/phreeqc3/trunk/HTMLversion/phreeqc3.chm" "doc/phreeqc3.chm"
fi
if [ ! -e jenkins-dist.sh ]; then
 svn export --force "http://internalbrr.cr.usgs.gov/svn_GW/IPhreeqc/trunk/jenkins-dist.sh"
fi
export VER
/bin/sh jenkins-dist.sh -v ${VER} -r ${REL} -d ${DATE} -pr ${TAG}
#-------------------------------------------------------------------------------
# Build doxygen docs
#-------------------------------------------------------------------------------
cd doc && /home/charlton/doxygen-1.8.8/bin/doxygen && cd ..
#-------------------------------------------------------------------------------
# Build configure tarball
#-------------------------------------------------------------------------------
autoreconf -fvi
mkdir _build
cd _build && ../configure && make -j 8 distcheck && cd ..
#-------------------------------------------------------------------------------
# Copy/Rename README files
#-------------------------------------------------------------------------------
cp doc/README README.IPhreeqc.${REL}.TXT
#-------------------------------------------------------------------------------
# Build zip
#-------------------------------------------------------------------------------
svn -R propget svn:eol-style | egrep native | awk '{print $1}' | xargs unix2dos
cd _build && make dist-zip && cd ..
svn -R propget svn:eol-style | egrep native | awk '{print $1}' | xargs dos2unix
