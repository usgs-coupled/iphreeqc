#!/bin/sh

#
# set DATE
#
if [ -z "$DATE" ]; then
 DATE=$(date "+%x")
fi
DATE=$(date -d $DATE "+%Y-%m-%d")

#
# set VER
#
if [ -z "$VER" ]; then
 ###wget https://cran.r-project.org/package=phreeqc -O index.xhtml 2> /dev/null
 ###VER=`xmlstarlet sel -t -m "/_:html/_:body/_:table[1]/_:tr[1]/_:td[2]" -v . -n index.xhtml 2> /dev/null`
 VER=$(curl https://raw.githubusercontent.com/usgs-coupled/phreeqc-version/main/phreeqc-version.txt)
 ver_major=$(echo "$VER" | cut -d '.' -f 1)
 ver_minor=$(echo "$VER" | cut -d '.' -f 2)
 ver_patch=$(echo "$VER" | cut -d '.' -f 3)
 ver_patch=$((ver_patch+1))
else
 ver_major=$(echo "$VER" | cut -d '.' -f 1)
 ver_minor=$(echo "$VER" | cut -d '.' -f 2)
 ver_patch=$(echo "$VER" | cut -d '.' -f 3)
fi
if [ -z "$ver_major" ] || [ -z "$ver_minor" ] || [ -z "$ver_patch" ]; then
 exit 1
fi
VER="${ver_major}.${ver_minor}.${ver_patch}"
export VER

#
# set REL
#
curl https://raw.githubusercontent.com/usgs-coupled/phreeqc-version/main/ver.py -o ver.py
HEAD=$(python ver.py)
if [ -z "$REL" ]; then
 REL="$HEAD"
fi

# sed files
/bin/sh jenkins-dist.sh -v ${VER} -r ${REL} -d ${DATE} -pr ${TAG}
cd R
mkdir -p phreeqc/man
mkdir -p phreeqc/src/phreeqcpp
mkdir -p phreeqc/R
mkdir -p phreeqc/data
make VERSION=${VER} RELEASE_DATE=${DATE} build
##make VERSION=${VER} RELEASE_DATE=${DATE} check
