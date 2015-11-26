# set date
if ([string]::IsNullOrEmpty($Env:DATE)) {
  $Env:DATE = date +%x
}
$Env:RELEASE_DATE = date -d $Env:DATE "+%B %e, %G"
# set ver
if ([string]::IsNullOrEmpty($Env:VER)) {
  $SRC_FILE=(plink -i C:\Users\Public\rsa-key-20151119.ppk charlton@parkplace `
            "cd ftp/iphreeqc; ls -t iphreeqc-*-*.tar.gz | awk '{if (NR == 1) {print}}'")
  $v = ($SRC_FILE -replace "^iphreeqc-", "" -replace "-.*tar.gz$", "") -split "\."
  if ([string]::IsNullOrEmpty($v[2])) {
    $v[2] = 0
  }
  $v[2] = 1 + $v[2]
  $Env:ver_major = $v[0]
  $Env:ver_minor = $v[1]
  $Env:ver_patch = $v[2]
  $Env:VER = $v -join "."
}
# set HEAD
$HEAD=(-split (svn --config-dir C:\Users\jenkins\svn-jenkins st -v configure.ac))[0]
if ([string]::IsNullOrEmpty($Env:REL) -or $Env:REL.CompareTo('HEAD') -eq 0) {
  $Env:REL = $HEAD
}
svn --config-dir C:\Users\jenkins\svn-jenkins export --force `
    "http://internalbrr.cr.usgs.gov/svn_GW/phreeqc3/trunk/HTMLversion/phreeqc3.chm" "doc/phreeqc3.chm"
if ($HEAD.CompareTo($Env:REL) -ne 0) {
  svn --config-dir C:\Users\jenkins\svn-jenkins export "-r$Env:REL" --force `
      "http://internalbrr.cr.usgs.gov/svn_GW/phreeqc3/trunk/HTMLversion/phreeqc3.chm" "doc/phreeqc3.chm"
}

# duplicate build/dist.sh
$sed_files=@('phreeqc3-doc/RELEASE.TXT', `
             'src/Version.h', `
             'src/IPhreeqc.h')
foreach ($file in $sed_files) {
  (Get-Content $file) | Foreach-Object {
    $_ -replace "(#define *VER_MAJOR\s*)[0-9]*",     "`${1}$Env:ver_major" `
       -replace "(#define *VER_MINOR\s*)[0-9]*",     "`${1}$Env:ver_minor" `
       -replace "(#define *VER_PATCH\s*)[0-9]*",     "`${1}$Env:ver_patch" `
       -replace "(#define *VER_REVISION\s*)[0-9]*",  "`${1}$Env:REL" `
       -replace "@RELEASE_DATE@",                    "$Env:RELEASE_DATE" `
       -replace "@PHREEQC_VER@",                     "$Env:VER" `
       -replace "@PHREEQC_DATE@",                    "$Env:RELEASE_DATE" `
       -replace "@REVISION_SVN@",                    "$Env:REL"
  } | Set-Content $file
}

# doxygen
cd doc
doxygen
hhc IPhreeqc.hhp
cd ..

# build
$MsBuild = "c:\WINDOWS\Microsoft.NET\Framework\v2.0.50727\MsBuild.exe"
$options = "IPhreeqc.2005.sln /t:IPhreeqc /p:Configuration=Release /p:Platform=Win32 /verbosity:detailed"
Invoke-Expression "$MsBuild $options"
