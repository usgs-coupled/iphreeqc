#
# To get the Invoke-WebRequest to work under the 'nt authority\system' account, the DOIRootCA2.cer
# CA needs to be installed by running internet explorer as system using 'psexec -sid cmd' from
# Sysinternals.  The -UseBasicParsing flag may also be required.
#

#
# set DATE
#
if ([string]::IsNullOrEmpty($Env:DATE)) {
  $Env:DATE = date +%x
}
$Env:DATE = date -d $Env:DATE +%x
$Env:RELEASE_DATE = date -d $Env:DATE "+%B %e, %G"

#
# set VER
#
if ([string]::IsNullOrEmpty($Env:VER)) {
  $request = Invoke-WebRequest https://raw.githubusercontent.com/usgs-coupled/phreeqc-version/main/phreeqc-version.txt -UseBasicParsing
  $v = ($request.Content) -split "\."
  if ([string]::IsNullOrEmpty($v[2])) {
    $v[2] = 0
  }
  $v[2] = 1 + $v[2]
  $Env:ver_major = $v[0]
  $Env:ver_minor = $v[1]
  $Env:ver_patch = $v[2]
  $Env:VER = $v -join "."
}
else {
  $v = ($Env:VER) -split "\."  
  $Env:ver_major = $v[0]
  $Env:ver_minor = $v[1]
  $Env:ver_patch = $v[2]
}
if ([string]::IsNullOrEmpty($v[0]) -or [string]::IsNullOrEmpty($v[1]) -or [string]::IsNullOrEmpty($v[2])) {
  throw "Bad VER"
}

#
# set REL
#
Invoke-WebRequest https://raw.githubusercontent.com/usgs-coupled/phreeqc-version/main/ver.py -OutFile ver.py -UseBasicParsing
$HEAD=$(python ver.py)
if ([string]::IsNullOrEmpty($Env:REL)) {
  $Env:REL = $HEAD
}

#
# replace strings
#
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

#
# doxygen
#
Set-Location doc
doxygen

#
# hhc seems to return 1 on success and 0 (zero) on failure
# see https://stackoverflow.com/questions/39012558/html-help-workshop-returns-error-after-successfully-compiled-chm-file
#
hhc IPhreeqc.hhp
if (-Not $?) {
  $LastExitCode = 0
} 
Set-Location ..
