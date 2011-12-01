#if defined(_WIN32) || defined(__CYGWIN32__)
#include <windows.h>
#else
#include <stdio.h>
#endif

#include <cassert>
#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/CompilerOutputter.h>

#include "TestVar.h"
#include "TestCVar.h"
#include "TestSelectedOutput.h"
#include "TestIPhreeqc.h"
#include "TestIPhreeqcLib.h"

int main(int argc, char **argv)
{
	CppUnit::TextUi::TestRunner runner;

	runner.addTest(TestVar::suite());
	runner.addTest(TestCVar::suite());
	runner.addTest(TestSelectedOutput::suite());
	runner.addTest(TestIPhreeqc::suite());
// COMMENT: {11/16/2011 11:12:52 PM}	runner.addTest(TestIPhreeqcLib::suite());

	runner.setOutputter(CppUnit::CompilerOutputter::defaultOutputter(&runner.result(), std::cout));

#if defined(_WIN32)
	int n = ::_fcloseall();
	assert(n == 0);
#endif

	bool wasSucessful = runner.run("", false);
	return wasSucessful;
}

#if defined(_WIN32) || defined(__CYGWIN32__)
bool FileExists(const char *szPathName)
{
	SECURITY_ATTRIBUTES sa;
	sa.nLength = sizeof(SECURITY_ATTRIBUTES);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;
	HANDLE fileHandle = ::CreateFile(szPathName, GENERIC_READ, 0, &sa, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	bool retValue;
	if (fileHandle == INVALID_HANDLE_VALUE)
	{
		char buffer[100];
		sprintf(buffer, "Could not open file (error %d)\n", GetLastError());
		retValue = false;
	}
	else
	{
		retValue = true;
		::CloseHandle(fileHandle);
	}
	return retValue;
}
#else
bool FileExists(const char *szPathName)
{
  FILE* fp;
  fp = fopen(szPathName, "r");
  if (fp == NULL) {
    return false;
  } else {
    fclose(fp);
    return true;
  }
}
#endif

#if defined(_WIN32) || defined(__CYGWIN32__)
// DeleteFile defined in <windows.h>
#else
int DeleteFile(const char* szPathName)
{
  if (remove(szPathName) == 0)
  {
    return 1;
  }
  return 0; // failure
}
#endif
