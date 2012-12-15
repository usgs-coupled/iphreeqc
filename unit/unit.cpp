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

#if defined(_MSC_VER)

typedef struct
{
	LARGE_INTEGER start;
	LARGE_INTEGER stop;
} stopWatch;
 
class CStopWatch
{
private:
	stopWatch timer;
	LARGE_INTEGER frequency;
	double LIToSecs(LARGE_INTEGER & L) ;
public:
	CStopWatch();
	void startTimer();
	void stopTimer();
	double getElapsedTime();
};

double CStopWatch::LIToSecs(LARGE_INTEGER &L)
{
	return ((double)L.QuadPart /(double)frequency.QuadPart);
}

CStopWatch::CStopWatch()
{
	timer.start.QuadPart=0;
	timer.stop.QuadPart=0;
	QueryPerformanceFrequency(&frequency);
}

void CStopWatch::startTimer()
{
	QueryPerformanceCounter(&timer.start);
}

void CStopWatch::stopTimer()
{
	QueryPerformanceCounter(&timer.stop);
}

double CStopWatch::getElapsedTime()
{
	LARGE_INTEGER time;
	time.QuadPart = timer.stop.QuadPart - timer.start.QuadPart;
	return LIToSecs(time);
}

#endif /* _MSC_VER */

int main(int argc, char **argv)
{
	CppUnit::TextUi::TestRunner runner;

#if defined(_MSC_VER)
	CStopWatch s;
	s.startTimer();
#endif

	//runner.addTest(TestVar::suite());
	//runner.addTest(TestCVar::suite());
	//runner.addTest(TestSelectedOutput::suite());
	//runner.addTest(TestIPhreeqc::suite());
	runner.addTest(TestIPhreeqcLib::suite());

	runner.setOutputter(CppUnit::CompilerOutputter::defaultOutputter(&runner.result(), std::cout));

#if defined(_WIN32)
	int n = ::_fcloseall();
	assert(n == 0);
#endif

	bool wasSucessful = runner.run("", false);

#if defined(_MSC_VER)
	s.stopTimer();
	std::cerr << s.getElapsedTime() << std::endl;
#endif

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

#if defined(_WIN32) || defined(__CYGWIN32__)
size_t FileSize(const char *szPathName)
{
	HANDLE hFile = ::CreateFile(
		szPathName,            // file to open
		GENERIC_READ,          // open for reading
		FILE_SHARE_READ,       // share for reading
		NULL,                  // default security
		OPEN_EXISTING,         // existing file only
		FILE_ATTRIBUTE_NORMAL, // normal file
		NULL);                 // no attr. template

	if (hFile != INVALID_HANDLE_VALUE)
	{
		// read file size
		LARGE_INTEGER liFileSize;
		::GetFileSizeEx(hFile, &liFileSize);
		::CloseHandle(hFile);
		return (size_t) liFileSize.QuadPart;
	}
	return 0;
}
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

size_t FileSize(const char *szPathName)
{
	struct stat s;
	stat(szPathName, &s);
	return (size_t) s.st_size;
}
#endif