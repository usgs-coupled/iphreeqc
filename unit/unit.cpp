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

// COMMENT: {8/16/2013 12:14:16 AM}	runner.addTest(TestVar::suite());
// COMMENT: {8/16/2013 12:14:16 AM}	runner.addTest(TestCVar::suite());
// COMMENT: {8/16/2013 12:14:16 AM}	runner.addTest(TestSelectedOutput::suite());
	runner.addTest(TestIPhreeqc::suite());
// COMMENT: {8/16/2013 12:14:19 AM}	runner.addTest(TestIPhreeqcLib::suite());

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

// COMMENT: {8/16/2013 11:40:58 PM}#if defined(_WIN32) || defined(__CYGWIN32__)
// COMMENT: {8/16/2013 11:40:58 PM}bool FileExists(const char *szPathName)
// COMMENT: {8/16/2013 11:40:58 PM}{
// COMMENT: {8/16/2013 11:40:58 PM}	SECURITY_ATTRIBUTES sa;
// COMMENT: {8/16/2013 11:40:58 PM}	sa.nLength = sizeof(SECURITY_ATTRIBUTES);
// COMMENT: {8/16/2013 11:40:58 PM}	sa.lpSecurityDescriptor = NULL;
// COMMENT: {8/16/2013 11:40:58 PM}	sa.bInheritHandle = TRUE;
// COMMENT: {8/16/2013 11:40:58 PM}	HANDLE fileHandle = ::CreateFile(szPathName, GENERIC_READ, 0, &sa, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
// COMMENT: {8/16/2013 11:40:58 PM}
// COMMENT: {8/16/2013 11:40:58 PM}	bool retValue;
// COMMENT: {8/16/2013 11:40:58 PM}	if (fileHandle == INVALID_HANDLE_VALUE)
// COMMENT: {8/16/2013 11:40:58 PM}	{
// COMMENT: {8/16/2013 11:40:58 PM}		char buffer[100];
// COMMENT: {8/16/2013 11:40:58 PM}		sprintf(buffer, "Could not open file (error %d)\n", GetLastError());
// COMMENT: {8/16/2013 11:40:58 PM}		retValue = false;
// COMMENT: {8/16/2013 11:40:58 PM}	}
// COMMENT: {8/16/2013 11:40:58 PM}	else
// COMMENT: {8/16/2013 11:40:58 PM}	{
// COMMENT: {8/16/2013 11:40:58 PM}		retValue = true;
// COMMENT: {8/16/2013 11:40:58 PM}		::CloseHandle(fileHandle);
// COMMENT: {8/16/2013 11:40:58 PM}	}
// COMMENT: {8/16/2013 11:40:58 PM}	return retValue;
// COMMENT: {8/16/2013 11:40:58 PM}}
// COMMENT: {8/16/2013 11:40:58 PM}#else
// COMMENT: {8/16/2013 11:40:58 PM}bool FileExists(const char *szPathName)
// COMMENT: {8/16/2013 11:40:58 PM}{
// COMMENT: {8/16/2013 11:40:58 PM}  FILE* fp;
// COMMENT: {8/16/2013 11:40:58 PM}  fp = fopen(szPathName, "r");
// COMMENT: {8/16/2013 11:40:58 PM}  if (fp == NULL) {
// COMMENT: {8/16/2013 11:40:58 PM}    return false;
// COMMENT: {8/16/2013 11:40:58 PM}  } else {
// COMMENT: {8/16/2013 11:40:58 PM}    fclose(fp);
// COMMENT: {8/16/2013 11:40:58 PM}    return true;
// COMMENT: {8/16/2013 11:40:58 PM}  }
// COMMENT: {8/16/2013 11:40:58 PM}}
// COMMENT: {8/16/2013 11:40:58 PM}#endif

// COMMENT: {8/16/2013 11:40:25 PM}#if defined(_WIN32) || defined(__CYGWIN32__)
// COMMENT: {8/16/2013 11:40:25 PM}// DeleteFile defined in <windows.h>
// COMMENT: {8/16/2013 11:40:25 PM}#else
// COMMENT: {8/16/2013 11:40:25 PM}int DeleteFile(const char* szPathName)
// COMMENT: {8/16/2013 11:40:25 PM}{
// COMMENT: {8/16/2013 11:40:25 PM}  if (remove(szPathName) == 0)
// COMMENT: {8/16/2013 11:40:25 PM}  {
// COMMENT: {8/16/2013 11:40:25 PM}    return 1;
// COMMENT: {8/16/2013 11:40:25 PM}  }
// COMMENT: {8/16/2013 11:40:25 PM}  return 0; // failure
// COMMENT: {8/16/2013 11:40:25 PM}}
// COMMENT: {8/16/2013 11:40:25 PM}#endif

// COMMENT: {8/16/2013 11:42:03 PM}#if defined(_WIN32) || defined(__CYGWIN32__)
// COMMENT: {8/16/2013 11:42:03 PM}size_t FileSize(const char *szPathName)
// COMMENT: {8/16/2013 11:42:03 PM}{
// COMMENT: {8/16/2013 11:42:03 PM}	HANDLE hFile = ::CreateFile(
// COMMENT: {8/16/2013 11:42:03 PM}		szPathName,            // file to open
// COMMENT: {8/16/2013 11:42:03 PM}		GENERIC_READ,          // open for reading
// COMMENT: {8/16/2013 11:42:03 PM}		FILE_SHARE_READ,       // share for reading
// COMMENT: {8/16/2013 11:42:03 PM}		NULL,                  // default security
// COMMENT: {8/16/2013 11:42:03 PM}		OPEN_EXISTING,         // existing file only
// COMMENT: {8/16/2013 11:42:03 PM}		FILE_ATTRIBUTE_NORMAL, // normal file
// COMMENT: {8/16/2013 11:42:03 PM}		NULL);                 // no attr. template
// COMMENT: {8/16/2013 11:42:03 PM}
// COMMENT: {8/16/2013 11:42:03 PM}	if (hFile != INVALID_HANDLE_VALUE)
// COMMENT: {8/16/2013 11:42:03 PM}	{
// COMMENT: {8/16/2013 11:42:03 PM}		// read file size
// COMMENT: {8/16/2013 11:42:03 PM}		LARGE_INTEGER liFileSize;
// COMMENT: {8/16/2013 11:42:03 PM}		::GetFileSizeEx(hFile, &liFileSize);
// COMMENT: {8/16/2013 11:42:03 PM}		::CloseHandle(hFile);
// COMMENT: {8/16/2013 11:42:03 PM}		return (size_t) liFileSize.QuadPart;
// COMMENT: {8/16/2013 11:42:03 PM}	}
// COMMENT: {8/16/2013 11:42:03 PM}	return 0;
// COMMENT: {8/16/2013 11:42:03 PM}}
// COMMENT: {8/16/2013 11:42:03 PM}#else
// COMMENT: {8/16/2013 11:42:03 PM}#include <sys/types.h>
// COMMENT: {8/16/2013 11:42:03 PM}#include <sys/stat.h>
// COMMENT: {8/16/2013 11:42:03 PM}#include <unistd.h>
// COMMENT: {8/16/2013 11:42:03 PM}
// COMMENT: {8/16/2013 11:42:03 PM}size_t FileSize(const char *szPathName)
// COMMENT: {8/16/2013 11:42:03 PM}{
// COMMENT: {8/16/2013 11:42:03 PM}	struct stat s;
// COMMENT: {8/16/2013 11:42:03 PM}	stat(szPathName, &s);
// COMMENT: {8/16/2013 11:42:03 PM}	return (size_t) s.st_size;
// COMMENT: {8/16/2013 11:42:03 PM}}
// COMMENT: {8/16/2013 11:42:03 PM}