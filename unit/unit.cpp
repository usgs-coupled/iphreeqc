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

	runner.addTest(TestVar::suite());
	runner.addTest(TestCVar::suite());
	runner.addTest(TestSelectedOutput::suite());
	runner.addTest(TestIPhreeqc::suite());
	runner.addTest(TestIPhreeqcLib::suite());

	runner.setOutputter(CppUnit::CompilerOutputter::defaultOutputter(&runner.result(), std::cout));

#if defined(_WIN32)
	int n = ::_fcloseall();
	assert(n == 0);
#endif

	bool wasSucessful = runner.run("", false);

#if defined(_MSC_VER)
	s.stopTimer();
	std::cerr << "Elapsed time: " << s.getElapsedTime() << std::endl;
#endif

	return wasSucessful ? 0 : 1;
}
