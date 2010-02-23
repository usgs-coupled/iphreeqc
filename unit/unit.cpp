#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/CompilerOutputter.h>

#include "TestVar.h"
#include "TestCVar.h"
#include "TestSelectedOutput.h"
#include "TestInterface.h"

int main(int argc, char **argv)
{
	CppUnit::TextUi::TestRunner runner;

	runner.addTest(TestVar::suite());
	runner.addTest(TestCVar::suite());
	runner.addTest(TestSelectedOutput::suite());
	runner.addTest(TestInterface::suite());

	runner.setOutputter(CppUnit::CompilerOutputter::defaultOutputter(&runner.result(), std::cout));

	bool wasSucessful = runner.run("", false);
	return wasSucessful;
}
