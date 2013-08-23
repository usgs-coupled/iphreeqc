#if !defined(TESTIPHREEQC_H_INCLUDED)
#define TESTIPHREEQC_H_INCLUDED

#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/TestFixture.h>

class TestIPhreeqc : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE( TestIPhreeqc );
	CPPUNIT_TEST( TestLoadDatabase );
	CPPUNIT_TEST( TestLoadDatabaseString );
	CPPUNIT_TEST( TestLoadDatabaseMissingFile );
	CPPUNIT_TEST( TestLoadDatabaseWithErrors );
	CPPUNIT_TEST( TestRunAccumulated );
	CPPUNIT_TEST( TestRunWithErrors );
	CPPUNIT_TEST( TestRunFile );
	CPPUNIT_TEST( TestRunString );
	CPPUNIT_TEST( TestGetSelectedOutputRowCount );
	CPPUNIT_TEST( TestGetSelectedOutputValue );
	CPPUNIT_TEST( TestGetSelectedOutputColumnCount );
	CPPUNIT_TEST( TestAddError );
	CPPUNIT_TEST( TestAccumulateLine );
	CPPUNIT_TEST( TestOutputErrorString );
	CPPUNIT_TEST( TestRunWithCallback );
	CPPUNIT_TEST( TestRunNoDatabaseLoaded );
	CPPUNIT_TEST( TestCase1 );
	CPPUNIT_TEST( TestCase2 );
	CPPUNIT_TEST( TestPrintSelectedOutputFalse );
	CPPUNIT_TEST( TestOutputFileOnOff );
	CPPUNIT_TEST( TestErrorFileOnOff );
	CPPUNIT_TEST( TestLogFileOnOff );
	CPPUNIT_TEST( TestDumpFileOnOff );
	CPPUNIT_TEST( TestSelOutFileOnOff );
	CPPUNIT_TEST( TestLongHeadings );
	CPPUNIT_TEST( TestDatabaseKeyword );
	CPPUNIT_TEST( TestDumpString );
	CPPUNIT_TEST( TestGetDumpStringLineCount );
	CPPUNIT_TEST( TestGetDumpStringLine );
	CPPUNIT_TEST( TestGetComponentCount );
	CPPUNIT_TEST( TestGetComponent );
	CPPUNIT_TEST( TestListComponents );
	CPPUNIT_TEST( TestSetDumpFileName );
	CPPUNIT_TEST( TestSetOutputFileName );
	CPPUNIT_TEST( TestOutputStringOnOff );
	CPPUNIT_TEST( TestGetOutputString );
	CPPUNIT_TEST( TestGetOutputStringLineCount );
	CPPUNIT_TEST( TestGetOutputStringLine );
	CPPUNIT_TEST( TestSetLogFileName );
	CPPUNIT_TEST( TestLogStringOnOff );
	CPPUNIT_TEST( TestGetLogString );
	CPPUNIT_TEST( TestGetLogStringLineCount );
	CPPUNIT_TEST( TestGetLogStringLine );
	CPPUNIT_TEST( TestSetErrorFileName );
	CPPUNIT_TEST( TestErrorStringOnOff );
	CPPUNIT_TEST( TestGetErrorString );
	CPPUNIT_TEST( TestGetErrorStringLineCount );
	CPPUNIT_TEST( TestSetSelectedOutputFileName );
	CPPUNIT_TEST( TestSelectedOutputStringOnOff );
	CPPUNIT_TEST( TestGetSelectedOutputString );
	CPPUNIT_TEST( TestGetSelectedOutputStringLineCount );
	CPPUNIT_TEST( TestGetSelectedOutputStringLine );
	CPPUNIT_TEST( TestGetSelectedOutputStringLineNotEnoughHeadings );
	CPPUNIT_TEST( TestLongUser_Punch );
	CPPUNIT_TEST( TestBasicSURF );
	CPPUNIT_TEST( TestCErrorReporter );
	CPPUNIT_TEST( TestDelete );
	CPPUNIT_TEST( TestRunFileMultiPunchOn );
	CPPUNIT_TEST( TestRunFileMultiPunchOff );
	CPPUNIT_TEST( TestRunFileMultiPunchSet );
	CPPUNIT_TEST( TestRunFileMultiPunchNoSet );
	CPPUNIT_TEST( TestMultiPunchSelectedOutputStringOn );
	CPPUNIT_TEST_SUITE_END();

public:
	TestIPhreeqc(void);
	~TestIPhreeqc(void);

public:
	void TestLoadDatabase(void);
	void TestLoadDatabaseString(void);
	void TestLoadDatabaseMissingFile(void);
	void TestLoadDatabaseWithErrors(void);
	void TestRunAccumulated(void);
	void TestRunWithErrors(void);
	void TestRunFile(void);
	void TestRunString(void);
	void TestGetSelectedOutputRowCount(void);
	void TestGetSelectedOutputValue(void);
	void TestGetSelectedOutputColumnCount(void);
	void TestAddError(void);
	void TestAccumulateLine(void);
	void TestOutputErrorString(void);
	void TestRunWithCallback(void);
	void TestRunNoDatabaseLoaded(void);
	void TestCase1(void);
	void TestCase2(void);
	void TestPrintSelectedOutputFalse(void);
	void TestOutputFileOnOff(void);
	void TestErrorFileOnOff(void);
	void TestLogFileOnOff(void);
	void TestDumpFileOnOff(void);
	void TestSelOutFileOnOff(void);
	void TestLongHeadings(void);
	void TestDatabaseKeyword(void);
	void TestDumpString(void);
	void TestGetDumpStringLineCount(void);
	void TestGetDumpStringLine(void);
	void TestGetComponentCount(void);
	void TestGetComponent(void);
	void TestListComponents(void);
	void TestSetDumpFileName(void);
	void TestSetOutputFileName(void);
	void TestOutputStringOnOff(void);
	void TestGetOutputString(void);
	void TestGetOutputStringLineCount(void);
	void TestGetOutputStringLine(void);
	void TestSetLogFileName(void);
	void TestLogStringOnOff(void);
	void TestGetLogString(void);
	void TestGetLogStringLineCount(void);
	void TestGetLogStringLine(void);
	void TestSetErrorFileName(void);
	void TestErrorStringOnOff(void);
	void TestGetErrorString(void);
	void TestGetErrorStringLineCount(void);
	void TestSetSelectedOutputFileName(void);
	void TestSelectedOutputStringOnOff(void);
	void TestGetSelectedOutputString(void);
	void TestGetSelectedOutputStringLineCount(void);
	void TestGetSelectedOutputStringLine(void);
	void TestGetSelectedOutputStringLineNotEnoughHeadings(void);
	void TestLongUser_Punch(void);
	void TestBasicSURF(void);
	void TestCErrorReporter(void);
	void TestDelete(void);
	void TestRunFileMultiPunchOn(void);
	void TestRunFileMultiPunchOff(void);
	void TestRunFileMultiPunchSet(void);
	void TestRunFileMultiPunchNoSet(void);
	void TestMultiPunchSelectedOutputStringOn(void);

protected:
	void TestFileOnOff(const char* FILENAME, bool output_file_on, bool error_file_on, bool log_file_on, bool selected_output_file_on, bool dump_file_on);
};

#endif // TESTIPHREEQC_H_INCLUDED
