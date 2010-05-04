#if !defined(TEST_IPHREEQC_LIB_H_INCLUDED)
#define TEST_IPHREEQC_LIB_H_INCLUDED

#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/TestFixture.h>

class TestIPhreeqcLib : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE( TestIPhreeqcLib );
	CPPUNIT_TEST( TestCreateIPhreeqc );
	CPPUNIT_TEST( TestDestroyIPhreeqc );
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
	CPPUNIT_TEST( TestAddError );
	CPPUNIT_TEST( TestOutputError );
	CPPUNIT_TEST( TestRunWithCallback );
	CPPUNIT_TEST( TestRunNoDatabaseLoaded );
	CPPUNIT_TEST( TestCase1 );
	CPPUNIT_TEST( TestCase2 );
	CPPUNIT_TEST( TestPrintSelectedOutputFalse );
	CPPUNIT_TEST( TestOutputOnOff );
	CPPUNIT_TEST( TestErrorOnOff );
	CPPUNIT_TEST( TestLogOnOff );
	CPPUNIT_TEST( TestDumpOn );
	CPPUNIT_TEST( TestSelOutOnOff );
	CPPUNIT_TEST( TestLongHeadings );
	CPPUNIT_TEST( TestDatabaseKeyword );
	CPPUNIT_TEST( TestDumpString );
	CPPUNIT_TEST( TestGetDumpLineCount );
	CPPUNIT_TEST( TestGetDumpLine );
	CPPUNIT_TEST( TestGetComponentCount );
	CPPUNIT_TEST( TestGetComponent );
	CPPUNIT_TEST( TestGetErrorLine );
	CPPUNIT_TEST( TestGetWarningLine );
	CPPUNIT_TEST_SUITE_END();

public:
	TestIPhreeqcLib(void);
	~TestIPhreeqcLib(void);

public:
	void TestCreateIPhreeqc(void);
	void TestDestroyIPhreeqc(void);
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
	void TestOutputError(void);
	void TestRunWithCallback(void);
	void TestRunNoDatabaseLoaded(void);
	void TestCase1(void);
	void TestCase2(void);
	void TestPrintSelectedOutputFalse(void);
	void TestOutputOnOff(void);
	void TestErrorOnOff(void);
	void TestLogOnOff(void);
	void TestDumpOn(void);
	void TestSelOutOnOff(void);
	void TestLongHeadings(void);
	void TestDatabaseKeyword();
	void TestDumpString();
	void TestGetDumpLineCount(void);
	void TestGetDumpLine(void);
	void TestGetComponentCount(void);
	void TestGetComponent(void);
	void TestGetErrorLine(void);
	void TestGetWarningLine(void);

protected:
	void TestOnOff(const char* FILENAME, int output_on, int error_on, int log_on, int selected_output_on, int dump_on);
};


#endif // TEST_IPHREEQC_LIB_H_INCLUDED