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
	CPPUNIT_TEST( TestOutputOnOff );
	CPPUNIT_TEST( TestErrorOnOff );
	CPPUNIT_TEST( TestLogOnOff );
	CPPUNIT_TEST( TestDumpOn );
	CPPUNIT_TEST( TestSelOutOnOff );
	CPPUNIT_TEST( TestLongHeadings );
	CPPUNIT_TEST( TestDatabaseKeyword );
	CPPUNIT_TEST( TestDumpString );
	CPPUNIT_TEST( TestGetDumpStringLineCount );
	CPPUNIT_TEST( TestGetDumpStringLine );
	CPPUNIT_TEST( TestGetComponentCount );
	CPPUNIT_TEST( TestGetComponent );
	CPPUNIT_TEST( TestListComponents );
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
	void TestOutputOnOff(void);
	void TestErrorOnOff(void);
	void TestLogOnOff(void);
	void TestDumpOn(void);
	void TestSelOutOnOff(void);
	void TestLongHeadings(void);
	void TestDatabaseKeyword(void);
	void TestDumpString(void);
	void TestGetDumpStringLineCount(void);
	void TestGetDumpStringLine(void);
	void TestGetComponentCount(void);
	void TestGetComponent(void);
	void TestListComponents(void);

};

#endif // TESTIPHREEQC_H_INCLUDED
