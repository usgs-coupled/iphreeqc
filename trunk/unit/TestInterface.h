#if !defined(TESTINTERFACE_H_INCLUDED)
#define TESTINTERFACE_H_INCLUDED

#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/TestFixture.h>


class TestInterface :
	public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE( TestInterface );
	CPPUNIT_TEST( TestLoadDatabase );
	CPPUNIT_TEST( TestLoadDatabaseString );
	CPPUNIT_TEST( TestLoadDatabaseMissingFile );
	CPPUNIT_TEST( TestLoadDatabaseWithErrors );
	CPPUNIT_TEST( TestRun );
	CPPUNIT_TEST( TestRunWithErrors );
	CPPUNIT_TEST( TestRunFile );
	CPPUNIT_TEST( TestRunString );
	CPPUNIT_TEST( TestGetSelectedOutputRowCount );
	CPPUNIT_TEST( TestGetSelectedOutputValue );
	CPPUNIT_TEST( TestGetSelectedOutputColumnCount );
	CPPUNIT_TEST( TestAddError );
	CPPUNIT_TEST( TestAccumulateLine );
	CPPUNIT_TEST( TestOutputLastError );
	CPPUNIT_TEST( TestRunWithCallback );
	CPPUNIT_TEST( TestRunNoDatabaseLoaded );
	CPPUNIT_TEST( TestRunFileNoDatabaseLoaded );
	CPPUNIT_TEST( TestCase1 );
	CPPUNIT_TEST( TestCase2 );
	CPPUNIT_TEST( TestPrintSelectedOutputFalse );
	CPPUNIT_TEST( TestOutputOnOff );
	CPPUNIT_TEST( TestErrorOnOff );
	CPPUNIT_TEST( TestLogOnOff );
	CPPUNIT_TEST( TestSelOutOnOff );	
	CPPUNIT_TEST( TestLongHeadings );
	CPPUNIT_TEST( TestDatabaseKeyword );
	CPPUNIT_TEST( TestDumpOn );
	CPPUNIT_TEST( TestDumpString );
	CPPUNIT_TEST( TestGetDumpStringLineCount );
	CPPUNIT_TEST( TestGetDumpStringLine );
	CPPUNIT_TEST( TestGetComponentCount );
	CPPUNIT_TEST( TestGetComponent );

	CPPUNIT_TEST_SUITE_END();

public:
	TestInterface(void);
	~TestInterface(void);

public:
	void TestLoadDatabase(void);
	void TestLoadDatabaseString(void);
	void TestLoadDatabaseMissingFile(void);
	void TestLoadDatabaseWithErrors(void);
	void TestRun(void);
	void TestRunWithErrors(void);
	void TestRunFile(void);
	void TestRunString(void);
	void TestGetSelectedOutputRowCount(void);
	void TestGetSelectedOutputValue(void);
	void TestGetSelectedOutputColumnCount(void);
	void TestAddError(void);
	void TestAccumulateLine(void);
	void TestOutputLastError(void);
	void TestRunWithCallback(void);
	void TestRunNoDatabaseLoaded(void);
	void TestRunFileNoDatabaseLoaded(void);
	void TestCase1(void);
	void TestCase2(void);
	void TestPrintSelectedOutputFalse(void);
	void TestOutputOnOff(void);
	void TestErrorOnOff(void);
	void TestLogOnOff(void);
	void TestSelOutOnOff(void);
	void TestLongHeadings(void);
	void TestDatabaseKeyword(void);
	void TestDumpOn(void);
	void TestDumpString(void);
	void TestGetDumpStringLineCount(void);
	void TestGetDumpStringLine(void);
	void TestGetComponentCount(void);
	void TestGetComponent(void);

};

#endif // TESTINTERFACE_H_INCLUDED
