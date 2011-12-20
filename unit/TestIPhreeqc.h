#if !defined(TESTIPHREEQC_H_INCLUDED)
#define TESTIPHREEQC_H_INCLUDED

#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/TestFixture.h>

class TestIPhreeqc : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE( TestIPhreeqc );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestLoadDatabase );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestLoadDatabaseString );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestLoadDatabaseMissingFile );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestLoadDatabaseWithErrors );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestRunAccumulated );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestRunWithErrors );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestRunFile );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestRunString );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestGetSelectedOutputRowCount );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestGetSelectedOutputValue );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestGetSelectedOutputColumnCount );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestAddError );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestAccumulateLine );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestOutputErrorString );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestRunWithCallback );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestRunNoDatabaseLoaded );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestCase1 );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestCase2 );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestPrintSelectedOutputFalse );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestOutputOnOff );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestErrorOnOff );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestLogOnOff );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestDumpOnOff );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestSelOutOnOff );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestLongHeadings );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestDatabaseKeyword );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestDumpString );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestGetDumpStringLineCount );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestGetDumpStringLine );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestGetComponentCount );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestGetComponent );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestListComponents );
// COMMENT: {12/19/2011 11:06:36 PM}	CPPUNIT_TEST( TestSetDumpFileName );
	CPPUNIT_TEST( TestSetOutputFileName );
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
	void TestDumpOnOff(void);
	void TestSelOutOnOff(void);
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
};

#endif // TESTIPHREEQC_H_INCLUDED
