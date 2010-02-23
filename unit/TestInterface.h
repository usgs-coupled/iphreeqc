#if !defined(TESTINTERFACE_H_INCLUDED)
#define TESTINTERFACE_H_INCLUDED

#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/TestFixture.h>


class TestInterface :
	public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE( TestInterface );
	CPPUNIT_TEST( TestLoadDatabase );
	CPPUNIT_TEST( TestLoadDatabaseMissingFile );
	CPPUNIT_TEST( TestLoadDatabaseWithErrors );
	CPPUNIT_TEST( TestRun );
	CPPUNIT_TEST( TestRunWithErrors );
	CPPUNIT_TEST( TestRunFile );
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

	CPPUNIT_TEST_SUITE_END();

public:
	TestInterface(void);
	~TestInterface(void);

public:
	void TestLoadDatabase();
	void TestLoadDatabaseMissingFile();
	void TestLoadDatabaseWithErrors();
	void TestRun();
	void TestRunWithErrors();
	void TestRunFile();
	void TestGetSelectedOutputRowCount();
	void TestGetSelectedOutputValue();
	void TestGetSelectedOutputColumnCount();
	void TestAddError();
	void TestAccumulateLine();
	void TestOutputLastError();
	void TestRunWithCallback();
	void TestRunNoDatabaseLoaded();
	void TestRunFileNoDatabaseLoaded();
	void TestCase1();
	void TestCase2();
	void TestPrintSelectedOutputFalse();
	void TestOutputOnOff();
	void TestErrorOnOff();
	void TestLogOnOff();
	void TestSelOutOnOff();
	void TestLongHeadings();

};

#endif // TESTINTERFACE_H_INCLUDED
