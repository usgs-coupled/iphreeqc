#if !defined(TESTSELECTEDOUTPUT_H_INCLUDED)
#define TESTSELECTEDOUTPUT_H_INCLUDED

#if defined(_DEBUG)
#pragma warning(disable : 4786)   // disable truncation warning
#endif

#include "../src/CSelectedOutput.hxx"
#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/TestFixture.h>


class TestSelectedOutput :
	public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE( TestSelectedOutput );
	CPPUNIT_TEST( TestEmpty );
	CPPUNIT_TEST( TestSinglePushBack );
	CPPUNIT_TEST( TestMultiplePushBack );
	CPPUNIT_TEST( TestNewHeadingsPushBack );
	CPPUNIT_TEST( TestPushBackDouble );
	CPPUNIT_TEST( TestPushBackLong );
	CPPUNIT_TEST( TestPushBackString );
	CPPUNIT_TEST( TestPushBackEmpty );
	CPPUNIT_TEST( TestDuplicateHeadings );
	CPPUNIT_TEST( TestEndRow );
	CPPUNIT_TEST( TestEndRow2 );
	CPPUNIT_TEST( TestTooManyHeadings );
	CPPUNIT_TEST( TestNotEnoughHeadings );
	CPPUNIT_TEST( TestInvalidRow );
	CPPUNIT_TEST( TestInvalidCol );
	CPPUNIT_TEST( TestGet );
	CPPUNIT_TEST( TestLongHeadings );
	CPPUNIT_TEST_SUITE_END();

public:
	TestSelectedOutput(void);
	~TestSelectedOutput(void);

public:
	void TestEmpty();
	void TestSinglePushBack();
	void TestMultiplePushBack();
	void TestNewHeadingsPushBack();
	void TestPushBackDouble();
	void TestPushBackLong();
	void TestPushBackString();
	void TestPushBackEmpty();
	void TestDuplicateHeadings();
	void TestEndRow();
	void TestEndRow2();
	void TestTooManyHeadings();
	void TestNotEnoughHeadings();
	void TestInvalidRow();
	void TestInvalidCol();
	void TestGet();
	void TestLongHeadings();
};

#endif // TESTSELECTEDOUTPUT_H_INCLUDED
