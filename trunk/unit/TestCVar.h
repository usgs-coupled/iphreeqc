#if !defined(TESTCVAR_H_INCLUDED)
#define TESTCVAR_H_INCLUDED

#include "../src/CVar.hxx"
#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/TestFixture.h>


class TestCVar :
	public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE( TestCVar );
	CPPUNIT_TEST( TestCVarCtor );
	CPPUNIT_TEST_SUITE_END();

public:
	TestCVar(void);
	~TestCVar(void);

public:
	void TestCVarCtor();
};

#endif // TESTCVAR_H_INCLUDED
