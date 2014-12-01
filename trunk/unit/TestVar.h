#if !defined(TESTVAR_H_INCLUDED)
#define TESTVAR_H_INCLUDED

#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/TestFixture.h>


class TestVar : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE( TestVar );
	CPPUNIT_TEST( TestVarInit );
	CPPUNIT_TEST_SUITE_END();

public:
	TestVar(void);
	~TestVar(void);

public:
	void TestVarInit();
};

#endif // TESTVAR_H_INCLUDED
