#include "TestCVar.h"


TestCVar::TestCVar()
{
}

TestCVar::~TestCVar()
{
}

void TestCVar::TestCVarCtor()
{
	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
}
