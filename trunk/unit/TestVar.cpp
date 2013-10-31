#include "TestVar.h"
#include "Var.h"

TestVar::TestVar()
{
}

TestVar::~TestVar()
{
}

void TestVar::TestVarInit()
{
	VAR v;
	::VarInit(&v);
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
}
