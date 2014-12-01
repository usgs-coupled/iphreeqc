#include "TestSelectedOutput.h"

#include "IPhreeqc.hpp"
#include "Phreeqc.h"

#if defined(_WIN32)
#define strdup _strdup
#endif


TestSelectedOutput::TestSelectedOutput()
{
}

TestSelectedOutput::~TestSelectedOutput()
{
}

void
TestSelectedOutput::TestEmpty()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());
	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());
}

void
TestSelectedOutput::TestSinglePushBack()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());
	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CVar v(7.0);
	CPPUNIT_ASSERT_EQUAL(0, co.PushBack("pH", v));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	// row count doesn't change until EndRow is called
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetRowCount());
	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());
#if defined(_DEBUG)
	co.Dump("TestSinglePushBack");
#endif
}

void
TestSelectedOutput::TestMultiplePushBack()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());
	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CVar v1(7.0);
	CPPUNIT_ASSERT_EQUAL(0, co.PushBack("pH", v1));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v2(8.0);
	CPPUNIT_ASSERT_EQUAL(0, co.PushBack("pH", v2));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)3, co.GetRowCount());
#if defined(_DEBUG)
	co.Dump("TestMultiplePushBack");
#endif
}

void 
TestSelectedOutput::TestNewHeadingsPushBack()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CVar v1(7.0);
	CPPUNIT_ASSERT_EQUAL(0, co.PushBack("pH", v1));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v2(8.0);
	CPPUNIT_ASSERT_EQUAL(0, co.PushBack("pH", v2));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v3(9.0);
	CPPUNIT_ASSERT_EQUAL(0, co.PushBack("user_pH", v3));

	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());


	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)3, co.GetRowCount());
#if defined(_DEBUG)
	co.Dump("TestNewHeadingsPushBack");
#endif
}

void
TestSelectedOutput::TestPushBackDouble()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackDouble("pH", 7.0));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetRowCount()); // heading

	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("pH"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval.type);
	CPPUNIT_ASSERT_EQUAL(7.0, vval.dVal);
}

void
TestSelectedOutput::TestPushBackLong()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackLong("Sim", 2));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetRowCount()); // heading plus first row

	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("Sim"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, vval.type);
	CPPUNIT_ASSERT_EQUAL(2l, vval.lVal);
}

void
TestSelectedOutput::TestPushBackString()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackString("state", "i_soln"));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetRowCount()); // heading

	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("state"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, vval.type);
	CPPUNIT_ASSERT_EQUAL(std::string("i_soln"), std::string(vval.sVal));
}

void
TestSelectedOutput::TestPushBackEmpty()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackEmpty("Empty"));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetRowCount()); // heading

	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("Empty"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
}

void
TestSelectedOutput::TestDuplicateHeadings()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackDouble("pH", 7.0));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetRowCount()); // heading

	// overwrite pH with 8.0
	//
	CPPUNIT_ASSERT_EQUAL(0, co.PushBackDouble("pH", 8.0));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetRowCount()); // heading

	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("pH"), std::string(v.sVal));


	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval.type);
	CPPUNIT_ASSERT_EQUAL(8.0, vval.dVal);
}

void
TestSelectedOutput::TestEndRow()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackDouble("pH", 7.0));

	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetRowCount()); // heading

	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("pH"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval.type);
	CPPUNIT_ASSERT_EQUAL(7.0, vval.dVal);

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackDouble("pH", 8.0));
	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)3, co.GetRowCount());

	CVar vval3;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval3.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &vval3));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval3.type);
	CPPUNIT_ASSERT_EQUAL(7.0, vval3.dVal);

	CVar vval2;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval2.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(2, 0, &vval2));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval2.type);
	CPPUNIT_ASSERT_EQUAL(8.0, vval2.dVal);
}

void
TestSelectedOutput::TestEndRow2()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackDouble("pH", 6.0));
	CPPUNIT_ASSERT_EQUAL(0, co.PushBackDouble("pH", 7.0));
	CPPUNIT_ASSERT_EQUAL(0, co.PushBackDouble("pH", 8.0));
	CPPUNIT_ASSERT_EQUAL(0, co.PushBackDouble("pH", 9.0));

	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("pH"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval.type);
	CPPUNIT_ASSERT_EQUAL(9.0, vval.dVal);   // dups get overwritten

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackDouble("pH", 8.0));
	CPPUNIT_ASSERT_EQUAL(0, co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)3, co.GetRowCount());

	CVar vval3;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval3.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &vval3));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval3.type);
	CPPUNIT_ASSERT_EQUAL(9.0, vval3.dVal);

	CVar vval2;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval2.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(2, 0, &vval2));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval2.type);
	CPPUNIT_ASSERT_EQUAL(8.0, vval2.dVal);
}


void
TestSelectedOutput::TestTooManyHeadings()
{
// COMMENT: {8/26/2013 4:12:03 PM}	IPhreeqc p;
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL((size_t)0, p.PtrSelectedOutput->GetColCount());
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL((size_t)0, p.PtrSelectedOutput->GetRowCount());
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	p.PtrSelectedOutput->Clear();
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL((size_t)0, p.PtrSelectedOutput->GetColCount());
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL((size_t)0, p.PtrSelectedOutput->GetRowCount());
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	// USER_PUNCH
// COMMENT: {8/26/2013 4:12:03 PM}	// -headings 1.name 1.type 1.moles
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	p.PhreeqcPtr->n_user_punch_index        = 0;
// COMMENT: {8/26/2013 4:12:03 PM}	p.PhreeqcPtr->UserPunch_map[1]          = UserPunch();
// COMMENT: {8/26/2013 4:12:03 PM}	p.PhreeqcPtr->current_user_punch        = &(p.PhreeqcPtr->UserPunch_map[1]);
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	std::vector< std::string > headings;
// COMMENT: {8/26/2013 4:12:03 PM}	headings.push_back("1.name");
// COMMENT: {8/26/2013 4:12:03 PM}	headings.push_back("1.type");
// COMMENT: {8/26/2013 4:12:03 PM}	headings.push_back("1.moles");
// COMMENT: {8/26/2013 4:12:03 PM}	p.PhreeqcPtr->UserPunch_map[1].Set_headings(headings);
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(0,   p.EndRow());
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL((size_t)3, p.PtrSelectedOutput->GetColCount());
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL((size_t)2, p.PtrSelectedOutput->GetRowCount());
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}#if defined(_DEBUG)
// COMMENT: {8/26/2013 4:12:03 PM}	p.PtrSelectedOutput->Dump("TestTooManyHeadings");
// COMMENT: {8/26/2013 4:12:03 PM}#endif
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	// clean up headings
// COMMENT: {8/26/2013 4:12:03 PM}	p.PhreeqcPtr->UserPunch_map[1].Get_headings().empty();
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	CVar head0, head1, head2;
// COMMENT: {8/26/2013 4:12:03 PM}	CVar val0, val1, val2;
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(VR_OK, p.PtrSelectedOutput->Get(0, 0, &head0));
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(VR_OK, p.PtrSelectedOutput->Get(0, 1, &head1));
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(VR_OK, p.PtrSelectedOutput->Get(0, 2, &head2));
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(VR_OK, p.PtrSelectedOutput->Get(1, 0, &val0));
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(VR_OK, p.PtrSelectedOutput->Get(1, 1, &val1));
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(VR_OK, p.PtrSelectedOutput->Get(1, 2, &val2));
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(TT_STRING, head0.type);
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(TT_STRING, head1.type);
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(TT_STRING, head2.type);
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, val0.type);
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, val1.type);
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, val2.type);
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(std::string("1.name"), std::string(head0.sVal));
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(std::string("1.type"), std::string(head1.sVal));
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(std::string("1.moles"), std::string(head2.sVal));
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(0, p.PtrSelectedOutput->PushBackLong("sim", 1));
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(0, p.PtrSelectedOutput->PushBackString("state", "i_soln"));
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(0, p.PtrSelectedOutput->PushBackLong("soln", 22));
// COMMENT: {8/26/2013 4:12:03 PM}
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL(0,  p.PtrSelectedOutput->EndRow());
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL((size_t)6, p.PtrSelectedOutput->GetColCount());
// COMMENT: {8/26/2013 4:12:03 PM}	CPPUNIT_ASSERT_EQUAL((size_t)3, p.PtrSelectedOutput->GetRowCount());
// COMMENT: {8/26/2013 4:12:03 PM}#if defined(_DEBUG)
// COMMENT: {8/26/2013 4:12:03 PM}	p.PtrSelectedOutput->Dump("TestTooManyHeadings");
// COMMENT: {8/26/2013 4:12:03 PM}#endif
}

void
TestSelectedOutput::TestNotEnoughHeadings()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	// USER_PUNCH
	// -headings 1.name 1.type 1.moles

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackLong("sim", 1));
	CPPUNIT_ASSERT_EQUAL(0, co.PushBackString("state", "i_soln"));
	CPPUNIT_ASSERT_EQUAL(0, co.PushBackLong("soln", 22));

	CPPUNIT_ASSERT_EQUAL(0,  co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)3, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());
#if defined(_DEBUG)
	co.Dump("TestNotEnoughHeadings");
#endif

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackLong("sim", 2));
	CPPUNIT_ASSERT_EQUAL(0, co.PushBackString("state", "react"));
	CPPUNIT_ASSERT_EQUAL(0, co.PushBackLong("soln", 23));

	CPPUNIT_ASSERT_EQUAL(0, co.PushBackEmpty("no_heading_1"));
	CPPUNIT_ASSERT_EQUAL(0, co.PushBackEmpty("no_heading_2"));
	CPPUNIT_ASSERT_EQUAL(0, co.PushBackEmpty("no_heading_3"));

#if defined(_DEBUG)
	co.Dump("TestNotEnoughHeadings");
#endif

	CPPUNIT_ASSERT_EQUAL(0,  co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)6, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)3, co.GetRowCount());

	CVar head0, head1, head2, head3, head4, head5;
	CVar val0, val1, val2, val3, val4, val5;

	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 0, &head0));
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 1, &head1));
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 2, &head2));
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 3, &head3));
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 4, &head4));
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 5, &head5));

	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &val0));
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 1, &val1));
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 2, &val2));
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 3, &val3));
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 4, &val4));
	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 5, &val5));

	CPPUNIT_ASSERT_EQUAL(TT_STRING, head0.type);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, head1.type);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, head2.type);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, head3.type);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, head4.type);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, head5.type);

	CPPUNIT_ASSERT_EQUAL(TT_LONG,   val0.type);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, val1.type);
	CPPUNIT_ASSERT_EQUAL(TT_LONG,   val2.type);
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  val3.type);
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  val4.type);
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  val5.type);

	CPPUNIT_ASSERT_EQUAL(std::string("sim"),          std::string(head0.sVal));
	CPPUNIT_ASSERT_EQUAL(std::string("state"),        std::string(head1.sVal));
	CPPUNIT_ASSERT_EQUAL(std::string("soln"),         std::string(head2.sVal));
	CPPUNIT_ASSERT_EQUAL(std::string("no_heading_1"), std::string(head3.sVal));
	CPPUNIT_ASSERT_EQUAL(std::string("no_heading_2"), std::string(head4.sVal));
	CPPUNIT_ASSERT_EQUAL(std::string("no_heading_3"), std::string(head5.sVal));

	CPPUNIT_ASSERT_EQUAL(1l, val0.lVal);
	CPPUNIT_ASSERT_EQUAL(std::string("i_soln"), std::string(val1.sVal));
	CPPUNIT_ASSERT_EQUAL(22l, val2.lVal);
}

void
TestSelectedOutput::TestInvalidRow()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, co.Get(0, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);


	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, co.Get(-1, -1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);

	CPPUNIT_ASSERT_EQUAL(0,  co.PushBackEmpty("heading"));
	CPPUNIT_ASSERT_EQUAL(0,  co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("heading"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);


	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, co.Get(2, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);
}

void
TestSelectedOutput::TestInvalidCol()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, co.Get(0, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);


	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, co.Get(-1, -1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);

	CPPUNIT_ASSERT_EQUAL(0,  co.PushBackEmpty("heading"));
	CPPUNIT_ASSERT_EQUAL(0,  co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("heading"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, co.Get(1, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);


	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, co.Get(0, 1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, v.vresult);

	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, co.Get(0, -1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, v.vresult);
}

void
TestSelectedOutput::TestGet()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0,  co.PushBackEmpty("heading"));
	CPPUNIT_ASSERT_EQUAL(0,  co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());


	CVar v0 = co.Get(0, 0);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v0.type);
	CPPUNIT_ASSERT_EQUAL(std::string("heading"), std::string(v0.sVal));

	CVar v1 = co.Get(1, 0);
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v1.type);
}

void
TestSelectedOutput::TestLongHeadings()
{
	CSelectedOutput co;
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	co.Clear();
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)0, co.GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0,  co.PushBackEmpty("heading890123456789012345678901234567890123456789"));
	CPPUNIT_ASSERT_EQUAL(0,  co.EndRow());
	CPPUNIT_ASSERT_EQUAL((size_t)1, co.GetColCount());
	CPPUNIT_ASSERT_EQUAL((size_t)2, co.GetRowCount());

	CVar v0 = co.Get(0, 0);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v0.type);
	CPPUNIT_ASSERT_EQUAL(std::string("heading890123456789012345678901234567890123456789"), std::string(v0.sVal));

	CVar v1 = co.Get(1, 0);
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v1.type);
}
