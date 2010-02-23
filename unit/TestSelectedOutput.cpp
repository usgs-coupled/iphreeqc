#include "TestSelectedOutput.h"


#include "../src/phreeqcns.hxx"
#undef free

#if defined(_WIN32)
#define strdup _strdup
#endif

int EndRow(void);


TestSelectedOutput::TestSelectedOutput()
{
}

TestSelectedOutput::~TestSelectedOutput()
{
}

void
TestSelectedOutput::TestEmpty()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());
}

void
TestSelectedOutput::TestSinglePushBack()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CVar v(7.0);
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBack("pH", v));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	// row count doesn't change until EndRow is called
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetRowCount());
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());
#if defined(_DEBUG)
	CSelectedOutput::Instance()->Dump("TestSinglePushBack");
#endif
}

void
TestSelectedOutput::TestMultiplePushBack()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CVar v1(7.0);
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBack("pH", v1));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v2(8.0);
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBack("pH", v2));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(3u, CSelectedOutput::Instance()->GetRowCount());
#if defined(_DEBUG)
	CSelectedOutput::Instance()->Dump("TestMultiplePushBack");
#endif
}

void 
TestSelectedOutput::TestNewHeadingsPushBack()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CVar v1(7.0);
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBack("pH", v1));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v2(8.0);
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBack("pH", v2));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v3(9.0);
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBack("user_pH", v3));

	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());


	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(3u, CSelectedOutput::Instance()->GetRowCount());
#if defined(_DEBUG)
	CSelectedOutput::Instance()->Dump("TestNewHeadingsPushBack");
#endif
}

void
TestSelectedOutput::TestPushBackDouble()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackDouble("pH", 7.0));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetRowCount()); // heading

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("pH"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval.type);
	CPPUNIT_ASSERT_EQUAL(7.0, vval.dVal);
}

void
TestSelectedOutput::TestPushBackLong()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackLong("Sim", 2));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetRowCount()); // heading plus first row

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("Sim"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, vval.type);
	CPPUNIT_ASSERT_EQUAL(2l, vval.lVal);
}

void
TestSelectedOutput::TestPushBackString()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackString("state", "i_soln"));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetRowCount()); // heading

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("state"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, vval.type);
	CPPUNIT_ASSERT_EQUAL(std::string("i_soln"), std::string(vval.sVal));
}

void
TestSelectedOutput::TestPushBackEmpty()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackEmpty("Empty"));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetRowCount()); // heading

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("Empty"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
}

void
TestSelectedOutput::TestDuplicateHeadings()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackDouble("pH", 7.0));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetRowCount()); // heading

	// overwrite pH with 8.0
	//
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackDouble("pH", 8.0));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetRowCount()); // heading

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("pH"), std::string(v.sVal));


	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval.type);
	CPPUNIT_ASSERT_EQUAL(8.0, vval.dVal);
}

void
TestSelectedOutput::TestEndRow()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackDouble("pH", 7.0));

	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetRowCount()); // heading

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("pH"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval.type);
	CPPUNIT_ASSERT_EQUAL(7.0, vval.dVal);

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackDouble("pH", 8.0));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(3u, CSelectedOutput::Instance()->GetRowCount());

	CVar vval3;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval3.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &vval3));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval3.type);
	CPPUNIT_ASSERT_EQUAL(7.0, vval3.dVal);

	CVar vval2;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval2.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(2, 0, &vval2));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval2.type);
	CPPUNIT_ASSERT_EQUAL(8.0, vval2.dVal);
}

void
TestSelectedOutput::TestEndRow2()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackDouble("pH", 6.0));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackDouble("pH", 7.0));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackDouble("pH", 8.0));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackDouble("pH", 9.0));

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("pH"), std::string(v.sVal));

	CVar vval;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &vval));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval.type);
	CPPUNIT_ASSERT_EQUAL(9.0, vval.dVal);   // dups get overwritten

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackDouble("pH", 8.0));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(3u, CSelectedOutput::Instance()->GetRowCount());

	CVar vval3;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval3.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &vval3));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval3.type);
	CPPUNIT_ASSERT_EQUAL(9.0, vval3.dVal);

	CVar vval2;
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, vval2.type);
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(2, 0, &vval2));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, vval2.type);
	CPPUNIT_ASSERT_EQUAL(8.0, vval2.dVal);
}


void
TestSelectedOutput::TestTooManyHeadings()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	// USER_PUNCH
	// -headings 1.name 1.type 1.moles

	n_user_punch_index  = 0;
	user_punch_headings = NULL;
	user_punch_count_headings = 0;

	user_punch_headings = (char**)::realloc(user_punch_headings, (size_t) (user_punch_count_headings + 1) * sizeof(char *));
	user_punch_headings[user_punch_count_headings] = ::strdup("1.name");
	user_punch_count_headings++;

	user_punch_headings = (char**)::realloc(user_punch_headings, (size_t) (user_punch_count_headings + 1) * sizeof(char *));
	user_punch_headings[user_punch_count_headings] = ::strdup("1.type");
	user_punch_count_headings++;

	user_punch_headings = (char**)::realloc(user_punch_headings, (size_t) (user_punch_count_headings + 1) * sizeof(char *));
	user_punch_headings[user_punch_count_headings] = ::strdup("1.moles");
	user_punch_count_headings++;

	CPPUNIT_ASSERT_EQUAL(0, ::EndRow());
	CPPUNIT_ASSERT_EQUAL(3u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());
#if defined(_DEBUG)
	CSelectedOutput::Instance()->Dump("TestTooManyHeadings");
#endif

	// clean up headings
	//
	for (int i = 0; i < user_punch_count_headings; ++i) {
		::free(user_punch_headings[i]);
	}
	::free(user_punch_headings);
	user_punch_headings = NULL;
	user_punch_count_headings = 0;

	CVar head0, head1, head2;
	CVar val0, val1, val2;

	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &head0));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 1, &head1));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 2, &head2));

	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &val0));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 1, &val1));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 2, &val2));

	CPPUNIT_ASSERT_EQUAL(TT_STRING, head0.type);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, head1.type);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, head2.type);

	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, val0.type);
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, val1.type);
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, val2.type);

	CPPUNIT_ASSERT_EQUAL(std::string("1.name"), std::string(head0.sVal));
	CPPUNIT_ASSERT_EQUAL(std::string("1.type"), std::string(head1.sVal));
	CPPUNIT_ASSERT_EQUAL(std::string("1.moles"), std::string(head2.sVal));


	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackLong("sim", 1));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackString("state", "i_soln"));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackLong("soln", 22));

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(6u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(3u, CSelectedOutput::Instance()->GetRowCount());
#if defined(_DEBUG)
	CSelectedOutput::Instance()->Dump("TestTooManyHeadings");
#endif
}

void
TestSelectedOutput::TestNotEnoughHeadings()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	// USER_PUNCH
	// -headings 1.name 1.type 1.moles

	n_user_punch_index  = 0;
	user_punch_headings = NULL;
	user_punch_count_headings = 0;

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackLong("sim", 1));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackString("state", "i_soln"));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackLong("soln", 22));

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(3u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());
#if defined(_DEBUG)
	CSelectedOutput::Instance()->Dump("TestNotEnoughHeadings");
#endif

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackLong("sim", 2));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackString("state", "react"));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackLong("soln", 23));

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackEmpty("no_heading_1"));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackEmpty("no_heading_2"));
	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->PushBackEmpty("no_heading_3"));

#if defined(_DEBUG)
	CSelectedOutput::Instance()->Dump("TestNotEnoughHeadings");
#endif

	CPPUNIT_ASSERT_EQUAL(0, CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(6u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(3u, CSelectedOutput::Instance()->GetRowCount());

	CVar head0, head1, head2, head3, head4, head5;
	CVar val0, val1, val2, val3, val4, val5;

	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &head0));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 1, &head1));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 2, &head2));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 3, &head3));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 4, &head4));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 5, &head5));

	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &val0));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 1, &val1));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 2, &val2));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 3, &val3));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 4, &val4));
	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 5, &val5));

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
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, CSelectedOutput::Instance()->Get(0, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);


	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, CSelectedOutput::Instance()->Get(-1, -1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);

	CPPUNIT_ASSERT_EQUAL(0,  CSelectedOutput::Instance()->PushBackEmpty("heading"));
	CPPUNIT_ASSERT_EQUAL(0,  CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("heading"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);


	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, CSelectedOutput::Instance()->Get(2, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);
}

void
TestSelectedOutput::TestInvalidCol()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, CSelectedOutput::Instance()->Get(0, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);


	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, CSelectedOutput::Instance()->Get(-1, -1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);

	CPPUNIT_ASSERT_EQUAL(0,  CSelectedOutput::Instance()->PushBackEmpty("heading"));
	CPPUNIT_ASSERT_EQUAL(0,  CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(0, 0, &v) );
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("heading"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, CSelectedOutput::Instance()->Get(1, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v.type);


	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, CSelectedOutput::Instance()->Get(0, 1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, v.vresult);

	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, CSelectedOutput::Instance()->Get(0, -1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, v.vresult);
}

void
TestSelectedOutput::TestGet()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0,  CSelectedOutput::Instance()->PushBackEmpty("heading"));
	CPPUNIT_ASSERT_EQUAL(0,  CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());


	CVar v0 = CSelectedOutput::Instance()->Get(0, 0);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v0.type);
	CPPUNIT_ASSERT_EQUAL(std::string("heading"), std::string(v0.sVal));

	CVar v1 = CSelectedOutput::Instance()->Get(1, 0);
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v1.type);
}

void
TestSelectedOutput::TestLongHeadings()
{
	CSelectedOutput::Instance()->Clear();
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(0u, CSelectedOutput::Instance()->GetRowCount());

	CPPUNIT_ASSERT_EQUAL(0,  CSelectedOutput::Instance()->PushBackEmpty("heading890123456789012345678901234567890123456789"));
	CPPUNIT_ASSERT_EQUAL(0,  CSelectedOutput::Instance()->EndRow());
	CPPUNIT_ASSERT_EQUAL(1u, CSelectedOutput::Instance()->GetColCount());
	CPPUNIT_ASSERT_EQUAL(2u, CSelectedOutput::Instance()->GetRowCount());

	CVar v0 = CSelectedOutput::Instance()->Get(0, 0);
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v0.type);
	CPPUNIT_ASSERT_EQUAL(std::string("heading890123456789012345678901234567890123456789"), std::string(v0.sVal));

	CVar v1 = CSelectedOutput::Instance()->Get(1, 0);
	CPPUNIT_ASSERT_EQUAL(TT_EMPTY, v1.type);
}
