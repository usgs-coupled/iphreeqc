#include "TestIPhreeqc.h"

#include <cmath>
#include <cfloat>
#include <cassert>
#include "IPhreeqc.hpp"
#include "Phreeqc.h"
#undef true
#undef false
#include "CVar.hxx"

#if defined(_WIN32) || defined(__CYGWIN32__)
// DeleteFile defined in <windows.h>
#else
int DeleteFile(const char* szPathName);
#endif

bool FileExists(const char *szPathName);
size_t FileSize(const char *szPathName);

VRESULT SOLUTION(IPhreeqc& obj, double C, double Ca, double Na);
VRESULT EQUILIBRIUM_PHASES(IPhreeqc& obj, const char* phase, double si, double amount);
VRESULT USER_PUNCH(IPhreeqc& obj, const char* element, int max);
VRESULT SELECTED_OUTPUT(IPhreeqc& obj);
VRESULT DUMP(IPhreeqc& obj);

TestIPhreeqc::TestIPhreeqc(void)
{
}

TestIPhreeqc::~TestIPhreeqc(void)
{
}

void TestIPhreeqc::TestLoadDatabase(void)
{
	IPhreeqc obj;
	for (int i = 0; i < 10; ++i)
	{
		CPPUNIT_ASSERT_EQUAL(true, ::FileExists("phreeqc.dat"));
		CPPUNIT_ASSERT(::FileSize("phreeqc.dat") > 0);
		CPPUNIT_ASSERT_EQUAL(0,    obj.LoadDatabase("phreeqc.dat"));
	}
}

void TestIPhreeqc::TestLoadDatabaseString(void)
{
	const char ex15_dat[] =
		"SOLUTION_MASTER_SPECIES\n"
		"C        CO2            2.0     61.0173         12.0111\n"
		"Cl       Cl-            0.0     Cl              35.453\n"
		"Co       Co+2           0.0     58.93           58.93   \n"
		"E        e-             0.0     0.0             0.0\n"
		"H        H+             -1.     1.008           1.008\n"
		"H(0)     H2             0.0     1.008\n"
		"H(1)     H+             -1.     1.008\n"
		"N        NH4+           0.0     14.0067         14.0067\n"
		"Na       Na+            0.0     Na              22.9898\n"
		"Nta      Nta-3          3.0     1.              1.\n"
		"O        H2O            0.0     16.00           16.00\n"
		"O(-2)    H2O            0.0     18.016\n"
		"O(0)     O2             0.0     16.00\n"
		"SOLUTION_SPECIES\n"
		"2H2O = O2 + 4H+ + 4e- \n"
		"        log_k   -86.08; -gamma  1e7   0.0\n"
		"2 H+ + 2 e- = H2\n"
		"        log_k   -3.15;  -gamma  1e7   0.0\n"
		"H+ = H+\n"
		"        log_k   0.0;    -gamma  1e7   0.0\n"
		"e- = e-\n"
		"        log_k   0.0;    -gamma  1e7   0.0\n"
		"H2O = H2O\n"
		"        log_k   0.0;    -gamma  1e7   0.0\n"
		"CO2 = CO2\n"
		"        log_k   0.0;    -gamma  1e7   0.0\n"
		"Na+ = Na+\n"
		"        log_k   0.0;    -gamma  1e7   0.0\n"
		"Cl- = Cl-\n"
		"        log_k   0.0;    -gamma  1e7   0.0\n"
		"Co+2 = Co+2\n"
		"        log_k   0.0;    -gamma  1e7   0.0\n"
		"NH4+ = NH4+\n"
		"        log_k   0.0;    -gamma  1e7   0.0\n"
		"Nta-3 = Nta-3\n"
		"        log_k   0.0;    -gamma  1e7   0.0\n"
		"Nta-3 + 3H+ = H3Nta\n"
		"        log_k   14.9;   -gamma  1e7   0.0\n"
		"Nta-3 + 2H+ = H2Nta-\n"
		"        log_k   13.3;   -gamma  1e7   0.0\n"
		"Nta-3 + H+ = HNta-2\n"
		"        log_k   10.3;   -gamma  1e7   0.0\n"
		"Nta-3 + Co+2 = CoNta-\n"
		"        log_k   11.7;   -gamma  1e7   0.0\n"
		"2 Nta-3 + Co+2 = CoNta2-4\n"
		"        log_k   14.5;   -gamma  1e7   0.0\n"
		"Nta-3 + Co+2 + H2O = CoOHNta-2 + H+\n"
		"        log_k   0.5;    -gamma  1e7   0.0\n"
		"Co+2 + H2O = CoOH+ + H+\n"
		"        log_k   -9.7;   -gamma  1e7   0.0\n"
		"Co+2 + 2H2O = Co(OH)2 + 2H+\n"
		"        log_k   -22.9;  -gamma  1e7   0.0\n"
		"Co+2 + 3H2O = Co(OH)3- + 3H+\n"
		"        log_k   -31.5;  -gamma  1e7   0.0\n"
		"CO2 + H2O = HCO3- + H+\n"
		"        log_k   -6.35;  -gamma  1e7   0.0\n"
		"CO2 + H2O = CO3-2 + 2H+\n"
		"        log_k   -16.68; -gamma  1e7   0.0\n"
		"NH4+ = NH3 + H+\n"
		"        log_k   -9.3;   -gamma  1e7   0.0\n"
		"H2O = OH- +  H+\n"
		"        log_k   -14.0;  -gamma  1e7   0.0\n"
		"END\n";

	IPhreeqc obj;
	CPPUNIT_ASSERT_EQUAL(0, obj.LoadDatabaseString(ex15_dat));
}

void TestIPhreeqc::TestLoadDatabaseMissingFile(void)
{
	CPPUNIT_ASSERT_EQUAL(false, ::FileExists("missing.file"));

	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL(1,     obj.LoadDatabase("missing.file"));
	CPPUNIT_ASSERT_EQUAL(1,     obj.LoadDatabase("missing.file"));
	CPPUNIT_ASSERT_EQUAL(1,     obj.LoadDatabase("missing.file"));

	const char expected[] =
		"ERROR: LoadDatabase: Unable to open:\"missing.file\".\n";

	const char* err = obj.GetErrorString();
	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );
}

void TestIPhreeqc::TestLoadDatabaseWithErrors(void)
{
#if defined(_WIN32)
	int n0 = ::_fcloseall();
	assert(n0 == 0);
#endif

	IPhreeqc obj;

	for (int i = 0; i < 10; ++i)
	{
		CPPUNIT_ASSERT_EQUAL(true, ::FileExists("missing_e.dat"));
		CPPUNIT_ASSERT(::FileSize("missing_e.dat") > 0);
		CPPUNIT_ASSERT_EQUAL(6,    obj.LoadDatabase("missing_e.dat"));

		const char *expected =
			"ERROR: Could not reduce equation to primary master species, CH4.\n"
			"ERROR: Could not reduce equation to primary master species, Cu+.\n"
			"ERROR: Could not reduce equation to primary master species, Fe+3.\n"
			"ERROR: Could not reduce equation to primary master species, H2.\n"
			"ERROR: Could not reduce equation to primary master species, Mn+3.\n"
			"ERROR: Could not reduce equation to primary master species, NH4+.\n"
			"ERROR: Could not reduce equation to primary master species, N2.\n"
			"ERROR: Could not reduce equation to primary master species, NO2-.\n"
			"ERROR: Could not reduce equation to primary master species, O2.\n"
			"ERROR: Could not reduce equation to primary master species, HS-.\n"
			"ERROR: Could not reduce equation to secondary master species, e-.\n"
			"ERROR: Non-master species in secondary reaction, e-.\n"
			"ERROR: No master species for element e.\n"
			"ERROR: Could not find primary master species for e.\n"
			"ERROR: No master species for element e.\n"
			"ERROR: Could not reduce equation to secondary master species, Hausmannite.\n"
			"ERROR: Could not reduce equation to secondary master species, Manganite.\n"
			"ERROR: Could not reduce equation to secondary master species, Pyrite.\n"
			"ERROR: Could not reduce equation to secondary master species, Pyrolusite.\n"
			"ERROR: Could not reduce equation to secondary master species, Sulfur.\n"
			"ERROR: e-, primary master species for E-, not defined.\n"
			"ERROR: Calculations terminating due to input errors.\n";

		const char* err = obj.GetErrorString();
		CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );
	}
#if defined(_WIN32)
	int n = ::_fcloseall();
	assert(n == 0);
#endif
}

void TestIPhreeqc::TestRunAccumulated(void)
{
#if defined(_WIN32)
	int n = ::_fcloseall();
	assert(n == 0);
#endif

	bool files_on = false;
	IPhreeqc obj;
	CPPUNIT_ASSERT_EQUAL(0,     obj.LoadDatabase("phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("solution 12"));
	obj.SetOutputFileOn(files_on);
	obj.SetErrorFileOn(files_on);
	obj.SetLogFileOn(files_on);
	obj.SetSelectedOutputFileOn(files_on);
	obj.SetDumpFileOn(files_on);
	CPPUNIT_ASSERT_EQUAL(0,     obj.RunAccumulated());
}

void TestIPhreeqc::TestRunWithErrors(void)
{
	const char dump_file[] = "error.inp";
	IPhreeqc obj;

	// remove dump file if it exists
	//
	if (::FileExists(dump_file))
	{
		CPPUNIT_ASSERT(::DeleteFile(dump_file));
	}

	bool files_on = false;
	CPPUNIT_ASSERT_EQUAL(0,     obj.LoadDatabase("phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("SOLUTION 1"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("	pH	7"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("	Na	1"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("PHASES"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("	Fix_H+"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("	H+ = H+"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("	log_k	0"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("EQUILIBRIUM_PHASES"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("	Fix_H+ -10 HCl	10"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.AccumulateLine("END"));

	obj.SetOutputFileOn(files_on);
	obj.SetErrorFileOn(files_on);
	obj.SetLogFileOn(files_on);
	obj.SetSelectedOutputFileOn(files_on);
	obj.SetDumpFileOn(files_on);
	CPPUNIT_ASSERT_EQUAL(1,     obj.RunAccumulated());

	const char expected[] =
		"ERROR: Numerical method failed on all combinations of convergence parameters\n";
	const char* err = obj.GetErrorString();

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );

	CPPUNIT_ASSERT_EQUAL( true, ::FileExists(dump_file) );
	CPPUNIT_ASSERT( ::FileSize(dump_file) > 0 );
	CPPUNIT_ASSERT( ::DeleteFile(dump_file) );
}

void TestIPhreeqc::TestRunFile(void)
{
	const char dump_file[] = "error.inp";

	// remove dump file if it exists
	//
	if (::FileExists(dump_file))
	{
		CPPUNIT_ASSERT(::DeleteFile(dump_file));
	}

	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL(0, obj.LoadDatabase("phreeqc.dat"));
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpFileOn(false);
	CPPUNIT_ASSERT_EQUAL(1, obj.RunFile("conv_fail.in"));

	const char expected[] =
		"ERROR: Numerical method failed on all combinations of convergence parameters\n";
	const char* err = obj.GetErrorString();

	CPPUNIT_ASSERT_EQUAL(std::string(expected), std::string(err));

	// Note: should this file exist since GetDumpFileOn is false?
	CPPUNIT_ASSERT_EQUAL( true, ::FileExists(dump_file) );
	CPPUNIT_ASSERT( ::FileSize(dump_file) > 0 );
	CPPUNIT_ASSERT( ::DeleteFile(dump_file) );
}

void TestIPhreeqc::TestRunString(void)
{
	const char input[] =
		"TITLE Example 1.--Add uranium and speciate seawater.\n"
		"SOLUTION 1  SEAWATER FROM NORDSTROM ET AL. (1979)\n"
		"        units   ppm\n"
		"        pH      8.22\n"
		"        pe      8.451\n"
		"        density 1.023\n"
		"        temp    25.0\n"
		"        redox   O(0)/O(-2)\n"
		"        Ca              412.3\n"
		"        Mg              1291.8\n"
		"        Na              10768.0\n"
		"        K               399.1\n"
		"        Fe              0.002\n"
		"        Mn              0.0002  pe\n"
		"        Si              4.28\n"
		"        Cl              19353.0\n"
		"        Alkalinity      141.682 as HCO3\n"
		"        S(6)            2712.0\n"
		"        N(5)            0.29    gfw   62.0\n"
		"        N(-3)           0.03    as    NH4\n"
		"        U               3.3     ppb   N(5)/N(-3)\n"
		"        O(0)            1.0     O2(g) -0.7\n"
		"SOLUTION_MASTER_SPECIES\n"
		"        U       U+4     0.0     238.0290     238.0290\n"
		"        U(4)    U+4     0.0     238.0290\n"
		"        U(5)    UO2+    0.0     238.0290\n"
		"        U(6)    UO2+2   0.0     238.0290\n"
		"SOLUTION_SPECIES\n"
		"        #primary master species for U\n"
		"        #is also secondary master species for U(4)\n"
		"        U+4 = U+4\n"
		"                log_k          0.0\n"
		"        U+4 + 4 H2O = U(OH)4 + 4 H+\n"
		"                log_k          -8.538\n"
		"                delta_h        24.760 kcal\n"
		"        U+4 + 5 H2O = U(OH)5- + 5 H+\n"
		"                log_k          -13.147\n"
		"                delta_h        27.580 kcal\n"
		"        #secondary master species for U(5)\n"
		"        U+4 + 2 H2O = UO2+ + 4 H+ + e-\n"
		"                log_k          -6.432\n"
		"                delta_h        31.130 kcal\n"
		"        #secondary master species for U(6)\n"
		"        U+4 + 2 H2O = UO2+2 + 4 H+ + 2 e-\n"
		"                log_k          -9.217\n"
		"                delta_h        34.430 kcal\n"
		"        UO2+2 + H2O = UO2OH+ + H+\n"
		"                log_k          -5.782\n"
		"                delta_h        11.015 kcal\n"
		"        2UO2+2 + 2H2O = (UO2)2(OH)2+2 + 2H+\n"
		"                log_k          -5.626\n"
		"                delta_h        -36.04 kcal\n"
		"        3UO2+2 + 5H2O = (UO2)3(OH)5+ + 5H+\n"
		"                log_k          -15.641\n"
		"                delta_h        -44.27 kcal\n"
		"        UO2+2 + CO3-2 = UO2CO3\n"
		"                log_k          10.064\n"
		"                delta_h        0.84 kcal\n"
		"        UO2+2 + 2CO3-2 = UO2(CO3)2-2\n"
		"                log_k          16.977\n"
		"                delta_h        3.48 kcal\n"
		"        UO2+2 + 3CO3-2 = UO2(CO3)3-4\n"
		"                log_k          21.397\n"
		"                delta_h        -8.78 kcal\n"
		"PHASES\n"
		"        Uraninite\n"
		"        UO2 + 4 H+ = U+4 + 2 H2O\n"
		"        log_k          -3.490\n"
		"        delta_h        -18.630 kcal\n"
		"END\n"
		"\n";

	IPhreeqc obj;

	char OUTPUT_FILE[80];
	sprintf(OUTPUT_FILE, "phreeqc.%d.out", obj.Index);

	if (::FileExists(OUTPUT_FILE))
	{
		CPPUNIT_ASSERT(::DeleteFile(OUTPUT_FILE));
	}
	CPPUNIT_ASSERT_EQUAL(false, ::FileExists(OUTPUT_FILE));
	CPPUNIT_ASSERT_EQUAL(0,     obj.LoadDatabase("phreeqc.dat"));
	obj.SetOutputFileOn(1);
	obj.SetErrorFileOn(0);
	obj.SetLogFileOn(0);
	obj.SetSelectedOutputFileOn(0);
	obj.SetDumpFileOn(0);
	CPPUNIT_ASSERT_EQUAL(false, ::FileExists(OUTPUT_FILE));
	CPPUNIT_ASSERT_EQUAL(0,     obj.RunString(input));
	CPPUNIT_ASSERT_EQUAL(true,  ::FileExists(OUTPUT_FILE));
	CPPUNIT_ASSERT(::FileSize(OUTPUT_FILE) > 0);
	if (::FileExists(OUTPUT_FILE))
	{
		CPPUNIT_ASSERT(::DeleteFile(OUTPUT_FILE));
	}
}

void TestIPhreeqc::TestGetSelectedOutputRowCount(void)
{
	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL(0,     obj.LoadDatabase("llnl.dat"));

	int max = 6;

	CPPUNIT_ASSERT_EQUAL(VR_OK, SOLUTION(obj, 1.0, 1.0, 1.0));
	CPPUNIT_ASSERT_EQUAL(VR_OK, EQUILIBRIUM_PHASES(obj, "calcite", 0.0, 0.010));
	CPPUNIT_ASSERT_EQUAL(VR_OK, USER_PUNCH(obj, "Ca", max));

	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpFileOn(false);
	CPPUNIT_ASSERT_EQUAL(0, obj.RunAccumulated());

	CPPUNIT_ASSERT_EQUAL(3, obj.GetSelectedOutputRowCount()); // rows + header
}

void TestIPhreeqc::TestGetSelectedOutputValue(void)
{
	int col;

	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL(0, obj.LoadDatabase("llnl.dat"));

	int max = 6;

	CPPUNIT_ASSERT_EQUAL(VR_OK, SOLUTION(obj, 1.0, 1.0, 1.0));
	CPPUNIT_ASSERT_EQUAL(VR_OK, EQUILIBRIUM_PHASES(obj, "calcite", 0.0, 0.010));
	CPPUNIT_ASSERT_EQUAL(VR_OK, USER_PUNCH(obj, "Ca", max));

	obj.SetOutputFileOn(0);
	obj.SetErrorFileOn(0);
	obj.SetLogFileOn(0);
	obj.SetSelectedOutputFileOn(0);
	obj.SetDumpFileOn(0);
	CPPUNIT_ASSERT_EQUAL(0, obj.RunAccumulated());

/*
EXPECTED selected.out:
         sim	       state	        soln	      dist_x	        time	        step	          pH	          pe	           C	          Ca	          Na	     m_CO3-2	     m_CaOH+	    m_NaCO3-	    la_CO3-2	    la_CaOH+	   la_NaCO3-	     Calcite	   d_Calcite	   si_CO2(g)	 si_Siderite	    pressure	   total mol	      volume	    g_CO2(g)	     g_N2(g)	    k_Albite	   dk_Albite	    k_Pyrite	   dk_Pyrite	     s_CaSO4	     s_SrSO4	      1.name	      1.type	     1.moles	      2.name	      2.type	     2.moles	      3.name	      3.type	     3.moles	      4.name	      4.type	     4.moles	      5.name	      5.type	     5.moles	      6.name	      6.type	     6.moles
           1	      i_soln	           1	         -99	         -99	         -99	           7	           4	 1.0000e-003	 1.0000e-003	 1.0000e-003	 4.2975e-007	 1.1819e-009	 1.1881e-009	-6.4686e+000	-8.9530e+000	-8.9507e+000	 0.0000e+000	 0.0000e+000	     -2.2870	   -999.9990	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	        Ca+2	          aq	 9.9178e-004	     CaHCO3+	          aq	 7.5980e-006	       CaCO3	          aq	 6.2155e-007	       CaOH+	          aq	 1.1819e-009
           1	       react	           1	         -99	           0	           1	     7.86135	       10.18	 1.1556e-003	 1.1556e-003	 1.0000e-003	 4.2718e-006	 9.7385e-009	 1.1620e-008	-5.4781e+000	-8.0388e+000	-7.9621e+000	 9.8444e-003	-1.5555e-004	     -3.0192	   -999.9990	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	     calcite	        equi	 9.8444e-003	        Ca+2	          aq	 1.1371e-003	     CaHCO3+	          aq	 1.1598e-005	       CaCO3	          aq	 6.8668e-006	       CaOH+	          aq	 9.7385e-009
*/


	CVar v;

	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, obj.GetSelectedOutputValue(-1, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);

	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, obj.GetSelectedOutputValue(obj.GetSelectedOutputRowCount(), 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);

	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, obj.GetSelectedOutputValue(0, -1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, v.vresult);

	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, obj.GetSelectedOutputValue(0, obj.GetSelectedOutputColumnCount(), &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, v.vresult);


	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("sim"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, 1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("state"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, 2, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("soln"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, 3, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("dist_x"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, 4, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("time"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, 5, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("step"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, 6, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("pH"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, 7, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("pe"), std::string(v.sVal));

	col = 7;

	// -totals C Ca Na
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("C(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("Ca(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("Na(mol/kgw)"), std::string(v.sVal));

	// -molalities CO3-2  CaOH+  NaCO3-
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("m_CO3-2(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("m_CaOH+(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("m_NaCO3-(mol/kgw)"), std::string(v.sVal));

	// -activities CO3-2  CaOH+  NaCO3-
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("la_CO3-2"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("la_CaOH+"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("la_NaCO3-"), std::string(v.sVal));

	// -equilibrium_phases Calcite
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("Calcite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("d_Calcite"), std::string(v.sVal));


	// -saturation_indices CO2(g) Siderite
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("si_CO2(g)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("si_Siderite"), std::string(v.sVal));

	// -gases CO2(g) N2(g)
	//                      pressure "total mol" volume g_CO2(g) g_N2(g)
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("pressure"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("total mol"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("volume"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("g_CO2(g)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("g_N2(g)"), std::string(v.sVal));

	// -kinetic_reactants Albite Pyrite
	//                               k_Albite dk_Albite k_Pyrite dk_Pyrite
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("k_Albite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("dk_Albite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("k_Pyrite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("dk_Pyrite"), std::string(v.sVal));

	// -solid_solutions CaSO4 SrSO4
	//                              s_CaSO4 s_SrSO4
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("s_CaSO4"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("s_SrSO4"), std::string(v.sVal));

	for (int i = 0; i < max; ++i)
	{
		std::ostringstream oss1, oss2, oss3;

		// 1.name
		//
		CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col + 1 + (i*3), &v));
		CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
		oss1 << i + 1 << ".name";
		CPPUNIT_ASSERT_EQUAL( oss1.str(), std::string(v.sVal));

		// 1.type
		//
		CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col + 2 + (i*3), &v));
		CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
		oss2 << i + 1 << ".type";
		CPPUNIT_ASSERT_EQUAL( oss2.str(), std::string(v.sVal));

		// 1.moles
		//
		CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(0, col + 3 + (i*3), &v));
		CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
		oss3 << i + 1 << ".moles";
		CPPUNIT_ASSERT_EQUAL( oss3.str(), std::string(v.sVal));
	}

	// sim
	//
	col = 0;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );

	// state
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("i_soln"), std::string(v.sVal) );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("react"), std::string(v.sVal) );

	// soln
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );

	// dist_x -- sometimes as double sometimes as long (depends on state)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( -99L, v.lVal );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( -99L, v.lVal );


	// time -- sometimes as double sometimes as long (depends on state)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( -99L, v.lVal );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, v.dVal, ::pow(10., -DBL_DIG) );

	// step
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL(-99L, v.lVal);
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL(1L, v.lVal);


	// pH
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.0, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.861354, v.dVal, ::pow(10., -6) );

	// pe
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 4.0, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 10.4, v.dVal, ::pow(10., -1) );

	//
	// -totals C Ca Na
	//

	// C(mol/kgw)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0000e-003, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1556e-003, v.dVal, ::pow(10., -7) );


	// Ca(mol/kgw)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0000e-003, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1556e-003, v.dVal, ::pow(10., -7) );


	// Na(mol/kgw)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0000e-003, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0000e-003, v.dVal, ::pow(10., -7) );

	// -molalities CO3-2  CaOH+  NaCO3-
	col += 3;

	// -activities CO3-2  CaOH+  NaCO3-
	col += 3;

	// -equilibrium_phases Calcite
	col += 2;

	// -saturation_indices CO2(g) Siderite
	col += 2;

	// -gases CO2(g) N2(g)
	col += 5;

	// -kinetic_reactants Albite Pyrite
	//                               k_Albite dk_Albite k_Pyrite dk_Pyrite
	col += 4;

	// -solid_solutions CaSO4 SrSO4
	//                              s_CaSO4 s_SrSO4
	col += 2;


	// 1.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("Ca+2"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("Calcite"),  std::string(v.sVal));

	// 1.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("equi"),  std::string(v.sVal));

	// 1.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 9.9177923E-04, v.dVal, ::pow(10., -11) );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 9.8444477E-03, v.dVal, ::pow(10., -10) );

	// 2.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("CaHCO3+"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca+2"),  std::string(v.sVal));

	// 2.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 2.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.5980e-006, v.dVal, ::pow(10., -10) );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1371e-003, v.dVal, ::pow(10., -7) );


	// 3.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("CaCO3"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("CaHCO3+"),  std::string(v.sVal));

	// 3.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 3.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 6.2155e-007, v.dVal, ::pow(10., -11) );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1598e-005, v.dVal, ::pow(10., -9) );



	// 4.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("CaOH+"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("CaCO3"),  std::string(v.sVal));

	// 4.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 4.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1819e-009, v.dVal, ::pow(10., -13) );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 6.8668e-006, v.dVal, ::pow(10., -10) );


	// 5.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("CaOH+"),  std::string(v.sVal));

	// 5.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 5.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_DOUBLE, v.type );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 9.7385e-009, v.dVal, ::pow(10., -13) );


	// 6.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );

	// 6.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );

	// 6.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(VR_OK, obj.GetSelectedOutputValue(2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
}

void TestIPhreeqc::TestGetSelectedOutputColumnCount(void)
{
	IPhreeqc obj;
	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("llnl.dat"));
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetSelectedOutputColumnCount() );
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(obj, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( VR_OK, EQUILIBRIUM_PHASES(obj, "calcite", 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( VR_OK, USER_PUNCH(obj, "Ca", 10) );
	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 62,    obj.GetSelectedOutputColumnCount() );
}

void TestIPhreeqc::TestAddError(void)
{
	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL(0, obj.LoadDatabase("phreeqc.dat"));

	// make sure initialized to empty
	//
	const char* err = obj.GetErrorString();
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(err) );

	// make sure initialized to empty
	//
	const char *expected = "TESTING AddError\n";
	CPPUNIT_ASSERT_EQUAL((size_t)1, obj.AddError(expected));

	// check 1
	//
	err = obj.GetErrorString();
	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );

	// check increment
	//
	const char *expected2 = "XXXXXX\n";
	CPPUNIT_ASSERT_EQUAL((size_t)2, obj.AddError(expected2));

	// check concatenation
	//
	err = obj.GetErrorString();
	CPPUNIT_ASSERT_EQUAL( std::string(expected) + std::string(expected2), std::string(err) );


	// clear errors
	//
	CPPUNIT_ASSERT_EQUAL(0, obj.LoadDatabase("phreeqc.dat"));

	// make sure back to empty
	//
	err = obj.GetErrorString();
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(err) );
}

void TestIPhreeqc::TestAccumulateLine(void)
{
	// TODO
}

void TestIPhreeqc::TestOutputErrorString(void)
{
	// TODO
}

void TestIPhreeqc::TestRunWithCallback(void)
{
	// TODO
}

void TestIPhreeqc::TestRunNoDatabaseLoaded(void)
{
	IPhreeqc obj;

	obj.UnLoadDatabase();
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpFileOn(false);
	CPPUNIT_ASSERT_EQUAL( 1, obj.RunAccumulated() );

	const char expected[] =
		"ERROR: RunAccumulated: No database is loaded\n";
	const char* err = obj.GetErrorString();

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );
}

void TestIPhreeqc::TestCase1(void)
{
	// Case 1 (see do_run)
	// pr.punch == TRUE
	// punch.new_def == FALSE
	// output_isopen(OUTPUT_PUNCH) == FALSE
	// selected_output_on == TRUE

	IPhreeqc obj;

	char SELECTED_OUT[80];
	sprintf(SELECTED_OUT, "selected.%d.out", obj.Index);

	// remove punch file if it exists
	if (::FileExists(SELECTED_OUT))
	{
		CPPUNIT_ASSERT(::DeleteFile(SELECTED_OUT));
	}
	CPPUNIT_ASSERT_EQUAL( false,    ::FileExists(SELECTED_OUT) );


	// clear all flags
	CPPUNIT_ASSERT_EQUAL( 0,       obj.LoadDatabase("phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( FALSE,   obj.PhreeqcPtr->punch.in);
	CPPUNIT_ASSERT_EQUAL( TRUE,    obj.PhreeqcPtr->pr.punch);


	CPPUNIT_ASSERT_EQUAL( VR_OK,   SOLUTION(obj, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( VR_OK,   USER_PUNCH(obj, "Ca", 10) );
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(true);
	obj.SetDumpFileOn(false);
	CPPUNIT_ASSERT_EQUAL( 0,          obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( true,       ::FileExists(SELECTED_OUT) );
	CPPUNIT_ASSERT( ::FileSize(SELECTED_OUT) > 0 );
	CPPUNIT_ASSERT_EQUAL( 62,         obj.GetSelectedOutputColumnCount() );

	CPPUNIT_ASSERT_EQUAL( VR_OK,      SOLUTION(obj, 1.0, 1.0, 1.0) );
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(true);
	obj.SetDumpFileOn(false);
	CPPUNIT_ASSERT_EQUAL( 0,          obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( true,       ::FileExists(SELECTED_OUT) );
	CPPUNIT_ASSERT( ::FileSize(SELECTED_OUT) > 0 );
	CPPUNIT_ASSERT_EQUAL( 62,         obj.GetSelectedOutputColumnCount() );
}

void TestIPhreeqc::TestCase2(void)
{
	// Case 2 (see do_run)
	// pr.punch == TRUE
	// punch.new_def == TRUE
	// output_isopen(OUTPUT_PUNCH) == FALSE
	// selected_output_on == TRUE

	IPhreeqc obj;

	// remove punch files if they exists
	//
	if (::FileExists("selected.out"))
	{
		::DeleteFile("selected.out");
	}
	if (::FileExists("case2.punch"))
	{
		::DeleteFile("case2.punch");
	}
	CPPUNIT_ASSERT_EQUAL( false,   ::FileExists("selected.out") );
	CPPUNIT_ASSERT_EQUAL( false,   ::FileExists("case2.punch") );

	// clear all flags
	CPPUNIT_ASSERT_EQUAL( 0,       obj.LoadDatabase("phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( FALSE,   obj.PhreeqcPtr->punch.in);
	CPPUNIT_ASSERT_EQUAL( TRUE,    obj.PhreeqcPtr->pr.punch);

	CPPUNIT_ASSERT_EQUAL( VR_OK,   SOLUTION(obj, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( VR_OK,   USER_PUNCH(obj, "Ca", 10) );
	CPPUNIT_ASSERT_EQUAL( VR_OK,   obj.AccumulateLine("-file case2.punch") ); // force have_punch_name to TRUE (see read_selected_ouput)
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(true);
	obj.SetDumpFileOn(false);
	CPPUNIT_ASSERT_EQUAL( 0,       obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( false,   ::FileExists("selected.out") );
	CPPUNIT_ASSERT_EQUAL( true,    ::FileExists("case2.punch") );
	CPPUNIT_ASSERT( ::FileSize("case2.punch") > 0 );
	CPPUNIT_ASSERT_EQUAL( 62,      obj.GetSelectedOutputColumnCount() );


	// remove punch files if they exist
	//
	if (::FileExists("selected.out"))
	{
		::DeleteFile("selected.out");
	}
	if (::FileExists("case2.punch"))
	{
		::DeleteFile("case2.punch");
	}
	CPPUNIT_ASSERT_EQUAL( false,    ::FileExists("selected.out") );
	CPPUNIT_ASSERT_EQUAL( false,    ::FileExists("case2.punch") );

	CPPUNIT_ASSERT_EQUAL( VR_OK,    SOLUTION(obj, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( VR_OK,    USER_PUNCH(obj, "Ca", 10) );
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(true);
	obj.SetDumpFileOn(false);
	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists("selected.out") );
	CPPUNIT_ASSERT_EQUAL( true,  ::FileExists("case2.punch") );
	CPPUNIT_ASSERT( ::FileSize("case2.punch") > 0 );
	CPPUNIT_ASSERT_EQUAL( 62,    obj.GetSelectedOutputColumnCount() );

	if (::FileExists("case2.punch"))
	{
		::DeleteFile("case2.punch");
	}
	CPPUNIT_ASSERT_EQUAL( false,  ::FileExists("case2.punch") );
}

void TestIPhreeqc::TestPrintSelectedOutputFalse(void)
{
	IPhreeqc obj;

	// remove punch files if they exists
	//
	if (::FileExists("selected.out"))
	{
		::DeleteFile("selected.out");
	}
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists("selected.out") );

	CPPUNIT_ASSERT_EQUAL( 0, obj.LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SELECTED_OUTPUT(obj) );

	// turn off selected output
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("PRINT; -selected_output false \n") );

	// run
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(true);
	obj.SetDumpFileOn(false);
	CPPUNIT_ASSERT_EQUAL( 0, obj.RunAccumulated() );

	CPPUNIT_ASSERT_EQUAL( 0, obj.GetSelectedOutputColumnCount() );
	CPPUNIT_ASSERT_EQUAL( 0, obj.GetSelectedOutputRowCount() );


	// reset pr.punch to TRUE
	CPPUNIT_ASSERT_EQUAL( 0, obj.LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SELECTED_OUTPUT(obj) );

	// run
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(true);
	obj.SetDumpFileOn(false);
	CPPUNIT_ASSERT_EQUAL( 0, obj.RunAccumulated() );

	CPPUNIT_ASSERT_EQUAL( 11, obj.GetSelectedOutputColumnCount() );
	CPPUNIT_ASSERT_EQUAL( 2, obj.GetSelectedOutputRowCount() );
}

void TestIPhreeqc::TestOutputFileOnOff()
{
#if defined(_WIN32)
	int n = ::_fcloseall();
	CPPUNIT_ASSERT_EQUAL(0, n);
#endif

	bool onoff[5];
	onoff[0] = true;   // output_file_on
	onoff[1] = false;  // error_file_on
	onoff[2] = false;  // log_file_on
	onoff[3] = false;  // selected_output_file_on
	onoff[4] = false;  // dump_file_on
	TestFileOnOff("phreeqc.%d.out", onoff[0], onoff[1], onoff[2], onoff[3], onoff[4]);
}

void TestIPhreeqc::TestErrorFileOnOff()
{
	bool onoff[5];
	onoff[0] = false;  // output_file_on
	onoff[1] = true;   // error_file_on
	onoff[2] = false;  // log_file_on
	onoff[3] = false;  // selected_output_file_on
	onoff[4] = false;  // dump_file_on
	TestFileOnOff("phreeqc.%d.err", onoff[0], onoff[1], onoff[2], onoff[3], onoff[4]);
}

void TestIPhreeqc::TestLogFileOnOff()
{
	bool onoff[5];
	onoff[0] = false;  // output_file_on
	onoff[1] = false;  // error_file_on
	onoff[2] = true;   // log_file_on
	onoff[3] = false;  // selected_output_file_on
	onoff[4] = false;  // dump_file_on
	TestFileOnOff("phreeqc.%d.log", onoff[0], onoff[1], onoff[2], onoff[3], onoff[4]);
}

void TestIPhreeqc::TestDumpFileOnOff()
{
	bool onoff[5];
	onoff[0] = false;  // output_file_on
	onoff[1] = false;  // error_file_on
	onoff[2] = false;  // log_file_on
	onoff[3] = false;  // selected_output_file_on
	onoff[4] = true;   // dump_file_on
	TestFileOnOff("dump.%d.out", onoff[0], onoff[1], onoff[2], onoff[3], onoff[4]);
}

void TestIPhreeqc::TestSelOutFileOnOff()
{
	bool onoff[5];
	onoff[0] = false;  // output_file_on
	onoff[1] = false;  // error_file_on
	onoff[2] = false;  // log_file_on
	onoff[3] = true;   // selected_output_file_on
	onoff[4] = false;  // dump_file_on
	TestFileOnOff("selected.%d.out", onoff[0], onoff[1], onoff[2], onoff[3], onoff[4]);
}

void TestIPhreeqc::TestFileOnOff(const char* FILENAME_FORMAT, bool output_file_on, bool error_file_on, bool log_file_on, bool selected_output_file_on, bool dump_file_on)
{
	IPhreeqc obj;

	char FILENAME[80];
	sprintf(FILENAME, FILENAME_FORMAT, obj.GetId());

	// remove FILENAME if it exists
	//
	if (::FileExists(FILENAME))
	{
		::DeleteFile(FILENAME);
	}
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(FILENAME) );

	CPPUNIT_ASSERT_EQUAL( 0, obj.LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SELECTED_OUTPUT(obj) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, DUMP(obj) );

	// run all off
	obj.SetDumpFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetOutputFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(FILENAME) );



	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SELECTED_OUTPUT(obj) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, DUMP(obj) );

	// run
	obj.SetDumpFileOn(dump_file_on);
	obj.SetErrorFileOn(error_file_on);
	obj.SetLogFileOn(log_file_on);
	obj.SetOutputFileOn(output_file_on);
	obj.SetSelectedOutputFileOn(selected_output_file_on);
	CPPUNIT_ASSERT_EQUAL( 0,    obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( true, ::FileExists(FILENAME) );
	CPPUNIT_ASSERT( ::DeleteFile(FILENAME) );
	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SELECTED_OUTPUT(obj) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, DUMP(obj) );

	// run
	obj.SetDumpFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetOutputFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(FILENAME) );

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SELECTED_OUTPUT(obj) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, DUMP(obj) );

	// run
	obj.SetDumpFileOn(dump_file_on);
	obj.SetErrorFileOn(error_file_on);
	obj.SetLogFileOn(log_file_on);
	obj.SetOutputFileOn(output_file_on);
	obj.SetSelectedOutputFileOn(selected_output_file_on);
	CPPUNIT_ASSERT_EQUAL( 0,    obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( true, ::FileExists(FILENAME) );
	CPPUNIT_ASSERT( ::DeleteFile(FILENAME) );
}

VRESULT
SOLUTION(IPhreeqc& obj, double C, double Ca, double Na)
{
	std::ostringstream oss;

	oss << "SOLUTION 1\n";
	oss << "C "  << C  << "\n";
	oss << "Ca " << Ca << "\n";
	oss << "Na " << Na << "\n";

	return obj.AccumulateLine(oss.str().c_str());
}

VRESULT
EQUILIBRIUM_PHASES(IPhreeqc& obj, const char* phase, double si, double amount)
{
	std::ostringstream oss;

	oss << "EQUILIBRIUM_PHASES\n";
	oss << phase << " " << si << " " << amount << "\n";
	return obj.AccumulateLine(oss.str().c_str());
}

VRESULT
USER_PUNCH(IPhreeqc& obj, const char* element, int max)
{
	std::ostringstream oss;

	oss << "USER_PUNCH\n";

	oss << "-head ";
	for (int i = 1; i <= max; ++i)
	{
		oss << i << ".name " << i << ".type " << i << ".moles ";
	}
	oss << "\n";
	oss << "-start" << "\n";
	oss << "10 n = sys(\"" << element << "\"" << ", count, names$, types$, moles)" << "\n";
	oss << "20 n = " << max << "\n";
	oss << "30 if count < " << max << " then n = count" << "\n";
	oss << "40 for i = 1 to count" << "\n";
	oss << "50 PUNCH names$(i), types$(i), moles(i)" << "\n";
	oss << "60 next i" << "\n";
	oss << "70 list" << "\n";
	oss << "-end" << "\n";
	oss << "SELECTED_OUTPUT" << "\n";
	oss << "-totals C Ca Na" << "\n";
	oss << "-molalities CO3-2  CaOH+  NaCO3-" << "\n";
	oss << "-activities CO3-2  CaOH+  NaCO3-" << "\n";
	oss << "-equilibrium_phases Calcite" << "\n";
	oss << "-saturation_indices CO2(g) Siderite" << "\n";
	oss << "-gases CO2(g) N2(g)" << "\n";
	oss << "-kinetic_reactants Albite Pyrite" << "\n";
	oss << "-solid_solutions CaSO4 SrSO4" << "\n";

	return obj.AccumulateLine(oss.str().c_str());
}

VRESULT
SELECTED_OUTPUT(IPhreeqc& obj)
{
	std::ostringstream oss;

	oss << "SELECTED_OUTPUT" << "\n";
	oss << "-totals C Ca Na" << "\n";

	return obj.AccumulateLine(oss.str().c_str());
}

VRESULT
DUMP(IPhreeqc& obj)
{
	std::ostringstream oss;
	oss << "DUMP" << "\n";
	oss << "-solution 1" << "\n";
	return obj.AccumulateLine(oss.str().c_str());
}

void TestIPhreeqc::TestLongHeadings()
{
	char long_header[] = "this_is_a_long_header_0123456789012345678901234567890123456789";
	char long_value[]  = "this_is_a_long_value_01234567890123456789012345678901234567890";

	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL(0, obj.LoadDatabase("phreeqc.dat"));

	std::ostringstream oss;
	oss << "SOLUTION" << "\n";

	oss << "SELECTED_OUTPUT" << "\n";
	oss << "-reset false" << "\n";

	oss << "USER_PUNCH" << "\n";
	oss << "-head " <<  long_header << "\n";
	oss << "-start" << "\n";
	oss << "10 PUNCH \"" << long_value << "\"\n";
	oss << "-end" << "\n";
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine(oss.str().c_str()) );

	obj.SetOutputFileOn(0);
	obj.SetErrorFileOn(0);
	obj.SetLogFileOn(0);
	obj.SetSelectedOutputFileOn(0);
	obj.SetDumpFileOn(0);
	CPPUNIT_ASSERT_EQUAL( 0, obj.RunAccumulated() );

	CPPUNIT_ASSERT_EQUAL( 2, obj.GetSelectedOutputRowCount());
	CPPUNIT_ASSERT_EQUAL( 1, obj.GetSelectedOutputColumnCount());

	CVar v;
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.GetSelectedOutputValue(0, 0, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string(long_header), std::string(v.sVal));

	CVar v1;
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.GetSelectedOutputValue(1, 0, &v1));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v1.type);
	CPPUNIT_ASSERT_EQUAL( std::string(long_value), std::string(v1.sVal));

	CPPUNIT_ASSERT_EQUAL( VR_INVALIDCOL, obj.GetSelectedOutputValue(1, 1, &v1));
	CPPUNIT_ASSERT_EQUAL( VR_INVALIDROW, obj.GetSelectedOutputValue(2, 0, &v1));
}

void TestIPhreeqc::TestDatabaseKeyword()
{
	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL(0, obj.LoadDatabase("phreeqc.dat"));
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpFileOn(false);
	CPPUNIT_ASSERT_EQUAL(1, obj.RunFile("dump"));

	const char *expected =
		"ERROR: Gas not found in PHASES data base, Amm(g).\n"
		"ERROR: Calculations terminating due to input errors.\n";

	const char* err = obj.GetErrorString();
	CPPUNIT_ASSERT_EQUAL(std::string(expected), std::string(err));

	const char *exp_warn =
		"WARNING: DATABASE keyword is ignored by IPhreeqc.\n"
		"WARNING: Cell-lengths were read for 1 cells. Last value is used till cell 100.\n"
		"WARNING: No dispersivities were read; disp = 0 assumed.\n"
		"WARNING: Could not find element in database, Amm.\n"
		"	Concentration is set to zero.\n";

	const char* warn = obj.GetWarningString();
	CPPUNIT_ASSERT_EQUAL(std::string(exp_warn), std::string(warn));
}

void TestIPhreeqc::TestDumpString()
{
	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL(0, obj.LoadDatabase("phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, DUMP(obj) );

	// run
	obj.SetOutputFileOn(0);
	obj.SetErrorFileOn(0);
	obj.SetLogFileOn(0);
	obj.SetSelectedOutputFileOn(0);
	obj.SetDumpFileOn(0);
	obj.SetDumpStringOn(1);
	CPPUNIT_ASSERT_EQUAL( 0, obj.RunAccumulated() );

	const char *expected =
#if defined(_MSC_VER)
		"SOLUTION_RAW       1 \n"
		"  -temp              25\n"
		"  -total_h           111.0132593403\n"
		"  -total_o           55.509043478605\n"
		"  -cb                0.0021723831003929\n"
		"  -totals\n"
		"    C(4)   0.0010000000000376\n"
		"    Ca   0.001000000004331\n"
		"    H(0)   1.4149476909313e-025\n"
		"    Na   0.001\n"
		"  -Isotopes\n"
		"  -pH                7\n"
		"  -pe                4\n"
		"  -mu                0.0028961089894362\n"
		"  -ah2o              0.99994915105857\n"
		"  -mass_water        1\n"
		"  -total_alkalinity  0.00082761690826911\n"
		"  -activities\n"
		"    C(-4)   -67.370522674574\n"
		"    C(4)   -6.4415889265024\n"
		"    Ca   -3.1040445240857\n"
		"    E   -4\n"
		"    H(0)   -25.15\n"
		"    Na   -3.0255625287599\n"
		"    O(0)   -42.080044167952\n"
		"  -gammas\n"
		"USE mix none\n"
		"USE reaction none\n"
		"USE reaction_temperature none\n"
		"USE reaction_pressure none\n"
		;

#endif
#if defined(__GNUC__)
		"SOLUTION_RAW       1 \n"
		"  -temp              25\n"
		"  -total_h           111.0132593403\n"
		"  -total_o           55.509043478605\n"
		"  -cb                0.0021723831003928\n"
		"  -totals\n"
		"    C(4)   0.0010000000000376\n"
		"    Ca   0.001000000004331\n"
		"    H(0)   1.4149476909313e-25\n"
		"    Na   0.001\n"
		"  -Isotopes\n"
		"  -pH                7\n"
		"  -pe                4\n"
		"  -mu                0.0028961089894362\n"
		"  -ah2o              0.99994915105857\n"
		"  -mass_water        1\n"
		"  -total_alkalinity  0.00082761690826912\n"
		"  -activities\n"
		"    C(-4)   -67.370522674574\n"
		"    C(4)   -6.4415889265024\n"
		"    Ca   -3.1040445240857\n"
		"    E   -4\n"
		"    H(0)   -25.15\n"
		"    Na   -3.0255625287599\n"
		"    O(0)   -42.080044167952\n"
		"  -gammas\n"
		"USE mix none\n"
		"USE reaction none\n"
		"USE reaction_temperature none\n"
		"USE reaction_pressure none\n"
		;
#endif

	const char* dump_str = obj.GetDumpString();
	CPPUNIT_ASSERT_EQUAL(std::string(expected), std::string(dump_str));
}

void TestIPhreeqc::TestGetDumpStringLineCount(void)
{
	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL(0,          obj.LoadDatabase("phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK,     ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK,     ::DUMP(obj) );

	// run
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetOutputFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetErrorFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetLogFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetSelectedOutputFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetDumpFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetDumpStringOn() );
	obj.SetDumpStringOn(true);
	CPPUNIT_ASSERT_EQUAL( true,      obj.GetDumpStringOn() );
	CPPUNIT_ASSERT_EQUAL( 0,         obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 30,        obj.GetDumpStringLineCount() );
}

void TestIPhreeqc::TestGetDumpStringLine(void)
{
	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL(0,          obj.LoadDatabase("phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK,     ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK,     ::DUMP(obj) );

	// run
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetOutputFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetErrorFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetLogFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetSelectedOutputFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetDumpFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetDumpStringOn() );

	obj.SetDumpStringOn(true);
	CPPUNIT_ASSERT_EQUAL( true,      obj.GetDumpStringOn() );
	CPPUNIT_ASSERT_EQUAL( 0,         obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 30,        obj.GetDumpStringLineCount() );

	int line = 0;
#if defined(_MSC_VER)
	CPPUNIT_ASSERT_EQUAL( std::string("SOLUTION_RAW       1 "),                    std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -temp              25"),                  std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_h           111.0132593403"),      std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_o           55.509043478605"),     std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -cb                0.0021723831003929"),  std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -totals"),                                std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(4)   0.0010000000000376"),            std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    Ca   0.001000000004331"),               std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    H(0)   1.4149476909313e-025"),          std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    Na   0.001"),                           std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -Isotopes"),                              std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -pH                7"),                   std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -pe                4"),                   std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -mu                0.0028961089894362"),  std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -ah2o              0.99994915105857"),    std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -mass_water        1"),                   std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_alkalinity  0.00082761690826911"), std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -activities"),                            std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(-4)   -67.370522674574"),             std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(4)   -6.4415889265024"),              std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    Ca   -3.1040445240857"),                std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    E   -4"),                               std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    H(0)   -25.15"),                        std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    Na   -3.0255625287599"),                std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    O(0)   -42.080044167952"),              std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -gammas"),                                std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("USE mix none"),                             std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction none"),                        std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction_temperature none"),            std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction_pressure none"),               std::string(obj.GetDumpStringLine(line++)) );
#endif

#if defined(__GNUC__)
	CPPUNIT_ASSERT_EQUAL( std::string("SOLUTION_RAW       1 "),                    std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -temp              25"),                  std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_h           111.0132593403"),      std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_o           55.509043478605"),     std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -cb                0.0021723831003928"),  std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -totals"),                                std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(4)   0.0010000000000376"),            std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    Ca   0.001000000004331"),               std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    H(0)   1.4149476909313e-25"),           std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    Na   0.001"),                           std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -Isotopes"),                              std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -pH                7"),                   std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -pe                4"),                   std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -mu                0.0028961089894362"),  std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -ah2o              0.99994915105857"),    std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -mass_water        1"),                   std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_alkalinity  0.00082761690826912"), std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -activities"),                            std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(-4)   -67.370522674574"),             std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(4)   -6.4415889265024"),              std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    Ca   -3.1040445240857"),                std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    E   -4"),                               std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    H(0)   -25.15"),                        std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    Na   -3.0255625287599"),                std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("    O(0)   -42.080044167952"),              std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("  -gammas"),                                std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("USE mix none"),                             std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction none"),                        std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction_temperature none"),            std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction_pressure none"),               std::string(obj.GetDumpStringLine(line++)) );
#endif

	// remaining lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(obj.GetDumpStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(obj.GetDumpStringLine(line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(obj.GetDumpStringLine(-1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(obj.GetDumpStringLine(-2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(obj.GetDumpStringLine(-3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(obj.GetDumpStringLine(-4)) );
}

void TestIPhreeqc::TestGetComponentCount(void)
{
	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL( 0,         obj.LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK,     ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// run
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetOutputFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetErrorFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetLogFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetSelectedOutputFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetDumpFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetDumpStringOn() );
	CPPUNIT_ASSERT_EQUAL( 0,         obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( (size_t)3, obj.GetComponentCount() );
}

void TestIPhreeqc::TestGetComponent(void)
{
	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL( 0,         obj.LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK,     ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// run
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetOutputFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetErrorFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetLogFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetSelectedOutputFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetDumpFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetDumpStringOn() );
	CPPUNIT_ASSERT_EQUAL( 0,         obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( (size_t)3, obj.GetComponentCount() );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(obj.GetComponent(-2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(obj.GetComponent(-1)) );

	CPPUNIT_ASSERT_EQUAL( std::string("C"),  std::string(obj.GetComponent(0)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca"), std::string(obj.GetComponent(1)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Na"), std::string(obj.GetComponent(2)) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(obj.GetComponent(3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(obj.GetComponent(4)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(obj.GetComponent(5)) );
}

void TestIPhreeqc::TestListComponents(void)
{
	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// run
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetOutputFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetErrorFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetLogFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetSelectedOutputFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetDumpFileOn() );
	CPPUNIT_ASSERT_EQUAL( false,     obj.GetDumpStringOn() );
	CPPUNIT_ASSERT_EQUAL( 0,         obj.RunAccumulated() );

	std::list< std::string > comps = obj.ListComponents();
	CPPUNIT_ASSERT_EQUAL( (size_t)3, comps.size() );

	std::list< std::string >::iterator it = comps.begin();
	CPPUNIT_ASSERT_EQUAL( std::string("C"),  std::string((*it++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca"), std::string((*it++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Na"), std::string((*it++)) );
}

void TestIPhreeqc::TestSetDumpFileName(void)
{
	char DUMP_FILENAME[80];
	sprintf(DUMP_FILENAME, "dump.%06d.out", ::rand());
	if (::FileExists(DUMP_FILENAME))
	{
		::DeleteFile(DUMP_FILENAME);
	}

	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// run
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpStringOn(false);
	obj.SetDumpFileOn(true);
	obj.SetDumpFileName(DUMP_FILENAME);

	CPPUNIT_ASSERT_EQUAL( 0,      obj.RunAccumulated() );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(DUMP_FILENAME) );

	std::string lines[32];
	std::ifstream ifs(DUMP_FILENAME);

	size_t i = 0;
	while (i < sizeof(lines)/sizeof(lines[0]) && std::getline(ifs, lines[i]))
	{
		++i;
	}

	int line = 0;
#if defined(_MSC_VER)
	CPPUNIT_ASSERT_EQUAL( std::string("SOLUTION_RAW       1 "),                    lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -temp              25"),                  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_h           111.0132593403"),      lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_o           55.509043478605"),     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -cb                0.0021723831003929"),  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -totals"),                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(4)   0.0010000000000376"),            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    Ca   0.001000000004331"),               lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    H(0)   1.4149476909313e-025"),          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    Na   0.001"),                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -Isotopes"),                              lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -pH                7"),                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -pe                4"),                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -mu                0.0028961089894362"),  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -ah2o              0.99994915105857"),    lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -mass_water        1"),                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_alkalinity  0.00082761690826911"), lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -activities"),                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(-4)   -67.370522674574"),             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(4)   -6.4415889265024"),              lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    Ca   -3.1040445240857"),                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    E   -4"),                               lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    H(0)   -25.15"),                        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    Na   -3.0255625287599"),                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    O(0)   -42.080044167952"),              lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -gammas"),                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("USE mix none"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction none"),                        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction_temperature none"),            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction_pressure none"),               lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                         lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                         lines[line++] );
#endif

#if defined(__GNUC__)
	CPPUNIT_ASSERT_EQUAL( std::string("SOLUTION_RAW       1 "),                    lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -temp              25"),                  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_h           111.0132593403"),      lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_o           55.509043478605"),     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -cb                0.0021723831003928"),  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -totals"),                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(4)   0.0010000000000376"),            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    Ca   0.001000000004331"),               lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    H(0)   1.4149476909313e-25"),           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    Na   0.001"),                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -Isotopes"),                              lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -pH                7"),                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -pe                4"),                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -mu                0.0028961089894362"),  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -ah2o              0.99994915105857"),    lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -mass_water        1"),                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -total_alkalinity  0.00082761690826912"), lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -activities"),                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(-4)   -67.370522674574"),             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    C(4)   -6.4415889265024"),              lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    Ca   -3.1040445240857"),                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    E   -4"),                               lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    H(0)   -25.15"),                        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    Na   -3.0255625287599"),                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("    O(0)   -42.080044167952"),              lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("  -gammas"),                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("USE mix none"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction none"),                        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction_temperature none"),            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("USE reaction_pressure none"),               lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                         lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                         lines[line++] );
#endif

	if (::FileExists(DUMP_FILENAME))
	{
		::DeleteFile(DUMP_FILENAME);
	}
}

void TestIPhreeqc::TestSetOutputFileName(void)
{
	char OUTPUT_FILENAME[80];
	sprintf(OUTPUT_FILENAME, "output.%06d.out", ::rand());
	if (::FileExists(OUTPUT_FILENAME))
	{
		::DeleteFile(OUTPUT_FILENAME);
	}

	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// run
	obj.SetOutputFileOn(true);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpStringOn(false);
	obj.SetDumpFileOn(false);
	obj.SetOutputFileName(OUTPUT_FILENAME);

	CPPUNIT_ASSERT_EQUAL( 0,      obj.RunAccumulated() );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(OUTPUT_FILENAME) );

	std::string lines[101];
	std::ifstream ifs(OUTPUT_FILENAME);

	size_t i = 0;
	while (i < sizeof(lines)/sizeof(lines[0]) && std::getline(ifs, lines[i]))
	{
		++i;
	}

	CPPUNIT_ASSERT_EQUAL( 98u, i );

	int line = 0;
#if defined(_MSC_VER)
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 1."),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	SOLUTION 1"),                                                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	C 1"),                                                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Ca 1"),                                                                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Na 1"),                                                                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	DUMP"),                                                                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	-solution 1"),                                                                         lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-------------------------------------------"),                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Beginning of initial solution calculations."),                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-------------------------------------------"),                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Initial solution 1.	"),                                                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------------------------Solution composition------------------------------"),      lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Elements           Molality       Moles"),                                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	C                1.000e-003  1.000e-003"),                                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Ca               1.000e-003  1.000e-003"),                                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Na               1.000e-003  1.000e-003"),                                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("----------------------------Description of solution----------------------------"),      lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                                       pH  =   7.000    "),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                                       pe  =   4.000    "),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                        Activity of water  =   1.000"),                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                           Ionic strength  =  2.896e-003"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                       Mass of water (kg)  =  1.000e+000"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                 Total alkalinity (eq/kg)  =  8.276e-004"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                       Total CO2 (mol/kg)  =  1.000e-003"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                      Temperature (deg C)  =  25.00"),                                  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                  Electrical balance (eq)  =  2.172e-003"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(" Percent error, 100*(Cat-|An|)/(Cat+|An|)  =  57.04"),                                  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                               Iterations  =   6"),                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                                  Total H  = 1.110133e+002"),                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                                  Total O  = 5.550904e+001"),                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("----------------------------Distribution of species----------------------------"),      lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                                                   Log       Log         Log "),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   Species                 Molality    Activity  Molality  Activity     Gamma"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   OH-                   1.062e-007  1.001e-007    -6.974    -7.000    -0.026"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   H+                    1.056e-007  1.000e-007    -6.976    -7.000    -0.024"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   H2O                   5.551e+001  9.999e-001     1.744    -0.000     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("C(-4)           0.000e+000"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CH4                   0.000e+000  0.000e+000   -67.371   -67.371     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("C(4)            1.000e-003"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   HCO3-                 8.171e-004  7.714e-004    -3.088    -3.113    -0.025"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CO2                   1.733e-004  1.734e-004    -3.761    -3.761     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CaHCO3+               8.204e-006  7.745e-006    -5.086    -5.111    -0.025"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CaCO3                 4.779e-007  4.782e-007    -6.321    -6.320     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CO3-2                 4.555e-007  3.618e-007    -6.342    -6.442    -0.100"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   NaHCO3                4.087e-007  4.090e-007    -6.389    -6.388     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   NaCO3-                6.736e-009  6.351e-009    -8.172    -8.197    -0.026"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca              1.000e-003"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   Ca+2                  9.913e-004  7.870e-004    -3.004    -3.104    -0.100"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CaHCO3+               8.204e-006  7.745e-006    -5.086    -5.111    -0.025"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CaCO3                 4.779e-007  4.782e-007    -6.321    -6.320     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CaOH+                 1.385e-009  1.306e-009    -8.859    -8.884    -0.026"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("H(0)            1.415e-025"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   H2                    7.075e-026  7.079e-026   -25.150   -25.150     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Na              1.000e-003"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   Na+                   9.996e-004  9.428e-004    -3.000    -3.026    -0.025"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   NaHCO3                4.087e-007  4.090e-007    -6.389    -6.388     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   NaCO3-                6.736e-009  6.351e-009    -8.172    -8.197    -0.026"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   NaOH                  6.225e-011  6.229e-011   -10.206   -10.206     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("O(0)            0.000e+000"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   O2                    0.000e+000  0.000e+000   -42.080   -42.080     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------Saturation indices-------------------------------"),      lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Phase               SI   log IAP   log K(298 K,   1 atm)"),                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Aragonite        -1.21     -9.55   -8.34  CaCO3"),                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Calcite          -1.07     -9.55   -8.48  CaCO3"),                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	CH4(g)          -64.51    -67.37   -2.86  CH4"),                                       lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	CO2(g)           -2.29     -3.76   -1.47  CO2"),                                       lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	H2(g)           -22.00    -25.15   -3.15  H2"),                                        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	H2O(g)           -1.51     -0.00    1.51  H2O"),                                       lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	O2(g)           -39.12    -42.08   -2.96  O2"),                                        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------"),                                                                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("End of simulation."),                                                                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------"),                                                                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 2."),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------"),                                                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("End of run."),                                                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------"),                                                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
#endif

#if defined(__GNUC__)
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 1."),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	SOLUTION 1"),                                                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	C 1"),                                                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Ca 1"),                                                                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Na 1"),                                                                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	DUMP"),                                                                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	-solution 1"),                                                                         lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-------------------------------------------"),                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Beginning of initial solution calculations."),                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-------------------------------------------"),                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Initial solution 1.	"),                                                                lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------------------------Solution composition------------------------------"),      lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Elements           Molality       Moles"),                                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	C                 1.000e-03   1.000e-03"),                                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Ca                1.000e-03   1.000e-03"),                                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Na                1.000e-03   1.000e-03"),                                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("----------------------------Description of solution----------------------------"),      lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                                       pH  =   7.000    "),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                                       pe  =   4.000    "),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                        Activity of water  =   1.000"),                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                           Ionic strength  =   2.896e-03"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                       Mass of water (kg)  =   1.000e+00"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                 Total alkalinity (eq/kg)  =   8.276e-04"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                       Total CO2 (mol/kg)  =   1.000e-03"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                      Temperature (deg C)  =  25.00"),                                  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                  Electrical balance (eq)  =   2.172e-03"),                             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(" Percent error, 100*(Cat-|An|)/(Cat+|An|)  =  57.04"),                                  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                               Iterations  =   6"),                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                                  Total H  = 1.110133e+02"),                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                                  Total O  = 5.550904e+01"),                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("----------------------------Distribution of species----------------------------"),      lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("                                                   Log       Log         Log "),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   Species                 Molality    Activity  Molality  Activity     Gamma"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   OH-                    1.062e-07   1.001e-07    -6.974    -7.000    -0.026"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   H+                     1.056e-07   1.000e-07    -6.976    -7.000    -0.024"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   H2O                    5.551e+01   9.999e-01     1.744    -0.000     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("C(-4)            0.000e+00"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CH4                    0.000e+00   0.000e+00   -67.371   -67.371     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("C(4)             1.000e-03"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   HCO3-                  8.171e-04   7.714e-04    -3.088    -3.113    -0.025"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CO2                    1.733e-04   1.734e-04    -3.761    -3.761     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CaHCO3+                8.204e-06   7.745e-06    -5.086    -5.111    -0.025"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CaCO3                  4.779e-07   4.782e-07    -6.321    -6.320     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CO3-2                  4.555e-07   3.618e-07    -6.342    -6.442    -0.100"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   NaHCO3                 4.087e-07   4.090e-07    -6.389    -6.388     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   NaCO3-                 6.736e-09   6.351e-09    -8.172    -8.197    -0.026"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca               1.000e-03"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   Ca+2                   9.913e-04   7.870e-04    -3.004    -3.104    -0.100"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CaHCO3+                8.204e-06   7.745e-06    -5.086    -5.111    -0.025"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CaCO3                  4.779e-07   4.782e-07    -6.321    -6.320     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   CaOH+                  1.385e-09   1.306e-09    -8.859    -8.884    -0.026"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("H(0)             1.415e-25"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   H2                     7.075e-26   7.079e-26   -25.150   -25.150     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Na               1.000e-03"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   Na+                    9.996e-04   9.428e-04    -3.000    -3.026    -0.025"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   NaHCO3                 4.087e-07   4.090e-07    -6.389    -6.388     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   NaCO3-                 6.736e-09   6.351e-09    -8.172    -8.197    -0.026"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   NaOH                   6.225e-11   6.229e-11   -10.206   -10.206     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("O(0)             0.000e+00"),                                                           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("   O2                     0.000e+00   0.000e+00   -42.080   -42.080     0.000"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------Saturation indices-------------------------------"),      lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Phase               SI   log IAP   log K(298 K,   1 atm)"),                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Aragonite        -1.21     -9.55   -8.34  CaCO3"),                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	Calcite          -1.07     -9.55   -8.48  CaCO3"),                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	CH4(g)          -64.51*   -67.37   -2.86  CH4	 Delta_V -54.21 cm3/mol"),             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	CO2(g)           -2.29*    -3.76   -1.47  CO2	 Delta_V -18.07 cm3/mol"),             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	H2(g)           -22.00    -25.15   -3.15  H2"),                                        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	H2O(g)           -1.51*    -0.00    1.51  H2O	 Delta_V  18.07 cm3/mol"),             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	O2(g)           -39.12*   -42.08   -2.96  O2	 Delta_V  36.14 cm3/mol"),             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("	                      *  with Delta_V * (P - 1) / 2.3RT."),                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------"),                                                                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("End of simulation."),                                                                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------"),                                                                   lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 2."),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------"),                                                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("End of run."),                                                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------"),                                                                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                     lines[line++] );
#endif

	if (::FileExists(OUTPUT_FILENAME))
	{
		::DeleteFile(OUTPUT_FILENAME);
	}
}

void TestIPhreeqc::TestOutputStringOnOff(void)
{
	IPhreeqc obj;
	CPPUNIT_ASSERT_EQUAL( false,    obj.GetOutputStringOn() );

	obj.SetOutputStringOn(true);
	CPPUNIT_ASSERT_EQUAL( true,     obj.GetOutputStringOn() );

	obj.SetOutputStringOn(false);
	CPPUNIT_ASSERT_EQUAL( false,    obj.GetOutputStringOn() );
}

void TestIPhreeqc::TestGetOutputString(void)
{
	char OUTPUT_FILENAME[80];
	sprintf(OUTPUT_FILENAME, "output.%06d.out", ::rand());
	if (::FileExists(OUTPUT_FILENAME))
	{
		::DeleteFile(OUTPUT_FILENAME);
	}
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(OUTPUT_FILENAME) );

	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// run
	obj.SetOutputFileOn(true);
	obj.SetOutputStringOn(true);

	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpStringOn(false);
	obj.SetDumpFileOn(false);
	obj.SetOutputFileName(OUTPUT_FILENAME);

	CPPUNIT_ASSERT_EQUAL( 0,      obj.RunAccumulated() );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(OUTPUT_FILENAME) );

	{
		std::ifstream ifs(OUTPUT_FILENAME);
		std::string fline((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

		std::string sline(obj.GetOutputString());
		CPPUNIT_ASSERT( sline.size() > 0 );

		CPPUNIT_ASSERT_EQUAL( fline, sline );
	}

	if (::FileExists(OUTPUT_FILENAME))
	{
		::DeleteFile(OUTPUT_FILENAME);
	}
}

void TestIPhreeqc::TestGetOutputStringLineCount(void)
{
	IPhreeqc obj;
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetOutputStringLineCount() );

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetOutputStringLineCount() );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// run
	obj.SetOutputStringOn(false);
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpStringOn(false);
	obj.SetDumpFileOn(false);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetOutputStringLineCount() );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	obj.SetOutputStringOn(true);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 98,    obj.GetOutputStringLineCount() );


	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	obj.SetOutputStringOn(false);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetOutputStringLineCount() );
}

void TestIPhreeqc::TestGetOutputStringLine(void)
{
	IPhreeqc obj;
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetOutputStringLineCount() );

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetOutputStringLineCount() );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// run
	obj.SetOutputStringOn(false);
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpStringOn(false);
	obj.SetDumpFileOn(false);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetOutputStringLineCount() );

	int line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(-1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(-2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(-3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(-4)) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	obj.SetOutputStringOn(true);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 98,    obj.GetOutputStringLineCount() );

	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                std::string(obj.GetOutputStringLine(0))  );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 1."),                                                std::string(obj.GetOutputStringLine(1))  );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                std::string(obj.GetOutputStringLine(2))  );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------------------------Solution composition------------------------------"),     std::string(obj.GetOutputStringLine(16)) );
	CPPUNIT_ASSERT_EQUAL( std::string("----------------------------Description of solution----------------------------"),     std::string(obj.GetOutputStringLine(24)) );
	CPPUNIT_ASSERT_EQUAL( std::string("----------------------------Distribution of species----------------------------"),     std::string(obj.GetOutputStringLine(40)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------Saturation indices-------------------------------"),     std::string(obj.GetOutputStringLine(73)) );
	CPPUNIT_ASSERT_EQUAL( std::string("End of run.")                                                                    ,     std::string(obj.GetOutputStringLine(95)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                    std::string(obj.GetOutputStringLine(98)) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	obj.SetOutputStringOn(false);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetOutputStringLineCount() );

	line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(-1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(-2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(-3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetOutputStringLine(-4)) );
}

void TestIPhreeqc::TestSetLogFileName(void)
{
	char LOG_FILENAME[80];
	sprintf(LOG_FILENAME, "log.%06d.out", ::rand());
	if (::FileExists(LOG_FILENAME))
	{
		::DeleteFile(LOG_FILENAME);
	}

	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("KNOBS") );
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("\t-logfile TRUE") );

	// run
	obj.SetLogFileOn(true);
	obj.SetErrorFileOn(false);
	obj.SetOutputFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpStringOn(false);
	obj.SetDumpFileOn(false);
	obj.SetLogFileName(LOG_FILENAME);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );

	CPPUNIT_ASSERT_EQUAL( true,  ::FileExists(LOG_FILENAME) );

	std::string lines[33];
	std::ifstream ifs(LOG_FILENAME);

	size_t i = 0;
	while (i < sizeof(lines)/sizeof(lines[0]) && std::getline(ifs, lines[i]))
	{
		++i;
	}

	CPPUNIT_ASSERT_EQUAL( 25u, i );

	int line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string("-------------------------------------------"), lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Beginning of initial solution calculations."), lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-------------------------------------------"), lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Initial solution 1.	"),                       lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Iterations in revise_guesses: 2"),             lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Number of infeasible solutions: 0"),           lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Number of basis changes: 0"),                  lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Number of iterations: 6"),                     lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------"),                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("End of simulation."),                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------"),                          lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 2."),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),        lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------"),                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("End of run."),                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------"),                                 lines[line++] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            lines[line++] );

	if (::FileExists(LOG_FILENAME))
	{
		::DeleteFile(LOG_FILENAME);
	}
}

void TestIPhreeqc::TestLogStringOnOff(void)
{
	IPhreeqc obj;
	CPPUNIT_ASSERT_EQUAL( false,    obj.GetLogStringOn() );

	obj.SetLogStringOn(true);
	CPPUNIT_ASSERT_EQUAL( true,     obj.GetLogStringOn() );

	obj.SetLogStringOn(false);
	CPPUNIT_ASSERT_EQUAL( false,    obj.GetLogStringOn() );
}

void TestIPhreeqc::TestGetLogString(void)
{
	char LOG_FILENAME[80];
	sprintf(LOG_FILENAME, "log.%06d.out", ::rand());
	if (::FileExists(LOG_FILENAME))
	{
		::DeleteFile(LOG_FILENAME);
	}
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(LOG_FILENAME) );

	IPhreeqc obj;

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("KNOBS") );
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("\t-logfile TRUE") );

	// run
	obj.SetLogFileOn(true);
	obj.SetLogStringOn(true);

	obj.SetDumpFileOn(false);
	obj.SetDumpStringOn(false);
	obj.SetErrorFileOn(false);
	obj.SetOutputFileOn(false);
	obj.SetOutputStringOn(false);
	obj.SetSelectedOutputFileOn(false);

	obj.SetLogFileName(LOG_FILENAME);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );

	CPPUNIT_ASSERT_EQUAL( true,  ::FileExists(LOG_FILENAME) );

	{
		std::ifstream ifs(LOG_FILENAME);
		std::string fline((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

		std::string sline(obj.GetLogString());
		CPPUNIT_ASSERT( sline.size() > 0 );

		CPPUNIT_ASSERT_EQUAL( fline, sline );
	}

	if (::FileExists(LOG_FILENAME))
	{
		::DeleteFile(LOG_FILENAME);
	}
}

void TestIPhreeqc::TestGetLogStringLineCount(void)
{
	IPhreeqc obj;
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetLogStringLineCount() );

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetLogStringLineCount() );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("KNOBS") );
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("\t-logfile TRUE") );

	// run
	obj.SetLogStringOn(false);
	obj.SetLogFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetOutputFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpStringOn(false);
	obj.SetDumpFileOn(false);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetLogStringLineCount() );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("KNOBS") );
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("\t-logfile TRUE") );

	obj.SetLogStringOn(true);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 29,    obj.GetLogStringLineCount() );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("KNOBS") );
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("\t-logfile TRUE") );

	obj.SetLogStringOn(false);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetLogStringLineCount() );
}

void TestIPhreeqc::TestGetLogStringLine(void)
{
	IPhreeqc obj;
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetLogStringLineCount() );

	CPPUNIT_ASSERT_EQUAL( 0,     obj.LoadDatabase("phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetLogStringLineCount() );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("KNOBS") );
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("\t-logfile TRUE") );

	// run
	obj.SetOutputStringOn(false);
	obj.SetOutputFileOn(false);
	obj.SetErrorFileOn(false);
	obj.SetLogFileOn(false);
	obj.SetSelectedOutputFileOn(false);
	obj.SetDumpStringOn(false);
	obj.SetDumpFileOn(false);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetLogStringLineCount() );

	int line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(-1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(-2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(-3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(-4)) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("KNOBS") );
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("\t-logfile TRUE") );

	obj.SetLogStringOn(true);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 29,    obj.GetLogStringLineCount() );
	
	line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),        std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 1."),        std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),        std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("-------------------------------------------"), std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Beginning of initial solution calculations."), std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("-------------------------------------------"), std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Initial solution 1.	"),                       std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Iterations in revise_guesses: 2"),             std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Number of infeasible solutions: 0"),           std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Number of basis changes: 0"),                  std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Number of iterations: 6"),                     std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------"),                          std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("End of simulation."),                          std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------"),                          std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),        std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 2."),        std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),        std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------"),                                 std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("End of run."),                                 std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------"),                                 std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(obj.GetLogStringLine(line++)) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::SOLUTION(obj, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::DUMP(obj) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("KNOBS") );
	CPPUNIT_ASSERT_EQUAL( VR_OK, obj.AccumulateLine("\t-logfile TRUE") );

	obj.SetLogStringOn(false);

	CPPUNIT_ASSERT_EQUAL( 0,     obj.RunAccumulated() );
	CPPUNIT_ASSERT_EQUAL( 0,     obj.GetLogStringLineCount() );

	line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(-1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(-2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(-3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(obj.GetLogStringLine(-4)) );
}
