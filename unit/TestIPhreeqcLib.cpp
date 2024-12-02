// -*- coding: windows-1252 -*-
#include "TestIPhreeqcLib.h"
#include "IPhreeqc.h"
#include "Phreeqc.h" /* snprintf */
#include "CVar.hxx"

#if defined(_WIN32)
#include <windows.h>
#endif

#include <fstream>
#include <string>
#include <string.h> // strstr
#include <cmath>
#include <cfloat>
#include <stdlib.h>

#include "FileTest.h"

#if defined(_WIN32) || defined(__CYGWIN32__)
// DeleteFile defined in <windows.h>
#else
int DeleteFile(const char* szPathName);
#endif

bool FileExists(const char *szPathName);
size_t FileSize(const char *szPathName);

IPQ_RESULT SOLUTION(int n, double C, double Ca, double Na);
IPQ_RESULT EQUILIBRIUM_PHASES(int n, const char* phase, double si, double amount);
IPQ_RESULT USER_PUNCH(int n, const char* element, int max);
IPQ_RESULT SELECTED_OUTPUT(int n);
IPQ_RESULT DUMP(int n);

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



TestIPhreeqcLib::TestIPhreeqcLib(void)
{
}

TestIPhreeqcLib::~TestIPhreeqcLib(void)
{
}

void TestIPhreeqcLib::TestCreateIPhreeqc(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestDestroyIPhreeqc(void)
{
	for (int i = 0; i < 10; ++i)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_BADINSTANCE, ::DestroyIPhreeqc(i));
	}

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestLoadDatabase(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);
	std::string FILES[] = { "../database/phreeqc.dat", "../database/pitzer.dat", "../database/wateq4f.dat",
		"../database/Amm.dat", "../database/frezchem.dat", "../database/iso.dat",
		"../database/llnl.dat", "../database/minteq.dat", "../database/minteq.v4.dat",
		"../database/sit.dat","../database/ColdChem.dat","../database/core10.dat",
		"../database/Tipping_Hurley.dat"
	};
	for (int j = 0; j < sizeof(FILES)/sizeof(std::string); ++j)
	{
		for (int i = 0; i < 10; ++i)
		{
			CPPUNIT_ASSERT_EQUAL(true,   ::FileExists(FILES[j].c_str()));
			CPPUNIT_ASSERT(::FileSize(FILES[j].c_str()) > 0);
			CPPUNIT_ASSERT_EQUAL(0,      ::LoadDatabase(n, FILES[j].c_str()));
		}
	}

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestLoadDatabaseString(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	for (int i = 0; i < 10; ++i)
	{
		CPPUNIT_ASSERT_EQUAL(0,      ::LoadDatabaseString(n, ex15_dat));
	}

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestLoadDatabaseMissingFile(void)
{
	CPPUNIT_ASSERT_EQUAL(false, ::FileExists("missing.file"));

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	for (int i = 0; i < 10; ++i)
	{
		CPPUNIT_ASSERT_EQUAL(1, ::LoadDatabase(n, "missing.file"));
	}

	const char expected[] =
		"ERROR: LoadDatabase: Unable to open:\"missing.file\".\n";

	const char* err = ::GetErrorString(n);

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestLoadDatabaseWithErrors(void)
{
	CPPUNIT_ASSERT_EQUAL(true, ::FileExists("missing_e.dat"));

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	for (int i = 0; i < 10; ++i)
	{
		CPPUNIT_ASSERT_EQUAL(true, ::FileExists("missing_e.dat"));
		CPPUNIT_ASSERT(::FileSize("missing_e.dat") > 0);
		CPPUNIT_ASSERT_EQUAL(6,    ::LoadDatabase(n, "missing_e.dat"));

		static const char *expected =
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

		const char* err = ::GetErrorString(n);

		CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );
	}

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestRunAccumulated(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(0,      ::LoadDatabase(n, "phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "solution 12"));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetErrorFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetLogFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetSelectedOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpStringOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::RunAccumulated(n));

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestRunWithErrors()
{
	const char dump_file[] = "error.inp";

	// remove dump file if it exists
	//
	FileTest dump(dump_file);
	CPPUNIT_ASSERT( dump.RemoveExisting() );

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(0,      ::LoadDatabase(n, "phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "SOLUTION 1"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "	pH	7"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "	Na	1"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "PHASES"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "	Fix_H+"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "	H+ = H+"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "	log_k	0"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "EQUILIBRIUM_PHASES"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "	Fix_H+ -10 HCl	10"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(n, "END"));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetErrorFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetLogFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetSelectedOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpStringOn(n));
	CPPUNIT_ASSERT_EQUAL(1,      ::RunAccumulated(n));

	const char expected[] =
		"ERROR: Numerical method failed on all combinations of convergence parameters, cell/soln/mix 1\n";
	const char* err = ::GetErrorString(n);

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
	CPPUNIT_ASSERT( dump.VerifyExists() );
}

void TestIPhreeqcLib::TestRunFile(void)
{
	static const char dump_file[] = "error.inp";

	// remove dump file if it exists
	//
	if (::FileExists(dump_file))
	{
		CPPUNIT_ASSERT(::DeleteFile(dump_file));
	}
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(dump_file) );

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(true, ::FileExists("phreeqc.dat"));
	CPPUNIT_ASSERT( ::FileSize("phreeqc.dat") > 0 );
	CPPUNIT_ASSERT_EQUAL(0,    ::LoadDatabase(n, "phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(0,    ::GetOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,    ::GetErrorFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,    ::GetLogFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,    ::GetSelectedOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,    ::GetDumpFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,    ::GetDumpStringOn(n));
	CPPUNIT_ASSERT_EQUAL(true, ::FileExists("conv_fail.in"));
	CPPUNIT_ASSERT( ::FileSize("conv_fail.in") > 0 );
	CPPUNIT_ASSERT_EQUAL(1,    ::RunFile(n, "conv_fail.in"));

	static const char expected[] =
		"ERROR: Numerical method failed on all combinations of convergence parameters, cell/soln/mix 1\n";
	const char* err = ::GetErrorString(n);

	CPPUNIT_ASSERT_EQUAL(std::string(expected), std::string(err));

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}

	// Note: should this file exist since GetDumpFileOn is false?
	CPPUNIT_ASSERT_EQUAL( true, ::FileExists(dump_file) );
	CPPUNIT_ASSERT( ::FileSize(dump_file) > 0 );
	CPPUNIT_ASSERT( ::DeleteFile(dump_file) );
}

void TestIPhreeqcLib::TestRunString(void)
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

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	char OUTPUT_FILE[80];
	snprintf(OUTPUT_FILE, sizeof(OUTPUT_FILE), "phreeqc.%d.out", n);

	if (::FileExists(OUTPUT_FILE))
	{
		CPPUNIT_ASSERT(::DeleteFile(OUTPUT_FILE));
	}
	CPPUNIT_ASSERT_EQUAL(false, ::FileExists(OUTPUT_FILE));

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(n, "phreeqc.dat"));
	::SetOutputFileOn(n, 1);
	::SetErrorFileOn(n, 0);
	::SetLogFileOn(n, 0);
	::SetSelectedOutputFileOn(n, 0);
	::SetDumpFileOn(n, 0);
	CPPUNIT_ASSERT_EQUAL(false, ::FileExists(OUTPUT_FILE));
	CPPUNIT_ASSERT_EQUAL(0,     ::RunString(n, input));
	CPPUNIT_ASSERT_EQUAL(true,  ::FileExists(OUTPUT_FILE));
	CPPUNIT_ASSERT(::FileSize(OUTPUT_FILE) > 0);
	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
	if (::FileExists(OUTPUT_FILE))
	{
		CPPUNIT_ASSERT(::DeleteFile(OUTPUT_FILE));
	}
}

void TestIPhreeqcLib::TestGetSelectedOutputRowCount()
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(true,   ::FileExists("llnl.dat"));
	CPPUNIT_ASSERT( ::FileSize("llnl.dat") > 0 );
	CPPUNIT_ASSERT_EQUAL(0,      ::LoadDatabase(n, "llnl.dat"));

	int max = 6;

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::EQUILIBRIUM_PHASES(n, "calcite", 0.0, 0.010));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::USER_PUNCH(n, "Ca", max));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetErrorFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetLogFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetSelectedOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpStringOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::RunAccumulated(n));
	CPPUNIT_ASSERT_EQUAL(3,      ::GetSelectedOutputRowCount(n)); // rows + header

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}
#define USE_VAR
void TestIPhreeqcLib::TestGetSelectedOutputValue(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(n, "llnl.dat"));

	int max = 6;

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::EQUILIBRIUM_PHASES(n, "calcite", 0.0, 0.010));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::USER_PUNCH(n, "Ca", max));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetErrorFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetLogFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetSelectedOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpStringOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::RunAccumulated(n));

	/*
	EXPECTED selected.out:
			 sim	       state	        soln	      dist_x	        time	        step	          pH	          pe	           C	          Ca	          Na	     m_CO3-2	     m_CaOH+	    m_NaCO3-	    la_CO3-2	    la_CaOH+	   la_NaCO3-	     Calcite	   d_Calcite	   si_CO2(g)	 si_Siderite	    pressure	   total mol	      volume	    g_CO2(g)	     g_N2(g)	    k_Albite	   dk_Albite	    k_Pyrite	   dk_Pyrite	     s_CaSO4	     s_SrSO4	      1.name	      1.type	     1.moles	      2.name	      2.type	     2.moles	      3.name	      3.type	     3.moles	      4.name	      4.type	     4.moles	      5.name	      5.type	     5.moles	      6.name	      6.type	     6.moles
			   1	      i_soln	           1	         -99	         -99	         -99	           7	           4	 1.0000e-003	 1.0000e-003	 1.0000e-003	 4.2975e-007	 1.1819e-009	 1.1881e-009	-6.4686e+000	-8.9530e+000	-8.9507e+000	 0.0000e+000	 0.0000e+000	     -2.2870	   -999.9990	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	        Ca+2	          aq	 9.9178e-004	     CaHCO3+	          aq	 7.5980e-006	       CaCO3	          aq	 6.2155e-007	       CaOH+	          aq	 1.1819e-009
			   1	       react	           1	         -99	           0	           1	     7.86135	       10.18	 1.1556e-003	 1.1556e-003	 1.0000e-003	 4.2718e-006	 9.7385e-009	 1.1620e-008	-5.4781e+000	-8.0388e+000	-7.9621e+000	 9.8444e-003	-1.5555e-004	     -3.0192	   -999.9990	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	     calcite	        equi	 9.8444e-003	        Ca+2	          aq	 1.1371e-003	     CaHCO3+	          aq	 1.1598e-005	       CaCO3	          aq	 6.8668e-006	       CaOH+	          aq	 9.7385e-009
	*/

#if defined(USE_VAR)
	VAR v;
	::VarInit(&v);
#else
	CVar v;
#endif

	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDROW, ::GetSelectedOutputValue(n, -1, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);

	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDROW, ::GetSelectedOutputValue(n, ::GetSelectedOutputRowCount(n), 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);

	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDCOL, ::GetSelectedOutputValue(n, 0, -1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, v.vresult);

	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDCOL, ::GetSelectedOutputValue(n, 0, ::GetSelectedOutputColumnCount(n), &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, v.vresult);


	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("sim"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, 1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("state"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, 2, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("soln"), std::string(v.sVal));

	//{{{{{{
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, 3, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("dist_x"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, 4, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("time"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, 5, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("step"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, 6, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("pH"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, 7, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("pe"), std::string(v.sVal));

	int col = 7;

	// -totals C Ca Na
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("C(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("Ca(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("Na(mol/kgw)"), std::string(v.sVal));

	// -molalities CO3-2  CaOH+  NaCO3-
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("m_CO3-2(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("m_CaOH+(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("m_NaCO3-(mol/kgw)"), std::string(v.sVal));

	// -activities CO3-2  CaOH+  NaCO3-
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("la_CO3-2"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("la_CaOH+"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("la_NaCO3-"), std::string(v.sVal));

	// -equilibrium_phases Calcite
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("Calcite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("d_Calcite"), std::string(v.sVal));


	// -saturation_indices CO2(g) Siderite
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("si_CO2(g)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("si_Siderite"), std::string(v.sVal));

	// -gases CO2(g) N2(g)
	//                      pressure "total mol" volume g_CO2(g) g_N2(g)
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("pressure"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("total mol"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("volume"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("g_CO2(g)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("g_N2(g)"), std::string(v.sVal));

	// -kinetic_reactants Albite Pyrite
	//                               k_Albite dk_Albite k_Pyrite dk_Pyrite
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("k_Albite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("dk_Albite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("k_Pyrite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("dk_Pyrite"), std::string(v.sVal));

	// -solid_solutions CaSO4 SrSO4
	//                              s_CaSO4 s_SrSO4
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("s_CaSO4"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("s_SrSO4"), std::string(v.sVal));

	for (int i = 0; i < max; ++i)
	{
		std::ostringstream oss1, oss2, oss3;

		// 1.name
		//
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col + 1 + (i*3), &v));
		CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
		oss1 << i + 1 << ".name";
		CPPUNIT_ASSERT_EQUAL( oss1.str(), std::string(v.sVal));

		// 1.type
		//
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col + 2 + (i*3), &v));
		CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
		oss2 << i + 1 << ".type";
		CPPUNIT_ASSERT_EQUAL( oss2.str(), std::string(v.sVal));

		// 1.moles
		//
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 0, col + 3 + (i*3), &v));
		CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
		oss3 << i + 1 << ".moles";
		CPPUNIT_ASSERT_EQUAL( oss3.str(), std::string(v.sVal));
	}

	// sim
	//
	col = 0;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );

	// state
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("i_soln"), std::string(v.sVal) );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("react"), std::string(v.sVal) );

	// soln
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );

	// dist_x -- sometimes as double sometimes as long (depends on state)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_EQUAL( -99., v.dVal );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_EQUAL( -99., v.dVal );


	// time -- sometimes as double sometimes as long (depends on state)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_EQUAL( -99., v.dVal );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, v.dVal, ::pow(10., -DBL_DIG) );

	// step
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL(-99L, v.lVal);
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_LONG, v.type);
	CPPUNIT_ASSERT_EQUAL(1L, v.lVal);


	// pH
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.0, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.861354, v.dVal, ::pow(10., -6) );

	// pe
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 4.0, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
// COMMENT: {8/8/2013 3:38:12 PM}	CPPUNIT_ASSERT_DOUBLES_EQUAL( 9.90852, v.dVal, ::pow(10., -1) );

	//
	// -totals C Ca Na
	//

	// C(mol/kgw)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0000e-003, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1556e-003, v.dVal, ::pow(10., -7) );


	// Ca(mol/kgw)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0000e-003, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1556e-003, v.dVal, ::pow(10., -7) );


	// Na(mol/kgw)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0000e-003, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
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
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("Ca+2"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("Calcite"),  std::string(v.sVal));

	// 1.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("equi"),  std::string(v.sVal));

	// 1.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 9.9177923E-04, v.dVal, ::pow(10., -11) );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 9.8444477E-03, v.dVal, ::pow(10., -10) );

	// 2.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("CaHCO3+"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca+2"),  std::string(v.sVal));

	// 2.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 2.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.5980e-006, v.dVal, ::pow(10., -10) );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1371e-003, v.dVal, ::pow(10., -7) );


	// 3.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("CaCO3"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("CaHCO3+"),  std::string(v.sVal));

	// 3.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 3.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 6.2155e-007, v.dVal, ::pow(10., -11) );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1598e-005, v.dVal, ::pow(10., -9) );



	// 4.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("CaOH+"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("CaCO3"),  std::string(v.sVal));

	// 4.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 4.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1819e-009, v.dVal, ::pow(10., -13) );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 6.8668e-006, v.dVal, ::pow(10., -10) );


	// 5.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("CaOH+"),  std::string(v.sVal));

	// 5.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 5.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_DOUBLE, v.type );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 9.7385e-009, v.dVal, ::pow(10., -13) );


	// 6.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );

	// 6.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );

	// 6.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 1, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(n, 2, col, &v));
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}

#if defined(USE_VAR)
	::VarClear(&v);
#endif
}

IPQ_RESULT
SOLUTION(int n, double C, double Ca, double Na)
{
	std::ostringstream oss;

	oss << "SOLUTION 1\n";
	oss << "C "  << C  << "\n";
	oss << "Ca " << Ca << "\n";
	oss << "Na " << Na << "\n";

	return ::AccumulateLine(n, oss.str().c_str());
}

IPQ_RESULT
EQUILIBRIUM_PHASES(int n, const char* phase, double si, double amount)
{
	std::ostringstream oss;

	oss << "EQUILIBRIUM_PHASES\n";
	oss << phase << " " << si << " " << amount << "\n";
	return ::AccumulateLine(n, oss.str().c_str());
}

IPQ_RESULT
USER_PUNCH(int n, const char* element, int max)
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

	return ::AccumulateLine(n, oss.str().c_str());
}

IPQ_RESULT
USER_PUNCH_NEH(int n)
{
	std::ostringstream oss;

	oss << "USER_PUNCH\n";

	oss << "-head head0 head1 head2\n";
	oss << "-start" << "\n";
	oss << "10 PUNCH \"have0\", \"have1\", \"have2\"" << "\n";
	oss << "20 PUNCH \"missing0\", \"missing1\", \"missing2\"" << "\n";
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

	return ::AccumulateLine(n, oss.str().c_str());
}


IPQ_RESULT
SELECTED_OUTPUT(int n)
{
	std::ostringstream oss;

	oss << "SELECTED_OUTPUT" << "\n";
	oss << "-totals C Ca Na" << "\n";

	return ::AccumulateLine(n, oss.str().c_str());
}

IPQ_RESULT
DUMP(int n)
{
	std::ostringstream oss;
	oss << "DUMP" << "\n";
	oss << "-solution 1" << "\n";
	return ::AccumulateLine(n, oss.str().c_str());
}

void TestIPhreeqcLib::TestGetSelectedOutputColumnCount(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "llnl.dat"));
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetSelectedOutputColumnCount(n) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::EQUILIBRIUM_PHASES(n, "calcite", 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::USER_PUNCH(n, "Ca", 10) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 62,     ::GetSelectedOutputColumnCount(n) );
	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestAddError(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(n, "phreeqc.dat"));

	// make sure initialized to empty
	//
	const char* err = ::GetErrorString(n);
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(err) );

	// make sure initialized to empty
	//
	const char *expected = "TESTING AddError\n";
	CPPUNIT_ASSERT_EQUAL(1, ::AddError(n, expected));

	// check 1
	//
	err = ::GetErrorString(n);
	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );

	// check increment
	//
	const char *expected2 = "XXXXXX\n";
	CPPUNIT_ASSERT_EQUAL(2, ::AddError(n, expected2));

	// check concatenation
	//
	err = ::GetErrorString(n);
	CPPUNIT_ASSERT_EQUAL( std::string(expected) + std::string(expected2), std::string(err) );


	// clear errors
	//
	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(n, "phreeqc.dat"));

	// make sure back to empty
	//
	err = ::GetErrorString(n);
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(err) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestAccumulateLine(void)
{
	// TODO
}

void TestIPhreeqcLib::TestOutputErrorString(void)
{
	// TODO
}

void TestIPhreeqcLib::TestRunWithCallback(void)
{
	// TODO
}

void TestIPhreeqcLib::TestRunNoDatabaseLoaded(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 1, ::RunAccumulated(n) );

	const char expected[] =
		"ERROR: RunAccumulated: No database is loaded\n";
	const char* err = ::GetErrorString(n);

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestCase1(void)
{
	// Case 1 (see do_run)
	// pr.punch == TRUE
	// punch.new_def == FALSE
	// output_isopen(OUTPUT_PUNCH) == FALSE
	// selected_output_on == TRUE

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	char SELECTED_OUT[80];
	snprintf(SELECTED_OUT, sizeof(SELECTED_OUT), "selected_1.%d.out", n);

	// remove punch file if it exists
	if (::FileExists(SELECTED_OUT))
	{
		CPPUNIT_ASSERT(::DeleteFile(SELECTED_OUT));
	}
	CPPUNIT_ASSERT_EQUAL( false,      ::FileExists(SELECTED_OUT) );
	CPPUNIT_ASSERT_EQUAL( true,       ::FileExists("phreeqc.dat") );
	CPPUNIT_ASSERT( ::FileSize("phreeqc.dat") > 0 );
	CPPUNIT_ASSERT_EQUAL( 0,          ::LoadDatabase(n, "phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK,     ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK,     ::USER_PUNCH(n, "Ca", 10) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK,     ::SetSelectedOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( 0,          ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( true,       ::FileExists(SELECTED_OUT) );
	CPPUNIT_ASSERT( ::FileSize(SELECTED_OUT) > 0 );
	CPPUNIT_ASSERT_EQUAL( 62,         ::GetSelectedOutputColumnCount(n) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK,     ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK,     ::SetSelectedOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( 0,          ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( true,       ::FileExists(SELECTED_OUT) );
	CPPUNIT_ASSERT( ::FileSize(SELECTED_OUT) > 0 );
	CPPUNIT_ASSERT_EQUAL( 62,         ::GetSelectedOutputColumnCount(n) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestCase2(void)
{
	// Case 2 (see do_run)
	// pr.punch == TRUE
	// punch.new_def == TRUE
	// output_isopen(OUTPUT_PUNCH) == FALSE
	// selected_output_on == TRUE

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	char SELECTED_OUT[80];
	snprintf(SELECTED_OUT, sizeof(SELECTED_OUT), "selected_1.%d.out", n);

	// remove punch files if they exists
	//
	if (::FileExists(SELECTED_OUT))
	{
		CPPUNIT_ASSERT(::DeleteFile(SELECTED_OUT));
	}
	if (::FileExists("case2.punch"))
	{
		CPPUNIT_ASSERT(::DeleteFile("case2.punch"));
	}
	CPPUNIT_ASSERT_EQUAL( false,   ::FileExists(SELECTED_OUT) );
	CPPUNIT_ASSERT_EQUAL( false,   ::FileExists("case2.punch") );
	CPPUNIT_ASSERT_EQUAL( 0,       ::LoadDatabase(n, "phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK,  ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK,  ::USER_PUNCH(n, "Ca", 10) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK,  ::AccumulateLine(n, "-file case2.punch") ); // force have_punch_name to TRUE (see read_selected_ouput)
	CPPUNIT_ASSERT_EQUAL( IPQ_OK,  ::SetSelectedOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( 0,       ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( false,   ::FileExists(SELECTED_OUT) );
	CPPUNIT_ASSERT_EQUAL( true,    ::FileExists("case2.punch") );
	CPPUNIT_ASSERT( ::FileSize("case2.punch") > 0 );
	CPPUNIT_ASSERT_EQUAL( 62,      ::GetSelectedOutputColumnCount(n) );


	// remove punch files if they exists
	//
	if (::FileExists(SELECTED_OUT))
	{
		CPPUNIT_ASSERT(::DeleteFile(SELECTED_OUT));
	}
	if (::FileExists("case2.punch"))
	{
		CPPUNIT_ASSERT(::DeleteFile("case2.punch"));
	}
	CPPUNIT_ASSERT_EQUAL( false,  ::FileExists(SELECTED_OUT) );
	CPPUNIT_ASSERT_EQUAL( false,  ::FileExists("case2.punch") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::USER_PUNCH(n, "Ca", 10) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( false,  ::FileExists(SELECTED_OUT) );
	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists("case2.punch") );
	CPPUNIT_ASSERT( ::FileSize("case2.punch") > 0 );
	CPPUNIT_ASSERT_EQUAL( 62,     ::GetSelectedOutputColumnCount(n) );

	if (::FileExists("case2.punch"))
	{
		CPPUNIT_ASSERT(::DeleteFile("case2.punch"));
	}
	CPPUNIT_ASSERT_EQUAL( false,  ::FileExists("case2.punch") );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestPrintSelectedOutputFalse(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	// remove punch files if they exists
	//
	if (::FileExists("selected.out"))
	{
		::DeleteFile("selected.out");
	}
	CPPUNIT_ASSERT_EQUAL( false,  ::FileExists("selected.out") );

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SELECTED_OUTPUT(n) );

	// turn off selected output
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "PRINT; -selected_output false \n") );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetSelectedOutputColumnCount(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetSelectedOutputRowCount(n) );


	// reset pr.punch to TRUE
	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SELECTED_OUTPUT(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( 11,     ::GetSelectedOutputColumnCount(n) );
	CPPUNIT_ASSERT_EQUAL( 2,      ::GetSelectedOutputRowCount(n) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestOutputFileOnOff()
{
	int onoff[5];
	onoff[0] = 1;  // output_file_on
	onoff[1] = 0;  // error_file_on
	onoff[2] = 0;  // log_file_on
	onoff[3] = 0;  // selected_output_file_on
	onoff[4] = 0;  // dump_file_on
	TestFileOnOff("phreeqc.%d.out", onoff[0], onoff[1], onoff[2], onoff[3], onoff[4]);
}

void TestIPhreeqcLib::TestErrorFileOnOff()
{
	int onoff[5];
	onoff[0] = 0;  // output_file_on
	onoff[1] = 1;  // error_file_on
	onoff[2] = 0;  // log_file_on
	onoff[3] = 0;  // selected_output_file_on
	onoff[4] = 0;  // dump_file_on
	TestFileOnOff("phreeqc.%d.err", onoff[0], onoff[1], onoff[2], onoff[3], onoff[4]);
}

void TestIPhreeqcLib::TestLogFileOnOff()
{
	int onoff[5];
	onoff[0] = 0;  // output_file_on
	onoff[1] = 0;  // error_file_on
	onoff[2] = 1;  // log_file_on
	onoff[3] = 0;  // selected_output_file_on
	onoff[4] = 0;  // dump_file_on
	TestFileOnOff("phreeqc.%d.log", onoff[0], onoff[1], onoff[2], onoff[3], onoff[4]);
}

void TestIPhreeqcLib::TestDumpFileOnOff()
{
	int onoff[5];
	onoff[0] = 0;  // output_file_on
	onoff[1] = 0;  // error_file_on
	onoff[2] = 0;  // log_file_on
	onoff[3] = 0;  // selected_output_file_on
	onoff[4] = 1;  // dump_file_on
	TestFileOnOff("dump.%d.out", onoff[0], onoff[1], onoff[2], onoff[3], onoff[4]);
}

void TestIPhreeqcLib::TestSelOutFileOnOff()
{
	int onoff[5];
	onoff[0] = 0;  // output_file_on
	onoff[1] = 0;  // error_file_on
	onoff[2] = 0;  // log_file_on
	onoff[3] = 1;  // selected_output_file_on
	onoff[4] = 0;  // dump_file_on
	TestFileOnOff("selected_1.%d.out", onoff[0], onoff[1], onoff[2], onoff[3], onoff[4]);
}

void TestIPhreeqcLib::TestFileOnOff(const char* FILENAME_FORMAT, int output_file_on, int error_file_on, int log_file_on, int selected_output_file_on, int dump_file_on)
{
	int dump_string_on = 0;

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	char FILENAME[80];
	snprintf(FILENAME, sizeof(FILENAME), FILENAME_FORMAT, n);

	// remove FILENAME if it exists
	//
	if (::FileExists(FILENAME))
	{
		::DeleteFile(FILENAME);
	}
	CPPUNIT_ASSERT_EQUAL( false,  ::FileExists(FILENAME) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SELECTED_OUTPUT(n) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run all off
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( false,  ::FileExists(FILENAME) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SELECTED_OUTPUT(n) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, dump_file_on) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, error_file_on) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, log_file_on) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, output_file_on) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, selected_output_file_on) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, dump_string_on) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(FILENAME) );
	CPPUNIT_ASSERT( ::DeleteFile(FILENAME) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SELECTED_OUTPUT(n) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( false,  ::FileExists(FILENAME) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SELECTED_OUTPUT(n) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, dump_file_on) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, error_file_on) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, log_file_on) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, output_file_on) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, selected_output_file_on) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, dump_string_on) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(FILENAME) );
	CPPUNIT_ASSERT( ::DeleteFile(FILENAME) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}


void TestIPhreeqcLib::TestLongHeadings(void)
{
	char long_header[] = "this_is_a_long_header_0123456789012345678901234567890123456789";
	char long_value[]  = "this_is_a_long_value_01234567890123456789012345678901234567890";

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(n, "phreeqc.dat"));

	std::ostringstream oss;
	oss << "SOLUTION" << "\n";

	oss << "SELECTED_OUTPUT" << "\n";
	oss << "-reset false" << "\n";

	oss << "USER_PUNCH" << "\n";
	oss << "-head " <<  long_header << "\n";
	oss << "-start" << "\n";
	oss << "10 PUNCH \"" << long_value << "\"\n";
	oss << "-end" << "\n";
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, oss.str().c_str()) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( 2,      ::GetSelectedOutputRowCount(n));
	CPPUNIT_ASSERT_EQUAL( 1,      ::GetSelectedOutputColumnCount(n));

	CVar v;
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::GetSelectedOutputValue(n, 0, 0, &v));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL( std::string(long_header), std::string(v.sVal));

	CVar v1;
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::GetSelectedOutputValue(n, 1, 0, &v1));
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v1.type);
	CPPUNIT_ASSERT_EQUAL( std::string(long_value), std::string(v1.sVal));

	CPPUNIT_ASSERT_EQUAL( IPQ_INVALIDCOL, ::GetSelectedOutputValue(n, 1, 1, &v1));
	CPPUNIT_ASSERT_EQUAL( IPQ_INVALIDROW, ::GetSelectedOutputValue(n, 2, 0, &v1));

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestDatabaseKeyword()
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( 1,      ::RunFile(n, "dump"));

	const char *exp_errs =
		"ERROR: Gas not found in PHASES database, Amm(g).\n"
		"ERROR: Calculations terminating due to input errors.\n";

	const char* err = ::GetErrorString(n);
	CPPUNIT_ASSERT_EQUAL(std::string(exp_errs), std::string(err));

	const char *exp_warn =
		"WARNING: DATABASE keyword is ignored by IPhreeqc.\n"
		"WARNING: Cell-lengths were read for 1 cells. Last value is used till cell 100.\n"
		"WARNING: No dispersivities were read; disp = 0 assumed.\n"
		"WARNING: Could not find element in database, Amm.\n"
		"	Concentration is set to zero.\n";

	const char* warn = ::GetWarningString(n);
	CPPUNIT_ASSERT_EQUAL(std::string(exp_warn), std::string(warn));

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestDumpString()
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(0,       ::LoadDatabase(n, "phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	const char* dump_str = ::GetDumpString(n);

	CPPUNIT_ASSERT(::strstr(dump_str, "SOLUTION_RAW")                  != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-temp")                         != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-total_h")                      != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-total_o")                      != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-cb")                           != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-totals")                       != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " C(4) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " Ca ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " H(0) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " Na ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-pH")                           != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-pe")                           != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-mu")                           != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-ah2o")                         != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-mass_water")                   != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-total_alkalinity")             != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-activities")                   != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " C(-4) ")                       != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " C(4) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " Ca ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " E ")                           != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " H(0) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " Na ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, " O(0) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "-gammas")                       != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "USE mix none")                  != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "USE reaction none")             != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "USE reaction_temperature none") != NULL);
	CPPUNIT_ASSERT(::strstr(dump_str, "USE reaction_pressure none")    != NULL);

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetDumpStringLineCount(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 34,     ::GetDumpStringLineCount(n) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetDumpStringLine(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 34,     ::GetDumpStringLineCount(n) );

	int line = 0;

	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "SOLUTION_RAW")                  != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-temp")                         != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-pressure")                     != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-potential")                    != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-total_h")                      != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-total_o")                      != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-cb")                           != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-density")                      != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-viscosity")                    != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-totals")                       != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " C(4) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " Ca ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " H(0) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " Na ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-pH")                           != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-pe")                           != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-mu")                           != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-ah2o")                         != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-mass_water")                   != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-soln_vol")                     != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-total_alkalinity")             != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-activities")                   != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " C(-4) ")                       != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " C(4) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " Ca ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " E ")                           != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " H(0) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " Na ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), " O(0) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "-gammas")                       != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "USE mix none")                  != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "USE reaction none")             != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "USE reaction_temperature none") != NULL);
	CPPUNIT_ASSERT(::strstr(::GetDumpStringLine(n, line++), "USE reaction_pressure none")    != NULL);

	// remaining lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(::GetDumpStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(::GetDumpStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(::GetDumpStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(::GetDumpStringLine(n, line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(::GetDumpStringLine(n, -1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(::GetDumpStringLine(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(::GetDumpStringLine(n, -3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(::GetDumpStringLine(n, -4)) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetComponentCount(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 3,      ::GetComponentCount(n) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetComponent(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 3,      ::GetComponentCount(n) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, -1)) );

	CPPUNIT_ASSERT_EQUAL( std::string("C"),  std::string(::GetComponent(n, 0)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca"), std::string(::GetComponent(n, 1)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Na"), std::string(::GetComponent(n, 2)) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 4)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 5)) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 3,      ::GetComponentCount(n) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, -1)) );

	CPPUNIT_ASSERT_EQUAL( std::string("C"),  std::string(::GetComponent(n, 0)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca"), std::string(::GetComponent(n, 1)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Na"), std::string(::GetComponent(n, 2)) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 4)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 5)) );

	// clear using LoadDatabase
	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetComponentCount(n) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, -1)) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 0)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 1)) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 3,      ::GetComponentCount(n) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, -1)) );

	CPPUNIT_ASSERT_EQUAL( std::string("C"),  std::string(::GetComponent(n, 0)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca"), std::string(::GetComponent(n, 1)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Na"), std::string(::GetComponent(n, 2)) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 4)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 5)) );


	// clear using LoadDatabaseString
	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabaseString(n, ex15_dat) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetComponentCount(n) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	// ex15.dat doesn't have Ca
	CPPUNIT_ASSERT_EQUAL( 2, ::GetWarningStringLineCount(n));
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Could not find element in database, Ca."), std::string(::GetWarningStringLine(n, 0)) );
	CPPUNIT_ASSERT_EQUAL( std::string("\tConcentration is set to zero."), std::string(::GetWarningStringLine(n, 1)) );

	CPPUNIT_ASSERT_EQUAL( 2,      ::GetComponentCount(n) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, -1)) );

	CPPUNIT_ASSERT_EQUAL( std::string("C"),  std::string(::GetComponent(n, 0)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Na"), std::string(::GetComponent(n, 1)) );

	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),   std::string(::GetComponent(n, 3)) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetErrorStringLine(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( 1,      ::RunFile(n, "dump") );
#if 0
	CPPUNIT_ASSERT_EQUAL( 8,      ::GetErrorStringLineCount(n) );
#else
	CPPUNIT_ASSERT_EQUAL( 2,      ::GetErrorStringLineCount(n) );
#endif

	int line = 0;
#if 0
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: DATABASE keyword is ignored by IPhreeqc."),                              std::string(::GetErrorStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Cell-lengths were read for 1 cells. Last value is used till cell 100."), std::string(::GetErrorStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: No dispersivities were read; disp = 0 assumed."),                        std::string(::GetErrorStringLine(n, line++)) );
#endif
	CPPUNIT_ASSERT_EQUAL( std::string("ERROR: Gas not found in PHASES database, Amm(g)."),                              std::string(::GetErrorStringLine(n, line++)) );
#if 0
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Could not find element in database, Amm."),                              std::string(::GetErrorStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("\tConcentration is set to zero."),                                                std::string(::GetErrorStringLine(n, line++)) );
#endif
	CPPUNIT_ASSERT_EQUAL( std::string("ERROR: Calculations terminating due to input errors."),                           std::string(::GetErrorStringLine(n, line++)) );
#if 0
	CPPUNIT_ASSERT_EQUAL( std::string("Stopping."),                                                                      std::string(::GetErrorStringLine(n, line++)) );
#endif

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestErrorFileOn(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	char FILENAME[80];
	snprintf(FILENAME, sizeof(FILENAME), "phreeqc.%d.err", n);

	if (::FileExists(FILENAME))
	{
		::DeleteFile(FILENAME);
	}

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( 1,      ::RunFile(n, "dump") );
	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(FILENAME) );

	std::string lines[10];
	std::ifstream ifs(FILENAME);

	size_t i = 0;
	while (i < sizeof(lines)/sizeof(lines[0]) && std::getline(ifs, lines[i]))
	{
		++i;
	}
	
	CPPUNIT_ASSERT_EQUAL((size_t)8, i);

	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: DATABASE keyword is ignored by IPhreeqc."),                                 lines[0] );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Cell-lengths were read for 1 cells. Last value is used till cell 100."),    lines[1] );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: No dispersivities were read; disp = 0 assumed."),                           lines[2] );
	CPPUNIT_ASSERT_EQUAL( std::string("ERROR: Gas not found in PHASES database, Amm(g)."),                                 lines[3] );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Could not find element in database, Amm."),                                 lines[4] );
	CPPUNIT_ASSERT_EQUAL( std::string("\tConcentration is set to zero."),                                                   lines[5] );
	CPPUNIT_ASSERT_EQUAL( std::string("ERROR: Calculations terminating due to input errors."),                              lines[6] );
	CPPUNIT_ASSERT_EQUAL( std::string("Stopping."),                                                                         lines[7] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                  lines[8] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                  lines[9] );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestLogFileOn(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	char FILENAME[80];
	snprintf(FILENAME, sizeof(FILENAME), "phreeqc.%d.log", n);

	if (::FileExists(FILENAME))
	{
		::DeleteFile(FILENAME);
	}

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL( 1,      ::RunFile(n, "dump") );
	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(FILENAME) );

	std::string lines[10];
	std::ifstream ifs(FILENAME);

	size_t i = 0;
	while (i < sizeof(lines)/sizeof(lines[0]) && std::getline(ifs, lines[i]))
	{
		++i;
	}
	
	CPPUNIT_ASSERT_EQUAL((size_t)6, i);

	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Cell-lengths were read for 1 cells. Last value is used till cell 100."),    lines[0] );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: No dispersivities were read; disp = 0 assumed."),                           lines[1] );
	CPPUNIT_ASSERT_EQUAL( std::string("ERROR: Gas not found in PHASES database, Amm(g)."),                                 lines[2] );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Could not find element in database, Amm."),                                 lines[3] );
	CPPUNIT_ASSERT_EQUAL( std::string("\tConcentration is set to zero."),                                                   lines[4] );
	CPPUNIT_ASSERT_EQUAL( std::string("ERROR: Calculations terminating due to input errors."),                              lines[5] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                  lines[8] );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                  lines[9] );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetWarningStringLine(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( 1,      ::RunFile(n, "dump") );

	CPPUNIT_ASSERT_EQUAL( 5,      ::GetWarningStringLineCount(n) );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: DATABASE keyword is ignored by IPhreeqc."),                              std::string(::GetWarningStringLine(n, 0)) );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Cell-lengths were read for 1 cells. Last value is used till cell 100."), std::string(::GetWarningStringLine(n, 1)) );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: No dispersivities were read; disp = 0 assumed."),                        std::string(::GetWarningStringLine(n, 2)) );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Could not find element in database, Amm."),                              std::string(::GetWarningStringLine(n, 3)) );
	CPPUNIT_ASSERT_EQUAL( std::string("	Concentration is set to zero."),                                                 std::string(::GetWarningStringLine(n, 4)) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestPitzer(void)
{
	CPPUNIT_ASSERT_EQUAL(true, ::FileExists("../database/pitzer.dat"));
	CPPUNIT_ASSERT( ::FileSize("../database/pitzer.dat") > 0 );

	int id = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(id >= 0);

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(id, "../database/pitzer.dat"));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "SOLUTION 1")           );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "units mol/kgw")        );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "pH    7")              );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "temp  25")             );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "Mg    0.1    charge")  );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "Cl    0")              );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "S(6)  1.0612244897959"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "EQUILIBRIUM_PHASES 1") );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "Halite       0 10")    );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "Sylvite      0 10")    );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "Bischofite   0 0")     );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "Carnallite   0 0")     );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "Epsomite     0 0")     );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "Kieserite    0 0")     );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "END")                  );

	// this fails -r4375
	// fixed in -r4380
	CPPUNIT_ASSERT_EQUAL(0, ::RunAccumulated(id));

	if (id >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(id));
	}
}

void TestIPhreeqcLib::TestClearAccumulatedLines(void)
{
	CPPUNIT_ASSERT_EQUAL(true, ::FileExists("../database/wateq4f.dat"));
	CPPUNIT_ASSERT( ::FileSize("../database/wateq4f.dat") > 0 );

	int id = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(id >= 0);

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(id, "../database/wateq4f.dat"));

	// phreeqc can now handle pH of -2 (-r5885)
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "SOLUTION 1"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "	pH	7"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "	Na	1"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "PHASES"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "	Fix_H+"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "	H+ = H+"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "	log_k	0"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "EQUILIBRIUM_PHASES"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "	Fix_H+ -10 HCl	10"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "END"));

	CPPUNIT_ASSERT_EQUAL(1, ::RunAccumulated(id));

	CPPUNIT_ASSERT_EQUAL(1, ::GetErrorStringLineCount(id));

	const char expected[] =
		"ERROR: Numerical method failed on all combinations of convergence parameters, cell/soln/mix 1\n";
	const char* err = ::GetErrorString(id);

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "SOLUTION 1"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "pH 2")      );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "END")       );

	// RunAccumulated clears accumulated lines on next call to AccumulateLine
	//
	CPPUNIT_ASSERT_EQUAL(0, ::RunAccumulated(id)         );
	CPPUNIT_ASSERT_EQUAL(0, ::GetErrorStringLineCount(id));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::ClearAccumulatedLines(id)       );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "SOLUTION 1"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "pH 2")      );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::AccumulateLine(id, "END")       );

	CPPUNIT_ASSERT_EQUAL(0, ::RunAccumulated(id));

	if (id >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(id));
	}
}

void TestIPhreeqcLib::TestSetDumpFileName(void)
{
	char DUMP_FILENAME[80];
	snprintf(DUMP_FILENAME, sizeof(DUMP_FILENAME), "dump.%06d.out", ::rand());
	if (::FileExists(DUMP_FILENAME))
	{
		::DeleteFile(DUMP_FILENAME);
	}

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileName(n, DUMP_FILENAME) );

	CPPUNIT_ASSERT_EQUAL( std::string(DUMP_FILENAME),      std::string(::GetDumpFileName(n)) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( std::string(DUMP_FILENAME),      std::string(::GetDumpFileName(n)) );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(DUMP_FILENAME) );

	std::string lines[44];
	std::ifstream ifs(DUMP_FILENAME);

	size_t i = 0;
	while (i < sizeof(lines)/sizeof(lines[0]) && std::getline(ifs, lines[i]))
	{
		++i;
	}

	int line = 0;
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "SOLUTION_RAW")                  != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-temp")                         != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-pressure")                     != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-potential")                    != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-total_h")                      != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-total_o")                      != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-cb")                           != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-density")                      != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-viscosity")                    != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-totals")                       != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " C(4) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " Ca ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " H(0) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " Na ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-pH")                           != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-pe")                           != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-mu")                           != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-ah2o")                         != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-mass_water")                   != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-soln_vol")                     != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-total_alkalinity")             != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-activities")                   != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " C(-4) ")                       != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " C(4) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " Ca ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " E ")                           != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " H(0) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " Na ")                          != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), " O(0) ")                        != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "-gammas")                       != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "USE mix none")                  != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "USE reaction none")             != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "USE reaction_temperature none") != NULL);
	CPPUNIT_ASSERT(::strstr(lines[line++].c_str(), "USE reaction_pressure none")    != NULL);

	if (::FileExists(DUMP_FILENAME))
	{
		::DeleteFile(DUMP_FILENAME);
	}

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestSetOutputFileName(void)
{
	char OUTPUT_FILENAME[80];
	snprintf(OUTPUT_FILENAME, sizeof(OUTPUT_FILENAME), "output.%06d.out", ::rand());
	if (::FileExists(OUTPUT_FILENAME))
	{
		::DeleteFile(OUTPUT_FILENAME);
	}

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,     ::LoadDatabase(n, "phreeqc.dat.old"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileName(n, OUTPUT_FILENAME) );

	CPPUNIT_ASSERT_EQUAL( std::string(OUTPUT_FILENAME),      std::string(::GetOutputFileName(n)) );


	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(OUTPUT_FILENAME) );

	std::string lines[200];
	std::ifstream ifs(OUTPUT_FILENAME);

	size_t i = 0;
	while (i < sizeof(lines)/sizeof(lines[0]) && std::getline(ifs, lines[i]))
	{
		++i;
	}

	CPPUNIT_ASSERT_EQUAL( (size_t)100, i );

	int line = 0;

	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "------------------------------------")                                             != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "Reading input data for simulation 1.")                                             != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "------------------------------------")                                             != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "	SOLUTION 1")                                                                    != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "	C 1")                                                                           != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "	Ca 1")                                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "	Na 1")                                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "	DUMP")                                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "	-solution 1")                                                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "-------------------------------------------")                                      != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "Beginning of initial solution calculations.")                                      != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "-------------------------------------------")                                      != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "Initial solution 1.	")                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "-----------------------------Solution composition--------")                        != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "	Elements           Molality       Moles")                                       != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "	C                ")                                                             != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "	Ca               ")                                                             != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "	Na               ")                                                             != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "----------------------------Description of solution------")                        != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                                       pH  =  ")                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                                       pe  =  ")                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                        Activity of water  =  ")                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                 Ionic strength (mol/kgw)  =  ")                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                       Mass of water (kg)  =  ")                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                 Total alkalinity (eq/kg)  =  ")                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                       Total CO2 (mol/kg)  =  ")                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                         Temperature (°C)  =  ")                                   != NULL);
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                  Electrical balance (eq)  =  ")                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), " Percent error, 100*(Cat-|An|)/(Cat+|An|)  =  ")                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                               Iterations  =  ")                                   != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                                  Total H  = ")                                    != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                                  Total O  = ")                                    != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "----------------------------Distribution of species----------------------------")  != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "                                               Log       Log       Log    mole V") != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   Species          Molality    Activity  Molality  Activity     Gamma   cm³/mol") != NULL);
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   OH- ")                                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   H+ ")                                                                           != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   H2O ")                                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "C(-4) ")                                                                           != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   CH4 ")                                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "C(4) ")                                                                            != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   HCO3- ")                                                                        != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   CO2 ")                                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   CaHCO3+ ")                                                                      != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   CaCO3 ")                                                                        != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   CO3-2 ")                                                                        != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   NaHCO3 ")                                                                       != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   NaCO3- ")                                                                       != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "Ca ")                                                                              != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   Ca+2 ")                                                                         != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   CaHCO3+ ")                                                                      != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   CaCO3 ")                                                                        != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   CaOH+ ")                                                                        != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "H(0) ")                                                                            != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   H2 ")                                                                           != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "Na ")                                                                              != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   Na+ ")                                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   NaHCO3 ")                                                                       != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   NaCO3- ")                                                                       != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   NaOH ")                                                                         != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "O(0) ")                                                                            != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "   O2 ")                                                                           != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "------------------------------Saturation indices-------------------------------")  != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "  Phase               SI** log IAP   log K(298 K,   1 atm)")                       != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "  Aragonite")                                                                      != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "  Calcite")                                                                        != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "  CH4(g)")                                                                         != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "  CO2(g)")                                                                         != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "  H2(g)")                                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "  H2O(g)")                                                                         != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "  O2(g)")                                                                          != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "**For a gas, SI = log10(fugacity). Fugacity = pressure * phi / 1 atm.")            != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "  For ideal gases, phi = 1.")                                                      != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "------------------")                                                               != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "End of simulation.")                                                               != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "------------------")                                                               != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "------------------------------------")                                             != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "Reading input data for simulation 2.")                                             != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "------------------------------------")                                             != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "----------------")                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "End of Run after ")                                                                != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "----------------")                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );
	CPPUNIT_ASSERT( ::strstr(lines[line++].c_str(), "")                                                                                 != NULL );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}

	if (::FileExists(OUTPUT_FILENAME))
	{
		::DeleteFile(OUTPUT_FILENAME);
	}
}

void TestIPhreeqcLib::TestOutputStringOnOff(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( false,  ::GetOutputStringOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( true,   ::GetOutputStringOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( false,  ::GetOutputStringOn(n) != 0 );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetOutputString(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	char OUTPUT_FILENAME[80];
	snprintf(OUTPUT_FILENAME, sizeof(OUTPUT_FILENAME), "output.%06d.out", ::rand());
	if (::FileExists(OUTPUT_FILENAME))
	{
		::DeleteFile(OUTPUT_FILENAME);
	}
	CPPUNIT_ASSERT_EQUAL( false,  ::FileExists(OUTPUT_FILENAME) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileName(n, OUTPUT_FILENAME) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(OUTPUT_FILENAME) );

	{
		std::ifstream ifs(OUTPUT_FILENAME);
		std::string fline((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

		std::string sline(::GetOutputString(n));
		CPPUNIT_ASSERT( sline.size() > 0 );

		CPPUNIT_ASSERT_EQUAL( fline, sline );
	}

	if (::FileExists(OUTPUT_FILENAME))
	{
		::DeleteFile(OUTPUT_FILENAME);
	}
	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetOutputStringLineCount(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetOutputStringLineCount(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat.old"));
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetOutputStringLineCount(n) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetOutputStringLineCount(n) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 100,    ::GetOutputStringLineCount(n) );


	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetOutputStringLineCount(n) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetOutputStringLine(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetOutputStringLineCount(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat.old"));
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetOutputStringLineCount(n) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetOutputStringLineCount(n) );

	int line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, -1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, -3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, -4)) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 100,    ::GetOutputStringLineCount(n) );

	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                std::string(::GetOutputStringLine(n, 0))  );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 1."),                                                std::string(::GetOutputStringLine(n, 1))  );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),                                                std::string(::GetOutputStringLine(n, 2))  );
	CPPUNIT_ASSERT_EQUAL( std::string("-----------------------------Solution composition------------------------------"),     std::string(::GetOutputStringLine(n, 16)) );
	CPPUNIT_ASSERT_EQUAL( std::string("----------------------------Description of solution----------------------------"),     std::string(::GetOutputStringLine(n, 24)) );
	CPPUNIT_ASSERT_EQUAL( std::string("----------------------------Distribution of species----------------------------"),     std::string(::GetOutputStringLine(n, 40)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------Saturation indices-------------------------------"),     std::string(::GetOutputStringLine(n, 73)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                                                                    std::string(::GetOutputStringLine(n, 100)) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetOutputStringLineCount(n) );

	line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, -1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, -3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetOutputStringLine(n, -4)) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestSetLogFileName(void)
{
	char LOG_FILENAME[80];
	snprintf(LOG_FILENAME, sizeof(LOG_FILENAME), "log.%06d.out", ::rand());
	if (::FileExists(LOG_FILENAME))
	{
		::DeleteFile(LOG_FILENAME);
	}

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "KNOBS") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "\t-logfile TRUE") );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileName(n, LOG_FILENAME) );


	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(LOG_FILENAME) );

	std::string lines[33];
	std::ifstream ifs(LOG_FILENAME);

	size_t i = 0;
	while (i < sizeof(lines)/sizeof(lines[0]) && std::getline(ifs, lines[i]))
	{
		++i;
	}

	CPPUNIT_ASSERT_EQUAL( (size_t)25, i );

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
	line++;
	line++;
	line++;
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            lines[line++] );

	if (::FileExists(LOG_FILENAME))
	{
		::DeleteFile(LOG_FILENAME);
	}

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestLogStringOnOff(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( false,    ::GetLogStringOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,   ::SetLogStringOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( true,     ::GetLogStringOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,   ::SetLogStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( false,    ::GetLogStringOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,   ::DestroyIPhreeqc(n) );
}

void TestIPhreeqcLib::TestGetLogString(void)
{
	char LOG_FILENAME[80];
	snprintf(LOG_FILENAME, sizeof(LOG_FILENAME), "log.%06d.out", ::rand());
	if (::FileExists(LOG_FILENAME))
	{
		::DeleteFile(LOG_FILENAME);
	}
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(LOG_FILENAME) );

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat"));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "KNOBS") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "\t-logfile TRUE") );


	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileName(n, LOG_FILENAME) );
	CPPUNIT_ASSERT_EQUAL( std::string(LOG_FILENAME), std::string(::GetLogFileName(n)) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( std::string(LOG_FILENAME), std::string(::GetLogFileName(n)) );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(LOG_FILENAME) );
	
	{
		std::ifstream ifs(LOG_FILENAME);
		std::string fline((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

		std::string sline(::GetLogString(n));
		CPPUNIT_ASSERT( sline.size() > 0 );

		CPPUNIT_ASSERT_EQUAL( fline, sline );
	}

	if (::FileExists(LOG_FILENAME))
	{
		::DeleteFile(LOG_FILENAME);
	}

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetLogStringLineCount(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetLogStringLineCount(n));

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetLogStringLineCount(n));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "KNOBS") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "\t-logfile TRUE") );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL(      0, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetLogStringLineCount(n) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "KNOBS") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "\t-logfile TRUE") );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL(      0, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(     29, ::GetLogStringLineCount(n) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "KNOBS") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "\t-logfile TRUE") );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogStringOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL(      0, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetLogStringLineCount(n) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetLogStringLine(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);
	CPPUNIT_ASSERT_EQUAL(      0, ::GetLogStringLineCount(n));

	CPPUNIT_ASSERT_EQUAL(      0, ::LoadDatabase(n, "phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL(      0, ::GetLogStringLineCount(n));

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "KNOBS") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "\t-logfile TRUE") );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL(      0, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetLogStringLineCount(n) );

	int line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, -1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, -3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, -4)) );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "KNOBS") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "\t-logfile TRUE") );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL(      0, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(     29, ::GetLogStringLineCount(n) );

	line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),        std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 1."),        std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),        std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("-------------------------------------------"), std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Beginning of initial solution calculations."), std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("-------------------------------------------"), std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Initial solution 1.	"),                       std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Iterations in revise_guesses: 2"),             std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Number of infeasible solutions: 0"),           std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Number of basis changes: 0"),                  std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Number of iterations: 6"),                     std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------"),                          std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("End of simulation."),                          std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------"),                          std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),        std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("Reading input data for simulation 2."),        std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string("------------------------------------"),        std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(::GetLogStringLine(n, line++)) );
	line++;
	line++;
	line++;
	CPPUNIT_ASSERT_EQUAL( std::string(""),                                            std::string(::GetLogStringLine(n, line++)) );

	// add solution block
	// add solution block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );

	// add dump block
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::DUMP(n) );

	// add knobs
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "KNOBS") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "\t-logfile TRUE") );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogStringOn(n, 0) );


	CPPUNIT_ASSERT_EQUAL(      0, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetLogStringLineCount(n) );

	line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, -1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, -3)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetLogStringLine(n, -4)) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestSetErrorFileName(void)
{
	char ERR_FILENAME[80];
	snprintf(ERR_FILENAME, sizeof(ERR_FILENAME), "error.%s.out", "TestIPhreeqcLib-TestSetErrorFileName");
	if (::FileExists(ERR_FILENAME))
	{
		::DeleteFile(ERR_FILENAME);
	}

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "SOLUTION 1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	pH	7") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Na	1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "PHASES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Fix_H+") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	H+ = H+") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	log_k	0") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "EQUILIBRIUM_PHASES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Fix_H+ -10 HCl	10") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "END") );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileName(n, ERR_FILENAME) );


	CPPUNIT_ASSERT_EQUAL( 1,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(ERR_FILENAME) );

	std::string lines[100];
	{
		std::ifstream ifs(ERR_FILENAME);

		size_t i = 0;
		while (i < sizeof(lines)/sizeof(lines[0]) && std::getline(ifs, lines[i]))
		{
			++i;
		}

		CPPUNIT_ASSERT_EQUAL( (size_t)90, i );
	}

	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Maximum iterations exceeded, 100"),                                                     lines[0] );
	CPPUNIT_ASSERT_EQUAL( std::string("WARNING: Numerical method failed with this set of convergence parameters."),                     lines[2] );
	CPPUNIT_ASSERT_EQUAL( std::string("ERROR: Numerical method failed on all combinations of convergence parameters, cell/soln/mix 1"), lines[88] );
	CPPUNIT_ASSERT_EQUAL( std::string("Stopping."),                                                                                     lines[89] );

	if (::FileExists(ERR_FILENAME))
	{
		::DeleteFile(ERR_FILENAME);
	}

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestErrorStringOnOff(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( true,     ::GetErrorStringOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,   ::SetErrorStringOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( true,     ::GetErrorStringOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,   ::SetErrorStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( false,    ::GetErrorStringOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,   ::SetErrorStringOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( true,     ::GetErrorStringOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,   ::DestroyIPhreeqc(n) );
}

void TestIPhreeqcLib::TestGetErrorString(void)
{
	char ERR_FILENAME[80];
	snprintf(ERR_FILENAME, sizeof(ERR_FILENAME), "error.%06d.out", ::rand());
	if (::FileExists(ERR_FILENAME))
	{
		::DeleteFile(ERR_FILENAME);
	}
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(ERR_FILENAME) );

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "SOLUTION 1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	pH	7") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Na	1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "PHASES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Fix_H+") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	H+ = H+") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	log_k	0") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "EQUILIBRIUM_PHASES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Fix_H+ -10 HCl	10") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "END") );

	// run
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileName(n, ERR_FILENAME) );
	CPPUNIT_ASSERT_EQUAL( std::string(ERR_FILENAME), std::string(::GetErrorFileName(n)) );

	CPPUNIT_ASSERT_EQUAL( 1,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( std::string(ERR_FILENAME), std::string(::GetErrorFileName(n)) );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(ERR_FILENAME) );
	
	{
#if 0
		std::ifstream ifs(ERR_FILENAME);
		std::string fline((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
#else
		std::string fline("ERROR: Numerical method failed on all combinations of convergence parameters, cell/soln/mix 1\n");
#endif

		std::string sline(::GetErrorString(n));
		CPPUNIT_ASSERT( sline.size() > 0 );

		CPPUNIT_ASSERT_EQUAL( fline, sline );
	}

	if (::FileExists(ERR_FILENAME))
	{
		::DeleteFile(ERR_FILENAME);
	}

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetErrorStringLineCount(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(      0, ::GetErrorStringLineCount(n));

	CPPUNIT_ASSERT_EQUAL(      0, ::LoadDatabase(n, "phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL(      0, ::GetErrorStringLineCount(n));

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "SOLUTION 1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	pH	7") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Na	1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "PHASES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Fix_H+") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	H+ = H+") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	log_k	0") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "EQUILIBRIUM_PHASES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Fix_H+ -10 HCl	10") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "END") );

	CPPUNIT_ASSERT_EQUAL(   true, ::GetErrorStringOn(n) != 0 );
	CPPUNIT_ASSERT_EQUAL(      1, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(      1, ::GetErrorStringLineCount(n) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "SOLUTION 1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	pH	7") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Na	1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "PHASES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Fix_H+") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	H+ = H+") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	log_k	0") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "EQUILIBRIUM_PHASES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Fix_H+ -10 HCl	10") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "END") );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorStringOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL(      1, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(      1, ::GetErrorStringLineCount(n) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "SOLUTION 1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	pH	7") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Na	1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "PHASES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Fix_H+") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	H+ = H+") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	log_k	0") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "EQUILIBRIUM_PHASES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "	Fix_H+ -10 HCl	10") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "END") );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorStringOn(n, 0) );

	CPPUNIT_ASSERT_EQUAL(      1, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetErrorStringLineCount(n) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestSetSelectedOutputFileName(void)
{
	char SELOUT_FILENAME[80];
	snprintf(SELOUT_FILENAME, sizeof(SELOUT_FILENAME), "selected_output.%06d.out", ::rand());
	if (::FileExists(SELOUT_FILENAME))
	{
		::DeleteFile(SELOUT_FILENAME);
	}

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(n, "llnl.dat"));

	int max = 6;

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::EQUILIBRIUM_PHASES(n, "calcite", 0.0, 0.010));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::USER_PUNCH(n, "Ca", max));

	CPPUNIT_ASSERT_EQUAL(0,      ::GetOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetErrorFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetLogFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetSelectedOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpStringOn(n));

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileName(n, SELOUT_FILENAME) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( true,   ::FileExists(SELOUT_FILENAME) );

	/*
	EXPECTED selected.out:
			 sim	       state	        soln	      dist_x	        time	        step	          pH	          pe	           C	          Ca	          Na	     m_CO3-2	     m_CaOH+	    m_NaCO3-	    la_CO3-2	    la_CaOH+	   la_NaCO3-	     Calcite	   d_Calcite	   si_CO2(g)	 si_Siderite	    pressure	   total mol	      volume	    g_CO2(g)	     g_N2(g)	    k_Albite	   dk_Albite	    k_Pyrite	   dk_Pyrite	     s_CaSO4	     s_SrSO4	      1.name	      1.type	     1.moles	      2.name	      2.type	     2.moles	      3.name	      3.type	     3.moles	      4.name	      4.type	     4.moles	      5.name	      5.type	     5.moles	      6.name	      6.type	     6.moles
			   1	      i_soln	           1	         -99	         -99	         -99	           7	           4	 1.0000e-003	 1.0000e-003	 1.0000e-003	 4.2975e-007	 1.1819e-009	 1.1881e-009	-6.4686e+000	-8.9530e+000	-8.9507e+000	 0.0000e+000	 0.0000e+000	     -2.2870	   -999.9990	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	        Ca+2	          aq	 9.9178e-004	     CaHCO3+	          aq	 7.5980e-006	       CaCO3	          aq	 6.2155e-007	       CaOH+	          aq	 1.1819e-009
			   1	       react	           1	         -99	           0	           1	     7.86135	       10.18	 1.1556e-003	 1.1556e-003	 1.0000e-003	 4.2718e-006	 9.7385e-009	 1.1620e-008	-5.4781e+000	-8.0388e+000	-7.9621e+000	 9.8444e-003	-1.5555e-004	     -3.0192	   -999.9990	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	     calcite	        equi	 9.8444e-003	        Ca+2	          aq	 1.1371e-003	     CaHCO3+	          aq	 1.1598e-005	       CaCO3	          aq	 6.8668e-006	       CaOH+	          aq	 9.7385e-009
	*/

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}

	if (::FileExists(SELOUT_FILENAME))
	{
		::DeleteFile(SELOUT_FILENAME);
	}
}

void TestIPhreeqcLib::TestSelectedOutputStringOnOff(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( false,    ::GetSelectedOutputFileOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,   ::SetSelectedOutputFileOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( true,     ::GetSelectedOutputFileOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,   ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( false,    ::GetSelectedOutputFileOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,   ::DestroyIPhreeqc(n) );
}

void TestIPhreeqcLib::TestGetSelectedOutputString(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(n, "llnl.dat"));

	int max = 6;

	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::EQUILIBRIUM_PHASES(n, "calcite", 0.0, 0.010));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::USER_PUNCH(n, "Ca", max));

	CPPUNIT_ASSERT_EQUAL(0,      ::GetOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetErrorFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetLogFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetSelectedOutputFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpFileOn(n));
	CPPUNIT_ASSERT_EQUAL(0,      ::GetDumpStringOn(n));

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	std::string sline( ::GetSelectedOutputString(n) );

	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "sim\t")         != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "state\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "soln\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "dist_x\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "time\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "step\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "pH\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "pe\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "C\t")           != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "Ca\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "Na\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "m_CO3-2\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "m_CaOH+\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "m_NaCO3-\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "la_CO3-2\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "la_CaOH+\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "la_NaCO3-\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "Calcite\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "d_Calcite\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "si_CO2(g)\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "si_Siderite\t") != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "pressure\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "total mol\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "volume\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "g_CO2(g)\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "g_N2(g)\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "k_Albite\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "dk_Albite\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "k_Pyrite\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "dk_Pyrite\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "s_CaSO4\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "s_SrSO4\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "1.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "1.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "1.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "2.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "2.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "2.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "3.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "3.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "3.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "4.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "4.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "4.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "5.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "5.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "5.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "6.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "6.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "6.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "\n")            != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "i_soln\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "react\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "Ca+2\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "aq\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "CaHCO3+\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "CaCO3\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "CaOH+\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "Calcite\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(sline.c_str(), "equi\t")        != NULL );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetSelectedOutputStringLineCount(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetSelectedOutputFileOn(n) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "llnl.dat") );

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetSelectedOutputFileOn(n) );

	int max = 6;

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::EQUILIBRIUM_PHASES(n, "calcite", 0.0, 0.010) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::USER_PUNCH(n, "Ca", max) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetOutputFileOn(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetErrorFileOn(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetLogFileOn(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetSelectedOutputFileOn(n) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetDumpFileOn(n ));
	CPPUNIT_ASSERT_EQUAL( 0,      ::GetDumpStringOn(n) );

	CPPUNIT_ASSERT_EQUAL(  false, ::GetSelectedOutputStringOn(n) != 0 );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL( 3,      ::GetSelectedOutputStringLineCount(n) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetSelectedOutputStringLine(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetSelectedOutputFileOn(n) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "llnl.dat") );

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetSelectedOutputFileOn(n) );

	int max = 6;

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::EQUILIBRIUM_PHASES(n, "calcite", 0.0, 0.010) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::USER_PUNCH(n, "Ca", max) );

	CPPUNIT_ASSERT_EQUAL(      0, ::GetOutputFileOn(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetErrorFileOn(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetLogFileOn(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetSelectedOutputFileOn(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetDumpFileOn(n ));
	CPPUNIT_ASSERT_EQUAL(      0, ::GetDumpStringOn(n) );

	CPPUNIT_ASSERT_EQUAL(  false, ::GetSelectedOutputStringOn(n) != 0 );

	CPPUNIT_ASSERT_EQUAL(      0, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetSelectedOutputStringLineCount(n) );

	int line = 0;
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, line++)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, line++)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, -1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, -3)) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::EQUILIBRIUM_PHASES(n, "calcite", 0.0, 0.010) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::USER_PUNCH(n, "Ca", max) );

	CPPUNIT_ASSERT_EQUAL(  false, ::GetSelectedOutputStringOn(n) != 0 );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL(      0, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(      3, ::GetSelectedOutputStringLineCount(n) );

	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "sim\t")         != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "state\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "soln\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "dist_x\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "time\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "step\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "pH\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "pe\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "C\t")           != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "Ca\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "Na\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "m_CO3-2\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "m_CaOH+\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "m_NaCO3-\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "la_CO3-2\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "la_CaOH+\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "la_NaCO3-\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "Calcite\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "d_Calcite\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "si_CO2(g)\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "si_Siderite\t") != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "pressure\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "total mol\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "volume\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "g_CO2(g)\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "g_N2(g)\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "k_Albite\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "dk_Albite\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "k_Pyrite\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "dk_Pyrite\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "s_CaSO4\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "s_SrSO4\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "1.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "1.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "1.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "2.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "2.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "2.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "3.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "3.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "3.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "4.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "4.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "4.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "5.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "5.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "5.moles\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "6.name\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "6.type\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "6.moles\t")     != NULL );

	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "i_soln\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "Ca+2\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "aq\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "CaHCO3+\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "CaCO3\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "CaOH+\t")       != NULL );

	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 2), "react\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 2), "Calcite\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 2), "equi\t")        != NULL );

	// after obj.GetSelectedOutputStringLineCount() should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, ::GetSelectedOutputStringLineCount(n)))     );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, ::GetSelectedOutputStringLineCount(n) + 1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, ::GetSelectedOutputStringLineCount(n) + 2)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, -1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, -3)) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestGetSelectedOutputStringLineNotEnoughHeadings(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetSelectedOutputFileOn(n) );

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "llnl.dat") );

	CPPUNIT_ASSERT_EQUAL( 0,      ::GetSelectedOutputFileOn(n) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SOLUTION(n, 1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::EQUILIBRIUM_PHASES(n, "calcite", 0.0, 0.010) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::USER_PUNCH_NEH(n) );

	CPPUNIT_ASSERT_EQUAL(      0, ::GetOutputFileOn(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetErrorFileOn(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetLogFileOn(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetSelectedOutputFileOn(n) );
	CPPUNIT_ASSERT_EQUAL(      0, ::GetDumpFileOn(n ));
	CPPUNIT_ASSERT_EQUAL(      0, ::GetDumpStringOn(n) );

	CPPUNIT_ASSERT_EQUAL(  false, ::GetSelectedOutputStringOn(n) != 0 );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputStringOn(n, 1) );

	CPPUNIT_ASSERT_EQUAL(      0, ::RunAccumulated(n) );
	CPPUNIT_ASSERT_EQUAL(      3, ::GetSelectedOutputStringLineCount(n) );

	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "sim\t")         != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "state\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "soln\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "dist_x\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "time\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "step\t")        != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "pH\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "pe\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "C\t")           != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "Ca\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "Na\t")          != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "m_CO3-2\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "m_CaOH+\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "m_NaCO3-\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "la_CO3-2\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "la_CaOH+\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "la_NaCO3-\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "Calcite\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "d_Calcite\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "si_CO2(g)\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "si_Siderite\t") != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "pressure\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "total mol\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "volume\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "g_CO2(g)\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "g_N2(g)\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "k_Albite\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "dk_Albite\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "k_Pyrite\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "dk_Pyrite\t")   != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "s_CaSO4\t")     != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "s_SrSO4\t")     != NULL );	
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "head0\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "head1\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 0), "head2\t")       != NULL );

	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "i_soln\t")      != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "have0\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "have1\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "have2\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "missing0\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "missing2\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 1), "missing2\t")    != NULL );

	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 2), "react\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 2), "have0\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 2), "have1\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 2), "have2\t")       != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 2), "missing0\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 2), "missing2\t")    != NULL );
	CPPUNIT_ASSERT( ::strstr(::GetSelectedOutputStringLine(n, 2), "missing2\t")    != NULL );

	// after obj.GetSelectedOutputStringLineCount() should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, ::GetSelectedOutputStringLineCount(n)))     );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, ::GetSelectedOutputStringLineCount(n) + 1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, ::GetSelectedOutputStringLineCount(n) + 2)) );

	// negative lines should be empty
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, -1)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, -2)) );
	CPPUNIT_ASSERT_EQUAL( std::string(""),  std::string(::GetSelectedOutputStringLine(n, -3)) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestLongUser_Punch(void)
{
	const char input[] =
		"PRINT\n"
		" -selected_output t\n"
		"SOLUTION\n"
		"SELECTED_OUTPUT\n"
		" -reset false\n"
		"USER_PUNCH\n"
		"1 REM 255 CHARACTER STRING\n"
		"10 temp$ = \"XXXXX123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345\"\n"
		"20 PUNCH temp$\n";

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputStringOn(n, 1) );
	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( 0,      ::RunString(n, input) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestBasicSURF(void)
{
	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "phreeqc.dat") );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "SURFACE_MASTER_SPECIES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 Surfa Surfa") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 Surfb Surfb") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "SURFACE_SPECIES") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 Surfa = Surfa") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 log_k 0") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 Surfb = Surfb") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 log_k 0") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 Surfa + Zn+2 = SurfaZn+2") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 log_k  5.") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 Surfb + Zn+2 = SurfbZn+2") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 log_k  6.") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 Surfa + Cu+2 = SurfaCu+2") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 log_k  4.5") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 Surfb + Cu+2 = SurfbCu+2") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "		 log_k  6.5") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "SOLUTION 1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "   pH        8") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "   units     mol/kgw") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "   Fe(3)     1e-2") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "   Zn        1e-4") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "   Cu        1e-5") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "   Na        1e-1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "   Cl        1e-1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "EQUILIBRIUM_PHASES 1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "   Fe(OH)3(a) 0 0") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "SELECTED_OUTPUT") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "USER_PUNCH") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "    -headings Hfo-Zn Surfa-Zn Surfb-Zn Surfa-Cu Surfb-Cu") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "-start") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "10 PUNCH SURF(\"Zn\",\"Hfo\")") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "20 PUNCH SURF(\"Zn\",\"Surfa\")") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "30 PUNCH SURF(\"Zn\",\"Surfb\")") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "40 PUNCH SURF(\"Cu\",\"Surfa\")") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "50 PUNCH SURF(\"Cu\",\"Surfb\")") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "-end") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "SURFACE 1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "    Hfo_sOH Fe(OH)3(a)      equilibrium_phase 0.005  53300") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "    Hfo_wOH Fe(OH)3(a)      equilibrium_phase 0.2") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "    Surfa  0.2 100. 2") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "    Surfb  0.1 100. 1") );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::AccumulateLine(n, "END") );


	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );


	CPPUNIT_ASSERT_EQUAL( 0,      ::RunAccumulated(n) );

	CPPUNIT_ASSERT_EQUAL(13,      ::GetSelectedOutputColumnCount(n));
	CPPUNIT_ASSERT_EQUAL(3,       ::GetSelectedOutputRowCount(n));

	VAR v;
	VarInit(&v);

	const int offset = 8;

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 0, offset + 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("Hfo-Zn"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 0, offset + 1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("Surfa-Zn"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 0, offset + 2, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("Surfb-Zn"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 0, offset + 3, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("Surfa-Cu"), std::string(v.sVal));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 0, offset + 4, &v));
	CPPUNIT_ASSERT_EQUAL(TT_STRING, v.type);
	CPPUNIT_ASSERT_EQUAL(std::string("Surfb-Cu"), std::string(v.sVal));


	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 1, offset + 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, v.dVal, ::pow(10., -FLT_DIG) );

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 1, offset + 1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, v.dVal, ::pow(10., -FLT_DIG) );

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 1, offset + 2, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, v.dVal, ::pow(10., -FLT_DIG) );

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 1, offset + 3, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, v.dVal, ::pow(10., -FLT_DIG) );

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 1, offset + 4, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, v.dVal, ::pow(10., -FLT_DIG) );


	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 2, offset + 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 6.3861e-005, v.dVal, ::pow(10., -FLT_DIG) );

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 2, offset + 1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.7868e-005, v.dVal, ::pow(10., -FLT_DIG) );

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 2, offset + 2, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.8248e-005, v.dVal, ::pow(10., -FLT_DIG) );

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 2, offset + 3, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 4.6216e-009, v.dVal, ::pow(10., -FLT_DIG) );

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,  ::GetSelectedOutputValue(n, 2, offset + 4, &v));
	CPPUNIT_ASSERT_EQUAL(TT_DOUBLE, v.type);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 4.7201e-008, v.dVal, ::pow(10., -FLT_DIG) );

	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
}

void TestIPhreeqcLib::TestIEEE(void)
{
// COMMENT: {1/18/2013 6:34:57 PM}	int n = ::CreateIPhreeqc();
// COMMENT: {1/18/2013 6:34:57 PM}	CPPUNIT_ASSERT(n >= 0);
// COMMENT: {1/18/2013 6:34:57 PM}
// COMMENT: {1/18/2013 6:34:57 PM}	CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "wateq4f.dat") );
// COMMENT: {1/18/2013 6:34:57 PM}	//CPPUNIT_ASSERT_EQUAL( 0,      ::LoadDatabase(n, "cdmusic_hiemstra.dat") );
// COMMENT: {1/18/2013 6:34:57 PM}	//::LoadDatabase(n, "cdmusic_hiemstra.dat");
// COMMENT: {1/18/2013 6:34:57 PM}	//const char* errdb = ::GetErrorString(n);
// COMMENT: {1/18/2013 6:34:57 PM}	//fprintf(stderr, "%s\n", errdb);
// COMMENT: {1/18/2013 6:34:57 PM}
// COMMENT: {1/18/2013 6:34:57 PM}	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputFileOn(n, 0) );
// COMMENT: {1/18/2013 6:34:57 PM}	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetOutputStringOn(n, 0) );
// COMMENT: {1/18/2013 6:34:57 PM}	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetErrorFileOn(n, 0) );
// COMMENT: {1/18/2013 6:34:57 PM}	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetLogFileOn(n, 0) );
// COMMENT: {1/18/2013 6:34:57 PM}	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetSelectedOutputFileOn(n, 0) );
// COMMENT: {1/18/2013 6:34:57 PM}	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpStringOn(n, 0) );
// COMMENT: {1/18/2013 6:34:57 PM}	CPPUNIT_ASSERT_EQUAL( IPQ_OK, ::SetDumpFileOn(n, 0) );
// COMMENT: {1/18/2013 6:34:57 PM}
// COMMENT: {1/18/2013 6:34:57 PM}	::RunFile(n, "IEEE");
// COMMENT: {1/18/2013 6:34:57 PM}	const char* err = ::GetErrorString(n);
// COMMENT: {1/18/2013 6:34:57 PM}	fprintf(stderr, "%s\n", err);
// COMMENT: {1/18/2013 6:34:57 PM}
// COMMENT: {1/18/2013 6:34:57 PM}	if (n >= 0)
// COMMENT: {1/18/2013 6:34:57 PM}	{
// COMMENT: {1/18/2013 6:34:57 PM}		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
// COMMENT: {1/18/2013 6:34:57 PM}	}
}

void TestIPhreeqcLib::TestDelete(void)
{
	const char input[] =
		"SOLUTION 1 # definition of initial condition 1\n"
		"COPY cell 1 7405 # copy cell 1 to placeholder cell with index larger than the number of cells in the model domain\n"
		"END\n"
		"DELETE # delete initial condition 1 to allow for a redefinition of all reactions\n"
		"-cell 1\n"
		"END\n"
		"# define other initial conditions and copy to another placeholder cell\n"
		"\n"
		"COPY cell 7405    1 # copy back from placeholder cell to domain cell 1\n"
		"END\n"
		"MIX    1 # mix according to initial moisture content\n"
		"   1 0.25\n"
		"END\n"
		"RUN_CELLS\n"
		"-cells 1\n"
		"-start_time 0\n"
		"-time_step 0\n"
		"DELETE # remove mix reaction in subsequent runs\n"
		"-mix 1\n"
		"END\n"
		"RUN_CELLS\n"
		"-cells 1\n";

	int n = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(n >= 0);

	char OUTPUT_FILE[80];
	snprintf(OUTPUT_FILE, sizeof(OUTPUT_FILE), "phreeqc.%d.out", n);

	if (::FileExists(OUTPUT_FILE))
	{
		CPPUNIT_ASSERT(::DeleteFile(OUTPUT_FILE));
	}
	CPPUNIT_ASSERT_EQUAL(false, ::FileExists(OUTPUT_FILE));

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(n, "phreeqc.dat"));
	::SetOutputFileOn(n, 0);
	::SetErrorFileOn(n, 0);
	::SetLogFileOn(n, 0);
	::SetSelectedOutputFileOn(n, 0);
	::SetDumpFileOn(n, 0);
	CPPUNIT_ASSERT_EQUAL(false, ::FileExists(OUTPUT_FILE));
	CPPUNIT_ASSERT_EQUAL(0,     ::RunString(n, input));
	CPPUNIT_ASSERT_EQUAL(false, ::FileExists(OUTPUT_FILE));
	if (n >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(n));
	}
	if (::FileExists(OUTPUT_FILE))
	{
		CPPUNIT_ASSERT(::DeleteFile(OUTPUT_FILE));
	}
}

void TestIPhreeqcLib::TestMultiPunchCSelectedOutput(void)
{
	VAR var;
	::VarInit(&var);

	int id = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(id >= 0);

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(id, "../database/phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(0, ::RunFile(id, "multi_punch"));

	CPPUNIT_ASSERT_EQUAL(6, ::GetSelectedOutputRowCount(id));
	CPPUNIT_ASSERT_EQUAL(35, ::GetSelectedOutputColumnCount(id));

	// headings
	int ncol = 0;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("sim"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("state"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("soln"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("dist_x"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("time"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("step"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("pH"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("pe"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("reaction"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("temp(C)"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Alk(eq/kgw)"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("mu"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("mass_H2O"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("charge(eq)"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("pct_err"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Na(mol/kgw)"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Ca(mol/kgw)"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("m_Na+(mol/kgw)"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("m_HCO3-(mol/kgw)"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("la_Ca+2"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("la_CO3-2"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("CO2(g)"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("d_CO2(g)"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("dolomite"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("d_dolomite"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("si_Halite"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("pressure"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("total mol"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("volume"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("g_N2(g)"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("k_Calcite"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("dk_Calcite"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("s_Anhydrite"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("s_Barite"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("V_TOTAL_C"), std::string(var.sVal));

	// sim
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 0, &var));   CPPUNIT_ASSERT_EQUAL((long) 8, var.lVal);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 0, &var));   CPPUNIT_ASSERT_EQUAL((long)10, var.lVal);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 0, &var));   CPPUNIT_ASSERT_EQUAL((long)11, var.lVal);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 0, &var));   CPPUNIT_ASSERT_EQUAL((long)12, var.lVal);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 0, &var));   CPPUNIT_ASSERT_EQUAL((long)14, var.lVal);

	// state
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 1, &var));   CPPUNIT_ASSERT_EQUAL(std::string("react"),  std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 1, &var));   CPPUNIT_ASSERT_EQUAL(std::string("react"),  std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 1, &var));   CPPUNIT_ASSERT_EQUAL(std::string("i_soln"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 1, &var));   CPPUNIT_ASSERT_EQUAL(std::string("i_soln"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 1, &var));   CPPUNIT_ASSERT_EQUAL(std::string("react"),  std::string(var.sVal));

	// pH
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 6, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.30475, var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 6, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.29765, var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 6, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 6.99738, var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 6, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 6.99698, var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 6, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.2942 , var.dVal, ::pow(10., -2) );

	// V_TOTAL_C
#ifdef SKIP_TEST
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 34, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 4.3729e-003, var.dVal, ::pow(10., -6) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 34, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 4.3090e-003, var.dVal, ::pow(10., -6) );
#endif
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 34, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0000e+000, var.dVal, ::pow(10., -6) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 34, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0000e+000, var.dVal, ::pow(10., -6) );
#ifdef SKIP_TEST
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 34, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 4.2784e-003, var.dVal, ::pow(10., -6) );
#endif

	// edge cases
	int r = ::GetSelectedOutputRowCount(id);
	int c = ::GetSelectedOutputColumnCount(id);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDROW, ::GetSelectedOutputValue(id, -1,  0, &var));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, var.vresult);

	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDROW, ::GetSelectedOutputValue(id,  r,  0, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, var.vresult);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDCOL, ::GetSelectedOutputValue(id,  0, -1, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, var.vresult);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDCOL, ::GetSelectedOutputValue(id,  0,  c, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, var.vresult);

	::SetCurrentSelectedOutputUserNumber(id, 2);
	CPPUNIT_ASSERT_EQUAL(7, ::GetSelectedOutputRowCount(id));
	CPPUNIT_ASSERT_EQUAL(16, ::GetSelectedOutputColumnCount(id));

	// headings
	ncol = 0;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("si_Halite"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("si_Calcite"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("DUMMY_1"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("DUMMY_2"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Sum_resid"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Sum_Delta/U"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("MaxFracErr"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_2"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_2_min"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_2_max"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_3"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_3_min"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_3_max"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Halite"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Halite_min"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Halite_max"), std::string(var.sVal));

	// si_Halite
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 0, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( -7.70857,  var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 0, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( -7.67087,  var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 0, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( -7.6362,   var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 0, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( -999.999,  var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 0, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( -7.60092,  var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 0, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( -7.60411,  var.dVal, ::pow(10., -2) );

	// si_Calcite
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 1, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.702316,  var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 1, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.695856,  var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 1, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.689518,  var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 1, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( -999.999,  var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 1, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( -999.999,  var.dVal, ::pow(10., -2) );
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 1, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.683300,  var.dVal, ::pow(10., -2) );

	// DUMMY_1
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 2, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 2, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Dummy1"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 2, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Dummy1"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 2, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Dummy1"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 2, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Dummy1"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 2, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Dummy1"), std::string(var.sVal));

	// DUMMY_2
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 3, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 3, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Dummy2"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 3, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Dummy2"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 3, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Dummy2"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 3, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Dummy2"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 3, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Dummy2"), std::string(var.sVal));

	// Sum_resid
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 4, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 4, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 4, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 4, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 4, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 4, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 4.12e-13,  var.dVal, ::pow(10., log10(4.12e-13)-2) );

	// Sum_Delta/U
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 5, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 5, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 5, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 5, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 5, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 5, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );
	
	// MaxFracErr
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 6, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 6, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 6, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 6, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 6, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 6, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Soln_2
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 7, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 7, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 7, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 7, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 7, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 7, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 1,  var.dVal, ::pow(10., -3) );

	// Soln_2_min
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 8, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 8, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 8, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 8, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 8, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 8, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Soln_2_max
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 9, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 9, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 9, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 9, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 9, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 9, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Soln_3
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 10, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 10, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 10, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 10, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 10, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 10, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 1,  var.dVal, ::pow(10., -3) );

	// Soln_3_min
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 11, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 11, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 11, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 11, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 11, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 11, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Soln_3_max
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 12, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 12, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 12, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 12, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 12, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 12, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Halite
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 13, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 13, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 13, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 13, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 13, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 13, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.001,  var.dVal, ::pow(10., -3) );

	// Halite_min
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 14, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 14, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 14, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 14, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 14, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 14, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Halite_max
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 15, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 2, 15, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 3, 15, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 4, 15, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 5, 15, &var));   CPPUNIT_ASSERT_EQUAL(TT_EMPTY,  var.type);
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 6, 15, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// edge cases
	r = ::GetSelectedOutputRowCount(id);
	c = ::GetSelectedOutputColumnCount(id);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDROW, ::GetSelectedOutputValue(id, -1,  0, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, var.vresult);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDROW, ::GetSelectedOutputValue(id,  r,  0, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, var.vresult);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDCOL, ::GetSelectedOutputValue(id,  0, -1, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, var.vresult);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDCOL, ::GetSelectedOutputValue(id,  0,  c, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, var.vresult);

	::SetCurrentSelectedOutputUserNumber(id, 3);
	CPPUNIT_ASSERT_EQUAL(2, ::GetSelectedOutputRowCount(id));
	CPPUNIT_ASSERT_EQUAL(12, ::GetSelectedOutputColumnCount(id));

	// headings
	ncol = 0;
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Sum_resid"),   std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Sum_Delta/U"), std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("MaxFracErr"),  std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_2"),      std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_2_min"),  std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_2_max"),  std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_3"),      std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_3_min"),  std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Soln_3_max"),  std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Halite"),      std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Halite_min"),  std::string(var.sVal));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 0, ncol++, &var));   CPPUNIT_ASSERT_EQUAL(std::string("Halite_max"),  std::string(var.sVal));

	// Sum_resid
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 0, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 3.69E-13,  var.dVal, ::pow(10., log10(3.69E-13)-2) );

	// Sum_Delta/U
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 1, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );
	
	// MaxFracErr
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 2, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Soln_2
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 3, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 1,  var.dVal, ::pow(10., -3) );

	// Soln_2_min
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 4, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Soln_2_max
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 5, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Soln_3
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 6, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 1,  var.dVal, ::pow(10., -3) );

	// Soln_3_min
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 7, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Soln_3_max
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 8, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Halite
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 9, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.001,  var.dVal, ::pow(10., -3) );

	// Halite_min
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 10, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );

	// Halite_max
	CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::GetSelectedOutputValue(id, 1, 11, &var));   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,  var.dVal, ::pow(10., -3) );


	// edge cases
	r = ::GetSelectedOutputRowCount(id);
	c = ::GetSelectedOutputColumnCount(id);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDROW, ::GetSelectedOutputValue(id, -1,  0, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, var.vresult);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDROW, ::GetSelectedOutputValue(id,  r,  0, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, var.vresult);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDCOL, ::GetSelectedOutputValue(id,  0, -1, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, var.vresult);
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDCOL, ::GetSelectedOutputValue(id,  0,  c, &var));  CPPUNIT_ASSERT_EQUAL(TT_ERROR, var.type);  CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, var.vresult);

	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDARG, ::SetCurrentSelectedOutputUserNumber(id, -1)); 
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::SetCurrentSelectedOutputUserNumber(id, 0)); 
	CPPUNIT_ASSERT_EQUAL(0,              ::GetCurrentSelectedOutputUserNumber(id)); 

	if (id >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(id));
	}
}

void TestIPhreeqcLib::TestGetSelectedOutputCount(void)
{
	int id = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(id >= 0);

	CPPUNIT_ASSERT_EQUAL(0, ::GetSelectedOutputCount(id));
	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(id, "../database/phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(0, ::GetSelectedOutputCount(id));
	CPPUNIT_ASSERT_EQUAL(0, ::RunFile(id, "multi_punch"));
	CPPUNIT_ASSERT_EQUAL(3, ::GetSelectedOutputCount(id));

	if (id >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(id));
	}
}

void TestIPhreeqcLib::TestGetNthSelectedOutputUserNumber(void)
{
	int id = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(id >= 0);

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase(id, "../database/phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(0, ::RunFile(id, "multi_punch"));

	CPPUNIT_ASSERT_EQUAL(3, ::GetSelectedOutputCount(id));

	CPPUNIT_ASSERT_EQUAL(1, ::GetNthSelectedOutputUserNumber(id, 0));
	CPPUNIT_ASSERT_EQUAL(2, ::GetNthSelectedOutputUserNumber(id, 1));
	CPPUNIT_ASSERT_EQUAL(3, ::GetNthSelectedOutputUserNumber(id, 2));

	// edge cases
	CPPUNIT_ASSERT_EQUAL((int)IPQ_INVALIDARG, ::GetNthSelectedOutputUserNumber(id, -1));
	CPPUNIT_ASSERT_EQUAL((int)IPQ_INVALIDARG, ::GetNthSelectedOutputUserNumber(id, 4));
	
	if (id >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(id));
	}
}

void TestIPhreeqcLib::TestGetCurrentSelectedOutputUserNumber(void)
{
	int id = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(id >= 0);
	CPPUNIT_ASSERT_EQUAL(1,  ::GetCurrentSelectedOutputUserNumber(id));

	CPPUNIT_ASSERT_EQUAL(0,  ::LoadDatabase(id, "../database/phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(1,  ::GetCurrentSelectedOutputUserNumber(id));
	CPPUNIT_ASSERT_EQUAL(0,  ::RunFile(id, "multi_punch"));

	CPPUNIT_ASSERT_EQUAL(1,  ::GetCurrentSelectedOutputUserNumber(id));
	CPPUNIT_ASSERT_EQUAL(6,  ::GetSelectedOutputRowCount(id));
	CPPUNIT_ASSERT_EQUAL(35, ::GetSelectedOutputColumnCount(id));

	::SetCurrentSelectedOutputUserNumber(id, 2);
	CPPUNIT_ASSERT_EQUAL(2,  ::GetCurrentSelectedOutputUserNumber(id));
	CPPUNIT_ASSERT_EQUAL(7,  ::GetSelectedOutputRowCount(id));
	CPPUNIT_ASSERT_EQUAL(16, ::GetSelectedOutputColumnCount(id));

	::SetCurrentSelectedOutputUserNumber(id, 3);
	CPPUNIT_ASSERT_EQUAL(3,  ::GetCurrentSelectedOutputUserNumber(id));
	CPPUNIT_ASSERT_EQUAL(2,  ::GetSelectedOutputRowCount(id));
	CPPUNIT_ASSERT_EQUAL(12, ::GetSelectedOutputColumnCount(id));

	// edge cases
	CPPUNIT_ASSERT_EQUAL(IPQ_INVALIDARG, ::SetCurrentSelectedOutputUserNumber(id, -1)); 
	CPPUNIT_ASSERT_EQUAL(3,              ::GetCurrentSelectedOutputUserNumber(id));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::SetCurrentSelectedOutputUserNumber(id, 0)); 
	CPPUNIT_ASSERT_EQUAL(0,              ::GetCurrentSelectedOutputUserNumber(id));

	// unload database
	CPPUNIT_ASSERT_EQUAL(0,  ::LoadDatabase(id, "../database/phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(1,  ::GetCurrentSelectedOutputUserNumber(id));

	if (id >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(id));
	}
}

void TestIPhreeqcLib::TestMultiSetSelectedOutputFileName(void)
{
	FileTest set1("state.sel");
	CPPUNIT_ASSERT( set1.RemoveExisting() );

	FileTest set2("si.sel");
	CPPUNIT_ASSERT( set2.RemoveExisting() );

	int id = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(id >= 0);

	CPPUNIT_ASSERT_EQUAL(0,              ::LoadDatabase(id, "../database/phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::SetCurrentSelectedOutputUserNumber(id, 1)); 
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::SetSelectedOutputStringOn(id, true)); 
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::SetSelectedOutputFileOn(id, true)); 
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::SetSelectedOutputFileName(id, set1.GetName().c_str()));
	
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::SetCurrentSelectedOutputUserNumber(id, 2)); 
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::SetSelectedOutputStringOn(id, true)); 
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::SetSelectedOutputFileOn(id, true)); 
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::SetSelectedOutputFileName(id, set2.GetName().c_str())); 

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "TITLE Temperature dependence of solubility"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "      of gypsum and anhydrite             "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "SOLUTION 1 Pure water                     "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "        pH      7.0                       "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "        temp    25.0                      "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "EQUILIBRIUM_PHASES 1                      "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "        Gypsum          0.0     1.0       "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "        Anhydrite       0.0     1.0       "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "REACTION_TEMPERATURE 1                    "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "        25.0 75.0 in 51 steps             "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "SELECTED_OUTPUT 1                         "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "        -temperature                      "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "SELECTED_OUTPUT 2                         "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "        -si     anhydrite  gypsum         "));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "END                                       "));

	CPPUNIT_ASSERT_EQUAL(0,              ::RunAccumulated(id));

	CPPUNIT_ASSERT( set1.VerifyExists() );
	CPPUNIT_ASSERT( set2.VerifyExists() );

	if (id >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(id));
	}
}

void TestIPhreeqcLib::TestWissmeier20131203(void)
{
	int id = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(id >= 0);

	CPPUNIT_ASSERT_EQUAL(0,              ::LoadDatabase(id, "../database/phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "selected_output 1"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "-totals O"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, ""));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "END"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "selected_output 1"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "-totals H"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, ""));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "solution"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "END"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "selected_output"));

	// original asserts here
	CPPUNIT_ASSERT_EQUAL( 0,             ::RunAccumulated(id));

	CPPUNIT_ASSERT_EQUAL( 1,             ::GetSelectedOutputCount(id) );

	CPPUNIT_ASSERT_EQUAL( 9,             ::GetSelectedOutputColumnCount(id) );
	CPPUNIT_ASSERT_EQUAL( 2,             ::GetSelectedOutputRowCount(id) );

	if (id >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(id));
	}
}

void TestIPhreeqcLib::TestWissmeier20131203_2(void)
{
	int id = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(id >= 0);

	CPPUNIT_ASSERT_EQUAL(0,              ::LoadDatabase(id, "../database/phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "selected_output 22"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "-totals O"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, ""));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "END"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "selected_output 22"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "-totals H"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, ""));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "solution"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "END"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "selected_output 22"));

	// original asserts here
	CPPUNIT_ASSERT_EQUAL( 0,             ::RunAccumulated(id));

	CPPUNIT_ASSERT_EQUAL( 1,             ::GetSelectedOutputCount(id) );

	CPPUNIT_ASSERT_EQUAL( IPQ_OK,        ::SetCurrentSelectedOutputUserNumber(id, 22) );
	CPPUNIT_ASSERT_EQUAL( 1,             ::GetSelectedOutputColumnCount(id) );
	CPPUNIT_ASSERT_EQUAL( 2,             ::GetSelectedOutputRowCount(id) );

	if (id >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(id));
	}
}

void TestIPhreeqcLib::TestWissmeier20131203_3(void)
{
	int id = ::CreateIPhreeqc();
	CPPUNIT_ASSERT(id >= 0);

	CPPUNIT_ASSERT_EQUAL(0,              ::LoadDatabase(id, "../database/phreeqc.dat"));

	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "selected_output 1"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "-reset false"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "-totals O"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, ""));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "END"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "selected_output 1"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "-reset false"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "-totals H"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, ""));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "solution"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "END"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "selected_output"));
	CPPUNIT_ASSERT_EQUAL(IPQ_OK,         ::AccumulateLine(id, "-reset false"));

	// original asserts here
	CPPUNIT_ASSERT_EQUAL( 0,             ::RunAccumulated(id));

	CPPUNIT_ASSERT_EQUAL( 1,             ::GetSelectedOutputCount(id) );

	CPPUNIT_ASSERT_EQUAL( 1,             ::GetSelectedOutputColumnCount(id) );
	CPPUNIT_ASSERT_EQUAL( 2,             ::GetSelectedOutputRowCount(id) );

	if (id >= 0)
	{
		CPPUNIT_ASSERT_EQUAL(IPQ_OK, ::DestroyIPhreeqc(id));
	}
}

void TestIPhreeqcLib::TestIsZeroInitialized(void)
{
	std::map<void*, void*> test_map;
	CPPUNIT_ASSERT( test_map.find((void*)0) == test_map.end() );
	CPPUNIT_ASSERT( test_map[(void*)0] == (void*)0 );

	int i = int();
	long l = long();
	float f = float();
	double d = double();

	// these would fail
	/**
	int i;
	long l;
	float f;
	double d;
	**/

	CPPUNIT_ASSERT_EQUAL( (int)0, i );
	CPPUNIT_ASSERT_EQUAL( (long)0, l );
	CPPUNIT_ASSERT_EQUAL( (float)0, f );
	CPPUNIT_ASSERT_EQUAL( (double)0, d );
}
