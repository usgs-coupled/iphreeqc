#include "TestInterface.h"

#if defined(_WIN32) || defined(__CYGWIN32__)
#include <windows.h>
#else
int DeleteFile(const char* szPathName)
{
  if (remove(szPathName) == 0) {
    return 1;
  }
  return 0; // failure
}
#endif

#include "../include/IPhreeqc.h"
#include "../src/CVar.hxx"
#include "../src/phreeqcns.hxx"

#include <iostream>
#include <cmath>
#include <cfloat>

bool FileExists(const char *szPathName);
VRESULT SOLUTION(double C, double Ca, double Na);
VRESULT EQUILIBRIUM_PHASES(const char* phase, double si, double amount);
VRESULT USER_PUNCH(const char* element, int max);
void TestOnOff(const char* FILENAME, int output_on, int error_on, int log_on, int selected_output_on);


TestInterface::TestInterface()
{
}

TestInterface::~TestInterface()
{
}

void TestInterface::TestLoadDatabase()
{
	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase("phreeqc.dat"));
}

void TestInterface::TestLoadDatabaseMissingFile()
{
	CPPUNIT_ASSERT_EQUAL(false, ::FileExists("missing.file"));
	CPPUNIT_ASSERT_EQUAL(1,     ::LoadDatabase("missing.file"));
	CPPUNIT_ASSERT_EQUAL(1,     ::LoadDatabase("missing.file"));
	CPPUNIT_ASSERT_EQUAL(1,     ::LoadDatabase("missing.file"));

	const char expected[] =
		"ERROR: LoadDatabase: Unable to open:\"missing.file\".\n"
		"Stopping.\n";

	const char* err = ::GetLastErrorString();

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );
}

void TestInterface::TestLoadDatabaseWithErrors()
{
	for (int i = 0; i < 5; ++i)
	{
	CPPUNIT_ASSERT_EQUAL(6, ::LoadDatabase("missing_e.dat"));

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
		"ERROR: Calculations terminating due to input errors.\n"
		"Stopping.\n";

	const char* err = ::GetLastErrorString();

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );
	}
}


void TestInterface::TestRun()
{
	int files_on = 0;
	CPPUNIT_ASSERT_EQUAL(0,     ::LoadDatabase("phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("solution 12"));
	CPPUNIT_ASSERT_EQUAL(0,     ::Run(files_on, files_on, files_on, files_on));
}

void TestInterface::TestRunWithErrors()
{
	const char dump_file[] = "error.inp";

	// remove dump file if it exists
	//
	if (::FileExists(dump_file))
	{
		CPPUNIT_ASSERT(::DeleteFile(dump_file));
	}

	int files_on = 0;
	CPPUNIT_ASSERT_EQUAL(0,     ::LoadDatabase("phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("SOLUTION 1"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("	pH	7"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("	Na	1"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("PHASES"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("	Fix_H+"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("	H+ = H+"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("	log_k	0"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("EQUILIBRIUM_PHASES"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("	Fix_H+ -10 HCl	10"));
	CPPUNIT_ASSERT_EQUAL(VR_OK, ::AccumulateLine("END"));
	CPPUNIT_ASSERT_EQUAL(1,     ::Run(files_on, files_on, files_on, files_on));


	const char expected[] =
		"ERROR: Numerical method failed on all combinations of convergence parameters\n"
		"Stopping.\n";
	const char* err = ::GetLastErrorString();

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );

	CPPUNIT_ASSERT_EQUAL( true,    ::FileExists(dump_file) );
	CPPUNIT_ASSERT(::DeleteFile(dump_file));
}


void TestInterface::TestRunFile()
{
	const char dump_file[] = "error.inp";

	// remove dump file if it exists
	//
	if (::FileExists(dump_file))
	{
		CPPUNIT_ASSERT(::DeleteFile(dump_file));
	}



	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase("phreeqc.dat"));
	CPPUNIT_ASSERT_EQUAL(1, ::RunFile("conv_fail.in", 0, 0, 0, 0));

	const char expected[] =
		"ERROR: Numerical method failed on all combinations of convergence parameters\n"
		"Stopping.\n";
	const char* err = ::GetLastErrorString();

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );

	CPPUNIT_ASSERT_EQUAL( true,    ::FileExists(dump_file) );
	CPPUNIT_ASSERT(::DeleteFile(dump_file));
}

void TestInterface::TestGetSelectedOutputRowCount()
{
	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase("llnl.dat"));

	int max = 6;

	CPPUNIT_ASSERT_EQUAL(VR_OK, SOLUTION(1.0, 1.0, 1.0));
	CPPUNIT_ASSERT_EQUAL(VR_OK, EQUILIBRIUM_PHASES("calcite", 0.0, 0.010));
	CPPUNIT_ASSERT_EQUAL(VR_OK, USER_PUNCH("Ca", max));
	CPPUNIT_ASSERT_EQUAL(0, ::Run(0, 0, 0, 1));
	//// CPPUNIT_ASSERT_EQUAL(0, ::Run(1, 1, 1, 1));

	CPPUNIT_ASSERT_EQUAL(3, ::GetSelectedOutputRowCount()); // rows + header
}

void TestInterface::TestGetSelectedOutputValue()
{
	int col;

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase("llnl.dat"));

	int max = 6;

	CPPUNIT_ASSERT_EQUAL(VR_OK, SOLUTION(1.0, 1.0, 1.0));
	CPPUNIT_ASSERT_EQUAL(VR_OK, EQUILIBRIUM_PHASES("calcite", 0.0, 0.010));
	CPPUNIT_ASSERT_EQUAL(VR_OK, USER_PUNCH("Ca", max));
	CPPUNIT_ASSERT_EQUAL(0, ::Run(0, 0, 0, 0));

/*
EXPECTED selected.out:
         sim	       state	        soln	      dist_x	        time	        step	          pH	          pe	           C	          Ca	          Na	     m_CO3-2	     m_CaOH+	    m_NaCO3-	    la_CO3-2	    la_CaOH+	   la_NaCO3-	     Calcite	   d_Calcite	   si_CO2(g)	 si_Siderite	    pressure	   total mol	      volume	    g_CO2(g)	     g_N2(g)	    k_Albite	   dk_Albite	    k_Pyrite	   dk_Pyrite	     s_CaSO4	     s_SrSO4	      1.name	      1.type	     1.moles	      2.name	      2.type	     2.moles	      3.name	      3.type	     3.moles	      4.name	      4.type	     4.moles	      5.name	      5.type	     5.moles	      6.name	      6.type	     6.moles
           1	      i_soln	           1	         -99	         -99	         -99	           7	           4	 1.0000e-003	 1.0000e-003	 1.0000e-003	 4.2975e-007	 1.1819e-009	 1.1881e-009	-6.4686e+000	-8.9530e+000	-8.9507e+000	 0.0000e+000	 0.0000e+000	     -2.2870	   -999.9990	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	        Ca+2	          aq	 9.9178e-004	     CaHCO3+	          aq	 7.5980e-006	       CaCO3	          aq	 6.2155e-007	       CaOH+	          aq	 1.1819e-009
           1	       react	           1	         -99	           0	           1	     7.86135	       10.18	 1.1556e-003	 1.1556e-003	 1.0000e-003	 4.2718e-006	 9.7385e-009	 1.1620e-008	-5.4781e+000	-8.0388e+000	-7.9621e+000	 9.8444e-003	-1.5555e-004	     -3.0192	   -999.9990	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	 0.0000e+000	     calcite	        equi	 9.8444e-003	        Ca+2	          aq	 1.1371e-003	     CaHCO3+	          aq	 1.1598e-005	       CaCO3	          aq	 6.8668e-006	       CaOH+	          aq	 9.7385e-009
*/


	CVar v;

	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, ::GetSelectedOutputValue(-1, 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);

	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, ::GetSelectedOutputValue(::GetSelectedOutputRowCount(), 0, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDROW, v.vresult);

	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, ::GetSelectedOutputValue(0, -1, &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, v.vresult);

	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, ::GetSelectedOutputValue(0, ::GetSelectedOutputColumnCount(), &v));
	CPPUNIT_ASSERT_EQUAL(TT_ERROR, v.type);
	CPPUNIT_ASSERT_EQUAL(VR_INVALIDCOL, v.vresult);


	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, 0, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("sim"), std::string(v.sVal));

	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, 1, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("state"), std::string(v.sVal));

	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, 2, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("soln"), std::string(v.sVal));

	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, 3, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("dist_x"), std::string(v.sVal));

	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, 4, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("time"), std::string(v.sVal));

	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, 5, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("step"), std::string(v.sVal));

	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, 6, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("pH"), std::string(v.sVal));

	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, 7, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("pe"), std::string(v.sVal));

	col = 7;

	// -totals C Ca Na
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("C(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("Na(mol/kgw)"), std::string(v.sVal));

	// -molalities CO3-2  CaOH+  NaCO3-
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("m_CO3-2(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("m_CaOH+(mol/kgw)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("m_NaCO3-(mol/kgw)"), std::string(v.sVal));

	// -activities CO3-2  CaOH+  NaCO3-
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("la_CO3-2"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("la_CaOH+"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("la_NaCO3-"), std::string(v.sVal));

	// -equilibrium_phases Calcite
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("Calcite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("d_Calcite"), std::string(v.sVal));


	// -saturation_indices CO2(g) Siderite
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("si_CO2(g)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("si_Siderite"), std::string(v.sVal));

	// -gases CO2(g) N2(g)
	//                      pressure "total mol" volume g_CO2(g) g_N2(g)
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("pressure"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("total mol"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("volume"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("g_CO2(g)"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("g_N2(g)"), std::string(v.sVal));

	// -kinetic_reactants Albite Pyrite
	//                               k_Albite dk_Albite k_Pyrite dk_Pyrite
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("k_Albite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("dk_Albite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("k_Pyrite"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("dk_Pyrite"), std::string(v.sVal));

	// -solid_solutions CaSO4 SrSO4
	//                              s_CaSO4 s_SrSO4
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("s_CaSO4"), std::string(v.sVal));
	++col;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("s_SrSO4"), std::string(v.sVal));

	for (int i = 0; i < max; ++i)
	{
		std::ostringstream oss1, oss2, oss3;

		// 1.name
		//
		CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col + 1 + (i*3) , &v) == VR_OK );
		CPPUNIT_ASSERT( v.type == TT_STRING );
		oss1 << i + 1 << ".name";
		CPPUNIT_ASSERT_EQUAL( oss1.str(), std::string(v.sVal));

		// 1.type
		//
		CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col + 2 + (i*3), &v) == VR_OK );
		CPPUNIT_ASSERT( v.type == TT_STRING );
		oss2 << i + 1 << ".type";
		CPPUNIT_ASSERT_EQUAL( oss2.str(), std::string(v.sVal));

		// 1.moles
		//
		CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, col + 3 + (i*3), &v) == VR_OK );
		CPPUNIT_ASSERT( v.type == TT_STRING );
		oss3 << i + 1 << ".moles";
		CPPUNIT_ASSERT_EQUAL( oss3.str(), std::string(v.sVal));
	}

	// sim
	//
	col = 0;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_LONG );
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_LONG );
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );

	// state
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("i_soln"), std::string(v.sVal) );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("react"), std::string(v.sVal) );

	// soln
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_LONG );
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_LONG );
	CPPUNIT_ASSERT_EQUAL( 1L, v.lVal );

	// dist_x -- sometimes as double sometimes as long (depends on state)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_LONG );
	CPPUNIT_ASSERT_EQUAL( -99L, v.lVal );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_LONG );
	CPPUNIT_ASSERT_EQUAL( -99L, v.lVal );


	// time -- sometimes as double sometimes as long (depends on state)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_LONG );
	CPPUNIT_ASSERT_EQUAL( -99L, v.lVal );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, v.dVal, ::pow(10., -DBL_DIG) );

	// step
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_LONG );
	CPPUNIT_ASSERT( v.lVal == -99 );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_LONG );
	CPPUNIT_ASSERT( v.lVal == 1 );


	// pH
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.0, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.861354, v.dVal, ::pow(10., -6) );

	// pe
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 4.0, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 10.4, v.dVal, ::pow(10., -1) );

	//
	// -totals C Ca Na
	//

	// C(mol/kgw)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0000e-003, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1556e-003, v.dVal, ::pow(10., -7) );


	// Ca(mol/kgw)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0000e-003, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1556e-003, v.dVal, ::pow(10., -7) );


	// Na(mol/kgw)
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0000e-003, v.dVal, ::pow(10., -DBL_DIG) );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
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
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca+2"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("calcite"),  std::string(v.sVal));

	// 1.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("equi"),  std::string(v.sVal));

	// 1.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 9.9177923E-04, v.dVal, ::pow(10., -11) );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 9.8444477E-03, v.dVal, ::pow(10., -10) );

	// 2.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("CaHCO3+"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("Ca+2"),  std::string(v.sVal));

	// 2.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 2.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 7.5980e-006, v.dVal, ::pow(10., -10) );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1371e-003, v.dVal, ::pow(10., -7) );


	// 3.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("CaCO3"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("CaHCO3+"),  std::string(v.sVal));

	// 3.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 3.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 6.2155e-007, v.dVal, ::pow(10., -11) );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1598e-005, v.dVal, ::pow(10., -9) );



	// 4.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT_EQUAL( TT_STRING, v.type );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("CaOH+"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("CaCO3"),  std::string(v.sVal));

	// 4.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 4.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.1819e-009, v.dVal, ::pow(10., -13) );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 6.8668e-006, v.dVal, ::pow(10., -10) );


	// 5.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("CaOH+"),  std::string(v.sVal));

	// 5.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_EMPTY );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string("aq"),  std::string(v.sVal));

	// 5.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_EMPTY );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_DOUBLE );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 9.7385e-009, v.dVal, ::pow(10., -13) );


	// 6.name
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );

	// 6.type
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_EMPTY );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );

	// 6.moles
	//
	++col;
	//   i_soln
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, col, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_EMPTY );
	//   react
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, col, &v) == VR_OK );
	CPPUNIT_ASSERT_EQUAL( TT_EMPTY, v.type );
}

void TestInterface::TestGetSelectedOutputColumnCount()
{
	CPPUNIT_ASSERT_EQUAL( 0,     ::LoadDatabase("llnl.dat"));
	CPPUNIT_ASSERT_EQUAL( 0,     ::GetSelectedOutputColumnCount() );
	CPPUNIT_ASSERT_EQUAL( VR_OK,   SOLUTION(1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( VR_OK,   EQUILIBRIUM_PHASES("calcite", 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( VR_OK,   USER_PUNCH("Ca", 10) );
	CPPUNIT_ASSERT_EQUAL( 0,     ::Run(0, 0, 0, 0) );
	CPPUNIT_ASSERT_EQUAL( 62,    ::GetSelectedOutputColumnCount() );
}

void TestInterface::TestAddError()
{
	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase("phreeqc.dat"));

	// make sure initialized to empty
	//
	const char* err = ::GetLastErrorString();
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(err) );

	// make sure initialized to empty
	//
	const char *expected = "TESTING AddError\n";
	CPPUNIT_ASSERT_EQUAL(1u, ::AddError(expected));

	// check 1
	//
	err = ::GetLastErrorString();
	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );

	// check increment
	//
	const char *expected2 = "XXXXXX\n";
	CPPUNIT_ASSERT_EQUAL(2u, ::AddError(expected2));

	// check concatenation
	//
	err = ::GetLastErrorString();
	CPPUNIT_ASSERT_EQUAL( std::string(expected) + std::string(expected2), std::string(err) );


	// clear errors
	//
	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase("phreeqc.dat"));

	// make sure back to empty
	//
	err = ::GetLastErrorString();
	CPPUNIT_ASSERT_EQUAL( std::string(""), std::string(err) );
}

void TestInterface::TestAccumulateLine()
{
	// TODO
}

void TestInterface::TestOutputLastError()
{
	// TODO
}

void TestInterface::TestRunWithCallback()
{
	// TODO
}

void TestInterface::TestRunNoDatabaseLoaded()
{
	UnLoadDatabase();
	CPPUNIT_ASSERT_EQUAL( 1,     ::Run(0, 0, 0, 0) );

	const char expected[] =
		"ERROR: Run: No database is loaded\n"
		"Stopping.\n";
	const char* err = ::GetLastErrorString();

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );
}

void TestInterface::TestRunFileNoDatabaseLoaded()
{
	UnLoadDatabase();
	CPPUNIT_ASSERT_EQUAL( 1, ::RunFile("dummy", 0, 0, 0, 0) );

	const char expected[] =
		"ERROR: RunFile: No database is loaded\n"
		"Stopping.\n";
	const char* err = ::GetLastErrorString();

	CPPUNIT_ASSERT_EQUAL( std::string(expected), std::string(err) );
}


void TestInterface::TestCase1()
{
	// Case 1 (see do_run)
	// pr.punch == TRUE
	// punch.new_def == FALSE
	// output_isopen(OUTPUT_PUNCH) == FALSE
	// selected_output_on == TRUE

	// remove punch file if it exists
	if (::FileExists("selected.out"))
	{
		CPPUNIT_ASSERT(::DeleteFile("selected.out"));
	}
	CPPUNIT_ASSERT_EQUAL( false,    ::FileExists("selected.out") );


	// clear all flags
	CPPUNIT_ASSERT_EQUAL( 0,       ::LoadDatabase("phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( FALSE,   punch.in);
	CPPUNIT_ASSERT_EQUAL( TRUE,    pr.punch);

	CPPUNIT_ASSERT_EQUAL( VR_OK,   SOLUTION(1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( VR_OK,   USER_PUNCH("Ca", 10) );
	CPPUNIT_ASSERT_EQUAL( 0,       ::Run(0, 0, 0, 1) );
	CPPUNIT_ASSERT_EQUAL( true,    ::FileExists("selected.out") );
	CPPUNIT_ASSERT_EQUAL( 62,      ::GetSelectedOutputColumnCount() );

	CPPUNIT_ASSERT_EQUAL( VR_OK,   SOLUTION(1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( 0,       ::Run(0, 0, 0, 1) );
	CPPUNIT_ASSERT_EQUAL( true,    ::FileExists("selected.out") );
	CPPUNIT_ASSERT_EQUAL( 62,      ::GetSelectedOutputColumnCount() );
}

void TestInterface::TestCase2()
{
	// Case 2 (see do_run)
	// pr.punch == TRUE
	// punch.new_def == TRUE
	// output_isopen(OUTPUT_PUNCH) == FALSE
	// selected_output_on == TRUE

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
	CPPUNIT_ASSERT_EQUAL( false,    ::FileExists("selected.out") );
	CPPUNIT_ASSERT_EQUAL( false,    ::FileExists("case2.punch") );

	// clear all flags
	CPPUNIT_ASSERT_EQUAL( 0,       ::LoadDatabase("phreeqc.dat") );
	CPPUNIT_ASSERT_EQUAL( FALSE,   punch.in);
	CPPUNIT_ASSERT_EQUAL( TRUE,    pr.punch);

	CPPUNIT_ASSERT_EQUAL( VR_OK,   SOLUTION(1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( VR_OK,   USER_PUNCH("Ca", 10) );
	CPPUNIT_ASSERT_EQUAL( VR_OK,   ::AccumulateLine("-file case2.punch") ); // force have_punch_name to TRUE (see read_selected_ouput)
	CPPUNIT_ASSERT_EQUAL( 0,       ::Run(0, 0, 0, 1) );
	CPPUNIT_ASSERT_EQUAL( false,   ::FileExists("selected.out") );
	CPPUNIT_ASSERT_EQUAL( true,    ::FileExists("case2.punch") );
	CPPUNIT_ASSERT_EQUAL( 62,      ::GetSelectedOutputColumnCount() );


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
	CPPUNIT_ASSERT_EQUAL( false,    ::FileExists("selected.out") );
	CPPUNIT_ASSERT_EQUAL( false,    ::FileExists("case2.punch") );

	CPPUNIT_ASSERT_EQUAL( VR_OK,   SOLUTION(1.0, 1.0, 1.0) );
	CPPUNIT_ASSERT_EQUAL( VR_OK,   USER_PUNCH("Ca", 10) );
	CPPUNIT_ASSERT_EQUAL( 0,       ::Run(0, 0, 0, 1) );
	CPPUNIT_ASSERT_EQUAL( false,   ::FileExists("selected.out") );
	CPPUNIT_ASSERT_EQUAL( true,    ::FileExists("case2.punch") );
	CPPUNIT_ASSERT_EQUAL( 62,      ::GetSelectedOutputColumnCount() );

	if (::FileExists("case2.punch"))
	{
		::DeleteFile("case2.punch");
	}
	CPPUNIT_ASSERT_EQUAL( false,    ::FileExists("case2.punch") );
}

void TestInterface::TestPrintSelectedOutputFalse()
{
	// remove punch files if they exists
	//
	if (::FileExists("selected.out"))
	{
		::DeleteFile("selected.out");
	}
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists("selected.out") );

	CPPUNIT_ASSERT_EQUAL( 0, ::LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(1.0, 1.0, 1.0) );

	// add selected output block
	{
		std::ostringstream oss;
		oss << "SELECTED_OUTPUT" << "\n";
		oss << "-file selected.out" << "\n";
		oss << "-totals C Ca Na" << "\n";
		CPPUNIT_ASSERT_EQUAL( VR_OK, ::AccumulateLine(oss.str().c_str()) );
	}

	// add selected output block
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::AccumulateLine("PRINT; -selected_output false \n") );

	// run
	CPPUNIT_ASSERT_EQUAL( 0, ::Run(0, 0, 0, 1) );

	CPPUNIT_ASSERT_EQUAL( 0, ::GetSelectedOutputColumnCount() );
	CPPUNIT_ASSERT_EQUAL( 0, ::GetSelectedOutputRowCount() );


	// reset pr.punch to TRUE
	CPPUNIT_ASSERT_EQUAL( 0, ::LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(1.0, 1.0, 1.0) );

	// add selected output block
	{
		std::ostringstream oss;
		oss << "SELECTED_OUTPUT" << "\n";
		oss << "-file selected.out" << "\n";
		oss << "-totals C Ca Na" << "\n";
		CPPUNIT_ASSERT_EQUAL( VR_OK, ::AccumulateLine(oss.str().c_str()) );
	}

	// run
	CPPUNIT_ASSERT_EQUAL( 0, ::Run(0, 0, 0, 1) );

	CPPUNIT_ASSERT_EQUAL( 11, ::GetSelectedOutputColumnCount() );
	CPPUNIT_ASSERT_EQUAL( 2, ::GetSelectedOutputRowCount() );
}

void TestInterface::TestOutputOnOff()
{
	int onoff[4];
	onoff[0] = 1;  // output_on
	onoff[1] = 0;  // error_on
	onoff[2] = 0;  // log_on
	onoff[3] = 0;  // selected_output_on
	TestOnOff("phreeqc.out", onoff[0], onoff[1], onoff[2], onoff[3]);
}

void TestInterface::TestErrorOnOff()
{
	int onoff[4];
	onoff[0] = 0;  // output_on
	onoff[1] = 1;  // error_on
	onoff[2] = 0;  // log_on
	onoff[3] = 0;  // selected_output_on
	TestOnOff("phreeqc.err", onoff[0], onoff[1], onoff[2], onoff[3]);
}

void TestInterface::TestLogOnOff()
{
	int onoff[4];
	onoff[0] = 0;  // output_on
	onoff[1] = 0;  // error_on
	onoff[2] = 1;  // log_on
	onoff[3] = 0;  // selected_output_on
	TestOnOff("phreeqc.log", onoff[0], onoff[1], onoff[2], onoff[3]);
}

void TestInterface::TestSelOutOnOff()
{
	int onoff[4];
	onoff[0] = 0;  // output_on
	onoff[1] = 0;  // error_on
	onoff[2] = 0;  // log_on
	onoff[3] = 1;  // selected_output_on
	TestOnOff("selected.out", onoff[0], onoff[1], onoff[2], onoff[3]);
}

VRESULT
SOLUTION(double C, double Ca, double Na)
{
	std::ostringstream oss;

	oss << "SOLUTION 1\n";
	oss << "C "  << C  << "\n";
	oss << "Ca " << Ca << "\n";
	oss << "Na " << Na << "\n";

	return ::AccumulateLine(oss.str().c_str());
}

VRESULT
EQUILIBRIUM_PHASES(const char* phase, double si, double amount)
{
	std::ostringstream oss;

	oss << "EQUILIBRIUM_PHASES\n";
	oss << phase << " " << si << " " << amount << "\n";
	return ::AccumulateLine(oss.str().c_str());
}

VRESULT
USER_PUNCH(const char* element, int max)
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

	return ::AccumulateLine(oss.str().c_str());
}

#if defined(_WIN32) || defined(__CYGWIN32__)
bool FileExists(const char *szPathName)
{
	SECURITY_ATTRIBUTES sa;
	sa.nLength = sizeof(SECURITY_ATTRIBUTES);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;
	HANDLE fileHandle = ::CreateFile(szPathName, GENERIC_READ, 0, &sa, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	bool retValue;
	if (fileHandle == INVALID_HANDLE_VALUE)
	{
		retValue = false;
	}
	else
	{
		retValue = true;
		::CloseHandle(fileHandle);
	}
	return retValue;
}
#else
bool FileExists(const char *szPathName)
{
  FILE* fp;
  fp = fopen(szPathName, "r");
  if (fp == NULL) {
    return false;
  } else {
    fclose(fp);
    return true;
  }
}
#endif

void TestOnOff(const char* FILENAME, int output_on, int error_on, int log_on, int selected_output_on)
{
	//const char *FILENAME = "phreeqc.out";

	// remove punch files if they exists
	//
	if (::FileExists(FILENAME))
	{
		::DeleteFile(FILENAME);
	}
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(FILENAME) );

	CPPUNIT_ASSERT_EQUAL( 0, ::LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(1.0, 1.0, 1.0) );

	// add selected output block
	{
		std::ostringstream oss;
		oss << "SELECTED_OUTPUT" << "\n";
		oss << "-file selected.out" << "\n";
		oss << "-totals C Ca Na" << "\n";
		CPPUNIT_ASSERT_EQUAL( VR_OK, ::AccumulateLine(oss.str().c_str()) );
	}

	// run all off
	CPPUNIT_ASSERT_EQUAL( 0, ::Run(0, 0, 0, 0) );
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(FILENAME) );



	CPPUNIT_ASSERT_EQUAL( 0, ::LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(1.0, 1.0, 1.0) );

	// add selected output block
	{
		std::ostringstream oss;
		oss << "SELECTED_OUTPUT" << "\n";
		oss << "-file selected.out" << "\n";
		oss << "-totals C Ca Na" << "\n";
		CPPUNIT_ASSERT_EQUAL( VR_OK, ::AccumulateLine(oss.str().c_str()) );
	}

	// run
	CPPUNIT_ASSERT_EQUAL( 0, ::Run(output_on, error_on, log_on, selected_output_on) );
	CPPUNIT_ASSERT_EQUAL( true, ::FileExists(FILENAME) );
	CPPUNIT_ASSERT( ::DeleteFile(FILENAME) );



	CPPUNIT_ASSERT_EQUAL( 0, ::LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(1.0, 1.0, 1.0) );

	// add selected output block
	{
		std::ostringstream oss;
		oss << "SELECTED_OUTPUT" << "\n";
		oss << "-file selected.out" << "\n";
		oss << "-totals C Ca Na" << "\n";
		CPPUNIT_ASSERT_EQUAL( VR_OK, ::AccumulateLine(oss.str().c_str()) );
	}

	// run
	CPPUNIT_ASSERT_EQUAL( 0, ::Run(0, 0, 0, 0) );
	CPPUNIT_ASSERT_EQUAL( false, ::FileExists(FILENAME) );

	CPPUNIT_ASSERT_EQUAL( 0, ::LoadDatabase("phreeqc.dat") );

	// add solution block
	CPPUNIT_ASSERT_EQUAL( VR_OK, SOLUTION(1.0, 1.0, 1.0) );

	// add selected output block
	{
		std::ostringstream oss;
		oss << "SELECTED_OUTPUT" << "\n";
		oss << "-file selected.out" << "\n";
		oss << "-totals C Ca Na" << "\n";
		CPPUNIT_ASSERT_EQUAL( VR_OK, ::AccumulateLine(oss.str().c_str()) );
	}

	// run
	CPPUNIT_ASSERT_EQUAL( 0, ::Run(output_on, error_on, log_on, selected_output_on) );
	CPPUNIT_ASSERT_EQUAL( true, ::FileExists(FILENAME) );
	CPPUNIT_ASSERT( ::DeleteFile(FILENAME) );
}

void
TestInterface::TestLongHeadings()
{
	char long_header[] = "this_is_a_long_header_0123456789012345678901234567890123456789";
	char long_value[]  = "this_is_a_long_value_01234567890123456789012345678901234567890";

	CPPUNIT_ASSERT_EQUAL(0, ::LoadDatabase("phreeqc.dat"));

	std::ostringstream oss;
	oss << "SOLUTION" << "\n";

	oss << "SELECTED_OUTPUT" << "\n";
	oss << "-reset false" << "\n";

	oss << "USER_PUNCH" << "\n";
	oss << "-head " <<  long_header << "\n";
	oss << "-start" << "\n";
	oss << "10 PUNCH \"" << long_value << "\"\n";
	oss << "-end" << "\n";
	CPPUNIT_ASSERT_EQUAL( VR_OK, ::AccumulateLine(oss.str().c_str()) );

	CPPUNIT_ASSERT_EQUAL( 0, ::Run(0, 0, 0, 0) );

	CPPUNIT_ASSERT_EQUAL(2, ::GetSelectedOutputRowCount());
	CPPUNIT_ASSERT_EQUAL(1, ::GetSelectedOutputColumnCount());

	CVar v;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(0, 0, &v) == VR_OK );
	CPPUNIT_ASSERT( v.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string(long_header), std::string(v.sVal));

	CVar v1;
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, 0, &v1) == VR_OK );
	CPPUNIT_ASSERT( v1.type == TT_STRING );
	CPPUNIT_ASSERT_EQUAL( std::string(long_value), std::string(v1.sVal));

	CPPUNIT_ASSERT( ::GetSelectedOutputValue(1, 1, &v1) == VR_INVALIDCOL );
	CPPUNIT_ASSERT( ::GetSelectedOutputValue(2, 0, &v1) == VR_INVALIDROW );
}
