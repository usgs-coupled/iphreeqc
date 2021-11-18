// -*- coding: windows-1252 -*-
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

#include <gtest/gtest.h>

#include <cmath>
#include <cfloat>
#include <cassert>
#include "IPhreeqc.hpp"
#include "Phreeqc.h"
#include "FileTest.h"
#undef true
#undef false
#include "CVar.hxx"

// VRESULT SOLUTION(IPhreeqc& obj, double C, double Ca, double Na);
// VRESULT EQUILIBRIUM_PHASES(IPhreeqc& obj, const char* phase, double si, double amount);
// VRESULT USER_PUNCH(IPhreeqc& obj, const char* element, int max);
// VRESULT SELECTED_OUTPUT(IPhreeqc& obj);
// VRESULT DUMP(IPhreeqc& obj);

// void TestFileOnOff(const char* FILENAME_FORMAT, bool output_file_on, bool error_file_on, bool log_file_on, bool selected_output_file_on, bool dump_file_on);

std::string LoadFileAsString(std::string file)
{
  std::ifstream f(file.c_str());
  std::string str;
  if (f) {
    std::ostringstream oss;
    oss << f.rdbuf();
    str = oss.str();
  }
  return str;
}

TEST(TestIPhreeqc, TestSimulatephreeqcExR)
{
	IPhreeqc obj;

	// ex1
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex1"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex1").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex10
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex10"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex10").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex11
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex11"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex11").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex12
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex12"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex12").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex13a
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex13a"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex13a").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex14
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex14"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex14").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex15 Not run
	ASSERT_EQ(true, ::FileExists("ex15.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("ex15.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex15"));
	// Not run : ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex15").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex16
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex16"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex16").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex17
	ASSERT_EQ(true, ::FileExists("pitzer.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("pitzer.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex17"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex17").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex18
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex18"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex18").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex19
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex19"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex19").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex2
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex2"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex2").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex20a
	ASSERT_EQ(true, ::FileExists("iso.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("iso.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex20a"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex20a").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex21 Not run
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	int save = obj.GetCurrentSelectedOutputUserNumber();
	obj.SetCurrentSelectedOutputUserNumber(1);
	obj.SetSelectedOutputFileOn(TRUE);
	obj.SetCurrentSelectedOutputUserNumber(save);
	ASSERT_EQ(true, ::FileExists("ex21"));
	// Not run: ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex21").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex22
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex22"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex22").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex3
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex3"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex3").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex4
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex4"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex4").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex5
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex5"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex5").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex6
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex6"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex6").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex7
	ASSERT_EQ(true, ::FileExists("phreeqc.dat"));
	ASSERT_EQ(0, obj.LoadDatabaseString(LoadFileAsString("phreeqc.dat").c_str()));
	obj.SetOutputStringOn(TRUE);
	ASSERT_EQ(true, ::FileExists("ex7"));
	ASSERT_EQ(0, obj.RunString(LoadFileAsString("ex7").c_str()));
	{
		const char* output = obj.GetOutputString();
		///std::cout << output << std::endl;
	}

	// ex8
	// ex9
}
