#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <IPhreeqc.hpp>
#include <sstream>
#include <iomanip>

class MyData
{
public:
	void Mydata() {};
	~MyData() {};

	// data members
	IPhreeqc * IPhreeqc_ptr;
	std::vector < VAR > results;
	double year;

	// methods
	void ExtractWrite(int cell)
	{
		results.clear();
		results.resize(IPhreeqc_ptr->GetSelectedOutputColumnCount());
		for (int j = 0; j < IPhreeqc_ptr->GetSelectedOutputColumnCount(); ++j) 
		{
			VarInit(&results[j]);
			IPhreeqc_ptr->GetSelectedOutputValue(1, j, &results[j]);
		}
		std::cerr << "Cell "     << cell << "  " << (int) results[7].dVal 
			<< std::setprecision(2) << std::fixed
			<< "\n\tpH: "        << results[5].dVal 
			<< "\tSR(calcite): " << results[6].dVal << "\n";
	}

	void EHandler(void)
	{
		IPhreeqc_ptr->OutputErrorString();
		exit(EXIT_FAILURE);	
	}
};

static double MyCallback(double x1, double x2, const char * str1, void *my_ptr)
{
	/*
	Use of a callback is optional.

	The callback provides a way to obtain data from a Basic program
	through the variables x1, x2, and str1, and send data to a 
	Basic program through the return value of the callback.

	The void pointer mydata can be used to obtain data from the
	calling program; in this example, it points to a structure.

	The callback function is called whenever CALLBACK(x1, x2, str$)  
	is used in a Basic program (usually USER_PUNCH). See file "ic".
	*/
	if (strcmp(str1, "Year") == 0)
	{
		fprintf(stderr, "\nCallback for cell %d: pH %8.2f\n", (int) x1, x2);
		return ((MyData *) my_ptr)->year;
	}
	return -1;
}
int main(void)
{
	MyData mydata;
	mydata.year = 2012.0;

	// Create module
	IPhreeqc * myIPhreeqc = new IPhreeqc;
	mydata.IPhreeqc_ptr = myIPhreeqc;
	// Load database
	if (myIPhreeqc->LoadDatabase("phreeqc.dat") != 0) mydata.EHandler();
	// Set callback
	myIPhreeqc->SetBasicCallback(&MyCallback, (void *) &mydata);
	// Define initial conditions and selected output 
	if (myIPhreeqc->RunFile("ic") != 0) mydata.EHandler();

	// Run cell 1
	if (myIPhreeqc->RunString("RUN_CELLS; -cells; 1; END") != 0) mydata.EHandler();
	// Extract/write results 
	mydata.ExtractWrite(1);

	// Advect cell 1 solution to cell 2
	mydata.year += 1.0;
	// Define new solution composition for cell 2
	std::ostringstream oss;
	oss << "SOLUTION_MODIFY 2" << "\n";
	oss << "   -cb      "      << mydata.results[0].dVal << "\n"; 
	oss << "   -total_h "      << mydata.results[1].dVal << "\n"; 
	oss << "   -total_o "      << mydata.results[2].dVal << "\n"; 
	oss << "   -totals  "      << "\n"; 
	oss << "      C     "      << mydata.results[3].dVal << "\n"; 
	oss << "      Ca    "      << mydata.results[4].dVal << "\n"; 
	// run cell 2
	oss << "RUN_CELLS; -cells; 2; END\n";
	if (myIPhreeqc->RunString(oss.str().c_str()) != 0) mydata.EHandler();
	// Extract/write results
	mydata.ExtractWrite(2);

	// Destroy module 
	delete myIPhreeqc;
}