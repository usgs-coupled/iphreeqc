#include <iostream>
#include <cassert>

#include <IPhreeqc.hpp>

int main(int argc, char* argv[])
{
	IPhreeqc obj;

	assert(obj.GetOutputOn() == false);
	obj.SetOutputOn(true);
	assert(obj.GetOutputOn() == true);

	assert(obj.GetErrorOn() == false);
	obj.SetErrorOn(true);
	assert(obj.GetErrorOn() == true);

	assert(obj.GetSelectedOutputOn() == false);
	obj.SetSelectedOutputOn(true);
	assert(obj.GetSelectedOutputOn() == true);

	assert(obj.GetDumpOn() == false);
	obj.SetDumpOn(true);
	assert(obj.GetDumpOn() == true);

	assert(obj.GetDumpStringOn() == false);
	obj.SetDumpStringOn(true);
	assert(obj.GetDumpStringOn() == true);

	assert(obj.GetLogOn() == false);
	obj.SetLogOn(true);
	assert(obj.GetLogOn() == true);


	if (obj.LoadDatabase("phreeqc.dat"))
	{
		goto error;
	}
	if (obj.RunFile("ex1"))
	{
		goto error;
	}
	std::cout << "Ok\n";
	return 0;
error:
	std::cout << obj.GetErrorString();
	return 1;
}

