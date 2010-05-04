#include <iostream>
#include <IPhreeqc.hpp>

int
main(int argc, const char* argv[])
{
  IPhreeqc iphreeqc;

  if (iphreeqc.LoadDatabase("phreeqc.dat") != 0)
  {
    std::cout << iphreeqc.GetErrorString();
    return 1;
  }

  if (iphreeqc.RunFile("ex1") != 0)
  {
    std::cout << iphreeqc.GetErrorString();
    return 2;
  }

  return 0;
}
