#include <cstdlib>
#include <iostream>
#include <IPhreeqc.hpp>

int main(void)
{
  IPhreeqc iphreeqc;

  if (iphreeqc.LoadDatabase("phreeqc.dat") != 0) {
    std::cout << iphreeqc.GetErrorString() << std::endl;
    return EXIT_FAILURE;
  }

  iphreeqc.SetCurrentSelectedOutputUserNumber(1);
  iphreeqc.SetSelectedOutputStringOn(true);
  iphreeqc.SetSelectedOutputFileOn(true);
  iphreeqc.SetSelectedOutputFileName("state.sel");

  iphreeqc.SetCurrentSelectedOutputUserNumber(2);
  iphreeqc.SetSelectedOutputStringOn(true);
  iphreeqc.SetSelectedOutputFileOn(true);
  iphreeqc.SetSelectedOutputFileName("si.sel");


  iphreeqc.AccumulateLine("TITLE Temperature dependence of solubility");
  iphreeqc.AccumulateLine("      of gypsum and anhydrite             ");
  iphreeqc.AccumulateLine("SOLUTION 1 Pure water                     ");
  iphreeqc.AccumulateLine("        pH      7.0                       ");
  iphreeqc.AccumulateLine("        temp    25.0                      ");
  iphreeqc.AccumulateLine("EQUILIBRIUM_PHASES 1                      ");
  iphreeqc.AccumulateLine("        Gypsum          0.0     1.0       ");
  iphreeqc.AccumulateLine("        Anhydrite       0.0     1.0       ");
  iphreeqc.AccumulateLine("REACTION_TEMPERATURE 1                    ");
  iphreeqc.AccumulateLine("        25.0 75.0 in 51 steps             ");
  iphreeqc.AccumulateLine("SELECTED_OUTPUT 1                         ");
  iphreeqc.AccumulateLine("        -temperature                      ");
  iphreeqc.AccumulateLine("SELECTED_OUTPUT 2                         ");
  iphreeqc.AccumulateLine("        -si     anhydrite  gypsum         ");
  iphreeqc.AccumulateLine("END                                       ");

  if (iphreeqc.RunAccumulated() != 0) {
    std::cout << iphreeqc.GetErrorString() << std::endl;
    return EXIT_FAILURE;
  }

  int c = iphreeqc.GetSelectedOutputCount();
  for (int k = 0; k < c; ++k) {
    int n = iphreeqc.GetNthSelectedOutputUserNumber(k);
    iphreeqc.SetCurrentSelectedOutputUserNumber(n);
    std::cout << "selected-output " << n << ":" << std::endl;
    std::cout << iphreeqc.GetSelectedOutputString() << std::endl;
  }
  return EXIT_SUCCESS;
}
