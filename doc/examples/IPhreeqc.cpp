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
  
  iphreeqc.AccumulateLine("TITLE Example 2.--Temperature dependence of solubility");
  iphreeqc.AccumulateLine("                  of gypsum and anhydrite             ");
  iphreeqc.AccumulateLine("SOLUTION 1 Pure water                                 ");
  iphreeqc.AccumulateLine("        pH      7.0                                   ");
  iphreeqc.AccumulateLine("        temp    25.0                                  ");
  iphreeqc.AccumulateLine("EQUILIBRIUM_PHASES 1                                  ");
  iphreeqc.AccumulateLine("        Gypsum          0.0     1.0                   ");
  iphreeqc.AccumulateLine("        Anhydrite       0.0     1.0                   ");
  iphreeqc.AccumulateLine("REACTION_TEMPERATURE 1                                ");
  iphreeqc.AccumulateLine("        25.0 75.0 in 51 steps                         ");
  iphreeqc.AccumulateLine("SELECTED_OUTPUT                                       ");
  iphreeqc.AccumulateLine("        -file   ex2.sel                               ");
  iphreeqc.AccumulateLine("        -temperature                                  ");
  iphreeqc.AccumulateLine("        -si     anhydrite  gypsum                     ");
  iphreeqc.AccumulateLine("END                                                   ");
  
  if (iphreeqc.RunAccumulated() != 0) {
    std::cout << iphreeqc.GetErrorString() << std::endl;
    return EXIT_FAILURE;
  }
  
  VAR v;
  ::VarInit(&v);
  
  std::cout << "selected-output:" << std::endl;
  for (int i = 0; i < iphreeqc.GetSelectedOutputRowCount(); ++i) {
    for (int j = 0; j < iphreeqc.GetSelectedOutputColumnCount(); ++j) {
      if (iphreeqc.GetSelectedOutputValue(i, j, &v) == VR_OK) {
        switch (v.type) {
        case TT_LONG:
          std::cout << v.lVal << " ";
          break;
        case TT_DOUBLE:
          std::cout << v.dVal << " ";
          break;
        case TT_STRING:
          std::cout << v.sVal << " ";
          break;
	case TT_EMPTY:
          std::cout << "<empty> ";
	  break;
	case TT_ERROR:
          std::cout << "<error> ";
	  break;
        }
      }
      ::VarClear(&v);
    }
    std::cout << std::endl;
  }
  return EXIT_SUCCESS;
}
