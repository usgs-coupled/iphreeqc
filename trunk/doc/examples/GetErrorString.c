#include <stdlib.h>
#include <stdio.h>
#include <IPhreeqc.h>

#define TRUE  1

const char input[] =
  "SOLUTION 1 Pure water     \n"
  "EQUILIBRIUM_PHASES 1      \n"
  "    Calcite 0 10          \n"
  "SAVE solution 1           \n"
  "SAVE equilibrium_phases 1 \n"
  "DUMP                      \n"
  "    -solution 1           \n"
  "    -equilibrium_phases  1\n";

int main(void)
{
  int id;
  
  id = CreateIPhreeqc();
  if (id < 0) {
    return EXIT_FAILURE;
  }
  
  if (SetErrorStringOn(id, TRUE) != IPQ_OK) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }
  
  if (RunString(id, input) != 0) {
    printf("Error:\n");
    printf("%s\n", GetErrorString(id));
    return EXIT_FAILURE;
  }
  
  if (DestroyIPhreeqc(id) != IPQ_OK) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}
