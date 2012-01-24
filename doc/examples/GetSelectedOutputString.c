#include <stdlib.h>
#include <stdio.h>
#include <IPhreeqc.h>

#define TRUE  1

int main(void)
{
  int id;

  id = CreateIPhreeqc();
  if (id < 0) {
    return EXIT_FAILURE;
  }

  if (SetSelectedOutputStringOn(id, TRUE) != IPQ_OK) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }

  if (LoadDatabase(id, "phreeqc.dat") != 0) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }

  if (RunFile(id, "ex2") != 0) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }

  printf("selected-output:\n");
  printf("%s\n", GetSelectedOutputString(id));

  if (DestroyIPhreeqc(id)) {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
