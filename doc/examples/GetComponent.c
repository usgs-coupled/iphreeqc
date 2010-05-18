#include <stdlib.h>
#include <stdio.h>
#include <IPhreeqc.h>

int main(void)
{
  int id, i;
  
  id = CreateIPhreeqc();
  if (id < 0) {
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
  
  for (i=0; i < GetComponentCount(id); ++i) {
    printf("comp %d = %s\n", i, GetComponent(id, i));
  }
  
  if (DestroyIPhreeqc(id) != IPQ_OK) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}
