#include <stdlib.h>
#include <IPhreeqc.h>

int main(void)
{
  int id;
  
  id = CreateIPhreeqc();
  if (id < 0) {
    return EXIT_FAILURE;
  }
  
  if (LoadDatabase(id, "phreeqc.dat") != 0) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }
  
  if (AccumulateLine(id, "SOLUTION 1") != IPQ_OK) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }
  
  if (AccumulateLine(id, "END") != IPQ_OK) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }
  
  if (RunAccumulated(id) != 0) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }
  
  if (DestroyIPhreeqc(id) != IPQ_OK) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}
