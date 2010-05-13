#include <stdlib.h>
#include <IPhreeqc.h>

int main(void)
{
  int id;

  id = CreateIPhreeqc();
  if (id < 0) {
    return EXIT_FAILURE;
  }
  
  if (DestroyIPhreeqc(id) != IPQ_OK) {
    OutputError(id);
    return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}
