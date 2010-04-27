#include <IPhreeqc.h>

int
main(int argc, const char* argv[])
{
  int id;
  
  id = CreateIPhreeqc();
  if (id < 0)
  {
    return 1;
  }
  
  if (LoadDatabase(id, "phreeqc.dat") != 0)
  {
    OutputLastError(id);
    return 2;
  }
  
  if (RunFile(id, "ex1") != 0)
  {
    OutputLastError(id);
    return 3;
  }
  
  if (DestroyIPhreeqc(id) != IPQ_OK)
  {
    OutputLastError(id);
    return 4;
  }
  
  return 0;
}
