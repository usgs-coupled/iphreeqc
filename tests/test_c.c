#include <IPhreeqc.h>

typedef int (*getFunc)(int);
typedef int (*setFunc)(int, int);
int TestGetSet(int, getFunc, setFunc);

int
main(int argc, const char* argv[])
{
  int id;

  id = CreateIPhreeqc();
  if (id < 0)
  {
    return 1;
  }

  /* Dump */
  if (TestGetSet(id, GetDumpOn, SetDumpOn))
  {
    return 2;
  }

  /* Dump string */
  if (TestGetSet(id, GetDumpStringOn, SetDumpStringOn))
  {
    return 2;
  }


  /* Error */
  if (TestGetSet(id, GetErrorOn, SetErrorOn))
  {
    return 2;
  }

  /* Log */
  if (TestGetSet(id, GetLogOn, SetLogOn))
  {
    return 2;
  }

  /* Output */
  if (TestGetSet(id, GetOutputOn, SetOutputOn))
  {
    return 2;
  }

  /* Selected output */
  if (TestGetSet(id, GetSelectedOutputOn, SetSelectedOutputOn))
  {
    return 2;
  }

  if (LoadDatabase(id, "phreeqc.dat") != 0)
  {
    OutputError(id);
    return 2;
  }

  if (RunFile(id, "ex1") != 0)
  {
    OutputError(id);
    return 3;
  }

  if (DestroyIPhreeqc(id) != IPQ_OK)
  {
    OutputError(id);
    return 4;
  }

  return 0;
}

int 
TestGetSet(int id, getFunc gf, setFunc sf)
{
  if (gf(id))
  {  
    return 2;
  }
  
  if (sf(id, 1) != IPQ_OK)
  {
    return 2;
  }
  
  if (!gf(id))
  {
    return 2;
  }
  
  if (sf(id,0) != IPQ_OK)
  {
    return 2;
  }
  
  return 0;
}
