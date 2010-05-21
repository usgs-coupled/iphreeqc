#include <stdlib.h>
#include <stdio.h>
#include <IPhreeqc.h>

int main(void)
{
  int id, i, j;
  
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
  
  VAR v;
  VarInit(&v);
  
  printf("selected-output:\n");
  for (i = 0; i < GetSelectedOutputRowCount(id); ++i) {
    for (j = 0; j < GetSelectedOutputColumnCount(id); ++j) {
      if (GetSelectedOutputValue(id, i, j, &v) == VR_OK) {
        switch (v.type) {
        case TT_LONG:
          printf("%d ", v.lVal);
          break;
        case TT_DOUBLE:
          printf("%g ", v.dVal);
          break;
        case TT_STRING:
          printf("%s ", v.sVal);
          break;
        }
      }
      VarClear(&v);
    }
    printf("\n");
  }
  return EXIT_SUCCESS;
}
