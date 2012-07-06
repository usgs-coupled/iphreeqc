#include <stdlib.h>
#include <stdio.h>
#include <IPhreeqc.h>

const int SVALUE_LENGTH = 100;

int main(void)
{
  int id, i, j, vtype;
  double dvalue
  char svalue[SVALUE_LENGTH];

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

  printf("selected-output:\n");
  for (i = 0; i < GetSelectedOutputRowCount(id); ++i) {
    for (j = 0; j < GetSelectedOutputColumnCount(id); ++j) {
      if (GetSelectedOutputValue2(id, i, j, vtype, dvalue, svalue, SVALUE_LENGTH) == VR_OK) {
        switch (v.type) {
        case TT_LONG:
          printf("%ld ", v.lVal);
          break;
        case TT_DOUBLE:
          printf("%g ", v.dVal);
          break;
        case TT_STRING:
          printf("%s ", v.sVal);
          break;
        }
      }
    }
    printf("\n");
  }

  if (DestroyIPhreeqc(id)) {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
