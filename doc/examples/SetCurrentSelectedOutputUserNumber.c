#include <stdlib.h>
#include <stdio.h>
#include <IPhreeqc.h>

int main(void)
{
  int id, i, n, r, c;
  char buffer[30];
  FILE* f;
  VAR v;

  id = CreateIPhreeqc();
  if (id < 0) {
    return EXIT_FAILURE;
  }

  if (LoadDatabase(id, "phreeqc.dat") != 0) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }

  if (RunFile(id, "multi_punch") != 0) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }

  VarInit(&v);

  for (i = 0; i < GetSelectedOutputCount(id); ++i) {
    n = GetNthSelectedOutputUserNumber(id, i);
    snprintf(buffer, sizeof(buffer), "sel_out.%d.out", n);

    if ((f = fopen(buffer, "w"))) {
      SetCurrentSelectedOutputUserNumber(id, n);

      for (r = 0; r < GetSelectedOutputRowCount(id); ++r) {

        for (c = 0; c < GetSelectedOutputColumnCount(id); ++c) {

          if (GetSelectedOutputValue(id, r, c, &v) == IPQ_OK) {

            switch (v.type) {
            case TT_LONG:
              fprintf(f, "%ld,", v.lVal);
              break;
            case TT_DOUBLE:
              fprintf(f, "%g,", v.dVal);
              break;
            case TT_STRING:
              fprintf(f, "%s,", v.sVal);
              break;
            default:
              fprintf(f, ",");
              break;
            }

          }

          VarClear(&v);
        }

        fprintf(f, "\n");
      }

      fclose(f);
    }
  }

  if (DestroyIPhreeqc(id) != IPQ_OK) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
