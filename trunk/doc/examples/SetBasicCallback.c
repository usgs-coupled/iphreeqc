#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <IPhreeqc.h>

#define TRUE  1

const char input[] =
  "SOLUTION 1-2                                            \n"
  "END                                                     \n"
  "EQUILIBRIUM_PHASES 1                                    \n"
  "   CO2(g) -1.5 10                                       \n"
  "EQUILIBRIUM_PHASES 2                                    \n"
  "   Calcite 0   10                                       \n"
  "SELECTED_OUTPUT 1                                       \n"
  "   -reset false                                         \n"
  "USER_PUNCH                                              \n"
  "   -Heading  pH  SR(calcite)   Year                     \n"
  "   10 PUNCH -LA(\"H+\"), SR(\"calcite\")                \n"
  "   20 PUNCH CALLBACK(cell_no, -LA(\"H+\"), \"Year\")    \n"
  "END                                                     \n"
  "RUN_CELLS                                               \n"
  "   -cells 1-2                                           \n"
  "END                                                     \n";

struct MyData {
  double year;
};

double MyCallback(double x1, double x2, const char *str1, void *mydata)
{
  /*
    Use of a callback is optional.

    The callback provides a way to obtain data from a Basic program
    through the variables x1, x2, and str1, and send data to a
    Basic program through the return value of the callback.

    The void pointer mydata can be used to obtain data from the
    calling program; in this example, it points to a structure.

    The callback function is called whenever CALLBACK(x1, x2, str$)
    is used in a Basic program (usually USER_PUNCH).
  */
  if (strcmp(str1, "Year") == 0)
  {
    fprintf(stderr, "Callback for cell %d: pH %8.2f\n", (int) x1, x2);
    return ((struct MyData *) mydata)->year;
  }
  return -1;
}

int main(void)
{
  int id;
  struct MyData mydata;

  mydata.year = 2012.0;

  id = CreateIPhreeqc();
  if (id < 0) {
    return EXIT_FAILURE;
  }

  if (LoadDatabase(id, "phreeqc.dat") != 0) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }

  if (SetSelectedOutputFileOn(id, TRUE) != IPQ_OK) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }

  if (SetBasicCallback(id, MyCallback, &mydata) != IPQ_OK) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }

  if (RunString(id, input) != 0) {
    OutputErrorString(id);
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
