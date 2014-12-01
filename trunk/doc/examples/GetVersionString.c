#include <stdlib.h>
#include <stdio.h>
#include <IPhreeqc.h>

int main(void)
{
  printf("Version:\n");
  printf("%s\n", GetVersionString());
  
  return EXIT_SUCCESS;
}
