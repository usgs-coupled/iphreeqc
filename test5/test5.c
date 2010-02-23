#include <stdio.h>

#include <IPhreeqc.h>

int main(void)
{
  int i;
  int r;
  for (i=0; i < 100; ++i) {
    printf("i=%d\n", i);
    r = LoadDatabase("phreeqc.dat");
    /*UnLoadDatabase();*/
  }
  return 0;  
}
