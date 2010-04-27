#define F_MAIN FC_FUNC(f_main, F_MAIN)

extern "C" void F_MAIN();

int main(void)
{
  F_MAIN();
  return 0;
}
