#include <cstdlib>
#include <iostream>
#include <IPhreeqc.hpp>

template <class TClass> class TTestGetSet
{
private:
  bool (TClass::*_get)(void)const;
  void (TClass::*_set)(bool);
  TClass* _p;
public:
  TTestGetSet(TClass* p, bool(TClass::*get)(void)const, void(TClass::*set)(bool))
    {
      _p = p;
      _get = get;
      _set = set;
    }
  int Test(void)
    {
      if ((*_p.*_get)())
      {
        return EXIT_FAILURE;
      }

      (*_p.*_set)(true);
      
      if (!(*_p.*_get)())
      {
        return EXIT_FAILURE;
      }
      
      (*_p.*_set)(false);
      
      if ((*_p.*_get)())
      {
        return EXIT_FAILURE;
      }
      
      return 0;
    }
};

int
main(int argc, const char* argv[])
{
  IPhreeqc iphreeqc;
  
  // Dump
  TTestGetSet<IPhreeqc> testDump(&iphreeqc, &IPhreeqc::GetDumpOn, &IPhreeqc::SetDumpOn);
  if (testDump.Test() != 0)
  {
    return EXIT_FAILURE;
  }
  
  // Dump string
  TTestGetSet<IPhreeqc> testDumpString(&iphreeqc, &IPhreeqc::GetDumpStringOn, &IPhreeqc::SetDumpStringOn);
  if (testDumpString.Test() != 0)
  {
    return EXIT_FAILURE;
  }
  
  // Error
  TTestGetSet<IPhreeqc> testError(&iphreeqc, &IPhreeqc::GetErrorOn, &IPhreeqc::SetErrorOn);
  if (testError.Test() != 0)
  {
    return EXIT_FAILURE;
  }
  
  // Log
  TTestGetSet<IPhreeqc> testLog(&iphreeqc, &IPhreeqc::GetLogOn, &IPhreeqc::SetLogOn);
  if (testLog.Test() != 0)
  {
    return EXIT_FAILURE;
  }
  
  // Output
  TTestGetSet<IPhreeqc> testOutput(&iphreeqc, &IPhreeqc::GetOutputOn, &IPhreeqc::SetOutputOn);
  if (testOutput.Test() != 0)
  {
    return EXIT_FAILURE;
  }
  
  // Selected output
  TTestGetSet<IPhreeqc> testSelectedOutput(&iphreeqc, &IPhreeqc::GetSelectedOutputOn, &IPhreeqc::SetSelectedOutputOn);
  if (testSelectedOutput.Test() != 0)
  {
    return EXIT_FAILURE;
  }
  
  if (iphreeqc.LoadDatabase("phreeqc.dat") != 0)
  {
    std::cout << iphreeqc.GetErrorString();
    return EXIT_FAILURE;
  }
  
  if (iphreeqc.RunFile("ex1") != 0)
  {
    std::cout << iphreeqc.GetErrorString();
    return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}
