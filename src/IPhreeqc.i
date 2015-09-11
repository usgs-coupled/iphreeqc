#ifdef SWIGJAVA
%module iphreeqc_java
#else
%module iphreeqc
#endif
%{
#include "IPhreeqc.hpp"
%}

%include "std_except.i"
%include "IPhreeqc.hpp"

