#ifdef SWIGJAVA
%module iphreeqc_java
#elif SWIGRUBY
%module iphreeqc_ruby
#else
%module iphreeqc
#endif
%{
#include "IPhreeqc.hpp"
#include "Var.h"
%}

%include "std_except.i"
%include "IPhreeqc.hpp"
%include "Var.h"
