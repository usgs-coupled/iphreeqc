#ifdef SWIGJAVA
%module(directors="1") iphreeqc_java
#elif SWIGRUBY
%module(directors="1") iphreeqc_ruby
#else
%module(directors="1") iphreeqc
#endif

%{
#include "IPhreeqc.hpp"
#include "Var.h"
#include "BasicCallback.h"
%}

%feature("director") BasicCallback;

%include "stl.i"
%include "std_except.i"

/* ignore callbacks */
%ignore SetBasicFortranCallback;

/* unnecessary PHRQ_io routines */
%ignore fpunchf_end_row;
%ignore error_msg;
%ignore fpunchf;
%ignore log_msg;
%ignore output_msg;
%ignore output_open;
%ignore punch_msg;
%ignore punch_open;
%ignore screen_msg;
%ignore warning_msg;

%include "IPhreeqc.hpp"
%include "Var.h"
%include "BasicCallback.h"
