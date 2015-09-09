%module iphreeqc
%{
#include "IPhreeqc.hpp"
%}

/*
%include "exception.i"
*/
%include "std_except.i"

/**
int         CreateIPhreeqc(void);
int         DestroyIPhreeqc(int id);
**/

%include "IPhreeqc.hpp"

