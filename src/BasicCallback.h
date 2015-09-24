#ifndef _INC_BASIC_CALLBACK_H
#define _INC_BASIC_CALLBACK_H

class BasicCallback
{
public:
	virtual ~BasicCallback() {}
	virtual double Callback(double x1, double x2, const char * str) { return 0.0; }
};


#endif  /* _INC_BASIC_CALLBACK_H */