#ifndef __IPHREEQC_LIB_H
#define __IPHREEQC_LIB_H

#include <map>

#include "Var.h"

/*! \brief Enumeration used to return error codes.
*/
typedef enum {
	IPL_OK            =  0,
	IPL_OUTOFMEMORY   = -1,
	IPL_BADINSTANCE   = -2,
	IPL_BADVARTYPE    = -3,
	IPL_INVALIDARG    = -4,
	IPL_INVALIDROW    = -5,
	IPL_INVALIDCOL    = -6,
} IPL_RESULT;


class IPhreeqc2;

#if defined(__cplusplus)
extern "C" {
#endif

	int AddErrorM(int id, const char* error_msg);

	int CreateIPhreeqc(void);

	IPL_RESULT DestroyIPhreeqc(int id);

	int LoadDatabaseM(int id, const char* filename);

	int LoadDatabaseStringM(int id, const char* input);

	int UnLoadDatabaseM(int id);

	const char* GetLastErrorStringM(int id);

	const char* GetDumpStringM(int id);

	int GetDumpLineCountM(int id);

	const char* GetDumpLineM(int id, int n);

	IPL_RESULT AccumulateLineM(int id, const char *line);

	int GetSelectedOutputOnM(int id);
	IPL_RESULT SetSelectedOutputOnM(int id, int value);

	int GetOutputOnM(int id);
	IPL_RESULT SetOutputOnM(int id, int value);

	int GetErrorOnM(int id);
	IPL_RESULT SetErrorOnM(int id, int value);

	int GetLogOnM(int id);
	IPL_RESULT SetLogOnM(int id, int value);

	int GetDumpOnM(int id);
	IPL_RESULT SetDumpOnM(int id, int value);

	int GetDumpStringOnM(int id);
	IPL_RESULT SetDumpStringOnM(int id, int value);

	int RunAccumulatedM(int id);

	int RunFileM(int id, const char* filename);

	int GetSelectedOutputRowCountM(int id);
	int GetSelectedOutputColumnCountM(int id);

	IPL_RESULT GetSelectedOutputValueM(int id, int row, int col, VAR* pVAR);

	int GetComponentCountM(int id);
	const char* GetComponentM(int id, int n);


#if defined(__cplusplus)
}
#endif

class IPhreeqcLib
{
public:
	static int CreateIPhreeqc(void);
	static IPL_RESULT DestroyIPhreeqc(int n);
	static IPhreeqc2* GetInstance(int n);

private:
	static std::map<size_t, IPhreeqc2*> Instances;
	static size_t InstancesIndex;
};

#endif // __IPHREEQC_LIB_H