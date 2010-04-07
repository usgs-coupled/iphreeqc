#ifndef _INC_IPHREEQC2_HPP
#define _INC_IPHREEQC2_HPP

#include <exception>
#include <list>
#include <vector>
#include <cstdarg>
#include "IPhreeqcCallbacks.h"      /* PFN_PRERUN_CALLBACK, PFN_POSTRUN_CALLBACK, PFN_CATCH_CALLBACK */
#include "Var.h"                    /* VRESULT */

class Phreeqc;
class IErrorReporter;
class CSelectedOutput;

class IPhreeqcStop : std::exception
{
};

class IPhreeqc2
{
public:
	IPhreeqc2(void);
	~IPhreeqc2(void);

public:

	const char* GetLastErrorString(void);
	const char* GetLastWarningString(void);
	const char* GetDumpString(void);

	int LoadDatabase(const char* filename);
	int LoadDatabaseString(const char* input);

	void UnLoadDatabase(void);

	size_t AddError(const char* error_msg);
	size_t AddWarning(const char* warning_msg);

	int RunAccumulated(void);
	int RunFile(const char* filename);
	int RunString(const char* input);

	int GetSelectedOutputRowCount(void)const;
	int GetSelectedOutputColumnCount(void)const;
	VRESULT GetSelectedOutputValue(int row, int col, VAR* pVAR);

	void OutputLastError(void);

	void OutputLines(void);
	const std::string& GetAccumulatedLines(void);
	void ClearAccumulatedLines(void);
	VRESULT AccumulateLine(const char *line);

	int GetDumpLineCount(void)const;
	const char* GetDumpLine(int n);

	int GetErrorLineCount(void)const;
	const char* GetErrorLine(int n);

	std::list< std::string > ListComponents(void);
	size_t GetComponentCount(void);
	const char* GetComponent(int n);

	bool GetDumpOn(void)const;
	void SetDumpOn(bool bValue);

	bool GetDumpStringOn(void)const;
	void SetDumpStringOn(bool bValue);

	bool GetErrorOn(void)const;
	void SetErrorOn(bool bValue);

	bool GetLogOn(void)const;
	void SetLogOn(bool bValue);

	bool GetOutputOn(void)const;
	void SetOutputOn(bool bValue);

	bool GetSelectedOutputOn(void)const;
	void SetSelectedOutputOn(bool bValue);

protected:
	static int handler(const int action, const int type, const char *err_str, const int stop, void *cookie, const char *format, va_list args);
	int output_handler(const int type, const char *err_str, const int stop, void *cookie, const char *format, va_list args);
	int open_handler(const int type, const char *file_name);

	static int module_handler(const int action, const int type, const char *err_str, const int stop, void *cookie, const char *format, va_list args);
	int module_isopen_handler(const int type);
	int module_open_handler(const int type, const char *file_name);

	int output_isopen(const int type);

	int EndRow(void);
	void AddSelectedOutput(const char* name, const char* format, va_list argptr);

	void check_database(const char* sz_routine);
	void do_run(const char* sz_routine, std::istream* pis, FILE* fp, PFN_PRERUN_CALLBACK pfn_pre, PFN_POSTRUN_CALLBACK pfn_post, void *cookie);

	void update_errors(void);

protected:
	bool                       DatabaseLoaded;
	bool                       SelectedOutputOn;
	bool                       OutputOn;
	bool                       LogOn;
	bool                       ErrorOn;
	bool                       DumpOn;
	bool                       DumpStringOn;

	IErrorReporter            *ErrorReporter;
	std::string                LastErrorString;
	std::vector< std::string > ErrorLines;

	IErrorReporter            *WarningReporter;
	std::string                LastWarningString;
	std::vector< std::string > WarningLines;

	CSelectedOutput           *SelectedOutput;
	std::string                PunchFileName;
	std::string                StringInput;

	std::string                DumpString;
	std::vector< std::string > DumpLines;

	std::list< std::string >   Components;

private:
#if defined(CPPUNIT)
	friend class TestIPhreeqc;
	friend class TestSelectedOutput;
#endif
	Phreeqc* PhreeqcPtr;
};

#endif // _INC_IPHREEQC2_HPP