#include <assert.h>
#if !defined(PHREEQC_CLASS)
#define EXTERNAL extern
#include "global.h"
#else
#include "Phreeqc.h"
#endif
#include <setjmp.h>
#include "output.h"
#include "phrqproto.h"
#include "phqalloc.h"
static char const svnid[] =
	"$Id$";
#if !defined(PHREEQC_CLASS)
#define MAX_CALLBACKS 10
static struct output_callback output_callbacks[MAX_CALLBACKS];
static size_t count_output_callback = 0;
static int forward_output_to_log = 0;
#endif

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
add_output_callback(PFN_OUTPUT_CALLBACK pfn, void *cookie)
/* ---------------------------------------------------------------------- */
{
	if (svnid == NULL)
		fprintf(stderr, " ");
	if (pfn)
	{
		if (count_output_callback >= MAX_CALLBACKS - 1)
		{
			sprintf(error_string, "Too many callbacks.\nSee %s\n", __FILE__);
			fprintf(stderr, "%s", error_string);
			error_msg(error_string, STOP);
			return ERROR;
		}
		output_callbacks[count_output_callback].callback = pfn;
		output_callbacks[count_output_callback].cookie = cookie;
		++count_output_callback;
	}
	return OK;
}

#ifdef SKIP_OUTPUT_MESSAGE
/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_message(const int type, const char *err_str, const int stop,
			   const char *format, va_list args)
/* ---------------------------------------------------------------------- */
{
#if !defined(PHREEQC_CLASS)
	extern jmp_buf mark;
#endif
	size_t i;

	for (i = 0; i < count_output_callback; ++i)
	{
#ifdef VACOPY
		va_list args_copy;
		va_copy(args_copy, args);
		(output_callbacks[i].callback) (ACTION_OUTPUT, type, err_str, stop,
										output_callbacks[i].cookie, format,
										args_copy);
		va_end(args_copy);
#else
		(output_callbacks[i].callback) (ACTION_OUTPUT, type, err_str, stop,
										output_callbacks[i].cookie, format,
										args);
#endif
	}

	if (stop == STOP)
	{
		longjmp(mark, input_error);
	}
	return OK;
}
#endif

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
clean_up_output_callbacks(void)
/* ---------------------------------------------------------------------- */
{
	count_output_callback = 0;
	return OK;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
error_msg(const char *err_str, const int stop, ...)
/* ---------------------------------------------------------------------- */
{
#if !defined(PHREEQC_CLASS)
	extern jmp_buf mark;
#endif
	size_t i;

	if (input_error <= 0)
		input_error = 1;

	for (i = 0; i < count_output_callback; ++i)
	{
		va_list args;
		va_start(args, stop);
		(output_callbacks[i].callback) (ACTION_OUTPUT, OUTPUT_ERROR, err_str, stop,
										output_callbacks[i].cookie, "",
										args);
		va_end(args);
	}
	if (stop == STOP)
	{
		longjmp(mark, input_error);
	}
	return OK;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
warning_msg(const char *err_str, ...)
/* ---------------------------------------------------------------------- */
{
	size_t i;
	for (i = 0; i < count_output_callback; ++i)
	{
		va_list args;
		va_start(args, err_str);
		(output_callbacks[i].callback) (ACTION_OUTPUT, OUTPUT_WARNING, err_str, CONTINUE,
										output_callbacks[i].cookie, "",
										args);
		va_end(args);
	}
	count_warnings++;
	return OK;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_msg(const int type, const char *format, ...)
/* ---------------------------------------------------------------------- */
{
	size_t i;
	for (i = 0; i < count_output_callback; ++i)
	{
		va_list args1;
		va_start(args1, format);
		(output_callbacks[i].callback) (ACTION_OUTPUT, type, NULL, CONTINUE,
										output_callbacks[i].cookie, format,
										args1);
		va_end(args1);

		if (phast == TRUE)
		{
			if (type == OUTPUT_CHECKLINE && pr.echo_input == TRUE && phreeqc_mpi_myself == 0)
			{
				va_list args2;
				va_start(args2, format);
				(output_callbacks[i].callback) (ACTION_OUTPUT, OUTPUT_ECHO, NULL, CONTINUE,
												output_callbacks[i].cookie, format,
												args2);
				va_end(args2);
			}
		}
	}
	return OK;
}

/* ---------------------------------------------------------------------- */
void CLASS_QUALIFIER
set_forward_output_to_log(int value)
/* ---------------------------------------------------------------------- */
{
	forward_output_to_log = value;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
get_forward_output_to_log(void)
/* ---------------------------------------------------------------------- */
{
	return forward_output_to_log;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_fflush(const int type, ...)
/* ---------------------------------------------------------------------- */
{
	size_t i;
	int check;

	check = OK;
	for (i = 0; i < count_output_callback; ++i)
	{
		va_list args;
		va_start(args, type);
		check =
			(output_callbacks[i].callback) (ACTION_FLUSH, type, NULL,
											CONTINUE,
											output_callbacks[i].cookie, NULL,
											args);
		va_end(args);
		if (check != OK)
			break;
	}
	if (check != OK)
		return (ERROR);
	return (OK);
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_rewind(const int type, ...)
/* ---------------------------------------------------------------------- */
{
	size_t i;
	int check;

	check = OK;
	for (i = 0; i < count_output_callback; ++i)
	{
		va_list args;
		va_start(args, type);
		check =
			(output_callbacks[i].callback) (ACTION_REWIND, type, NULL,
											CONTINUE,
											output_callbacks[i].cookie, NULL,
											args);
		va_end(args);
		if (check != OK)
			break;
	}
	if (check != OK)
		return (ERROR);
	return (OK);
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_close(const int type, ...)
/* ---------------------------------------------------------------------- */
{
	size_t i;
	int check;

	check = OK;
	for (i = 0; i < count_output_callback; ++i)
	{
		va_list args;
		va_start(args, type);
		check =
			(output_callbacks[i].callback) (ACTION_CLOSE, type, NULL,
											CONTINUE,
											output_callbacks[i].cookie, NULL,
											args);
		va_end(args);
		if (check != OK)
			break;
	}
	if (check != OK)
		return (ERROR);
	return (OK);
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_open(const int type, const char *file_name, ...)
/* ---------------------------------------------------------------------- */
{
	size_t i;
	int check;
	assert(file_name && strlen(file_name));

	check = OK;
	for (i = 0; i < count_output_callback; ++i)
	{
		va_list args;
		va_start(args, file_name);
		check =
			(output_callbacks[i].callback) (ACTION_OPEN, type, file_name,
											CONTINUE,
											output_callbacks[i].cookie, NULL,
											args);
		va_end(args);
		if (check != OK)
			break;
	}
	if (check != OK)
		return (ERROR);
	return (OK);
}

#if defined(HDF5_CREATE)
extern void HDFWriteHyperSlabV(const char *name, const char *format,
							   va_list argptr);
#endif

#if defined(USE_MPI) && defined(HDF5_CREATE) && defined(MERGE_FILES)
extern int Merge_fpunchf(const int length, const char *format,
						 va_list argptr);
#endif

int CLASS_QUALIFIER
fpunchf(const char *name, const char *format, ...)
{
	size_t i;
	for (i = 0; i < count_output_callback; ++i)
	{
		va_list args;
		va_start(args, format);
		(output_callbacks[i].callback) (ACTION_OUTPUT, OUTPUT_PUNCH, name, CONTINUE,
										output_callbacks[i].cookie, format,
										args);
		va_end(args);
	}
	return OK;
}

int CLASS_QUALIFIER
fpunchf_user(int user_index, const char *format, ...)
{
	static int s_warning = 0;
	size_t i;
	static char buffer[80];
	char *name;

	if (user_index < user_punch_count_headings)
	{
		name = user_punch_headings[user_index];
	}
	else
	{
		if (s_warning == 0)
		{
			sprintf(error_string,
					"USER_PUNCH: Headings count doesn't match number of calls to PUNCH.\n");
			warning_msg(error_string);
			s_warning = 1;
		}
		sprintf(buffer, "no_heading_%d",
				(user_index - user_punch_count_headings) + 1);
		name = buffer;
	}

	for (i = 0; i < count_output_callback; ++i)
	{
		va_list args;
		va_start(args, format);
		(output_callbacks[i].callback) (ACTION_OUTPUT, OUTPUT_PUNCH, name, CONTINUE,
										output_callbacks[i].cookie, format,
										args);
		va_end(args);
	}
	return OK;
}

int CLASS_QUALIFIER
fpunchf_end_row(const char *format, ...)
{
	size_t i;
	for (i = 0; i < count_output_callback; ++i)
	{
		va_list args;
		va_start(args, format);
		(output_callbacks[i].callback) (ACTION_OUTPUT, OUTPUT_PUNCH_END_ROW, "", CONTINUE,
										output_callbacks[i].cookie, format,
										args);
		va_end(args);
	}
	return OK;
}
