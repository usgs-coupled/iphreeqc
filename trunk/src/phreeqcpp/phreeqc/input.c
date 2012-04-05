#include <assert.h>
#if !defined(PHREEQC_CLASS)
#define EXTERNAL extern
#include "global.h"
#else
#include "Phreeqc.h"
#endif
#include <setjmp.h>
#include "input.h"
#include "output.h"
#include "phrqproto.h"
#include "phqalloc.h"

static char const svnid[] = "$Id$";

int check_line_return;

#if !defined(PHREEQC_CLASS)
static struct read_callback s_read_callback;
#endif


/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
set_read_callback(PFN_READ_CALLBACK pfn, void *cookie, int database)
/* ---------------------------------------------------------------------- */
{
	if (svnid == NULL)
		fprintf(stderr, " ");

	s_read_callback.cookie = cookie;
	s_read_callback.callback = pfn;
	s_read_callback.database = database;
	return OK;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
reading_database(void)
/* ---------------------------------------------------------------------- */
{
	return s_read_callback.database;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
check_line(const char *string, int allow_empty, int allow_eof,
		   int allow_keyword, int print)
/* ---------------------------------------------------------------------- */
{
	assert(s_read_callback.callback != NULL);
	if (s_read_callback.callback == NULL)
		return EOF;
	if (reading_database())
		print = FALSE;
	return check_line_impl(s_read_callback.callback, s_read_callback.cookie,
						   string, allow_empty, allow_eof, allow_keyword,
						   print);
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
check_line_impl(PFN_READ_CALLBACK pfn, void *cookie, const char *string,
				int allow_empty, int allow_eof, int allow_keyword, int print)
/* ---------------------------------------------------------------------- */
{
/*
 *   Function gets a new line and checks for empty, eof, and keywords.
 *
 *   Arguments:
 *      string        Input, character string used in printing error message
 *      allow_empty   Input, True or false, if a blank line is accepable
 *                       if false, another line is read
 *      allow_eof     Input, True or false, if EOF is acceptable
 *      allow_keyword Input, True or false, if a keyword is acceptable
 *
 *   Returns:
 *      EMPTY         if empty line read and allow_empty == true
 *      KEYWORD       if line begins with keyword
 *      EOF           if eof and allow_eof == true
 *      OK            otherwise
 *      OPTION        if line begins with -[alpha]
 *
 *   Terminates       if EOF and allow_eof == false.
 */
	int i;


/* Get line */
	do
	{
		i = get_line(pfn, cookie);
		if ((print == TRUE && i != EOF) || i == KEYWORD)
		{
			output_msg(OUTPUT_CHECKLINE, "\t%s\n", line_save);
		}
	}
	while (i == EMPTY && allow_empty == FALSE);
/* Check eof */
	if (i == EOF && allow_eof == FALSE)
	{
		sprintf(error_string,
				"Unexpected eof while reading %s\nExecution terminated.\n",
				string);
		error_msg(error_string, STOP);
	}
/* Check keyword */
	if (i == KEYWORD && allow_keyword == FALSE)
	{
		sprintf(error_string,
				"Expected data for %s, but got a keyword ending data block.",
				string);
		error_msg(error_string, CONTINUE);
		input_error++;
	}
	check_line_return = i;
	return (i);
}
#if !defined (MERGE_INCLUDE_FILES)
/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
get_line(PFN_READ_CALLBACK pfn, void *cookie)
/* ---------------------------------------------------------------------- */
{
/*
 *   Read a line from input file put in "line".
 *   Copy of input line is stored in "line_save".
 *   Characters after # are discarded in line but retained in "line_save"
 *
 *   Arguments:
 *      fp is file name
 *   Returns:
 *      EMPTY,
 *      EOF,
 *      KEYWORD,
 *      OK,
 *      OPTION
 */
	int i, j, return_value, empty, l;
	char *ptr;
	char token[MAX_LENGTH];

	return_value = EMPTY;
	while (return_value == EMPTY)
	{
/*
 *   Eliminate all characters after # sign as a comment
 */
		i = -1;
		empty = TRUE;
/*
 *   Get line, check for eof
 */
		if (get_logical_line(pfn, cookie, &l) == EOF)
		{
			next_keyword = 0;
			return (EOF);
		}
/*
 *   Get long lines
 */
		j = l;
		ptr = strchr(line_save, '#');
		if (ptr != NULL)
		{
			j = (int) (ptr - line_save);
		}
		strncpy(line, line_save, (unsigned) j);
		line[j] = '\0';
		for (i = 0; i < j; i++)
		{
			if (!isspace((int) line[i]))
			{
				empty = FALSE;
				break;
			}
		}
/*
 *   New line character encountered
 */

		if (empty == TRUE)
		{
			return_value = EMPTY;
		}
		else
		{
			return_value = OK;
		}
	}
/*
 *   Determine return_value
 */
	if (return_value == OK)
	{
		if (check_key(line) == TRUE)
		{
			return_value = KEYWORD;
		}
		else
		{
			ptr = line;
			copy_token(token, &ptr, &i);
			if (token[0] == '-' && isalpha((int) token[1]))
			{
				return_value = OPTION;
			}
		}
	}

	return (return_value);
}
#else
/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
get_line(PFN_READ_CALLBACK pfn, void *l_cookie)
/* ---------------------------------------------------------------------- */
{
/*
 *   Read a line from input file put in "line".
 *   Copy of input line is stored in "line_save".
 *   Characters after # are discarded in line but retained in "line_save"
 *
 *   Arguments:
 *      fp is file name
 *   Returns:
 *      EMPTY,
 *      EOF,
 *      KEYWORD,
 *      OK,
 *      OPTION
 */
	int i, j, return_value, empty, l;
	char *ptr;
	char token[MAX_LENGTH];
	void *cookie;
	bool continue_loop;

	// loop for include files
	for (;;)
	{
		cookie = this->get_cookie();
		if (cookie == NULL)
		{
			break;
		}
		return_value = EMPTY;
		while (return_value == EMPTY)
		{
			/*
			*   Eliminate all characters after # sign as a comment
			*/
			i = -1;
			empty = TRUE;
			/*
			*   Get line, check for eof
			*/
			continue_loop = false;
			if (get_logical_line(pfn, cookie, &l) == EOF)
			{
					//pop next file
					this->pop_cookie();
					continue_loop = true;
					break;
			}
			/*
			*   Get long lines
			*/
			j = l;
			ptr = strchr(line_save, '#');
			if (ptr != NULL)
			{
				j = (int) (ptr - line_save);
			}
			strncpy(line, line_save, (unsigned) j);
			line[j] = '\0';
			for (i = 0; i < j; i++)
			{
				if (!isspace((int) line[i]))
				{
					empty = FALSE;
					break;
				}
			}
			/*
			*   New line character encountered
			*/

			if (empty == TRUE)
			{
				return_value = EMPTY;
			}
			else
			{
				return_value = OK;
			}
		}
		if (continue_loop) continue;
		/*
		*   Determine return_value
		*/
		if (return_value == OK)
		{
			if (check_key(line) == TRUE)
			{
				return_value = KEYWORD;
			}
			else
			{
				ptr = line;
				copy_token(token, &ptr, &i);
				if (token[0] == '-' && isalpha((int) token[1]))
				{
					return_value = OPTION;
				}
			}
		}
		// add new include file to stack
		ptr = line;
		copy_token(token, &ptr, &i);
		str_tolower(token);
		if ((strstr(token,"include$") == token) || (strstr(token,"include_file") == token))
		{
			char file_name[MAX_LENGTH];
			strcpy(file_name, ptr);
			
			if (string_trim(file_name) != EMPTY)
			{
				std::ifstream *next_stream = new std::ifstream(file_name, std::ifstream::in);
				if (!next_stream->is_open())
				{
					// error opening file
					sprintf(error_string, "Could not open include file %s", file_name);
					error_msg(error_string, STOP);
				}
				this->set_cookie(next_stream);
				continue;
			}
		}
		return (return_value);
	}
	next_keyword = 0;
	return EOF;

}
#endif

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
get_logical_line(PFN_READ_CALLBACK pfn, void *cookie, int *l)
/* ---------------------------------------------------------------------- */
{
/*
 *   Reads file fp until end of line, ";", or eof
 *   stores characters in line_save
 *   reallocs line_save and line if more space is needed
 *
 *   returns:
 *           EOF on empty line on end of file or
 *           OK otherwise
 *           *l returns length of line
 */
	int i, j;
	int pos;
	char c;
	i = 0;
	if (!pfn)
		return EOF;
	while ((j = pfn(cookie)) != EOF)
	{
		c = (char) j;
		if (c == '#')
		{
			/* ignore all chars after # until newline */
			do
			{
				c = (char) j;
				if (c == '\n')
				{
					break;
				}
				add_char_to_line(&i, c);
			}
			while ((j = pfn(cookie)) != EOF);
		}
		if (c == ';')
			break;
		if (c == '\n')
		{
			break;
		}
		if (c == '\\')
		{
			pos = i;
			add_char_to_line(&i, c);
			while ((j = pfn(cookie)) != EOF)
			{
				c = (char) j;
				if (c == '\\')
				{
					pos = i;
					add_char_to_line(&i, c);
					continue;
				}
				if (c == '\n')
				{
					/* remove '\\' */
					for (; pos < i; pos++)
					{
						line_save[pos] = line_save[pos + 1];
					}
					i--;
					break;
				}
				add_char_to_line(&i, c);
				if (!isspace(j))
					break;
			}
		}
		else
		{
			add_char_to_line(&i, c);
		}
	}
	if (j == EOF && i == 0)
	{
		*l = 0;
		line_save[i] = '\0';
		return (EOF);
	}
	line_save[i] = '\0';
	*l = i;
	return (OK);
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
add_char_to_line(int *i, char c)
/* ---------------------------------------------------------------------- */
{
	if (*i + 20 >= max_line)
	{
		max_line *= 2;
		line_save =
			(char *) PHRQ_realloc(line_save,
								  (size_t) max_line * sizeof(char));
		if (line_save == NULL)
			malloc_error();
		line = (char *) PHRQ_realloc(line, (size_t) max_line * sizeof(char));
		if (line == NULL)
			malloc_error();
	}
	line_save[*i] = c;
	*i += 1;
	return (OK);
}
