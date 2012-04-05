#define EXTERNAL
#include "global.h"
#include "output.h"
#include "phrqproto.h"
#include "input.h"

/*#define PHREEQC_XML*/
#ifdef PHREEQC_XML
#include "SAXPhreeqc.h"
extern void SAX_cleanup(void);
#endif

static char const svnid[] = "$Id$";

#ifdef DOS
static int write_banner(void);
#endif

/* ----------------------------------------------------------------------
 *   MAIN
 * ---------------------------------------------------------------------- */
int
main(int argc, char *argv[])
/*
 *   Main program for PHREEQC
 */
{

	int errors;
	void *db_cookie = NULL;
	void *input_cookie = NULL;
#if defined(WIN32_MEMORY_DEBUG)
	int tmpDbgFlag;

	/*
	 * Set the debug-heap flag to keep freed blocks in the
	 * heap's linked list - This will allow us to catch any
	 * inadvertent use of freed memory
	 */
	tmpDbgFlag = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);
	/*tmpDbgFlag |= _CRTDBG_DELAY_FREE_MEM_DF;*/
	tmpDbgFlag |= _CRTDBG_LEAK_CHECK_DF;
	/*tmpDbgFlag |= _CRTDBG_CHECK_ALWAYS_DF;*/
	_CrtSetDbgFlag(tmpDbgFlag);
	/*_crtBreakAlloc = 9482;*/
#endif
#ifdef SKIP
_clearfp(); /*Always call _clearfp before setting the control */

unsigned int cw = _controlfp(0, 0); /*Get the default control*/

cw &=~(EM_OVERFLOW|EM_UNDERFLOW|EM_ZERODIVIDE|
       EM_DENORMAL|EM_INVALID);

unsigned int cwOriginal = _controlfp(cw, MCW_EM); /*Set it.*/

#endif
	if (svnid == NULL)
		fprintf(stderr, " ");
	phast = FALSE;
/*
 *   Add callbacks for error_msg and warning_msg
 */
	if (add_output_callback(phreeqc_handler, NULL) != OK)
	{
		fprintf(stderr, "ERROR: %s\n",
				"NULL pointer returned from malloc or realloc.");
		fprintf(stderr, "ERROR: %s\n", "Program terminating.");
		return -1;
	}

/*
 *   Open input/output files
 */
	errors = process_file_names(argc, argv, &db_cookie, &input_cookie, TRUE);
	if (errors != 0)
	{
		clean_up();
		return errors;
	}
#ifdef DOS
	write_banner();
#endif

/*
 *   Initialize arrays
 */
	errors = do_initialize();
	if (errors != 0)
	{
		clean_up();
		return errors;
	}

/*
 *   Load database into memory
 */
#if defined(MERGE_INCLUDE_FILES) 
	errors = read_database(istream_getc, db_cookie);
#else
	errors = read_database(getc_callback, db_cookie);
#endif
	if (errors != 0)
	{
		clean_up();
		return errors;
	}

/*
 *   Read input data for simulation
 */

#if defined(MERGE_INCLUDE_FILES) 
	errors = run_simulations(istream_getc, input_cookie);
#else
	errors = run_simulations(getc_callback, input_cookie);
#endif
	if (errors != 0)
	{
		clean_up();
		return errors;
	}

/*
 *   Display successful status
 */
	errors = do_status();
	if (errors != 0)
	{
		clean_up();
		return errors;
	}
#ifdef PHREEQC_XML
	{
		int n;
		SAX_StartSystem();
		for (n = 0; n < count_solution; ++n)
		{
			SAX_AddSolution(solution[n]);
		}
		SAX_EndSystem();
		SAX_UnpackSolutions(SAX_GetXMLStr(), SAX_GetXMLLength());
	}
#endif
	clean_up();
	close_input_files();
	close_output_files();
#ifdef PHREEQC_XML
	SAX_cleanup();
#endif
	return 0;
}

/* ---------------------------------------------------------------------- */
int
write_banner(void)
/* ---------------------------------------------------------------------- */
{
	char buffer[80];
	int len, indent;
	output_msg(OUTPUT_SCREEN,
			   "              ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÛ\n");
	output_msg(OUTPUT_SCREEN,
			   "              º                                            º\n");

	/* version */
	len = sprintf(buffer, "* PHREEQC-%s *", "@VERSION@");
	indent = (44 - len) / 2;
	output_msg(OUTPUT_SCREEN, "%14cº%*c%s%*cº\n", ' ', indent, ' ', buffer,
			   44 - indent - len, ' ');

	output_msg(OUTPUT_SCREEN,
			   "              º                                            º\n");
	output_msg(OUTPUT_SCREEN,
			   "              º      A hydrogeochemical transport model    º\n");
	output_msg(OUTPUT_SCREEN,
			   "              º                                            º\n");
	output_msg(OUTPUT_SCREEN,
			   "              º                    by                      º\n");
	output_msg(OUTPUT_SCREEN,
			   "              º       D.L. Parkhurst and C.A.J. Appelo     º\n");
	output_msg(OUTPUT_SCREEN,
			   "              º                                            º\n");


	/* date */
	len = sprintf(buffer, "%s", "@VER_DATE@");
	indent = (44 - len) / 2;
	output_msg(OUTPUT_SCREEN, "%14cº%*c%s%*cº\n", ' ', indent, ' ', buffer,
			   44 - indent - len, ' ');

	output_msg(OUTPUT_SCREEN,
			   "              ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÛ\n\n");

	return 0;
}
