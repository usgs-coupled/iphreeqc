#if defined(WIN32)
#include <windows.h>
#endif

/* Run-time library for use with "p2c", the Pascal to C translator */

/* "p2c"  Copyright (C) 1989, 1990, 1991 Free Software Foundation.
 * By Dave Gillespie, daveg@csvax.cs.caltech.edu.  Version --VERSION--.
 * This file may be copied, modified, etc. in any way.  It is not restricted
 * by the licence agreement accompanying p2c itself.
 */

#include "p2c.h"

#define STOP 1
#define CONTINUE 0

#define NO_TIME
#ifndef NO_TIME
# include <time.h>
#endif
#include <stdlib.h>

#define Isspace(c)  isspace(c)	/* or "((c) == ' ')" if preferred */

#if !defined(PHREEQC_CLASS)
#define EXTERNAL extern
#include "output.h"
static char const svnid[] =
	"$Id$";
static int P_argc = 0;
static char **P_argv = NULL;
static char *_ShowEscape(char *buf, int code, int ior, char *prefix);

int P_escapecode = 0;
int P_ioresult = 0;

long EXCP_LINE = 0;					/* Used by Pascal workstation system */

Anyptr __MallocTemp__;

__p2c_jmp_buf *__top_jb;
#define CLASS_QUALIFIER
#else
#include "Phreeqc.h"
#include "output.h"
#define CLASS_QUALIFIER Phreeqc::
#endif /* PHREEQC_CLASS */

void CLASS_QUALIFIER
PASCAL_MAIN(int argc, Char **argv)
{
	/*
	if (svnid == NULL)
		fprintf(stderr, " ");
	*/
	P_argc = argc;
	P_argv = argv;
	__top_jb = NULL;
	P_escapecode = 0;
	P_ioresult = 0;
#ifdef LOCAL_INIT
	LOCAL_INIT();
#endif
}





/* In case your system lacks these... */

long CLASS_QUALIFIER
my_labs(long l_x)
{
	return ((l_x > 0) ? l_x : -l_x);
}


/* #define __STDC__  */ /* PHREEQ98 */

Anyptr  CLASS_QUALIFIER
my_memmove(Anyptr d, Const Anyptr l_s, size_t n)
{
	register char *dd = (char *) d, *ss = (char *) l_s;
	if (dd < ss || (unsigned int) (dd - ss) >= n)
	{
		memcpy(dd, ss, n);
	}
	else if (n > 0)
	{
		dd += n;
		ss += n;
		while (n-- > 0)
			*--dd = *--ss;
	}
	return d;
}


Anyptr  CLASS_QUALIFIER
my_memcpy(Anyptr d, Const Anyptr l_s, size_t n)
{
	register char *ss = (char *) l_s, *dd = (char *) d;
	while (n-- > 0)
		*dd++ = *ss++;
	return d;
}

int CLASS_QUALIFIER
my_memcmp(Const Anyptr s1, Const Anyptr s2, size_t n)
{
	register char *a = (char *) s1, *b = (char *) s2;
	register int i;
	while (n-- > 0)
		if ((i = (*a++) - (*b++)) != 0)
			return i;
	return 0;
}

Anyptr CLASS_QUALIFIER
my_memset(Anyptr d, int c, size_t n)
{
	register char *dd = (char *) d;
	while (n-- > 0)
		*dd++ = (char) c;
	return d;
}

int CLASS_QUALIFIER
my_toupper(int c)
{
	if (islower(c))
		return _toupper(c);
	else
		return c;
}


int CLASS_QUALIFIER
my_tolower(int c)
{
	if (isupper(c))
		return _tolower(c);
	else
		return c;
}




long CLASS_QUALIFIER
ipow(long a, long b)
{
	long v;

	if (a == 0 || a == 1)
		return a;
	if (a == -1)
		return (b & 1) ? -1 : 1;
	if (b < 0)
		return 0;
	if (a == 2)
		return 1L << b;
	v = (b & 1) ? a : 1;
	while ((b >>= 1) > 0)
	{
		a *= a;
		if (b & 1)
			v *= a;
	}
	return v;
}




/* Common string functions: */

/* Store in "ret" the substring of length "len" starting from "pos" (1-based).
   Store a shorter or null string if out-of-range.  Return "ret". */
char * CLASS_QUALIFIER
strsub(register char *ret, register char *l_s, register int pos,
	   register int len)
{
	register char *s2;

	if (--pos < 0 || len <= 0)
	{
		*ret = 0;
		return ret;
	}
	while (pos > 0)
	{
		if (!*l_s++)
		{
			*ret = 0;
			return ret;
		}
		pos--;
	}
	s2 = ret;
	while (--len >= 0)
	{
		if (!(*s2++ = *l_s++))
			return ret;
	}
	*s2 = 0;
	return ret;
}


/* Return the index of the first occurrence of "pat" as a substring of "s",
   starting at index "pos" (1-based).  Result is 1-based, 0 if not found. */

int CLASS_QUALIFIER
strpos2(char *l_s, register char *pat, register int pos)
{
	register char *cp, ch;
	register int slen;

	if (--pos < 0)
		return 0;
	slen = (int) strlen(l_s) - pos;
	cp = l_s + pos;
	if (!(ch = *pat++))
		return 0;
	pos = (int) strlen(pat);
	slen -= pos;
	while (--slen >= 0)
	{
		if (*cp++ == ch && !strncmp(cp, pat, pos))
			return (int) (cp - l_s);
	}
	return 0;
}


/* Case-insensitive version of strcmp. */
int CLASS_QUALIFIER
strcicmp(register char *s1, register char *s2)
{
	register unsigned char c1, c2;

	while (*s1)
	{
		if (*s1++ != *s2++)
		{
			if (!s2[-1])
				return 1;
			c1 = (unsigned char) toupper(s1[-1]);
			c2 = (unsigned char) toupper(s2[-1]);
			if (c1 != c2)
				return c1 - c2;
		}
	}
	if (*s2)
		return -1;
	return 0;
}




/* HP and Turbo Pascal string functions: */

/* Trim blanks at left end of string. */
char *  CLASS_QUALIFIER
strltrim(register char *l_s)
{
	while (Isspace((int) *l_s++));
	return l_s - 1;
}


/* Trim blanks at right end of string. */
char * CLASS_QUALIFIER
strrtrim(register char *l_s)
{
	register char *s2 = l_s;

	if (!*l_s)
		return l_s;
	while (*++s2);
	while (s2 > l_s && Isspace((int) *--s2))
		*s2 = 0;
	return l_s;
}


/* Store in "ret" "num" copies of string "s".  Return "ret". */
#ifdef SKIP
char * CLASS_QUALIFIER
strrpt(ret, s, num)
	 char *ret;
	 register char *s;
	 register int num;
{
	register char *s2 = ret;
	register char *s1;

	while (--num >= 0)
	{
		s1 = s;
		while ((*s2++ = *s1++));
		s2--;
	}
	return ret;
}
#endif

/* Store in "ret" string "s" with enough pad chars added to reach "size". */

#ifdef SKIP
char * CLASS_QUALIFIER
strpad(ret, s, padchar, num)
	 char *ret;
	 register char *s;
	 register int padchar, num;
{
	register char *d = ret;

	if (s == d)
	{
		while (*d++);
	}
	else
	{
		while ((*d++ = *s++));
	}
	num -= (--d - ret);
	while (--num >= 0)
		*d++ = (char) padchar;
	*d = 0;
	return ret;
}
#endif

/* Copy the substring of length "len" from index "spos" of "s" (1-based)
   to index "dpos" of "d", lengthening "d" if necessary.  Length and
   indices must be in-range. */
void CLASS_QUALIFIER
strmove(register int len, register char *l_s, register int spos,
		register char *d, register int dpos)
{
	l_s += spos - 1;
	d += dpos - 1;
	while (*d && --len >= 0)
		*d++ = *l_s++;
	if (len > 0)
	{
		while (--len >= 0)
			*d++ = *l_s++;
		*d = 0;
	}
}


/* Delete the substring of length "len" at index "pos" from "s".
   Delete less if out-of-range. */
#ifdef SKIP
void CLASS_QUALIFIER
strdelete(s, pos, len)
	 register char *s;
	 register int pos, len;
{
	register int slen;

	if (--pos < 0)
		return;
	slen = strlen(s) - pos;
	if (slen <= 0)
		return;
	s += pos;
	if (slen <= len)
	{
		*s = 0;
		return;
	}
	while ((*s = s[len]))
		s++;
}
#endif

/* Insert string "src" at index "pos" of "dst". */
void CLASS_QUALIFIER
strinsert(register char *src, register char *dst, register int pos)
{
	register int slen, dlen;

	if (--pos < 0)
		return;
	dlen = (int) strlen(dst);
	dst += dlen;
	dlen -= pos;
	if (dlen <= 0)
	{
		strcpy(dst, src);
		return;
	}
	slen = (int) strlen(src);
	do
	{
		dst[slen] = *dst;
		--dst;
	}
	while (--dlen >= 0);
	dst++;
	while (--slen >= 0)
		*dst++ = *src++;
}




/* File functions */

/* Peek at next character of input stream; return EOF at end-of-file. */
int CLASS_QUALIFIER
P_peek(FILE * f)
{
	int ch;

	ch = getc(f);
	if (ch == EOF)
		return EOF;
	ungetc(ch, f);
	return (ch == '\n') ? ' ' : ch;
}


/* Check if at end of file, using Pascal "eof" semantics.  End-of-file for
   stdin is broken; remove the special case for it to be broken in a
   different way. */
/*int P_eof(FILE *f)*/
int CLASS_QUALIFIER
P_eof(void)
{
#ifdef SKIP
	register int ch;
	if (feof(f))
		return 1;
	if (f == stdin)
		return 0;				/* not safe to look-ahead on the keyboard! */
	ch = getc(f);
	if (ch == EOF)
		return 1;
	ungetc(ch, f);
#endif
	return 0;
}


/* Check if at end of line (or end of entire file). */
int CLASS_QUALIFIER
P_eoln(FILE * f)
{
	register int ch;

	ch = getc(f);
	if (ch == EOF)
		return 1;
	ungetc(ch, f);
	return (ch == '\n');
}


/* Read a packed array of characters from a file. */
void CLASS_QUALIFIER
P_readpaoc(FILE * f, char *l_s, int len)
{
	int ch;

	for (;;)
	{
		if (len <= 0)
			return;
		ch = getc(f);
		if (ch == EOF || ch == '\n')
			break;
		*l_s++ = (char) ch;
		--len;
	}
	while (--len >= 0)
		*l_s++ = ' ';
	if (ch != EOF)
		ungetc(ch, f);
}

void CLASS_QUALIFIER
P_readlnpaoc(FILE * f, char *l_s, int len)
{
	int ch;

	for (;;)
	{
		ch = getc(f);
		if (ch == EOF || ch == '\n')
			break;
		if (len > 0)
		{
			*l_s++ = (char) ch;
			--len;
		}
	}
	while (--len >= 0)
		*l_s++ = ' ';
}


/* Compute maximum legal "seek" index in file (0-based). */
long CLASS_QUALIFIER
P_maxpos(FILE * f)
{
	long savepos = ftell(f);
	long val;

	if (fseek(f, 0L, SEEK_END))
		return -1;
	val = ftell(f);
	if (fseek(f, savepos, SEEK_SET))
		return -1;
	return val;
}


/* Use packed array of char for a file name. */
Char * CLASS_QUALIFIER
P_trimname(register Char * fn, register int len)
{
	static Char fnbuf[256];
	register Char *cp = fnbuf;

	while (--len >= 0 && *fn && !isspace((int) *fn))
		*cp++ = *fn++;
	*cp = 0;
	return fnbuf;
}




/* Pascal's "memavail" doesn't make much sense in Unix with virtual memory.
   We fix memory size as 10Meg as a reasonable compromise. */

long CLASS_QUALIFIER
memavail(void)
{
	return 10000000;			/* worry about this later! */
}

long  CLASS_QUALIFIER
maxavail(void)
{
	return memavail();
}




/* Sets are stored as an array of longs.  S[0] is the size of the set;
   S[N] is the N'th 32-bit chunk of the set.  S[0] equals the maximum
   I such that S[I] is nonzero.  S[0] is zero for an empty set.  Within
   each long, bits are packed from lsb to msb.  The first bit of the
   set is the element with ordinal value 0.  (Thus, for a "set of 5..99",
   the lowest five bits of the first long are unused and always zero.) */

/* (Sets with 32 or fewer elements are normally stored as plain longs.) */
long * CLASS_QUALIFIER
P_setunion(register long *d, register long *s1, register long *s2)	/* d := s1 + s2 */
{
	long *dbase = d++;
	register int sz1 = *s1++, sz2 = *s2++;
	while (sz1 > 0 && sz2 > 0)
	{
		*d++ = *s1++ | *s2++;
		sz1--, sz2--;
	}
	while (--sz1 >= 0)
		*d++ = *s1++;
	while (--sz2 >= 0)
		*d++ = *s2++;
	*dbase = (int) (d - dbase - 1);
	return dbase;
}

long * CLASS_QUALIFIER
P_setint(register long *d, register long *s1, register long *s2)	/* d := s1 * s2 */
{
	long *dbase = d++;
	register int sz1 = *s1++, sz2 = *s2++;
	while (--sz1 >= 0 && --sz2 >= 0)
		*d++ = *s1++ & *s2++;
	while (--d > dbase && !*d);
	*dbase = (int) (d - dbase);
	return dbase;
}

long * CLASS_QUALIFIER
P_setdiff(register long *d, register long *s1, register long *s2)	/* d := s1 - s2 */
{
	long *dbase = d++;
	register int sz1 = *s1++, sz2 = *s2++;
	while (--sz1 >= 0 && --sz2 >= 0)
		*d++ = *s1++ & ~*s2++;
	if (sz1 >= 0)
	{
		while (sz1-- >= 0)
			*d++ = *s1++;
	}
	while (--d > dbase && !*d);
	*dbase = (int) (d - dbase);
	return dbase;
}

long * CLASS_QUALIFIER
P_setxor(register long *d, register long *s1, register long *s2)	/* d := s1 / s2 */
{
	long *dbase = d++;
	register int sz1 = *s1++, sz2 = *s2++;
	while (sz1 > 0 && sz2 > 0)
	{
		*d++ = *s1++ ^ *s2++;
		sz1--, sz2--;
	}
	while (--sz1 >= 0)
		*d++ = *s1++;
	while (--sz2 >= 0)
		*d++ = *s2++;
	while (--d > dbase && !*d);
	*dbase = (int) (d - dbase);
	return dbase;
}

#ifdef SKIP
int CLASS_QUALIFIER
P_inset(register unsigned val, register long *s)	/* val IN s */
{
	register int bit;
	bit = val % SETBITS;
	val /= SETBITS;
	if ((long) val < (*s++ && ((1L << bit) & s[val])))
		return 1;
	return 0;
}
#endif
long * CLASS_QUALIFIER
P_addset(register long *l_s, register unsigned val)	/* s := s + [val] */
{
	register long *sbase = l_s;
	register int bit, size;
	bit = val % SETBITS;
	val /= SETBITS;
	size = *l_s;
	if ((long) ++val > size)
	{
		l_s += size;
		while ((long) val > size)
			*++l_s = 0, size++;
		*sbase = size;
	}
	else
		l_s += val;
	*l_s |= 1L << bit;
	return sbase;
}

long * CLASS_QUALIFIER
P_addsetr(register long *l_s, register unsigned v1, register unsigned v2)	/* s := s + [v1..v2] */
{
	register long *sbase = l_s;
	register int b1, b2, size;
	if ((int) v1 > (int) v2)
		return sbase;
	b1 = v1 % SETBITS;
	v1 /= SETBITS;
	b2 = v2 % SETBITS;
	v2 /= SETBITS;
	size = *l_s;
	v1++;
	if ((int) ++v2 > size)
	{
		while ((int) v2 > size)
			l_s[++size] = 0;
		l_s[v2] = 0;
		*l_s = v2;
	}
	l_s += v1;
	if (v1 == v2)
	{
		*l_s |= (~((-2L) << (b2 - b1))) << b1;
	}
	else
	{
		*l_s++ |= (-1L) << b1;
		while (++v1 < v2)
			*l_s++ = -1;
		*l_s |= ~((-2L) << b2);
	}
	return sbase;
}

long *  CLASS_QUALIFIER
P_remset(register long *l_s, register unsigned val)	/* s := s - [val] */
{
	register int bit;
	bit = val % SETBITS;
	val /= SETBITS;
	if ((long) ++val <= *l_s)
	{
		if (!(l_s[val] &= ~(1L << bit)))
			while (*l_s && !l_s[*l_s])
				(*l_s)--;
	}
	return l_s;
}

int CLASS_QUALIFIER
P_setequal(register long *s1, register long *s2)	/* s1 = s2 */
{
	register int size = *s1++;
	if (*s2++ != size)
		return 0;
	while (--size >= 0)
	{
		if (*s1++ != *s2++)
			return 0;
	}
	return 1;
}

int CLASS_QUALIFIER
P_subset(register long *s1, register long *s2)	/* s1 <= s2 */
{
	register int sz1 = *s1++, sz2 = *s2++;
	if (sz1 > sz2)
		return 0;
	while (--sz1 >= 0)
	{
		if (*s1++ & ~*s2++)
			return 0;
	}
	return 1;
}

long * CLASS_QUALIFIER
P_setcpy(register long *d, register long *l_s)	/* d := s */
{
	register long *save_d = d;

#ifdef SETCPY_MEMCPY
	memcpy(d, l_s, (*l_s + 1) * sizeof(long));
#else
	register int i = *l_s + 1;
	while (--i >= 0)
		*d++ = *l_s++;
#endif
	return save_d;
}


/* s is a "smallset", i.e., a 32-bit or less set stored
   directly in a long. */
long * CLASS_QUALIFIER
P_expset(register long *d, register long l_s)	/* d := s */
{
	if (l_s)
	{
		d[1] = l_s;
		*d = 1;
	}
	else
		*d = 0;
	return d;
}

long CLASS_QUALIFIER
P_packset(register long *l_s)		/* convert s to a small-set */
{
	if (*l_s++)
		return *l_s;
	else
		return 0;
}



#ifdef SKIP

/* Oregon Software Pascal extensions, courtesy of William Bader */
int CLASS_QUALIFIER
P_getcmdline(int l, int h, Char * line)
{
	int i, len;
	char *s;

	h = h - l + 1;
	len = 0;
	for (i = 1; i < P_argc; i++)
	{
		s = P_argv[i];
		while (*s)
		{
			if (len >= h)
				return len;
			line[len++] = *s++;
		}
		if (len >= h)
			return len;
		line[len++] = ' ';
	}
	return len;
}
#endif
#ifndef NO_TIME
void CLASS_QUALIFIER
TimeStamp(Day, Month, Year, Hour, Min, Sec)
	 int *Day, *Month, *Year, *Hour, *Min, *Sec;
{
	struct tm *tm;
	long clock;

	time(&clock);
	tm = localtime(&clock);
	*Day = tm->tm_mday;
	*Month = tm->tm_mon + 1;	/* Jan = 0 */
	*Year = tm->tm_year;
	if (*Year < 1900)
		*Year += 1900;			/* year since 1900 */
	*Hour = tm->tm_hour;
	*Min = tm->tm_min;
	*Sec = tm->tm_sec;
}

void CLASS_QUALIFIER
VAXdate(s)
	 char *s;
{
	long clock;
	char *c;
	int i;
	static int where[] = { 8, 9, 0, 4, 5, 6, 0, 20, 21, 22, 23 };

	time(&clock);
	c = ctime(&clock);
	for (i = 0; i < 11; i++)
		s[i] = my_toupper(c[where[i]]);
	s[2] = '-';
	s[6] = '-';
}

void CLASS_QUALIFIER
VAXtime(s)
	 char *s;
{
	long clock;
	char *c;
	int i;

	time(&clock);
	c = ctime(&clock);
	for (i = 0; i < 8; i++)
		s[i] = c[i + 11];
	s[8] = '.';
	s[9] = '0';
	s[10] = '0';
}
#endif



#ifdef SKIP
/* SUN Berkeley Pascal extensions */
void CLASS_QUALIFIER
P_sun_argv(register char *s, register int len, register int n)
{
	register char *cp;

	if (n < P_argc)
		cp = P_argv[n];
	else
		cp = "";
	while (*cp && --len >= 0)
		*s++ = *cp++;
	while (--len >= 0)
		*s++ = ' ';
}
#endif



int CLASS_QUALIFIER
_OutMem(void)
{
	return _Escape(-2);
}

int  CLASS_QUALIFIER
_CaseCheck(void)
{
	return _Escape(-9);
}

int  CLASS_QUALIFIER
_NilCheck(void)
{
	return _Escape(-3);
}





/* The following is suitable for the HP Pascal operating system.
   It might want to be revised when emulating another system. */

#ifdef SKIP
static char * CLASS_QUALIFIER
_ShowEscape(buf, code, ior, prefix)
	 char *buf, *prefix;
	 int code, ior;
#endif
char * CLASS_QUALIFIER
_ShowEscape(char *buf, int code, int ior, char *prefix)
{
	char *bufp;

	if (prefix && *prefix)
	{
		strcpy(buf, prefix);
		strcat(buf, ": ");
		bufp = buf + strlen(buf);
	}
	else
	{
		bufp = buf;
	}
	if (code == -10)
	{
		sprintf(bufp, "Pascal system I/O error %d", ior);
		switch (ior)
		{
		case 3:
			strcat(buf, " (illegal I/O request)");
			break;
		case 7:
			strcat(buf, " (bad file name)");
			break;
		case FileNotFound:		/*10 */
			strcat(buf, " (file not found)");
			break;
		case FileNotOpen:		/*13 */
			strcat(buf, " (file not open)");
			break;
		case BadInputFormat:	/*14 */
			strcat(buf, " (bad input format)");
			break;
		case 24:
			strcat(buf, " (not open for reading)");
			break;
		case 25:
			strcat(buf, " (not open for writing)");
			break;
		case 26:
			strcat(buf, " (not open for direct access)");
			break;
		case 28:
			strcat(buf, " (string subscript out of range)");
			break;
		case EndOfFile:		/*30 */
			strcat(buf, " (end-of-file)");
			break;
		case FileWriteError:	/*38 */
			strcat(buf, " (file write error)");
			break;
		}
	}
	else
	{
		sprintf(bufp, "Pascal system error %d", code);
		switch (code)
		{
		case -2:
			strcat(buf, " (out of memory)");
			break;
		case -3:
			strcat(buf, " (reference to NIL pointer)");
			break;
		case -4:
			strcat(buf, " (integer overflow)");
			break;
		case -5:
			strcat(buf, " (divide by zero)");
			break;
		case -6:
			strcat(buf, " (real math overflow)");
			break;
		case -8:
			strcat(buf, " (value range error)");
			break;
		case -9:
			strcat(buf, " (CASE value range error)");
			break;
		case -12:
			strcat(buf, " (bus error)");
			break;
		case -20:
			strcat(buf, " (stopped by user)");
			break;
		}
	}
	return buf;
}

int CLASS_QUALIFIER
_Escape(int code)
{
	char l_buf[100];
	char token[200], empty[2] = { "\0" };

	P_escapecode = code;
	if (__top_jb)
	{
		__p2c_jmp_buf *jb = __top_jb;
		__top_jb = jb->next;
		longjmp(jb->jbuf, 1);
	}
	if (code == 0)
		/*        exit(EXIT_SUCCESS); */
		error_msg("Exit success in Basic", STOP);
	if (code == -1)
	{
		error_msg("Fatal error in Basic interpreter.", CONTINUE);
		sprintf(token, "%s",
				_ShowEscape(l_buf, P_escapecode, P_ioresult, empty));
		error_msg(token, STOP);
		exit(EXIT_FAILURE);
	}
	/* fprintf(stderr, "%s\n", _ShowEscape(l_buf, P_escapecode, P_ioresult, "")); */
	/* exit(EXIT_FAILURE); */
	error_msg("Fatal error in Basic interpreter.", CONTINUE);
	sprintf(token, "%s", _ShowEscape(l_buf, P_escapecode, P_ioresult, empty));
	error_msg(token, STOP);
	return (1);
}

int CLASS_QUALIFIER
_EscIO(int code)
{
	P_ioresult = code;
	return _Escape(-10);
}




/* End. */
