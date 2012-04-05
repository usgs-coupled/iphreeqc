#ifndef _INC_BASIC_H
#define _INC_BASIC_H

#define forloop         0
#define whileloop       1
#define gosubloop       2

#define checking	true
#define varnamelen      20
#define maxdims	 4

typedef Char varnamestring[varnamelen + 1];
typedef Char string255[256];

typedef struct tokenrec
{
	struct tokenrec *next;
	int kind;
	union
	{
		struct varrec *vp;
		LDBLE num;
		Char *sp;
		Char snch;
	} UU;
#ifdef PHREEQCI_GUI
	size_t n_sz;
	Char *sz_num;
#endif
} tokenrec;

typedef struct linerec
{
	long num, num2;
	tokenrec *txt;
	Char inbuf[MAX_LINE];
	struct linerec *next;
} linerec;

typedef struct varrec
{
	varnamestring name;
	struct varrec *next;
	long dims[maxdims];
	char numdims;
	boolean stringvar;
	union
	{
		struct
		{
			LDBLE *arr;
			LDBLE *val, rv;
		} U0;
		struct
		{
			Char **sarr;
			Char **sval, *sv;
		} U1;
	} UU;
} varrec;

typedef struct valrec
{
	boolean stringval;
	union
	{
		LDBLE val;
		Char *sval;
	} UU;
} valrec;

typedef struct looprec
{
	struct looprec *next;
	linerec *homeline;
	tokenrec *hometok;
	int kind;
	union
	{
		struct
		{
			varrec *vp;
			LDBLE max, step;
		} U0;
	} UU;
} looprec;

/* Local variables for exec: */
struct LOC_exec
{
	boolean gotoflag, elseflag;
	tokenrec *t;
};

#define tokvar               0
#define toknum               1
#define tokstr               2
#define toksnerr             3
#define tokplus              4
#define tokminus             5
#define toktimes             6
#define tokdiv               7
#define tokup                8
#define toklp                9
#define tokrp               10
#define tokcomma            11
#define toksemi             12
#define tokcolon            13
#define tokeq               14
#define toklt               15
#define tokgt               16
#define tokle               17
#define tokge               18
#define tokne               19
#define tokand              20
#define tokor               21
#define tokxor              22
#define tokmod              23
#define toknot              24
#define toksqr              25
#define toksqrt             26
#define toksin              27
#define tokcos              28
#define toktan              29
#define tokarctan           30
#define toklog              31
#define tokexp              32
#define tokabs              33
#define toksgn              34
#define tokstr_             35
#define tokval              36
#define tokchr_             37
#define tokasc              38
#define toklen              39
#define tokmid_             40
#define tokpeek             41
#define tokrem              42
#define toklet              43
#define tokprint            44
#define tokinput            45
#define tokgoto             46
#define tokif               47
#define tokend              48
#define tokstop             49
#define tokfor              50
#define toknext             51
#define tokwhile            52
#define tokwend             53
#define tokgosub            54
#define tokreturn           55
#define tokread             56
#define tokdata             57
#define tokrestore          58
#define tokgotoxy           59
#define tokon               60
#define tokdim              61
#define tokpoke             62
#define toklist             63
#define tokrun              64
#define toknew              65
#define tokload             66
#define tokmerge            67
#define toksave             68
#define tokbye              69
#define tokdel              70
#define tokrenum            71
#define tokthen             72
#define tokelse             73
#define tokto               74
#define tokstep             75
#define toktc               76
#define tokm0               77
#define tokm                78
#define tokparm             79
#define tokact              80
#define tokmol              81
#define tokla               82
#define toklm               83
#define toksr               84
#define toksi               85
#define toktot              86
#define toktk               87
#define toktime             88
#define toklog10            89
#define toksim_time         90
#define tokequi             91
#define tokgas              92
#define tokpunch            93
#define tokkin              94
#define toks_s              95
#define tokmu               96
#define tokalk              97
#define tokrxn              98
#define tokdist             99
#define tokmisc1           100
#define tokmisc2           101
#define tokedl             102
#define tokstep_no         103
#define toksim_no          104
#define toktotal_time      105
#define tokput             106
#define tokget             107
#define tokcharge_balance  109
#define tokpercent_error   110
#if defined PHREEQ98 || MULTICHART
#define tokgraph_x         111
#define tokgraph_y         112
#define tokgraph_sy        113
#endif
#define tokcell_no         114
#define tokexists          115
#define toksurf            116
#define toklk_species      117
#define toklk_named        118
#define toklk_phase        119
#define toksum_species     120
#define toksum_gas         121
#define toksum_s_s         122
#define tokcalc_value      123
#define tokdescription     124
#define toksys             125
#define tokinstr           126
#define tokltrim           127
#define tokrtrim           128
#define toktrim            129
#define tokpad             130
#define tokchange_por      131
#define tokget_por         132
#define tokosmotic         133
#define tokchange_surf     134
#define tokporevolume      135
#define toksc              136
#define tokgamma           137
#define toklg              138
/* VP: Density Start */
#define tokrho             139
/* VP: Density End */
#define tokcell_volume      140
#define tokcell_pore_volume 141
#define tokcell_porosity    142
#define tokcell_saturation  143
#if defined MULTICHART
#define tokplot_xy          144
#endif
#define toktotmole          145
#define tokiso              146
#define tokiso_unit         147
#define toktotmol           148
#define toktotmoles         149
#define tokeol_             150
#define tokceil             151
#define tokfloor            152
#define tokphase_formula    153
#define toklist_s_s         154
#endif /* _INC_BASIC_H */
