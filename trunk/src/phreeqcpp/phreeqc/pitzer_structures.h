#ifndef _INC_PITZER_STRUCTURES_H
#define _INC_PITZER_STRUCTURES_H
typedef enum
{ TYPE_B0, TYPE_B1, TYPE_B2, TYPE_C0, TYPE_THETA, TYPE_LAMDA, TYPE_ZETA,
  TYPE_PSI, TYPE_ETHETA, TYPE_ALPHAS, TYPE_MU, TYPE_ETA, TYPE_Other,
  TYPE_SIT_EPSILON, TYPE_SIT_EPSILON_MU
} pitz_param_type;

struct pitz_param
{
	char *species[3];
	int ispec[3];
	pitz_param_type type;
	LDBLE p;
	union
	{
		LDBLE b0;
		LDBLE b1;
		LDBLE b2;
		LDBLE c0;
		LDBLE theta;
		LDBLE lamda;
		LDBLE zeta;
		LDBLE psi;
		LDBLE alphas;
		LDBLE mu;
		LDBLE eta;
	  LDBLE eps;
	  LDBLE eps1;
	} U;
	LDBLE a[6];
	LDBLE alpha;
	LDBLE os_coef;
	LDBLE ln_coef[3];
	struct theta_param *thetas;
};

struct theta_param
{
	LDBLE zj;
	LDBLE zk;
	LDBLE etheta;
	LDBLE ethetap;
};
#endif /* _INC_PITZER_STRUCTURES_H */
