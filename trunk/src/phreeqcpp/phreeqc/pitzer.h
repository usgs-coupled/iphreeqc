#ifndef _INC_PITZER_H
#define _INC_PITZER_H

PITZER_EXTERNAL LDBLE VP, DW0;

/* Pitzer parameters */

PITZER_EXTERNAL struct pitz_param **pitz_params;
PITZER_EXTERNAL int count_pitz_param, max_pitz_param;
PITZER_EXTERNAL struct pitz_param **sit_params;
PITZER_EXTERNAL int count_sit_param, max_sit_param;

/* routines define in pitzer_structures.c */
PITZER_EXTERNAL struct pitz_param *pitz_param_read(char *string, int n);
PITZER_EXTERNAL int pitz_param_search(struct pitz_param *pzp_ptr);
PITZER_EXTERNAL int sit_param_search(struct pitz_param *pzp_ptr);
PITZER_EXTERNAL struct theta_param *theta_param_search(LDBLE zj, LDBLE zk);
PITZER_EXTERNAL struct theta_param *theta_param_alloc(void);
PITZER_EXTERNAL int theta_param_init(struct theta_param *theta_param_ptr);

/* defined in DW */
PITZER_EXTERNAL int DW(LDBLE T);
PITZER_EXTERNAL LDBLE DC(LDBLE T);

PITZER_EXTERNAL struct theta_param **theta_params;
PITZER_EXTERNAL int count_theta_param, max_theta_param;
PITZER_EXTERNAL int use_etheta;
PITZER_EXTERNAL LDBLE OTEMP;
#endif /* _INC_PITZER_H */
