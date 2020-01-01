/*********************************************************************
 * Class Name : eos_Taylor
 *              store and evaluate a Taylor Polynomial defined by specified coefficients
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/

#define _EOS_TAYLOR_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_Taylor.h"

/* Setup SESAME file for read (common for 1D and 2D); return ses_file_handle */
EOS_COMMON_TAYLOR_INFO* _eos_getCommonTaylorInfo(eos_Data *me, ses_material_id mid, ses_table_id tid, EOS_INTEGER *err)
{
  EOS_INTEGER nmats = 0;
  EOS_INTEGER *tmp = NULL, size;
  ses_material_id_reference indexdata = (ses_material_id_reference)NULL;
  ses_error_flag ses_err = SES_NO_ERROR;
  ses_file_handle sesFile;
  EOS_COMMON_TAYLOR_INFO *rvalue = (EOS_COMMON_TAYLOR_INFO*) malloc(sizeof(EOS_COMMON_TAYLOR_INFO));

  *err = EOS_OK;

  if (! rvalue) {
    *err = EOS_MEM_ALLOCATION_FAILED;
    return NULL;
  }
    
  rvalue->sesFile = -1;
  rvalue->NTYPS = -1;
  rvalue->TBLREF_L = 0;
  rvalue->NTBLS = NULL;
  rvalue->TBLREF = NULL;

  /* setup file for query */
  *err = eos_SesGetFileInfoFromCache (me->dataFileIndex, &nmats, &indexdata, &sesFile);
  if (*err) {
    _eos_DestroyCommonTaylorInfo(&rvalue);
    return NULL;
  }
  ses_err = ses_setup(sesFile, mid, tid);
  if (ses_err != SES_NO_ERROR) {
    _eos_DestroyCommonTaylorInfo(&rvalue);
    *err = EOS_READ_DATA_FAILED;
    return NULL;
  }
  rvalue->sesFile = sesFile;

  /* get NTYPS for tid */
  *err = _eos_GetNextArray_i(sesFile, &tmp, &size);
  if (*err) {
    _eos_DestroyCommonTaylorInfo(&rvalue);
    return NULL;
  }
  rvalue->NTYPS = tmp[0];
  assert(rvalue->NTYPS == size);
  EOS_FREE(tmp);

  /* get NTBLS[] for tid */
  *err = _eos_GetNextArray_i(sesFile, &(rvalue->NTBLS), &size);
  if (*err) {
    _eos_DestroyCommonTaylorInfo(&rvalue);
    return NULL;
  }

  /* get TBLREF[] for tid */
  *err = _eos_GetNextArray_i(sesFile, &(rvalue->TBLREF), &size);
  if (*err) {
    _eos_DestroyCommonTaylorInfo(&rvalue);
    return NULL;
  }
  rvalue->TBLREF_L = size;

  return rvalue;
}

/* destroy a EOS_COMMON_TAYLOR_INFO struct */
EOS_INTEGER _eos_DestroyCommonTaylorInfo(EOS_COMMON_TAYLOR_INFO **me)
{
  EOS_INTEGER err = EOS_OK;
  (*me)->sesFile = -1;
  (*me)->NTYPS = -1;
  (*me)->TBLREF_L = -1;
  EOS_FREE((*me)->NTBLS);
  EOS_FREE((*me)->TBLREF);
  return err;
}

/* get the next EOS_REAL array */
EOS_INTEGER _eos_GetNextArray_r(ses_file_handle sesFile, EOS_REAL **a, EOS_INTEGER *asize)
{
  int i;
  if (ses_has_next(sesFile) == SES_TRUE) {
    *asize = ses_array_size_next(sesFile);
    ses_word_reference the_buffer = ses_read_next(sesFile);
    if (! the_buffer) {
      return EOS_READ_DATA_FAILED;
    }
    *a = (EOS_REAL*) malloc(*asize * sizeof(EOS_REAL));
    if (! *a) {
      EOS_FREE(the_buffer);
      return EOS_MEM_ALLOCATION_FAILED;
    }
    for (i=0; i<*asize; i++) {
      (*a)[i] = (EOS_REAL) the_buffer[i];
    }
    EOS_FREE(the_buffer);
  }
  else {
    *a = NULL;
    *asize = 0;
    return EOS_READ_DATA_FAILED;
  }
  return EOS_OK;
}

/* get the next EOS_INTEGER array */
EOS_INTEGER _eos_GetNextArray_i(ses_file_handle sesFile, EOS_INTEGER **a, EOS_INTEGER *asize)
{
  int i;
  if (ses_has_next(sesFile) == SES_TRUE) {
    ses_word_reference the_buffer = NULL;
    *asize = ses_array_size_next(sesFile);
    the_buffer = ses_read_next(sesFile);
    if (! the_buffer) {
      return EOS_READ_DATA_FAILED;
    }
    *a = (EOS_INTEGER*) malloc(*asize * sizeof(EOS_INTEGER));
    if (! *a) {
      EOS_FREE(the_buffer);
      return EOS_MEM_ALLOCATION_FAILED;
    }
    for (i=0; i<*asize; i++) {
      (*a)[i] = (EOS_INTEGER) the_buffer[i];
    }
    EOS_FREE(the_buffer);
  }
  else {
    *a = NULL;
    *asize = 0;
    return EOS_READ_DATA_FAILED;
  }
  return EOS_OK;
}

/* Load 1D Taylor information from a SESAME file into an array of 1D Taylor objects, T[M] */
EOS_INTEGER eos_Load1DTaylor(eos_Data *me, ses_material_id mid, ses_table_id tid, EOS_INTEGER subTableNum, EOS_REAL **TX, EOS_INTEGER *M, eos_Taylor ***T)
{
  EOS_INTEGER err = EOS_OK;

  return(err);
}

/* Load 2D Taylor information from a SESAME file into an array of 2D Taylor objects, T[M*N] */
EOS_INTEGER eos_Load2DTaylor(eos_Data *me, ses_material_id mid, ses_table_id tid, EOS_INTEGER subTableNum, EOS_REAL **TX, EOS_REAL **TY, EOS_INTEGER *M, EOS_INTEGER *N, eos_Taylor ***T)
{
  int i, j, k;
  EOS_INTEGER err = EOS_OK;
  EOS_COMMON_TAYLOR_INFO *commonInfo;

  /* Setup SESAME file for read (common for 1D and 2D); return EOS_COMMON_TAYLOR_INFO struct */
  commonInfo = _eos_getCommonTaylorInfo(me, mid, tid, &err);
  if (! commonInfo)
    return EOS_READ_DATA_FAILED;

  /* TBLREF entries are of Taylor type, 001.
   * Currently, this is part of the table number (<TBLNUM><TYPE><SUBTBL>):
   * <TBLNUM>   The base table number (i.e., 701, 703, 704, 705, ...)
   * <TYPE>     Three-digit type of a record (i.e., 001, 002, 003, 004, ...),
   *            which corresponds to a curve-fit and/or analytic form
   * <SUBTBL>   Three-digit sub-table number (i.e., 001, 002, 003, 004, ...) corresponding to P, E, A, S, ...
   */
  for (i=0; i<commonInfo->TBLREF_L; i++) {
    EOS_INTEGER TBLNUM = tid * 1000000 + 1000 + subTableNum; // search for free energy Taylor
    if (commonInfo->TBLREF[i] == TBLNUM) break;
  }
  if (i >= commonInfo->TBLREF_L) {
    err = EOS_NO_DATA_TABLE;
  }
  else {

    ses_table_id tidref = commonInfo->TBLREF[i];
    ses_error_flag ses_err = SES_NO_ERROR;
    EOS_INTEGER NX, NY, size, total_size = 0, *tmp = NULL;
    EOS_REAL *C_flat = NULL, **C = NULL;

    /* setup file for query */
    ses_err = ses_setup(commonInfo->sesFile, mid, tidref);
    if (ses_err != SES_NO_ERROR) {
      _eos_DestroyCommonTaylorInfo(&commonInfo);
      return EOS_OPEN_SESAME_FILE_FAILED;
    }

    /* get M for tidref */
    err = _eos_GetNextArray_i(commonInfo->sesFile, &tmp, &size);
    if (err) {
      _eos_DestroyCommonTaylorInfo(&commonInfo);
      return err;
    }
    *M = tmp[0];
    EOS_FREE(tmp);

    /* get N for tidref */
    err = _eos_GetNextArray_i(commonInfo->sesFile, &tmp, &size);
    if (err) {
      _eos_DestroyCommonTaylorInfo(&commonInfo);
      return err;
    }
    *N = tmp[0];
    EOS_FREE(tmp);

    /* get NX for tidref */
    err = _eos_GetNextArray_i(commonInfo->sesFile, &tmp, &size);
    if (err) {
	_eos_DestroyCommonTaylorInfo(&commonInfo);
	return err;
    }
    NX = tmp[0];
    EOS_FREE(tmp);

    /* get NY for tidref */
    err = _eos_GetNextArray_i(commonInfo->sesFile, &tmp, &size);
    if (err) {
	_eos_DestroyCommonTaylorInfo(&commonInfo);
	return err;
    }
    NY = tmp[0];
    EOS_FREE(tmp);

    /* get TX for tidref */
    err = _eos_GetNextArray_r(commonInfo->sesFile, TX, &size);
    if (err) {
	_eos_DestroyCommonTaylorInfo(&commonInfo);
	return err;
    }
    assert(*M + 1 == size);

    /* get TY for tidref */
    err = _eos_GetNextArray_r(commonInfo->sesFile, TY, &size);
    if (err) {
	_eos_DestroyCommonTaylorInfo(&commonInfo);
	return err;
    }
    assert(*N + 1 == size);

    /* allocate T[M*N] */
    *T = (eos_Taylor**) malloc((*M)*(*N) * sizeof(eos_Taylor*));
    if (! *T) {
      _eos_DestroyCommonTaylorInfo(&commonInfo);
      return EOS_MEM_ALLOCATION_FAILED;
    }

    _eos_AllocateArray(NX, NY, &C);

    /* create M*N 2D Taylor objects of dimension, [NX,NY] */
    for (i=0; i<(*M); i++) {
      for (j=0; j<(*N); j++) {
 	int ii, jj;
	k = i + j * (*M);

	/* get next C[][] for tidref */
	err = _eos_GetNextArray_r(commonInfo->sesFile, &C_flat, &size);
	if (err) {
	  _eos_DestroyCommonTaylorInfo(&commonInfo);
	  for (i=0; i<NX; i++)
	    EOS_FREE(C[i]);
	  EOS_FREE(C);
	  return err;
	}
	assert(NX*NY == size);
	total_size += size;

	for (ii=0; ii<NX; ii++)
	  for (jj=0; jj<NY; jj++)
	    C[jj][ii] = C_flat[ii+jj*NX]; /* transpose column-major data to row-major array */

	EOS_FREE(C_flat);

	(*T)[k] = _eos_Create2DTaylor(NX, NY);
	(*T)[k]->SetIntervals((*T)[k], *TX+i, *TY+j);
	(*T)[k]->SetCoefficients((*T)[k], C);

	//(*T)[k]->Print((*T)[k], 0, stdout);

      }
    }
    assert((*M)*(*N)*NX*NY == total_size);

    for (i=0; i<NX; i++)
      EOS_FREE(C[i]);
    EOS_FREE(C);
  }

  /* clean up */
  _eos_DestroyCommonTaylorInfo(&commonInfo);

  return (err);
}

//! Univariate Taylor constructor
//! \param[in] *me Taylor object pointer
//! \param[in] n Taylor Polynomial order
eos_Taylor* _eos_Create1DTaylor(EOS_INTEGER n)
{
  eos_Taylor *me = (eos_Taylor*) malloc(sizeof(eos_Taylor));

  _eos_Init1DTaylor(me, n);

  return (me);
}

//! Bivariate Taylor constructor
//! \param[in] *me Taylor object pointer
//! \param[in] nx Taylor Polynomial order of x
//! \param[in] ny Taylor Polynomial order of y
eos_Taylor* _eos_Create2DTaylor(EOS_INTEGER nx, EOS_INTEGER ny)
{
  eos_Taylor *me = (eos_Taylor*) malloc(sizeof(eos_Taylor));

  _eos_Init2DTaylor(me, nx, ny);

  return (me);
}

//! Taylor destructor
//! \param[in] *me Taylor object pointer
void _eos_DestroyTaylor(void *ptr)
{
  eos_Taylor *me = (eos_Taylor*) ptr;
  int i, M=0;

  if (me->_type == T_UNIVARIATE)
    M = 1;
  if (me->_type == T_BIVARIATE)
    M = me->Nx;

  if (me->c) {
    for (i=0; i<M; i++)
      EOS_FREE((me->c)[i]);
    EOS_FREE(me->c);
  }
  if (me->f) {
    for (i=0; i<me->f_NX; i++)
      EOS_FREE((me->f)[i]);
    EOS_FREE(me->f);
  }
  if (me->df) {
    for (i=0; i<me->f_NX; i++)
      EOS_FREE((me->df)[i]);
    EOS_FREE(me->df);
  }

  EOS_FREE(me->P);
  EOS_FREE(me->factorial);
  me->_type = T_UNDEFINED;

  for (i=0; i<2; i++) {
    me->tx_interval[i] = 0.0;
    me->ty_interval[i] = 0.0;
  }

  me->a = 0.0;
  me->b = 0.0;

  me->Nx = 0;
  me->Ny = 0;

  me->Destroy          = NULL;
  me->SetIntervals     = NULL;
  me->SetCoefficients  = NULL;
  me->GetCoefficients  = NULL;
  me->Print            = NULL;
  me->Evaluate         = NULL;
  me->Derivative       = NULL;
  me->EvaluateScalar   = NULL;
  me->DerivativeScalar = NULL;

  EOS_FREE(me); // free me which was allocated in either _eos_Create1DTaylor or _eos_Create2DTaylor
}

//! set interval bounds for univariate Taylor
//! \param[in] *me Taylor object pointer
//! \param[in] *tx Taylor interval bounds (2 values required)
void _eos_Set1DTaylorIntervals(void *ptr, EOS_REAL *tx, EOS_REAL *ty)
{
  eos_Taylor *me = (eos_Taylor*) ptr;
  int i;

  for (i=0; i<2; i++) {
    me->tx_interval[i] = tx[i];
  }

  me->a = (tx[0]+tx[1])/2.0;
}

//! set interval bounds for bivariate Taylor
//! \param[in] *me Taylor object pointer
//! \param[in] *tx Taylor x-interval bounds (2 values required)
//! \param[in] *tx Taylor y-interval bounds (2 values required)
void _eos_Set2DTaylorIntervals(void *ptr, EOS_REAL *tx, EOS_REAL *ty)
{
  eos_Taylor *me = (eos_Taylor*) ptr;
  int i;

  for (i=0; i<2; i++) {
    me->tx_interval[i] = tx[i];
    me->ty_interval[i] = ty[i];
  }

  me->a = (tx[0]+tx[1])/2.0;
  me->b = (ty[0]+ty[1])/2.0;
}

//! set c array for univariate Taylor (array dimension: [Nx])
//! \param[in] *me Taylor object pointer
//! \param[in] C[] partial derivatives (df^(n)/dx^(k), k=0..n; array dimension: [nx]) used in construction of c array Taylor coefficient values (array dimension: [Nx])
void _eos_Set1DTaylorCoefficients(void *ptr, EOS_REAL **C)
{
  eos_Taylor *me = (eos_Taylor*) ptr;
  int i;

  if (me->divideByFactorial) {
    for (i=0; i<me->Nx; i++)
      me->c[0][i] = (*C)[i] / me->factorial[i];
  }
  else {
    for (i=0; i<me->Nx; i++)
      me->c[0][i] = (*C)[i]; /* me->factorial[i] already included in C */
  }
}

//! set c array for bivariate Taylor (array dimension: [Nx][Ny])
//! \param[in] *me Taylor object pointer
//! \param[in] C[][] partial derivatives (df^(n)/{dx^(k)dy^(n-k)}, k=0..n; array dimension: [nx][ny]) used in construction of c array Taylor coefficient values (array dimension: [Nx][Ny])
void _eos_Set2DTaylorCoefficients(void *ptr, EOS_REAL **C)
{
  eos_Taylor *me = (eos_Taylor*) ptr;
  int i, j;

  if (me->divideByFactorial) {
    for (i=0; i<me->Nx; i++)
      for (j=0; j<me->Ny; j++)
	me->c[i][j] = C[i][j] / (me->factorial[i] * me->factorial[j]);
  }
  else {
    for (i=0; i<me->Nx; i++)
      for (j=0; j<me->Ny; j++)
	me->c[i][j] = C[i][j]; /* (me->factorial[i] * me->factorial[j]) already included in C */
  }
}

//! return pointer to **c
//! \param[in] *ptr void Taylor object pointer
//! \return pointer to me->c
EOS_REAL** _eos_GetTaylorCoefficients(void *ptr)
{
  eos_Taylor *me = (eos_Taylor*) ptr;

  return (me->c);
}

/* Print the coefficients of all of the Taylor objects, T[M*N], to fh */
void eos_PrintTaylorCoefficients(eos_Taylor **T, EOS_INTEGER M, EOS_INTEGER N, EOS_INTEGER dataType, FILE *fh)
{
  int i, j;
  for (i=0; i<M; i++)
  {
    for (j=0; j<N; j++)
    {
      int k = i + j * M;

      T[k]->Print(T[k], dataType, fh);
    }
  }
}

//! Print contents of c[0][] to stdout
//! \param[in] *void object pointer
//! \param[in] *pad char string for padding
void _eos_Print1DTaylorCoefficients(void *ptr, EOS_INTEGER dataType, FILE *fh)
{

  eos_Taylor *me = (eos_Taylor*) ptr;

  int i;
  char *pad = "  ";

  EOS_INTEGER indepVar1 = EOS_TYPE_TO_INDEP_VAR1(dataType);

  fprintf(fh, "x-interval boundaries (1/(%s)) = %.15e   %.15e\n",
	  get_VarStr(indepVar1,3), me->tx_interval[0], me->tx_interval[1]);
  fprintf(fh, "c: %dx%d\n" , me->Nx, 1);
  fprintf(fh, "a (1/(%s)) = %.15e\n", get_VarStr(indepVar1,3), me->a);
  fprintf(fh, "%s{ ", pad);
  for (i=0; i<me->Nx; i++)
  {
    fprintf(fh, "%.15e%s ", me->c[0][i], ((i<me->Nx-1)?",":""));
  }
  fprintf(fh, "}\n");
}

//! Print contents of c[][] to stdout
//! \param[in] *void object pointer
//! \param[in] *pad char string for padding
void _eos_Print2DTaylorCoefficients(void *ptr, EOS_INTEGER dataType, FILE *fh)
{

  eos_Taylor *me = (eos_Taylor*) ptr;

  int i, j;
  char *pad = "  ";

  EOS_INTEGER indepVar1 = EOS_TYPE_TO_INDEP_VAR1(dataType);
  EOS_INTEGER indepVar2 = EOS_TYPE_TO_INDEP_VAR2(dataType);

  fprintf(fh, "x-interval boundaries (1/(%s)) = %.15e   %.15e\n",
	  get_VarStr(indepVar1,3), me->tx_interval[0], me->tx_interval[1]);
  fprintf(fh, "y-interval boundaries (%s) = %.15e   %.15e\n",
	  get_VarStr(indepVar2,3), me->ty_interval[0], me->ty_interval[1]);
  fprintf(fh, "a (1/(%s)) = %.15e\nb (%s) = %.15e\n",
	  get_VarStr(indepVar1,3), me->a, get_VarStr(indepVar2,3), me->b);
  fprintf(fh, "c: %dx%d, \n" , me->Nx, me->Ny);
  fprintf(fh, "%s{\n", pad);
  for (i=0; i<me->Nx; i++)
  {
    fprintf(fh, "%s%s{ ", pad, pad);
    for (j=0; j<me->Ny; j++)
      fprintf(fh, "%.15e%s", me->c[i][j], ((j==me->Ny-1)?((i<me->Nx-1)?" },":" }"):", "));
    fprintf(fh, "\n");
  }
  fprintf(fh, "%s}\n", pad);
}

//! Calculate the univariate polynomial value at x
//! \param[in] *me Taylor object pointer
//! \param[in] a constant to be subtracted from x
//! \param[in] x value at which to evaluate the Taylor polynomial
//! \return Taylor polynomial value at x
EOS_REAL _eos_Evaluate1DTaylorValueScalar(void *ptr, EOS_REAL x, EOS_REAL y)
{
  EOS_REAL result = 0;
  eos_Taylor *me = (eos_Taylor*) ptr;

  result = _eos_horner(me, (x - me->a), me->c[0], me->Nx, 0);

  return(result);
}

//! Calculate the bivariate polynomial value at (x,y)
//! \param[in] *me Taylor object pointer
//! \param[in] a constant to be subtracted from x
//! \param[in] b constant to be subtracted from y
//! \param[in] x value at which to evaluate the Taylor polynomial
//! \param[in] y value at which to evaluate the Taylor polynomial
//! \return Taylor polynomial value at (x,y)
EOS_REAL _eos_Evaluate2DTaylorValueScalar(void *ptr, EOS_REAL x, EOS_REAL y)
{
  int i;
  EOS_REAL result = 0;
  eos_Taylor *me = (eos_Taylor*) ptr;

  for (i=0; i<me->Nx; i++)
    me->P[i] = _eos_horner(me, (y - me->b), me->c[i], me->Ny, 0);

  result = _eos_horner(me, (x - me->a), me->P, me->Nx, 0);

  return(result);
}

//! Calculate the univariate polynomial value(s) at x[i] for i=0..M
//! and store each result in Taylor::f[0][i]
//! \param[in] *me Taylor object pointer
//! \param[in] M extent of array x[]
//! \param[in] a constant to be subtracted from each x[i]
//! \param[in] x[] array of values at which to evaluate the Taylor polynomial
void _eos_Evaluate1DTaylorValueArray(void *ptr, EOS_INTEGER M, EOS_INTEGER N, EOS_REAL *x, EOS_REAL *y)
{
  eos_Taylor *me = (eos_Taylor*) ptr;
  int i;

  // f  = Taylor results; array dimension: [1][M]
  _eos_AllocateArray(1, M, &(me->f));
  me->f_NX = 1;
  me->f_NY = M;

  for (i=0; i<M; i++)
    me->f[0][i] = _eos_horner(me, (x[i] - me->a), me->c[0], me->Nx, 0);
}

//! Calculate the bivariate polynomial value(s) at (x[i],y[j]) for i=0..M, j=0..N
//! and store each result in Taylor::f[i][j]
//! \param[in] *me Taylor object pointer
//! \param[in] M extent of array x[]
//! \param[in] N extent of array y[]
//! \param[in] a constant to be subtracted from each x[i]
//! \param[in] b constant to be subtracted from each y[j]
//! \param[in] x[] array of values at which to evaluate the Taylor polynomial
//! \param[in] y[] array of values at which to evaluate the Taylor polynomial
void _eos_Evaluate2DTaylorValueArray(void *ptr, EOS_INTEGER M, EOS_INTEGER N, EOS_REAL *x, EOS_REAL *y)
{
  eos_Taylor *me = (eos_Taylor*) ptr;
  int i, ii, jj;

  // f  = Taylor results; array dimension: [M][N]
  _eos_AllocateArray(M, N, &(me->f));
  me->f_NX = M;
  me->f_NY = N;

  for (ii=0; ii<M; ii++)
  {
    for (jj=0; jj<N; jj++)
    {
      for (i=0; i<me->Nx; i++)
 	me->P[i] = _eos_horner(me, (y[jj] - me->b), me->c[i], me->Ny, 0);

      me->f[ii][jj] = _eos_horner(me, (x[ii] - me->a), me->P, me->Nx, 0);
    }
  }
}

//! Calculate the univariate polynomial n-th derivative value at x
//! \param[in] *me Taylor object pointer
//! \param[in] n Taylor Polynomial derivative order
//! \param[in] a constant to be subtracted from x
//! \param[in] x value at which to evaluate the Taylor derivative
//! \return derivative value at x
EOS_REAL _eos_Evaluate1DTaylorDerivativeScalar(void *ptr, EOS_INTEGER nux, EOS_INTEGER nuy, EOS_REAL x, EOS_REAL y)
{
  EOS_REAL result = 0;
  eos_Taylor *me = (eos_Taylor*) ptr;

  result = _eos_horner(me, (x - me->a), me->c[0], me->Nx, nux);

  return(result);
}

//! Calculate the bivariate polynomial nux- by nuy-th derivative value at (x,y)
//! Calculate the univariate polynomial n-th derivative value at x
//! \param[in] *me Taylor object pointer
//! \param[in] nux Taylor Polynomial derivative order of x
//! \param[in] nuy Taylor Polynomial derivative order of y
//! \param[in] a constant to be subtracted from x
//! \param[in] b constant to be subtracted from y
//! \param[in] x value at which to evaluate the Taylor derivative
//! \param[in] y value at which to evaluate the Taylor derivative
//! \return derivative value at (x.y)
EOS_REAL _eos_Evaluate2DTaylorDerivativeScalar(void *ptr, EOS_INTEGER nux, EOS_INTEGER nuy, EOS_REAL x, EOS_REAL y)
{
  int i;
  EOS_REAL result = 0;
  eos_Taylor *me = (eos_Taylor*) ptr;

  for (i=0; i<me->Nx; i++)
  {
    me->P[i] = _eos_horner(me, (y - me->b), me->c[i], me->Ny, nuy);
  }

  result = _eos_horner(me, (x - me->a), me->P, me->Nx, nux);

  return(result);
}

//! Calculate the univariate polynomial n-th derivative values at x[i] for i=0..M
//! and store each result in Taylor::df[0][i]
//! \param[in] *me Taylor object pointer
//! \param[in] n Taylor Polynomial derivative order
//! \param[in] a constant to be subtracted from x
//! \param[in] M extent of array x[]
//! \param[in] x[] array of values at which to evaluate the Taylor derivative
void _eos_Evaluate1DTaylorDerivativeArray(void *ptr, EOS_INTEGER nux, EOS_INTEGER nuy, EOS_INTEGER M, EOS_INTEGER N, EOS_REAL *x, EOS_REAL *y)
{
  eos_Taylor *me = (eos_Taylor*) ptr;
  int i;

  // df  = Taylor derivative results; array dimension: [1][M]
  _eos_AllocateArray(1, M, &(me->df));
  me->f_NX = 1;
  me->f_NY = M;

  for (i=0; i<M; i++)
  me->df[0][i] = _eos_Evaluate1DTaylorDerivativeScalar(me, nux, 0, x[i], 0);
}

//! Calculate the bivariate polynomial nux- by nuy-th derivative values at (x[i],y[j]) for i=0..M, j=0..N
//! and store each result in Taylor::df[i][j]
//! \param[in] *me Taylor object pointer
//! \param[in] nux Taylor Polynomial derivative order of x
//! \param[in] nuy Taylor Polynomial derivative order of y
//! \param[in] a constant to be subtracted from each x[i]
//! \param[in] b constant to be subtracted from each y[j]
//! \param[in] M extent of array x[]
//! \param[in] N extent of array y[]
//! \param[in] x[] array of values at which to evaluate the Taylor derivative
//! \param[in] y[] array of values at which to evaluate the Taylor derivative
void _eos_Evaluate2DTaylorDerivativeArray(void *ptr, EOS_INTEGER nux, EOS_INTEGER nuy, EOS_INTEGER M, EOS_INTEGER N, EOS_REAL *x, EOS_REAL *y)
{
  eos_Taylor *me = (eos_Taylor*) ptr;
  int i, j;

  // df  = Taylor derivative results; array dimension: [M][N]
  _eos_AllocateArray(M, N, &(me->df));
  me->f_NX = M;
  me->f_NY = N;

  for (i=0; i<M; i++)
  {
    for (j=0; j<N; j++)
      me->df[i][j] = _eos_Evaluate2DTaylorDerivativeScalar(me, nux, nuy, x[i], y[j]);
  }
}

//! Empty Taylor initializer
//! \param[in] *me Taylor object pointer
void _eos_Init0DTaylor(eos_Taylor *me)
{
  int i;

  me->debug = EOS_FALSE;

  me->_type = T_CONSTANT;

  me->f = NULL;
  me->df = NULL;

  me->P = NULL;
  me->c = NULL;

  for (i=0; i<2; i++) {
    me->tx_interval[i] = 0.0;
    me->ty_interval[i] = 0.0;
  }

  me->a = 0.0;
  me->b = 0.0;

  me->Nx = 0;
  me->Ny = 0;

  me->factorial = NULL;
  me->divideByFactorial = EOS_FALSE;

  /* define virtual functions */
  me->Destroy          = _eos_DestroyTaylor;
  me->SetIntervals     = NULL; /* dimension-specific function to be defined later */
  me->SetCoefficients  = NULL; /* dimension-specific function to be defined later */
  me->GetCoefficients  = _eos_GetTaylorCoefficients;
  me->Print            = NULL; /* dimension-specific function to be defined later */
  me->Evaluate         = NULL; /* dimension-specific function to be defined later */
  me->Derivative       = NULL; /* dimension-specific function to be defined later */
  me->EvaluateScalar   = NULL; /* dimension-specific function to be defined later */
  me->DerivativeScalar = NULL; /* dimension-specific function to be defined later */
}

//! Univariate Taylor initializer
//! \param[in] *me Taylor object pointer
//! \param[in] n Taylor Polynomial order
void _eos_Init1DTaylor(eos_Taylor *me, EOS_INTEGER n)
{
  int i;

  _eos_Init0DTaylor(me);

  me->_type = T_UNIVARIATE;

  me->Nx = n;

  me->P = (EOS_REAL*) malloc(me->Nx * sizeof(EOS_REAL));

  me->factorial = (EOS_REAL*) malloc(me->Nx * sizeof(EOS_REAL));
  me->factorial[0] = 1;
  for (i=1; i<me->Nx; i++)
    me->factorial[i] = i * me->factorial[i-1];

  // Derivative coefficients; array dimension: [1][nx]
  _eos_AllocateArray(1, me->Nx, &(me->c));

  /* define virtual functions */
  me->SetIntervals     = _eos_Set1DTaylorIntervals;
  me->SetCoefficients  = _eos_Set1DTaylorCoefficients;
  me->Print            = _eos_Print1DTaylorCoefficients;
  me->Evaluate         = _eos_Evaluate1DTaylorValueArray;
  me->Derivative       = _eos_Evaluate1DTaylorDerivativeArray;
  me->EvaluateScalar   = _eos_Evaluate1DTaylorValueScalar;
  me->DerivativeScalar = _eos_Evaluate1DTaylorDerivativeScalar;
}

//! Bivariate Taylor initializer
//! \param[in] *me Taylor object pointer
//! \param[in] nx Taylor Polynomial order of x
//! \param[in] ny Taylor Polynomial order of y
void _eos_Init2DTaylor(eos_Taylor *me, EOS_INTEGER nx, EOS_INTEGER ny)
{
  int i;

  _eos_Init0DTaylor(me);

  me->_type = T_BIVARIATE;

  me->Nx = nx;
  me->Ny = ny;

  me->P = (EOS_REAL*) malloc(me->Nx * sizeof(EOS_REAL));

  me->factorial = (EOS_REAL*) malloc(MAX(me->Nx,me->Ny) * sizeof(EOS_REAL));
  me->factorial[0] = 1;
  for (i=1; i<MAX(me->Nx,me->Ny); i++)
    me->factorial[i] = i * me->factorial[i-1];

  // Derivative coefficients; array dimension: [nx][ny]
  _eos_AllocateArray(me->Nx, me->Ny, &(me->c));

  /* define virtual functions */
  me->SetIntervals     = _eos_Set2DTaylorIntervals;
  me->SetCoefficients  = _eos_Set2DTaylorCoefficients;
  me->Print            = _eos_Print2DTaylorCoefficients;
  me->Evaluate         = _eos_Evaluate2DTaylorValueArray;
  me->Derivative       = _eos_Evaluate2DTaylorDerivativeArray;
  me->EvaluateScalar   = _eos_Evaluate2DTaylorValueScalar;
  me->DerivativeScalar = _eos_Evaluate2DTaylorDerivativeScalar;
}

//! Horner scheme to efficiently evaluate a polynomial or its n-th derivative at x:
//! f(x) = b[0] + x*b[1] + x^2*b[2] + ... + x^(N-1)*b[N-1]
//! \param[in] *me Taylor object pointer
//! \param[in] x value at which to evaluate the Taylor value
//! \param[in] b[] array of Taylor coefficients
//! \param[in] N extent of b[]
//! \param[in] nu order of derivative to calculate
//! \return Talor value at x
EOS_REAL _eos_horner(eos_Taylor *me, EOS_REAL x, EOS_REAL *b, EOS_INTEGER N, EOS_INTEGER nu)
{
  int i;
  EOS_REAL result = 0;

  if (nu>N-1)
    return 0;
  if (nu == 0)
  {
    result = b[N-1];
    for (i=N-2; i>=nu; i--)
      result = result * x + b[i];
  }
  else
 
  {
    result = b[N-1]*(me->factorial[N-1]/me->factorial[N-1-nu]);
    for (i=N-2; i>=nu; i--)
      result = result * x + b[i]*(me->factorial[i]/me->factorial[i-nu]);
  }

  return(result);
}

//! Allocate a 2d Array
//! \param[in] *me Taylor object pointer
//! \param[in] M extent of first dimension of f[][]
//! \param[in] N extent of second dimension of f[][]
//! \param[in,out] f pointer to array to be allocated
void _eos_AllocateArray(EOS_INTEGER M, EOS_INTEGER N, EOS_REAL ***f)
{
  int i;

  if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */

  if (*f)
  { // free any pre-existing *f
    for (i=0; i<M; i++)
      EOS_FREE((*f)[i]);
    EOS_FREE(*f);
  }
  *f = (EOS_REAL_ptr_ptr) malloc(M * sizeof(EOS_REAL_ptr));

  // allocate f
  for (i=0; i<M; i++)
    (*f)[i] = (EOS_REAL_ptr) malloc(N * sizeof(EOS_REAL));
}
