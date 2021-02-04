/********************************************************************
 * Class Name : eos_UtilsRage
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>
#include <stdarg.h>

#define _EOS_UTILSRAGE_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_UtilsRage.h"

#ifndef _POSIX_
#define _POSIX_
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>

#include "eos_Utils.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "eos_Interpolation.h"

#define TABLE_SIZE 20000
#define ARRAY_SIZE 500
#define NFIT_MAX 3
#define PDOMEMAX 1.0e12
#define BLKMODMIN 1.0e4

static EOS_BOOLEAN doedit = EOS_TRUE;
static EOS_INTEGER fudge_num = 0;
static EOS_INTEGER fudge_mat[] = { 0 };
static EOS_REAL fudge_scale[] = { (EOS_REAL) 0.0 };
static EOS_BOOLEAN fudge_crit[] = { EOS_FALSE };
static EOS_INTEGER fudge_mat2[] =
  { 7160, 7171, 7180, 7190, 7230, 7930, 7931, 0 };
static EOS_REAL fudge_matdef2[] =
  { 6.0E08, 2.0E08, 5.0E08, 3.0E08, 23.0E08, 10.0E08, 39.0E08, 0.0 };

/* olgi:  doedit passed all the way down from the call to sesame_file_read() called from teos_tabbld,
          first call passes true, the next 2 calls false. */
//    character(8) :: matident(MAX_MAT)
//        real(REAL8)  :: matdef(200,MAX_MAT)

//teos_matdef(200,MAX_MATID)
//teos_matdef = ZERO (looks like the first element is material id, the rest are zeroes )
//teos_matdef(1,m) = real(matid(m), REAL8)(m = 1, nmat *)
//teos_matdef[1][m] = (double) matid

/* the values are then read in: */
//call mgname('matid',        matid,          EOS_TRUE, MAX_MATID)
//call mgname('eostype',      eostype,        EOS_TRUE, MAX_MATID)
//call mgname('matdef',       teos_matdef,    EOS_TRUE, 200, MAX_MATID)
//matdef(1:200,1) = teos_matdef(1:200,m) (m = 1, nmat *)

void _eos_find_realp (EOS_REAL **p301, EOS_INTEGER nR, EOS_INTEGER j,
                      EOS_INTEGER *ireturn)
{
  /* ----- purpose
     find i value of first pressure on this isotherm which is >= preal
     either + or -
     process densities from high to low
   */
  EOS_INTEGER i;
  EOS_REAL preal = 1.0E+04;

  *ireturn = -1;
  for (i = nR - 1; i >= 0; i--) {
    if (FABS (p301[j][i] >= preal)) {
      *ireturn = i;
      return;
    }
  }
}

void rational_func_int (EOS_REAL x, EOS_REAL y, EOS_REAL *tbls,
                        EOS_INTEGER ifn, EOS_REAL *z, EOS_REAL *dzdx,
                        EOS_REAL *dzdy, EOS_INTEGER locx, EOS_INTEGER ix,
                        EOS_INTEGER nx, EOS_INTEGER locy, EOS_INTEGER iy,
                        EOS_INTEGER ny, EOS_INTEGER locz, EOS_INTEGER nz)
{
  /*
     !---------------------------------------------------------------
     !
     !  purpose:     interpolate for a function z(x,y) and its
     !               derivatives from tables located in array tbls.
     !
     !  arguments:   x,y  (input) - independent variables
     !               tbls (input) - array to be interpolated
     !               ifn  (input) - order of interpolation: = 0; rational function
     !                                                      = 1; bilinear
     !               z    (output)- value of function
     !               dzdx (output)- x derivative of function
     !               dzdy (output)- y derivative of function
     !               locx (input) - location of x vector
     !               ix   (input) - index of x vector
     !               nx   (input) - length of x vector
     !               locy (input) - location of y vector
     !               iy   (input) - index of y vector
     !               ny   (input) - length of y vector
     !               locz (input) - location of z(x,y) array
     !               nz   (input) - spacing of z array
     !
     !  remarks:     unless bilinear form is specified, routine
     !               uses rational function method with quadratic
     !               estimate of derivatives at the mesh points.
     !               set ifn = 1 for the bilinear form.
     !
     !  externals:   none
     !
     !  programmer:  g. i. kerley, t-4.
     !
     !  date:        24 february 1977
     !
     !---------------------------------------------------------------
   */
  /* ----- local variables */

  EOS_INTEGER i, k, ibr, nbr, iz, kz;
  EOS_REAL q, d, r, s, sp, c1, c2, c3, c4, dm, sm, dx, qx, zz[5], zd[5];
  EOS_REAL HALF = 0.5, ZERO = 0.0, ONE = 1.0;

  for (k = 1; k <= 4; k++) {
    switch (k) {
    case 1:                    // ... i,j

      i = locx + ix - 1;
      iz = locz + nz * (ix - 1 + nx * (iy - 1));
      kz = nz;
      ibr = ix;
      nbr = nx - ix;
      q = x - tbls[i];
      d = tbls[i + 1] - tbls[i];
      r = d - q;
      break;

    case 2:                    // ... i,j+1

      iz = iz + nx * nz;
      break;

    case 3:                    // ... i,j

      dx = d;
      qx = q;
      i = locy + iy - 1;
      kz = kz * nx;
      iz = iz - kz;
      ibr = iy;
      nbr = ny - iy;
      q = y - tbls[i];
      d = tbls[i + 1] - tbls[i];
      r = d - q;
      break;

    case 4:                    // ... i+1,j

      iz = iz + nz;
      break;

    }

    s = (tbls[iz + kz] - tbls[iz]) / d;

    // .....   bilinear interpolation

    zz[k] = HALF * (tbls[iz] + q * s);
    zd[k] = HALF * s;

    if (ifn == 0) {
      // .....     rational function interpolation

      if (ibr > 1) {
        dm = tbls[i] - tbls[i - 1];
        sm = (tbls[iz] - tbls[iz - kz]) / dm;
        c1 = (s - sm) / (d + dm);
        if (nbr > 1) {
          if (ibr < 4)
            if (sm * (sm - dm * c1) <= ZERO)
              c1 = (s - sm - sm) / d;
          sp =
            (tbls[iz + kz + kz] - tbls[iz + kz]) / (tbls[i + 2] -
                                                    tbls[i + 1]);
          c2 = (sp - s) / (tbls[i + 2] - tbls[i]);
          c3 = FABS (c2 * r);
          c4 = c3 + FABS (c1 * q);
          if (c4 > ZERO)
            c3 = c3 / c4;
          c4 = c2 + c3 * (c1 - c2);
          zz[k] = zz[k] - q * r * c4;
          zd[k] = zd[k] + (q - r) * c4 + d * (c4 - c2) * (ONE - c3);
        }
        else {
          zz[k] = zz[k] - q * r * c1;
          zd[k] = zd[k] + (q - r) * c1;
        }
      }
      else {
        sp =
          (tbls[iz + kz + kz] - tbls[iz + kz]) / (tbls[i + 2] - tbls[i + 1]);
        c2 = (sp - s) / (tbls[i + 2] - tbls[i]);
        if (s * (s - d * c2) <= ZERO)
          c2 = s / d;
        zz[k] = zz[k] - q * r * c2;
        zd[k] = zd[k] + (q - r) * c2;
      }
    }
  }                             // k

  zz[2] = (zz[2] - zz[1]) / d;
  zz[4] = (zz[4] - zz[3]) / dx;
  *z = zz[1] + zz[2] * q + zz[3] + zz[4] * qx;
  *dzdx = zz[4] + zd[1] + (zd[2] - zd[1]) * q / d;
  *dzdy = zz[2] + zd[3] + (zd[4] - zd[3]) * qx / dx;

}

void _eos_RageBiRationalInterp (EOS_INTEGER numZones, EOS_INTEGER numXVals,
                                EOS_INTEGER numYVals, EOS_REAL *XValues,
                                EOS_REAL *YValues, EOS_REAL **FValues,
                                EOS_REAL *searchXVals, EOS_REAL *searchYVals,
                                EOS_REAL *searchFVals, EOS_REAL *searchDFx,
                                EOS_REAL *searchDFy, EOS_INTEGER *xyBounds,
                                EOS_INTEGER *err)
{
  EOS_REAL x, y, *tbls, *z, *dzdx, *dzdy;
  EOS_INTEGER ifn, locx, ix, nx, locy, iy, ny, locz, nz;

  nz = 1;
  ifn = 0;
  x = searchXVals[0];
  y = searchYVals[0];
  z = &(searchFVals[0]);
  dzdx = &(searchDFx[0]);
  dzdy = &(searchDFy[0]);
  locx = 0;
  locy = nx = numXVals;
  ny = numYVals;
  locz =
    (&(FValues[0][0]) == XValues + nx + ny) ? nx + ny : nx + ny + nx * ny;
  tbls = XValues - 1;

  // get ix
  _eos_srchdf (1, &x, 1, nx, XValues, 1, &ix, NULL, xyBounds, err);
  // get iy
  _eos_srchdf (1, &y, 1, ny, YValues, 1, &iy, NULL, xyBounds, err);

  rational_func_int (x, y, tbls, ifn, z, dzdx, dzdy, locx + 1, ix + 1, nx,
                     locy + 1, iy + 1, ny, locz + 1, nz);
}

void _eos_RageRationalInterp (EOS_INTEGER nsrch, EOS_INTEGER nData,
                              EOS_REAL *xData, EOS_REAL *fData,
                              EOS_REAL *yvalv, EOS_REAL *fvalv,
                              EOS_REAL *dfvalv, EOS_INTEGER *xyBounds,
                              EOS_INTEGER *err)
{
  /* this function is a wrapper around the EOSPAC interpolators, in lieu of implementing RAGE's
     original rational interpolation routine.
   */
  EOS_INTEGER i;

  for (i = 0; i < nsrch; i++) {
    eos_LineInterpolate (EOS_FALSE, nsrch, nData, 1, 0, xData, &fData,
                         yvalv, fvalv, dfvalv, 'y', NULL, xyBounds, err);
  }
}

void _eos_find_both_sides (EOS_INTEGER ix, EOS_REAL pfind, EOS_REAL *rfind,
                           EOS_REAL **p301, EOS_REAL *r301, EOS_REAL *t301,
                           EOS_INTEGER nr301, EOS_INTEGER nt301,
                           EOS_INTEGER iy, EOS_REAL t)
{
/*  find vapor density on isotherm iy at which p equals pfind */

  EOS_REAL rholo, rhohi, dpdr, dpdt, p;
  EOS_INTEGER err = EOS_OK, xyBounds;

/* ..... determine density */

  if (ix < 0)
    *rfind = r301[0] * (p301[iy][0] / pfind);
  else if (ix >= nr301 - 1)
    *rfind = r301[nr301 - 1] * (pfind / p301[iy][nr301 - 1]);
  else {
    rholo = r301[ix];
    rhohi = r301[ix + 1];
    *rfind = (EOS_REAL) 0.5 *(rholo + rhohi);

/* .....   interval halving to find density at which p equals pfind */
    while ((rhohi - rholo) > (EOS_REAL) 1.0E-8 * *rfind) {
      /* interpolate P at at rfind, t to get p  */
      _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301, p301, rfind, &t,
                                 &p, &dpdr, &dpdt, &xyBounds, &err);
      if (FABS (pfind - p) < (EOS_REAL) 1.0E-6 * pfind)
        break;
      else {
        if (pfind > p)
          rholo = *rfind;
        else
          rhohi = *rfind;
        *rfind = (EOS_REAL) 0.5 *(rholo + rhohi);
      }
    }
  }
}

void _eos_find_hi_side (EOS_REAL pfind, EOS_REAL *rfind, EOS_REAL **p301,
                        EOS_REAL *r301, EOS_REAL *t301, EOS_INTEGER nr301,
                        EOS_INTEGER nt301, EOS_INTEGER iy, EOS_REAL t)
{
/* find liquid density on isotherm iy at which p equals pfind */

  EOS_INTEGER i, ix;

/* ..... locate lower and upper bounds on density */

  ix = 0;
  for (i = nr301 - 1; i >= 0; i--) {
    if (pfind >= p301[iy][i]) {
      ix = i;
      break;
    }
  }

/* ..... determine density */

  _eos_find_both_sides (ix, pfind, rfind, p301, r301, t301, nr301, nt301, iy,
                        t);

}

void _eos_find_low_side (EOS_INTEGER j, EOS_REAL pfind, EOS_REAL *rfind,
                         EOS_REAL **p301, EOS_REAL *r301, EOS_REAL *t301,
                         EOS_INTEGER nr301, EOS_INTEGER nt301, EOS_INTEGER iy,
                         EOS_REAL t)
{

/*      find vapor density on isotherm iy at which p equals pfind */

  EOS_INTEGER i, ix;

/* ..... locate lower and upper bounds on density */

  ix = nr301 - 1;
  for (i = j; i < nr301; i++) {
    if (pfind < p301[iy][i]) {
      ix = i - 1;
      break;
    }
    else if (i > 1) {
      if (p301[iy][i] <= p301[iy][i - 1]) {
        ix = i - 2;
        break;
      }
    }
  }

/* ..... determine density */
  _eos_find_both_sides (ix, pfind, rfind, p301, r301, t301, nr301, nt301, iy,
                        t);
}


void _eos_find_notpos (EOS_REAL **p301, EOS_INTEGER nR, EOS_INTEGER j,
                       EOS_INTEGER *iloc, EOS_INTEGER *err)
/*
!   search for first dpdrho <= 0 or p <= 0 on this isotherm
!   process densities from high to low
!   find first i index on this isotherm which has a 'real' pressure
*/
{
  /* EOS_REAL preal = 1.0E+04; */
  EOS_REAL p, psave;
  EOS_INTEGER i, istart;

  *iloc = -1;

  _eos_find_realp (p301, nR, j, &istart);
  _eos_DEBUG_PRINT ("j = %i, iloc = %i\n", j + 1, istart + 1);
  _eos_DEBUG_PRINT ("P1=%23.14e, P2=%23.14e\n", p301[j][istart - 1],
                    p301[j][istart]);
  //istart = ix_low + 1;
  fflush (stdout);
  if (istart < 1) {
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_OK)
      *err = EOS_INTERP_EXTRAPOLATED;   /* WARNING - isotherm j has no valid pressure */
    return;
  }

  psave = p301[j][istart];
  for (i = istart - 1; i >= 0; i--) {
    p = p301[j][i];
    if (p < psave && p >= (EOS_REAL) 0.0)
      psave = p;
    else {
      *iloc = i + 1;
      _eos_DEBUG_PRINT ("i=%i, iloc=%i, p=%23.14e\n", i + 1,
                        *iloc + ((*iloc == -1) ? 0 : 1), p);
      break;
    }
  }
  if (*iloc == -1)
    _eos_DEBUG_PRINT ("i=%i, iloc=%i, p=%23.14e\n", i + 1, *iloc, p);
}

void _eos_remove_patch (EOS_REAL **p301, EOS_REAL *r301, EOS_INTEGER *k,
                        EOS_INTEGER j, EOS_INTEGER imax)
/* ..... remove small patches of equal pressures due to crack formation */
{
  EOS_INTEGER l, kmax;
  EOS_REAL psave, dpdrho;

  _eos_DEBUG_PRINT
    ("j=%i: remove small patches of equal pressures due to crack formation...\n",
     j + 1);
  kmax = *k;
  psave = p301[j][*k];
  *k = *k - 1;
  if (p301[j][*k] == psave && psave >= (EOS_REAL) 0.0) {
    while (1) {
      *k = *k - 1;
      if (p301[j][*k] != psave || p301[j][*k] <= (EOS_REAL) 0.0)
        break;
      if (*k == 0)
        break;
    }

    *k = *k + 1;

    dpdrho = (p301[j][kmax + 1] - psave) / (r301[kmax + 1] - r301[*k]);

    for (l = *k + 1; l <= kmax; l++) {
      _eos_DEBUG_PRINT ("P[%i][%i]: %23.14e -> %23.14e\n", j + 1, l,
                        p301[j][l],
                        (EOS_REAL) (p301[j][l - 1] +
                                    dpdrho * (r301[l] - r301[l - 1])));
      p301[j][l] = p301[j][l - 1] + dpdrho * (r301[l] - r301[l - 1]);
    }
  }
}

void _eos_integrate (EOS_REAL pfind, EOS_INTEGER *ifail, EOS_INTEGER niter,
                     EOS_REAL rho1, EOS_REAL rho2, EOS_REAL **p301,
                     EOS_REAL *r301, EOS_REAL *t301, EOS_INTEGER nr301,
                     EOS_INTEGER nt301, EOS_REAL t, EOS_INTEGER iy,
                     EOS_REAL *pmin, EOS_REAL *pmax)
{

  EOS_INTEGER n, nmax, xyBounds, err;
  EOS_REAL v1, v2, vtest, vrat, sumpos, sumneg, v2save, delvv, vmid, rhomid;
  EOS_REAL pses, dpdr, dpdt, delta;

  v1 = (EOS_REAL) 1.0 / rho1;
  v2 = (EOS_REAL) 1.0 / rho2;
  vtest = v2 / v1;
  if (vtest <= (EOS_REAL) 3.0)
    nmax = 100;
  else if (vtest <= 1.0E5)
    nmax = MAX (200, (int) ((EOS_REAL) 200.0 * log (vtest)));
  else
    nmax = MAX (1000, (int) ((EOS_REAL) 100.0 * log (vtest)));

  vrat = pow (vtest, (EOS_REAL) 1.0 / (EOS_REAL) nmax);

/* ..... integrate from v1  to  v2 */

  sumpos = (EOS_REAL) 0.0;
  sumneg = (EOS_REAL) 0.0;

  v2save = v2;
  v2 = v1;
  for (n = 0; n < nmax; n++) {
    v1 = v2;
    if (n == nmax)
      v2 = v2save;
    else
      v2 = vrat * v2;

    delvv = v2 - v1;
    vmid = (EOS_REAL) 0.5 *(v1 + v2);
    rhomid = (EOS_REAL) 1.0 / vmid;
    /* interpolate at rhomid, t to get pses, dpdr, dpdt */
    _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301, p301, &rhomid, &t,
                               &pses, &dpdr, &dpdt, &xyBounds, &err);
    delta = delvv * (pses - pfind);
    if (delta > (EOS_REAL) 0.0)
      sumpos = sumpos + delta;
    else
      sumneg = sumneg + FABS (delta);
    v1 = v1 + delvv;
  }

  *ifail = 1;

  if (niter < 30) {
    if (FABS (sumpos - sumneg) < (EOS_REAL) 1.0e-5 * (sumpos + sumneg))
      *ifail = 0;
  }
  else if (niter < 40) {
    if (FABS (sumpos - sumneg) < (EOS_REAL) 1.0e-4 * (sumpos + sumneg))
      *ifail = 0;
  }
  else if (niter < 45) {
    if (FABS (sumpos - sumneg) < (EOS_REAL) 1.0e-3 * (sumpos + sumneg))
      *ifail = 0;
  }
  else if (niter < 50) {
    if (FABS (sumpos - sumneg) < (EOS_REAL) 1.0e-2 * (sumpos + sumneg))
      *ifail = 0;
  }
  else {
    if (FABS (sumpos - sumneg) < (EOS_REAL) 5.0e-2 * (sumpos + sumneg))
      *ifail = 0;
  }

  if (sumpos > sumneg)
    *pmin = pfind;
  else
    *pmax = pfind;
}

void _eos_store_dome (EOS_INTEGER *ifound, EOS_REAL *domet, EOS_REAL *foundp,
                      EOS_REAL *foundr1, EOS_REAL *foundr2, EOS_REAL *founde1,
                      EOS_REAL *founde2, EOS_REAL *r301, EOS_REAL *t301,
                      EOS_INTEGER nr301, EOS_INTEGER nt301, EOS_REAL **p301,
                      EOS_REAL **e301, EOS_REAL pfind, EOS_REAL t,
                      EOS_REAL *rho1, EOS_REAL *rho2)
{
  EOS_INTEGER err = EOS_OK, xyBounds = EOS_OK;
  EOS_REAL rho, e, dedr, dedt;

  *ifound = *ifound + 1;
  domet[*ifound] = t;
  foundp[*ifound] = pfind;
  foundr1[*ifound] = *rho1;
  foundr2[*ifound] = *rho2;
  rho = *rho1;
  /* interpolate P at rho, t to get e */
  _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301, e301, &rho, &t, &e,
                             &dedr, &dedt, &xyBounds, &err);
  founde1[*ifound] = e;
  rho = *rho2;

  /* interpolate P at rho, t to get e */
  _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301, e301, &rho, &t, &e,
                             &dedr, &dedt, &xyBounds, &err);
  founde2[*ifound] = e;
}

void _eos_build_dome (EOS_REAL **p301, EOS_REAL **e301, EOS_REAL *r301,
                      EOS_REAL *t301, EOS_INTEGER nr301, EOS_INTEGER nt301,
                      EOS_REAL *avgAtomicNumber401, EOS_REAL *avgAtomicWgt401,
                      EOS_REAL *refDensity401, EOS_INTEGER *nt401,
                      EOS_REAL **p401, EOS_REAL **t401, EOS_REAL **rg401,
                      EOS_REAL **rl401, EOS_REAL **eg401, EOS_REAL **el401,
                      EOS_REAL **ag401, EOS_REAL **al401, EOS_INTEGER mat,
                      EOS_REAL *savep, EOS_INTEGER *idome, EOS_REAL tcrit,
                      EOS_REAL pcrit, EOS_REAL rcrit, EOS_REAL ecrit,
                      EOS_BOOLEAN *found_401, EOS_INTEGER *err, EOS_CHAR **errMsg)
{
  EOS_BOOLEAN flt, done;
  EOS_INTEGER i, j, k, ix, iy, ifound, niter, kcurrent, xyBounds;
  EOS_INTEGER ix1, ix2, ifail, icrit, nitmax = 60;
  EOS_REAL pfind, rhoc, tc, pc, t, e, rho, p, ptest, pmin, pmax, dpdr,
    dpdt, dedr, dedt, ec, x, rho1, rho2, pminiso, pmaxiso;

  EOS_REAL foundp[ARRAY_SIZE], foundr1[ARRAY_SIZE], foundr2[ARRAY_SIZE],
    founde1[ARRAY_SIZE], founde2[ARRAY_SIZE];
  EOS_REAL domet[ARRAY_SIZE], pdome_at0 = (EOS_REAL) 0.0, room_temp =
    (EOS_REAL) 0.025, room_p = (EOS_REAL) 1.0E06;

  _eos_DEBUG_PRINT ("\n*** ENTERING BUILD_DOME\n");

  t = tcrit;
  rho = rcrit;
  /* interpolate at rho, t to get pc, dpdr, dpdt */
  _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301, p301, &rho, &t, &pc,
                             &dpdr, &dpdt, &xyBounds, err);

  /* interpolate at rho, t to get ec, dpdr, dpdt */
  _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301, e301, &rho, &t, &ec,
                             &dpdr, &dpdt, &xyBounds, err);

  /* find index of t in t301 array */
  _eos_srchdf (1, &t, 1, nt301, t301, 1, &j, NULL, &xyBounds, err);
  iy = j;

  ifound = 0;
  foundp[0] = pcrit;
  foundr1[0] = rcrit;
  foundr2[0] = rcrit;
  founde1[0] = ecrit;
  founde2[0] = ecrit;
  domet[0] = tcrit;
  pfind = pcrit;

  /* find index of rho in r301 array */
  _eos_srchdf (1, &rho, 1, nr301, r301, 1, &icrit, NULL, &xyBounds, err);
  i = icrit;

  rhoc = rcrit;
  rho1 = rhoc;
  rho2 = rhoc;
  tc = tcrit;

  /* find index of tc in t301 array */
  _eos_srchdf (1, &tc, 1, nt301, t301, 1, &k, NULL, &xyBounds, err);

  // ..... is critical temperature exactly on temperature grid point
  // ..... if so, have already defined dome for this temperature

  if (tc == t301[k])
    k = k - 1;
  kcurrent = k;

  while (1) {
    if (kcurrent < 0)
      break;
    if (idome[kcurrent] < 0)
      break;
    t = t301[kcurrent];
    iy = kcurrent;
    /* find index of rho1, rho2 in r301 array */
    _eos_srchdf (1, &rho2, 1, nr301, r301, 1, &ix2, NULL, &xyBounds, err);
    _eos_srchdf (1, &rho1, 1, nr301, r301, 1, &ix1, NULL, &xyBounds, err);

    /*
       ! .....   check to see if this isotherm has loops or already has
       ! .....   maxwell construction (is flat)
       ! .....   cannot be maxwell construction unless p>0
       ! .....   if already flat, cannot use rational function interpolation routine
       ! .....   inside this region, since it introduces phony loops
     */

    flt = EOS_FALSE;
    rho1 = r301[ix1];
    while (1) {
      if (ix1 == nr301 - 1)
        break;
      if (ix1 > 0) {
        if (p301[iy][ix1 - 1] != p301[iy][ix1])
          break;
      }

      if (p301[iy][ix1] > pdome_at0) {
        flt = EOS_TRUE;
        ix1 = ix1 + 1;
        rho1 = r301[ix1];
        pfind = p301[iy][ix1];
      }
      else
        ix1 = ix1 + 1;
    }                           /*  ix1 - loop */

    rho2 = r301[ix2];

    while (1) {
      if (ix2 == 0)
        break;
      if (ix2 < nr301 - 1) {
        if (p301[iy][ix2 + 1] != p301[iy][ix2])
          break;
      }

      if (p301[iy][ix2] > pdome_at0) {
        flt = EOS_TRUE;
        ix2 = ix2 - 1;
        rho2 = r301[ix2];
        pfind = p301[iy][ix2];
      }
    }                           /* ix2 - loop */

    for (i = MIN (nr301 - 2, ix2); i < MAX (1, ix1); i++) {
      if (p301[iy][i] == p301[iy][i + 1]) {
        if (p301[iy][i] > pdome_at0) {
          flt = EOS_TRUE;
          pfind = p301[iy][i];
          break;
        }
      }
    }                           /* i-loop */

    if (flt) {
      /*
         ! .....     material 2290 has 2 adjacent flat isotherms, with the
         ! .....     lower t having the higher p.  avoid this problem
       */
      if (pfind >= foundp[ifound])
        pfind = (EOS_REAL) 0.98 *foundp[ifound];

      /* .....     find limit of flat region on low density side of dome */

      i = 0;
      _eos_find_low_side (i, pfind, &rho2, p301, r301, t301, nr301, nt301, iy,
                          t);

      /* .....     find limit of flat region on high density side of dome */

      _eos_find_hi_side (pfind, &rho1, p301, r301, t301, nr301, nt301, iy, t);
    }
    else {                      /* !flt */

      if (t == (EOS_REAL) 0.0) {
        pfind = pdome_at0;
        if (p301[iy][1] > p301[iy][0]) {
          if (p301[iy][0] < pdome_at0) {
            /*  ! .....           this isotherm has vdw loop and pdome_at0 exists */
            i = 0;
            _eos_find_low_side (i, pfind, &rho2, p301, r301, t301, nr301,
                                nt301, iy, t);
          }
          else
            rho2 = r301[0];
        }
        else if (p301[iy][0] >= pdome_at0) {
          /* ! .....          no complete loop, but pdome_at0 exists in what is there */
          i = 0;
          _eos_find_low_side (i, pfind, &rho2, p301, r301, t301, nr301, nt301,
                              iy, t);
        }
        else {
          /* .....                      does this isotherm contain 1.5 loops */
          pmin = p301[iy][1];
          for (i = 2; i < nr301 - 1; i++) {
            if (p301[iy][i] <= pmin)
              pmin = p301[iy][i];
            else
              break;
          }

          i = i - 1;
          pmax = p301[iy][i];
          for (j = i + 1; j < nr301; j++) {
            if (p301[iy][j] >= pmax)
              pmax = p301[iy][j];
            else
              break;
          }

          if (j < nr301 - 1) {
            /* .....           isotherm has 1.5 loops */

            if (pmax > pfind)
              _eos_find_low_side (i, pfind, &rho2, p301, r301, t301, nr301,
                                  nt301, iy, t);
            else
              /* .....             arbitrarily pick a point */
              rho2 = (EOS_REAL) 0.7 *foundr2[ifound];
          }
          else
            /* .....             arbitrarily pick a point */
            rho2 = (EOS_REAL) 0.7 *foundr2[ifound];
        }                       /* if !(p301[iy][0] >= pdome_at0)  */

        _eos_find_hi_side (pfind, &rho1, p301, r301, t301, nr301, nt301, iy,
                           t);
      }
      else if (p301[iy][1] > p301[iy][0]) {
        /*
           ! .....       this isotherm must contain a van der waals loop
           ! .....       define maximum pressure which steam dome can have at this t
         */
        pmaxiso = p301[iy][1];
        for (i = 2; i < nr301 - 1; i++) {
          if (p301[iy][i] > pmaxiso)
            pmaxiso = p301[iy][i];
          else
            break;
        }

        /* .....       define minimum pressure which steam dome can have at this t */

        pminiso = p301[iy][nr301 - 1];
        for (i = nr301 - 2; i >= 0; i--) {
          if (p301[iy][i] < pminiso) {
            if (p301[iy][i] > pdome_at0)
              pminiso = p301[iy][i];
            else {
              pminiso = pdome_at0;
              break;
            }
          }
          else {
            pminiso = pdome_at0;
            break;
          }
        }                       /* for i */
        /*
           ! .....         do maxwell construction by interval halving between pmin and pmax
           ! .....         to define pfind
         */
        pmin = pminiso;
        pmax = pmaxiso;
        if (pmax > pmin) {
          done = EOS_FALSE;
          niter = 0;
          while (1) {
            if (done)
              break;
            niter = niter + 1;
            //  .....                         as long as possible, insist on convergence through equal area
            // .....                          construction in subroutine integrate
            if (niter > nitmax || (pmax - pmin < (EOS_REAL) 0.00001 * pmax)) {
              //                    ! .....             some isotherms cannot meet the requirements of a maxwell
              //                    ! .....             construction. typically these have large neg pressure loops,
              //                    ! .....             which require a non-physical negative pressure to satisfy
              //                    ! .....             the maxwell construction
              //                    ! .....             define the dome properties

              dpdt =
                (EOS_REAL) 0.5 *(foundp[ifound] - pdome_at0) / domet[ifound];
              pfind = foundp[ifound] - dpdt * (domet[ifound] - t301[iy]);
              if (pfind > pmaxiso)
                pfind = pmaxiso;
              i = 0;

              _eos_find_low_side (i, pfind, &rho2, p301, r301, t301, nr301,
                                  nt301, iy, t);
              _eos_find_hi_side (pfind, &rho1, p301, r301, t301, nr301, nt301,
                                 iy, t);

              if (mat == 7761 && t > (EOS_REAL) 0.03)
                rho1 = MIN (rho1, (EOS_REAL) 2.3);
              done = EOS_TRUE;
            }
            else {
              pfind = (EOS_REAL) 0.5 *(pmin + pmax);
              i = 0;
              _eos_find_low_side (i, pfind, &rho2, p301, r301, t301, nr301,
                                  nt301, iy, t);
              _eos_find_hi_side (pfind, &rho1, p301, r301, t301, nr301, nt301,
                                 iy, t);
              _eos_integrate (pfind, &ifail, niter, rho1, rho2, p301, r301,
                              t301, nr301, nt301, t, iy, &pmin, &pmax);

              if (ifail == 0) {
                //    .....                maxwell construction satisfied
                //    .....                is pressure reasonable
                dpdt =
                  (EOS_REAL) 0.5 *(foundp[ifound] -
                                   pdome_at0) / domet[ifound];
                ptest = foundp[ifound] - dpdt * (domet[ifound] - t301[iy]);
                if (ptest > (EOS_REAL) 5.0 * pfind)
                  //    .....                   maxwell construction absurdly low 
                  //   .....                    force it to be set to a reasonable value */
                  niter = nitmax + 1;
                else if (pfind >= foundp[ifound])
                  //    .....                   maxwell construction too large
                  //    .....                   force it to be set to a reasonable value
                  niter = nitmax + 1;
                else
                  done = EOS_TRUE;
              }                 /* if (ifail == 0)  */
            }                   /* if (niter > nitmax || (pmax - pmin < (EOS_REAL) 0.00001 * pmax)) */
          }                     /* done */
        }
        else {                  /* if (pmax <= pmin) */

          // .....                      cannot do maxwell construction; must be defined
          // .....                      define dome pressure, extrapolating so that p = pdome_at0 at t = 0

          dpdt = (EOS_REAL) 0.5 *(foundp[ifound] - pdome_at0) / domet[ifound];
          pfind = foundp[ifound] - dpdt * (domet[ifound] - t301[iy]);
          if (pmax > pfind) {
            i = 0;
            _eos_find_low_side (i, pfind, &rho2, p301, r301, t301, nr301,
                                nt301, iy, t);
          }
          else
            rho2 = (EOS_REAL) 0.7 *foundr2[ifound];

          if (pmin < pfind)
            _eos_find_hi_side (pfind, &rho1, p301, r301, t301, nr301, nt301,
                               iy, t);
          else
            _eos_find_hi_side (pmin, &rho1, p301, r301, t301, nr301, nt301,
                               iy, t);
        }
      }
      else {                    /* if !(p301[iy][1] > p301[iy][0]) */


        // .....          this isotherm does not contain a complete vdw loop, or contains
        // .....           more than 1. cannot do maxwell construction; must be defined

        // .....          define dome pressure, extrapolating so that p = pdome_at0 at t = 0

        dpdt = (foundp[ifound] - pdome_at0) / domet[ifound];
        pfind = foundp[ifound] - dpdt * (domet[ifound] - t301[iy]);

        // .....          does this isotherm contain 1.5 loops

        pmin = p301[iy][1];
        for (i = 2; i < nr301 - 1; i++) {
          if (p301[iy][i] < pmin)
            pmin = p301[iy][i];
          else
            break;
        }

        i = i - 1;
        pmax = p301[iy][i];
        for (j = i; j < nr301; j++) {
          if (p301[iy][j] >= pmax)
            pmax = p301[iy][j];
          else
            break;
        }

        if (j < nr301) {
          // .....           isotherm has 1.5 loops
          if (pfind > pmax)
            pfind = pmax;
          _eos_find_low_side (i, pfind, &rho2, p301, r301, t301, nr301, nt301,
                              iy, t);
        }
        else {
          // .....           arbitrarily pick a point
          rho2 = (EOS_REAL) 0.7 *foundr2[ifound];
        }

        _eos_find_hi_side (pfind, &rho1, p301, r301, t301, nr301, nt301, iy,
                           t);
      }                         /* if !(p301[iy][1] > p301[iy][0]) */
    }                           // flt

    // .....   make sure steam dome is 'smooth'

    if (rho2 > foundr2[ifound])
      rho2 = foundr2[ifound];
    if (rho1 < foundr1[ifound])
      rho1 = foundr1[ifound];

    // .....   store these limits and do next isotherm

    _eos_store_dome (&ifound, domet, foundp, foundr1, foundr2, founde1,
                     founde2, r301, t301, nr301, nt301, p301, e301, pfind, t,
                     &rho1, &rho2);
    kcurrent = kcurrent - 1;
  }                             // while idome[kcurrent] > 0

  if (ifound > 0) {
    *found_401 = EOS_TRUE;
    *nt401 = ifound + 1;
    *p401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);
    *t401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);
    *rg401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);
    *rl401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);
    *eg401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);
    *el401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);

    _eos_DEBUG_PRINT
      ("build_dome: initial vapor dome constructed (nt401=%i)\n%15s %15s %15s %15s %15s %15s \n",
       *nt401, "t401", "p401", "rg401", "rl401", "eg401", "el401");

    for (i = 0; i < *nt401; i++) {
      (*t401)[i] = domet[*nt401 - i - 1];
      (*p401)[i] = foundp[*nt401 - i - 1];
      (*rg401)[i] = foundr2[*nt401 - i - 1];
      (*rl401)[i] = foundr1[*nt401 - i - 1];
      (*eg401)[i] = founde2[*nt401 - i - 1];
      (*el401)[i] = founde1[*nt401 - i - 1];

      _eos_DEBUG_PRINT ("%15.7e %15.7e %15.7e %15.7e %15.7e %15.7e\n",
                        (*t401)[i], (*p401)[i], (*rg401)[i], (*rl401)[i],
                        (*eg401)[i], (*el401)[i]);

    }
    _eos_DEBUG_PRINT ("\n");

    /* .....   ensure that ambient conditions are outside the dome */

    if ((*t401)[*nt401 - 1] > room_temp && (*t401)[0] < room_temp) {
      for (i = 0; i < *nt401; i++) {
        if ((*t401)[i] >= room_temp)
          break;
      }

      if (i < *nt401 - 1) {
        if (i > 0) {
          /* .....         how many dome levels below room_temp exceed room_p */

          k = 0;
          for (j = 0; j <= i; j++) {
            if ((*p401)[j] > room_p)
              k = k + 1;
          }

          if (k < 3) {
            x = (room_temp - (*t401)[i - 1]) / ((*t401)[i] - (*t401)[i - 1]);
            p = (*p401)[i - 1] + x * ((*p401)[i] - (*p401)[i - 1]);
            if ((p > room_p && p < (EOS_REAL) 1.E07) ||
                mat == 7122 || mat == 7154 || mat == 7610) {
              if ((*p401)[i - 1] > (EOS_REAL) 0.95E06) {
                dpdt = (EOS_REAL) 0.5 *(0.95E06 - pdome_at0) / room_temp;
                (*p401)[i - 1] =
                  (EOS_REAL) 0.95E06 - dpdt * (room_temp - (*t401)[i - 1]);
              }

              /* .....                              make room_temp pressure = 0.95e6 ergs/cc */

              (*p401)[i] =
                ((EOS_REAL) 0.95E06 -
                 (*p401)[i - 1] * ((EOS_REAL) 1.0 - x)) / x;
              t = (*t401)[i];
              _eos_srchdf (1, &t, 1, nt301, t301, 1, &iy, NULL, &xyBounds, err);
              _eos_find_hi_side ((*p401)[i], &rho1, p301, r301, t301, nr301,
                                 nt301, iy, t);
              (*rl401)[i] = rho1;
              _eos_srchdf (1, &rho1, 1, nr301, r301, 1, &ix, NULL, &xyBounds, err);
              if (ix != 0) {
                /* .....                                          do not redefine vapor density unless the pressure exists */
                if (p301[iy][1] > p301[iy][0]) {
                  pmaxiso = p301[iy][1];
                  for (j = 2; j < nr301; j++) {
                    if (p301[iy][j] > pmaxiso)
                      pmaxiso = p301[iy][j];
                    else
                      break;
                  }

                  if (pmaxiso > (*p401)[i]) {
                    ix1 = 0;
                    _eos_find_low_side (ix1, (*p401)[i], &rho2, p301, r301,
                                        t301, nr301, nt301, iy, t);
                    (*rg401)[i] = rho2;
                  }
                }               /* if (p301[iy][1] > p301[iy][1])  */
              }                 /* if (ix != 0) */

              t = (*t401)[i - 1];
              iy = iy - 1;
              _eos_find_hi_side ((*p401)[i - 1], &rho1, p301, r301, t301,
                                 nr301, nt301, iy, t);
              (*rl401)[i - 1] = rho1;
              _eos_srchdf (1, &rho1, 1, nr301, r301, 1, &ix, NULL, &xyBounds, err);

              if (ix != 0) {
                /* .....                                  do not redefine vapor density unless the pressure exists */

                if (p301[iy][1] > p301[iy][0]) {
                  pmaxiso = p301[iy][1];
                  for (j = 0; j < nr301; j++) {
                    if (p301[iy][j] > pmaxiso)
                      pmaxiso = p301[iy][j];
                    else
                      break;
                  }

                  if (pmaxiso > (*p401)[i - 1]) {
                    ix1 = 0;
                    _eos_find_low_side (ix1, (*p401)[i - 1], &rho2, p301,
                                        r301, t301, nr301, nt301, iy, t);
                    (*rg401)[i - 1] = rho2;
                  }
                }               /* if (p301([iy][1] > p301[iy][0]) */
              }                 /* if (ix != 0) */

              rho = (*rg401)[i - 1];
              /* interpolate at rho, t to get e, dedr, dedt */
              _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301, e301,
                                         &rho, &t, &e, &dedr, &dedt,
                                         &xyBounds, err);
              (*eg401)[i - 1] = e;

              rho = (*rl401)[i - 1];
              /* interpolate at rho, t to get e, dedr, dedt */
              _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301, e301,
                                         &rho, &t, &e, &dedr, &dedt,
                                         &xyBounds, err);
              (*el401)[i - 1] = e;

              //                    iy = iy+1; Olga! This is passed into interpolator, and it's diff from iy, find out hat it's used for!
              //                    call rational_func_int(rho, t, tbls, ifn, e, dedr, dedt, &
              //                                           locx, ix, nr301, locy, iy, nt301, locz, nz)
              rho = (*rg401)[i];
              /* interpolate at rho, t to get e, dedr, dedt */
              _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301, e301,
                                         &rho, &t, &e, &dedr, &dedt,
                                         &xyBounds, err);
              (*eg401)[i] = e;

              rho = (*rl401)[i];
              /* interpolate at rho, t to get e, dedr, dedt */
              _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301, e301,
                                         &rho, &t, &e, &dedr, &dedt,
                                         &xyBounds, err);
              (*el401)[i] = e;

              /* .....                               make sure steam dome is still 'smooth' */

              if (x > (EOS_REAL) 0.5)
                ix1 = i + 1;
              else
                ix1 = i;

              if (ix1 > 0) {
                for (i = ix1 - 2; i >= 0; i--) {
                  if ((*rg401)[i] > (*rg401)[i + 1]) {
                    (*rg401)[i] = (*rg401)[i + 1];
                    t = (*t401)[i];
                    rho = (*rg401)[i];
                    _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301,
                                               e301, &rho, &t, &e, &dedr,
                                               &dedt, &xyBounds, err);
                    (*eg401)[i] = e;
                  }

                  if ((*rl401)[i] < (*rl401)[i + 1]) {
                    (*rl401)[i] = (*rl401)[i + 1];
                    t = (*t401)[i];
                    rho = (*rl401)[i];
                    // not needed                                                       _eos_srchdf (1, &rho, 1, nr301, r301, 1, &ix, NULL, &xyBounds, err);
                    _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301,
                                               e301, &rho, &t, &e, &dedr,
                                               &dedt, &xyBounds, err);
                    (*el401)[i] = e;
                  }
                }               // i
              }                 /* if ix > 0 */

              if (ix1 < *nt401 - 1) {
                for (i = ix1 + 1; i < *nt401 - 1; i++) {
                  if ((*rg401)[i] < (*rg401)[i - 1]) {
                    (*rg401)[i] = (*rg401)[i - 1];
                    t = (*t401)[i];
                    rho = (*rg401)[i];
                    _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301,
                                               e301, &rho, &t, &e, &dedr,
                                               &dedt, &xyBounds, err);
                    (*eg401)[i] = e;
                  }

                  if ((*rl401)[i] > (*rl401)[i - 1]) {
                    (*rl401)[i] = (*rl401)[i - 1];
                    t = (*t401)[i];
                    rho = (*rl401)[i];
                    // not needed                                                       _eos_srchdf (1, &rho, 1, nr301, r301, 1, &ix, NULL, &xyBounds, err);
                    _eos_RageBiRationalInterp (1, nr301, nt301, r301, t301,
                                               e301, &rho, &t, &e, &dedr,
                                               &dedt, &xyBounds, err);
                    (*el401)[i] = e;
                  }
                }               /* i-loop */
              }                 /* if (ix1 < nt401-1) */
            }                   /* if ((p > room_p && p < (EOS_REAL) 1.E07) */
          }                     /* if (k < 3) */
        }                       /* if (i > 0) */
      }                         /* if (i< nt401-1) */
    }                           /* if (t401[nt401-1] > room_temp && t401[0] < room_temp) */
    else if ((*t401)[*nt401 - 1] == room_temp) {
      if ((*p401)[*nt401 - 1] > room_p)
        (*p401)[*nt401 - 1] = (EOS_REAL) 0.95 *room_p;
    }
    else if ((*t401)[0] == room_temp) {
      if ((*p401)[0] > room_p)
        (*p401)[0] = (EOS_REAL) 0.95 *room_p;
    }

    /* .....   restore pressure grid */
    for (j = 0; j < nt301; j++)
      for (i = 0; i < nr301; i++)
        p301[j][i] = savep[j * nr301 + i];

    /* .....  make sure dome energies are well behaved */
    if (*nt401 > 2) {
      for (i = *nt401 - 2; i > 0; i--) {
        if ((*el401)[i] > (*eg401)[i]) {
          if ((*el401)[i] > (*el401)[i + 1]
              && (*el401)[i - 1] < (*eg401)[i - 1])
            (*el401)[i] = (EOS_REAL) 0.5 *((*el401)[i + 1] + (*el401)[i - 1]);
        }
      }
    }
  }                             /* if (ifound > 0) */
  else {
    *err = EOS_UNDEFINED;
    eos_SetCustomMsg_str (errMsg,
                           "eos_UtilsRage::_eos_build_dome ERROR cannot construct dome");
    // call global_error('BUILD_DOME: cannot construct dome')
    //      8765   format('  found no points in steam dome, despite finding a critical point')
  }


  _eos_DEBUG_PRINT
    ("build_dome: final vapor dome constructed (nt401=%i)\n%15s %15s %15s %15s %15s %15s \n",
     *nt401, "t401", "p401", "rg401", "rl401", "eg401", "el401");

  for (i = 0; i < *nt401; i++) {
    _eos_DEBUG_PRINT ("%15.7e %15.7e %15.7e %15.7e %15.7e %15.7e\n",
                      (*t401)[i], (*p401)[i], (*rg401)[i], (*rl401)[i],
                      (*eg401)[i], (*el401)[i]);
  }
  _eos_DEBUG_PRINT ("\n");
  _eos_DEBUG_PRINT ("*** LEAVING BUILD_DOME\n");

}                               /* _eos_build_dome */

void _eos_find_critical_pt (EOS_REAL **P, EOS_REAL **E, EOS_INTEGER nR,
                            EOS_INTEGER nT, EOS_REAL *R, EOS_REAL *T,
                            EOS_INTEGER mat, EOS_REAL *savep,
                            EOS_INTEGER *idome, EOS_REAL *tcrit,
                            EOS_REAL *pcrit, EOS_REAL *rhoc, EOS_REAL *ecrit,
                            EOS_INTEGER *err)
{
  EOS_INTEGER i, j, ix, iy, k;
  EOS_INTEGER numbnd, iloc, ix2, outer_cntr, inner_cntr;
  EOS_BOOLEAN exit_loop;
  EOS_INTEGER xyBounds;
  EOS_REAL pc, t, delrho, tc, tmax, psave, p, rho, rholim, dpdrho, dedr, dedt;
  EOS_REAL dpdr, dpdt, ec, dpdrhomn, faktor = (EOS_REAL) 2.0;

  /* ..... save pressure grid */
  _eos_DEBUG_PRINT ("\n*** ENTERING FIND_CRITICAL_PT\n");

  for (i = 0; i < nR; i++)
    for (j = 0; j < nT; j++)
      savep[j * nR + i] = P[j][i];

  for (j = 0; j < nT; j++) {
    _eos_find_notpos (P, nR, j, &iloc, err);
    idome[j] = iloc;
    _eos_DEBUG_PRINT ("  idome[%i] = %i\n", j + 1,
                      iloc + ((iloc == -1) ? 0 : 1));
  }

  /* ..... remove crack model if present
     ..... find lowest t isotherm that is in the dome */

  for (j = 0; j < nT; j++) {
    if (idome[j] >= 0)
      break;
  }

  k = idome[j];
  _eos_remove_patch (P, R, &k, j, nR);
  _eos_find_notpos (P, nR, j, &iloc, err);
  if (iloc > 0)
    idome[j] = iloc;
  else {
    for (i = 0; i < nR; i++)
      P[j][j] = savep[j * nR + i];
  }

  for (k = j + 1; k < nT; k++) {
    if (idome[k] >= 0) {
      i = idome[k];
      _eos_remove_patch (P, R, &i, k, nR);
      _eos_find_notpos (P, nR, k, &iloc, err);
      if (iloc >= 0) {
        if (idome[k - 1] < 0) {
          idome[k] = -1;
          for (i = 0; i < nR; i++)
            P[k][i] = savep[k * nR + i];
        }
        else {
          if (iloc > idome[k - 1] + 1) {
            if (mat != 7761)
              idome[k] = -1;
            for (i = 0; i < nR; i++)
              P[k][i] = savep[k * nR + i];
          }
          else
            idome[k] = iloc;
          _eos_DEBUG_PRINT ("idome[%i] -> %i\n", k + 1, iloc + 1);
        }
      }                         /* if (iloc >= 0) */
      else {
        if (idome[k - 1] < 0 || idome[k] > idome[k - 1] + 1)
          idome[k] = -1;

        for (i = 0; i < nR; i++)
          P[k][i] = savep[k * nR + i];
      }                         /* if (iloc >= 0) */
    }                           /* if (idome[k-1]) */
  }                             /* j-loop */

  /* ..... ignore any unstable regions that are not connected */

  for (i = 0; i < nT; i++) {
    if (idome[i] >= 0) {
      for (j = i; j < nT; j++) {
        if (idome[j] < 0) {
          for (k = j; k < nT; k++)
            idome[k] = -1;
        }
      }                         /* j */
      break;
    }
  }                             /* i */

  numbnd = 0;
  for (j = 0; j < nT; j++) {
    if (idome[j] >= 0)
      numbnd = numbnd + 1;
  }

  _eos_DEBUG_PRINT ("numbnd = %i\n", numbnd);

  if (numbnd == 0) {
    /*
       if (doedit .and. mype.eq.iope) then
       write(*,*)' **** no dome for this material ****'
       endif
     */
    _eos_DEBUG_PRINT (" **** no dome for this material ****\n");
    *tcrit = (EOS_REAL) -1.0;
  }
  else {
    /* setup_sesdat : pack data into 1 array for rational function interpolator
       call setup_sesdat(tbls, locx, locy, loczp, locze) */

    /* .....   start search for critical point */
    for (j = nT - 1; j >= 0; j--) {
      if (idome[j] >= 0)
        break;
    }

    _eos_DEBUG_PRINT ("\nfound idome[%d] = %d\n", j + 1, idome[j] + 1);
    fflush (stderr);
    fflush (stdout);

    /* .....   upper and lower bounds on critical temperature */
    _eos_DEBUG_PRINT
      ("Find upper and lower bounds on critical temperature.\n");
    if (j < nT - 1) {
      tmax = T[j + 1];
      tc = T[j];
    }
    else {
      tmax = T[nT - 1];
      tc = T[nT - 2];
    }
    _eos_DEBUG_PRINT (" found Tupper = %23.14e, Tlower = %23.14e\n", tmax,
                      tc);
    _eos_DEBUG_PRINT ("    from the following temperature data:\n");
    for (i = 0; i < nT; i++)
      _eos_DEBUG_PRINT ("    T[%i] = %23.14e\n", i + 1, T[i]);

    /* .....   do not go into first zone looking for critical pt
       .....   this zone is usually not believable */

    dpdrhomn = (EOS_REAL) 1.0E30;
    _eos_DEBUG_PRINT ("\nSearch for critical point density (rhoc)\n");
    for (i = nR - 1; i > 1; i--) {
      if (P[j][i] <= (EOS_REAL) 0.0 || P[j][i - 1] <= (EOS_REAL) 0.0
          || P[j][i] <= P[j][i - 1]) {
        *rhoc = R[i];
        break;
      }

      dpdrho = (P[j][i] - P[j][i - 1]) / (R[i] - R[i - 1]);
      if (dpdrho > (EOS_REAL) 0.0) {
        if (dpdrho < dpdrhomn) {
          dpdrhomn = dpdrho;
          *rhoc = (EOS_REAL) .5 *(R[i] + R[i - 1]);
        }
      }
      else {
        *rhoc = (EOS_REAL) .5 *(R[i] + R[i - 1]);
        break;
      }

      _eos_DEBUG_PRINT ("i=%i, rhoc=%23.14e\n", i + 1, *rhoc);

    }                           /* for i */

    /*
       if (doedit .and. mype.eq.iope) then
       write(*,"(/,'  tc between ',1p,e12.5,' and ',e12.5,6x,'rhoc = ',e12.5)")tc,tmax,rhoc
       endif
     */
    _eos_DEBUG_PRINT
      ("(  tc between  %12.5e and %12.5e      rhoc = %12.5e)\n\n", tc, tmax,
       *rhoc);

    /*   start binary search for critical point */
    _eos_DEBUG_PRINT ("start binary search for critical point\n");

    rho = MIN (faktor * (*rhoc), R[nR - 2]);
    _eos_srchdf (1, &rho, 1, nR, R, 1, &ix, NULL, &xyBounds, err);
    ix2 = ix;
    rholim = *rhoc / ((EOS_REAL) 4.0 * faktor);
    delrho = (EOS_REAL) -0.01 * rho;

    _eos_DEBUG_PRINT
      ("INITIAL rho = %23.14e, ix = %i, rholim = %23.14e, delrho = %23.14e\n",
       rho, ix + 1, rholim, delrho);

    k = 0;
    outer_cntr = inner_cntr = 0;
    while (1) {
      outer_cntr++;
      /* _eos_DEBUG_PRINT("top, outer loop(%8i): tmax=%12.5e, tc=%12.5e\n",outer_cntr,tmax,tc); */

      if ((tmax - tc) < (EOS_REAL) 1.0E-07 * tc)
        break;
      k = k + 1;
      if (k > 20 && tc == (EOS_REAL) 0.0)
        break;
      t = (EOS_REAL) .5 *(tc + tmax);
      _eos_srchdf (1, &t, 1, nT, T, 1, &iy, NULL, &xyBounds, err);
      p = (EOS_REAL) 1.0E30;
      exit_loop = EOS_FALSE;

      while (1) {
        if (exit_loop)
          break;

        inner_cntr++;
        //_eos_DEBUG_PRINT("  top, inner loop(%8i): tmax=%12.5e, tc=%12.5e, p=%12.5e, t=%12.5e, rho=%12.5e, delrho=%12.5e\n",inner_cntr,tmax,tc,p,t,rho,delrho);

        /* .....    step through each cell along this isotherm */

        rho = rho + delrho;
        _eos_srchdf (1, &rho, 1, nR, R, 1, &ix, NULL, &xyBounds, err);
        //_eos_DEBUG_PRINT("  outer loop(%8i), inner loop(%8i): ix=%i, iy=%i\n",outer_cntr,inner_cntr,ix+1,iy+1);

        if (ix != ix2) {
          ix2 = ix;
          if (ix < nR - 1)
            delrho = (EOS_REAL) -0.01 * (R[ix + 1] - R[ix]);
          else
            delrho = (EOS_REAL) -0.01 * R[nR - 1];
        }

        if (rho < rholim) {
          tmax = t;
          exit_loop = EOS_TRUE;
          rho = MIN (faktor * (*rhoc), R[nR - 2]);
          rholim = *rhoc / ((EOS_REAL) 4.0 * faktor);
          delrho = (EOS_REAL) -0.01 * (*rhoc);
        }
        else {
          psave = p;
          /* interpolate P at at rhoc, t to get p  */
          _eos_RageBiRationalInterp (1, nR, nT, R, T, P, &rho, &t, &p, &dpdr,
                                     &dpdt, &xyBounds, err);

          //_eos_DEBUG_PRINT("  rat. interp. results: rho=%12.5e, t=%12.5e, p=%12.5e\n",rho,t,p);

          if (p >= psave || p <= (EOS_REAL) 0.0) {
            tc = t;
            exit_loop = EOS_TRUE;
            *rhoc = rho - delrho;
            rho = MIN (faktor * (*rhoc), R[nR - 2]);
            rholim = rho / ((EOS_REAL) 4.0 * faktor);
            delrho = (EOS_REAL) -0.01 * (*rhoc);
          }
        }
      }                         /* while 1 (exit _loop) */
    }                           /* while(1) */

    _eos_DEBUG_PRINT
      ("\nend binary search for critical point (outer_cntr=%i, inner_cntr=%i)\n\n",
       outer_cntr, inner_cntr);

    if (tc == (EOS_REAL) 0.0)
      *tcrit = (EOS_REAL) -1.0;
    else {
      _eos_DEBUG_PRINT
        ("\njust before rat. interp.: tc = %23.14e, rhoc = %23.14e\n\n", tc,
         *rhoc);

      /* interpolate P at rhoc, tc to get pc  */
      _eos_RageBiRationalInterp (1, nR, nT, R, T, P, rhoc, &tc, &pc, &dpdr,
                                 &dpdt, &xyBounds, err);

      /* interpolate P at rhoc, tc to get ec */
      _eos_RageBiRationalInterp (1, nR, nT, R, T, E, rhoc, &tc, &ec, &dedr,
                                 &dedt, &xyBounds, err);

      *pcrit = pc;
      *tcrit = tc;
      *ecrit = ec;
      /*
         if (doedit .and. mype.eq.iope) then
         write(*,1000)tcrit,rhoc,pcrit,ecrit
         endif
       */
      _eos_DEBUG_PRINT
        ("********************************************************************************\n");
      _eos_DEBUG_PRINT
        ("tc =  %13.6e  rhoc = %13.6e  pc = %13.6e  ec = %13.6e\n", *tcrit,
         *rhoc, *pcrit, *ecrit);
      _eos_DEBUG_PRINT
        ("********************************************************************************\n");
    }
  }                             /* if (numbnd!=0) */

  _eos_DEBUG_PRINT
    ("\nfound critical point: pcrit = %23.14e, tcrit = %23.14e, ecrit = %23.14e, rhoc = %23.14e\n\n",
     *pcrit, *tcrit, *ecrit, *rhoc);

  _eos_DEBUG_PRINT ("*** LEAVING FIND_CRITICAL_PT\n");
}

void _eos_sesame_vapor_t (EOS_REAL t, EOS_INTEGER vapor_num,
                          EOS_REAL *vapor_p, EOS_REAL *vapor_t,
                          EOS_REAL *vapor_vl, EOS_REAL *vapor_vg,
                          EOS_REAL *vapor_rl, EOS_REAL *vapor_rg,
                          EOS_REAL *vapor_el, EOS_REAL *vapor_eg, EOS_REAL *p,
                          EOS_REAL *rg, EOS_REAL *rl, EOS_REAL *vg,
                          EOS_REAL *vl, EOS_REAL *eg, EOS_REAL *el,
                          EOS_REAL *dedvl)
     /* given t, vapor_num, vapor_t, compute p, rg, rl, vg, vl, eg, el, dedvl */
{
  EOS_BOOLEAN use_log;
  EOS_INTEGER itlo, ithi, it;
  EOS_REAL s;

  //_eos_DEBUG_PRINT("\n*** ENTERING SESAME_VAPOR_T\n");

  *p = (EOS_REAL) 0.0;
  *rg = (EOS_REAL) 0.0;
  *rl = (EOS_REAL) 0.0;
  *vg = (EOS_REAL) 0.0;
  *vl = (EOS_REAL) 0.0;
  *eg = (EOS_REAL) 0.0;
  *el = (EOS_REAL) 0.0;

  if (vapor_num >= 0) {
    if (t < vapor_t[vapor_num - 1]) {

      if (t <= vapor_t[0]) {
        *p = (EOS_REAL) 0.0;
        *vl = vapor_vl[0];
        *vg = vapor_vg[0];
        *el = vapor_el[0];
        *eg = vapor_eg[0];
        *dedvl = (EOS_REAL) 0.0;
      }
      else {
        itlo = 0;
        ithi = vapor_num;
        while (ithi - itlo > 1) {
          it = (itlo + ithi) / 2;
          if (t >= vapor_t[it])
            itlo = it;
          else
            ithi = it;
        }

        ithi = itlo + 1;

        use_log = (vapor_p[itlo] > (EOS_REAL) 0.0
                   && vapor_p[ithi] > (EOS_REAL) 0.0)?EOS_TRUE:EOS_FALSE;

        s = (t - vapor_t[itlo]) / (vapor_t[ithi] - vapor_t[itlo]);

        if (use_log)
          *p =
            exp (log (vapor_p[itlo]) +
                 s * (log (vapor_p[ithi]) - log (vapor_p[itlo])));
        else
          *p = vapor_p[itlo] + s * (vapor_p[ithi] - vapor_p[itlo]);

        *vg =
          (EOS_REAL) 1.0 / (vapor_rg[itlo] +
                            s * (vapor_rg[ithi] - vapor_rg[itlo]));
        *vl = vapor_vl[itlo] + s * (vapor_vl[ithi] - vapor_vl[itlo]);
        *el = vapor_el[itlo] + s * (vapor_el[ithi] - vapor_el[itlo]);
        *eg = vapor_eg[itlo] + s * (vapor_eg[ithi] - vapor_eg[itlo]);
        if (vapor_vl[ithi] != vapor_vl[itlo])
          *dedvl =
            (vapor_el[ithi] - vapor_el[itlo]) / (vapor_vl[ithi] -
                                                 vapor_vl[itlo]);
        else
          *dedvl = (EOS_REAL) 0.0;

        if (use_log && *p > (EOS_REAL) 0.0) {
          *vg =
            (EOS_REAL) 1.0 / (vapor_rg[itlo] *
                              exp (log (*p / vapor_p[itlo]) *
                                   log (vapor_rg[ithi] / vapor_rg[itlo]) /
                                   log (vapor_p[ithi] / vapor_p[itlo])
                              ));
          *vl =
            (EOS_REAL) 1.0 / (vapor_rl[itlo] * exp (log (*p / vapor_p[itlo])
                                                    * log (vapor_rl[ithi] /
                                                           vapor_rl[itlo])
                                                    / log (vapor_p[ithi] /
                                                           vapor_p[itlo])));
        }
      }                         /* t > vapor_t[0] */

      *rg = (EOS_REAL) 1.0 / *vg;
      *rl = (EOS_REAL) 1.0 / *vl;

    }                           /* if (t < vapor_t[vapor_num-1]) */
  }                             /* if (vapor_num >= 0) */

  //_eos_DEBUG_PRINT("*** LEAVING SESAME_VAPOR_T\n");
}

void _eos_CheckTable (EOS_INTEGER nR, EOS_INTEGER nT, EOS_REAL *R,
                      EOS_REAL *T, EOS_REAL **P, EOS_REAL **E,
                      EOS_INTEGER mat, EOS_INTEGER nt401,
                      EOS_INTEGER *table_type, EOS_BOOLEAN *table_good)
{
/* checks the table, sets the table_type, table_good */

  EOS_BOOLEAN flat, down;
  EOS_INTEGER it, ir, k;
  EOS_REAL pavg;
  EOS_REAL *p301copy;

  *table_good = EOS_TRUE;
  p301copy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nR);

/* .....   determine type of table */

  down = EOS_FALSE;
  flat = EOS_FALSE;

  for (it = 0; it < nT; it++) {
    for (ir = 0; ir < nR; ir++)
      p301copy[ir] = P[it][ir];

/* .....     remove some single point loops */
    for (ir = 0; ir < nR - 3; ir++) {
      if (p301copy[ir] < p301copy[ir + 3] &&
          p301copy[ir] < p301copy[ir + 1] &&
          p301copy[ir + 1] > p301copy[ir + 2] &&
          p301copy[ir + 2] < p301copy[ir + 3]) {
        pavg = (EOS_REAL) 0.5 *(p301copy[ir + 1] + p301copy[ir + 2]);
        if (pavg <= p301copy[ir] && pavg >= p301copy[ir + 3])
          pavg = (EOS_REAL) 0.5 *(p301copy[ir] + p301copy[ir + 3]);
        p301copy[ir + 1] = pavg;
        p301copy[ir + 2] = pavg;
      }
    }

/* .....     remove some double point loops */

    for (ir = 0; ir < nR - 4; ir++) {
      if (p301copy[ir] < p301copy[ir + 4] &&
          p301copy[ir] < p301copy[ir + 1] &&
          p301copy[ir + 1] > p301copy[ir + 2] &&
          p301copy[ir + 2] > p301copy[ir + 3] &&
          p301copy[ir + 3] < p301copy[ir + 4]) {
        pavg =
          ((EOS_REAL) 1.0 / (EOS_REAL) 3.0) * (p301copy[ir + 1] +
                                               p301copy[ir + 2] +
                                               p301copy[ir + 3]);
        if (pavg <= p301copy[ir] || pavg >= p301copy[ir + 4])
          pavg = (EOS_REAL) 0.5 *(p301copy[ir] + p301copy[ir + 4]);
        p301copy[ir + 1] = pavg;
        p301copy[ir + 2] = pavg;
        p301copy[ir + 3] = pavg;
      }
    }

/* .....     make almost flat into flat */

    for (ir = 0; ir < nR - 1; ir++) {
      if (p301copy[ir] > (EOS_REAL) 0.0 && p301copy[ir + 1] > (EOS_REAL) 0.0) {
        if (p301copy[ir + 1] < p301copy[ir]
            && p301copy[ir + 1] > (EOS_REAL) 0.9999 * p301copy[ir])
          p301copy[ir + 1] = p301copy[ir];
      }
    }

/* .....     determine if the loops have been removed */

    k = 0;
    for (ir = 1; ir < nR; ir++) {
      if (p301copy[ir] <= (EOS_REAL) 0.0)
        k = 0;
      else if (p301copy[ir] != p301copy[ir - 1])
        k = 0;
      else {
        k = k + 1;
        if (k != 3 && p301copy[ir] <= PDOMEMAX) {
          flat = EOS_TRUE;
          break;
        }
      }
    }

/* .....     determine if the loops exist */
    for (ir = 1; ir < nR; ir++) {
      if (p301copy[ir - 1] > (EOS_REAL) 0.0 &&
          p301copy[ir] > (EOS_REAL) 0.0 && p301copy[ir] < p301copy[ir - 1]) {
        down = EOS_TRUE;
        break;
      }
    }
  }                             /* it-loop */

/* .....   you can not have it both ways */

  if (flat && down)
    *table_good = EOS_FALSE;

/* .....   set table type */

  if (down)
    *table_type = -1;
  else if (flat)
    *table_type = 0;
  else
    *table_type = 1;

  EOS_FREE (p301copy);
}

void _eos_sesame_fit2_c (EOS_INTEGER code, EOS_INTEGER num, EOS_REAL *x,
                         EOS_REAL *y, EOS_REAL *c)
/*
!       Determine the coefficents for a quadratic least square fit y = f(x) with the
!       option of forcing the fit thru (x[code], y[code]) if code >= 0
*/
{
  EOS_INTEGER n, j, i;
  EOS_REAL u, v, denom, a[3][3], b[3];
/*
        if (code.lt.0) then
          call global_error('SESAME_FIT2_C: code.lt.0')
        else if (code.gt.num) then
          call global_error('SESAME_FIT2_C: code.gt.num')
        endif
*/
  if (code >= num)
    return;

  for (i = 0; i < 3; i++) {
    b[i] = (EOS_REAL) 0.0;
    for (j = 0; j < 3; j++)
      a[i][j] = 0;
  }

  if (code < 0) {               /* normal fit, don't force fit through the point */
    /*
       if (num.lt.3) then
       call global_error('SESAME_FIT2_C: code.eq.0 .and. num.lt.3')
       endif
     */
    if (num < 3)
      return;

    for (n = 0; n < num; n++) {
      a[0][0] = a[0][0] + (EOS_REAL) 1.0;
      a[0][1] = a[0][1] + x[n];
      a[0][2] = a[0][2] + x[n] * x[n];
      a[1][2] = a[1][2] + x[n] * x[n] * x[n];
      a[2][2] = a[2][2] + x[n] * x[n] * x[n] * x[n];
      b[0] = b[0] + y[n];
      b[1] = b[1] + y[n] * x[n];
      b[2] = b[2] + y[n] * x[n] * x[n];
    }

    a[1][0] = a[0][1];
    a[1][1] = a[0][2];
    a[2][0] = a[0][2];
    a[2][1] = a[1][2];

    denom = a[0][0] * (a[1][1] * a[2][2] - a[1][2] * a[2][1])
      - a[1][0] * (a[0][1] * a[2][2] - a[0][2] * a[2][1])
      + a[2][0] * (a[0][1] * a[1][2] - a[0][2] * a[1][1]);

    c[0] = (b[0] * (a[1][1] * a[2][2] - a[1][2] * a[2][1])
            - b[1] * (a[0][1] * a[2][2] - a[0][2] * a[2][1])
            + b[2] * (a[0][1] * a[1][2] - a[0][2] * a[1][1])) / denom;

    c[1] = (b[0] * (a[1][2] * a[2][0] - a[1][0] * a[2][2])
            - b[1] * (a[0][2] * a[2][0] - a[0][0] * a[2][2])
            + b[2] * (a[0][2] * a[1][0] - a[0][0] * a[1][2])) / denom;

    c[2] = (b[0] * (a[1][0] * a[2][1] - a[1][1] * a[2][0])
            - b[1] * (a[0][0] * a[2][1] - a[0][1] * a[2][0])
            + b[2] * (a[0][0] * a[1][1] - a[0][1] * a[1][0])) / denom;

  }
  else {
/* .....   force the fit thru point (x[code],y[code]) */

    if (num < 2)
      return;
//            call global_error('SESAME_FIT2_C: code.gt.0 .and. num.lt.2')

    for (n = 0; n < num; n++) {
      if (n != code) {
        u = x[n] - x[code];
        v = y[n] - y[code];
        a[0][0] = a[0][0] + u * u;
        a[0][1] = a[0][1] + u * u * u;
        a[1][1] = a[1][1] + u * u * u * u;
        b[0] = b[0] + v * u;
        b[1] = b[1] + v * u * u;
      }
    }
    a[1][0] = a[0][1];

    denom = a[0][0] * a[1][1] - a[1][0] * a[0][1];
    c[0] = y[code];
    c[1] = (b[0] * a[1][1] - b[1] * a[0][1]) / denom;
    c[2] = (b[1] * a[0][0] - b[0] * a[1][0]) / denom;

    c[0] = c[0] - (c[1] - c[2] * x[code]) * x[code];
    c[1] = c[1] - (EOS_REAL) 2.0 *c[2] * x[code];
  }
}

/* NOTE: P and E have cold curve added back in */
void _eos_sesame_vapor_build (EOS_REAL **P, EOS_REAL **E, EOS_INTEGER nR,
                              EOS_INTEGER nT, EOS_REAL *R, EOS_REAL *T,
                              EOS_REAL *V, EOS_BOOLEAN *build301,
                              EOS_INTEGER mat, EOS_INTEGER *nt401,
                              EOS_REAL *avgAtomicNumber401,
                              EOS_REAL *avgAtomicWgt401,
                              EOS_REAL *refDensity401, EOS_REAL **p401,
                              EOS_REAL **t401, EOS_REAL **rg401,
                              EOS_REAL **rl401, EOS_REAL **eg401,
                              EOS_REAL **el401, EOS_REAL **ag401,
                              EOS_REAL **al401, EOS_INTEGER *num_vapor,
                              EOS_INTEGER table_type, EOS_REAL **vapor_t,
                              EOS_REAL **vapor_p, EOS_REAL **vapor_rl,
                              EOS_REAL **vapor_rg, EOS_REAL **vapor_vl,
                              EOS_REAL **vapor_vg, EOS_REAL **vapor_el,
                              EOS_REAL **vapor_eg, EOS_BOOLEAN *found_401,
                              EOS_INTEGER *irlo301, EOS_INTEGER *irhi301,
                              EOS_REAL adjustVapPres, EOS_INTEGER *err, EOS_CHAR **errMsg)
     /* returns num_vapor */
{
  //    char bad_point[3];
  EOS_INTEGER bad, find_crit, bad_dome, vapor_num;
  EOS_INTEGER i, it, ir, ir1, ir2, ir3, itadd, nfit, itlo, ithi;
  EOS_INTEGER irlo, irhi, n, loop, xyBounds = EOS_OK;
  EOS_REAL drdp, dedv, r, v, e, p, t, dedv_g, dedt, elo, ehi, dpdt, tmp_dpdr;
  EOS_REAL sum_1, sum_v, sum_e, sum_vv, sum_ve, cnst;
  EOS_REAL avg_e, sigma, sr;
  EOS_REAL test, coef[3];
  EOS_REAL plg, rg, rl, vg, vl, eg, el, dedvl;
  EOS_REAL pcrit, tcrit, rcrit, ecrit, pmin, pmax, s, scale;
  EOS_REAL adjust_prs;
  char *bad_point = NULL;

  EOS_REAL *savep = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nR * nT);
  EOS_INTEGER *idome = (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * nT);

  EOS_BOOLEAN justCalled_BUILD_DOME = EOS_FALSE;

  _eos_DEBUG_PRINT ("\n*** ENTERING SESAME_VAPOR_BUILD\n");

  scale = (EOS_REAL) 1.0;
  find_crit = EOS_FALSE;

  /* compute scale */
  for (n = 0; n < fudge_num; n++) {
    if (fudge_mat[n] == mat) {
      scale = fudge_scale[n];
      find_crit = fudge_crit[n];

      if (scale > (EOS_REAL) 1.0 || scale < (EOS_REAL) 0.0) {
        *err = EOS_UNDEFINED;
        eos_SetCustomMsg_str (errMsg,
			      "eos_UtilsRage::_eos_vapor_build ERROR scale (%.8e) is not in [0,1] range",
			      scale);
        EOS_FREE (savep);
        EOS_FREE (idome);
        return;
        //call global_error('SESAME_VAPOR_BUILD: scale.gt.1.0')
        //call global_error('SESAME_VAPOR_BUILD: scale.lt.0.0')
      }
    }
  }

  vapor_num = 0;
  /* initialize irlo, irhi */
  for (i = 0; i < nT; i++) {
    irlo301[i] = nR - 1;
    irhi301[i] = -1;
  }

  /* *found_401 is passed in, it's true when 401 table was found and loaded, false otherwise */
  if (!(*found_401) && table_type < 0 && !find_crit) {
    tcrit = -1.0;
    _eos_find_critical_pt (P, E, nR, nT, R, T, mat, savep, idome, &tcrit,
                           &pcrit, &rcrit, &ecrit, err);

    if (tcrit > (EOS_REAL) 0.0) {
      _eos_DEBUG_PRINT
        ("\nbefore BUILD_DOME call in SESAME_VAPOR_BUILD, found_401 = %s\n",
         ((*found_401) ? "true" : "false"));
      _eos_build_dome (P, E, R, T, nR, nT, avgAtomicNumber401,
                       avgAtomicWgt401, refDensity401, nt401, p401, t401,
                       rg401, rl401, eg401, el401, ag401, al401, mat, savep,
                       idome, tcrit, pcrit, rcrit, ecrit, found_401, err, errMsg);
      _eos_DEBUG_PRINT
        ("\nafter BUILD_DOME call in SESAME_VAPOR_BUILD, found_401 = %s\n\n",
         ((*found_401) ? "true" : "false"));
      /* this will set found_401. if not set */
      justCalled_BUILD_DOME = EOS_TRUE;
    }
    else {
      *err = EOS_UNDEFINED;     /* no critical point found for this material */
      eos_SetCustomMsg_str (errMsg,
			    "eos_UtilsRage::_eos_vapor_build ERROR critical point not found");
      EOS_FREE (savep);
      EOS_FREE (idome);
      return;
    }
  }

  if (*found_401) {
    if (justCalled_BUILD_DOME)
      _eos_DEBUG_PRINT ("SESAME_VAPOR_BUILD just called BUILD_DOME\n\n");

    /* .... adjust to vapor dome to insure ambient conditions are reasonable */
    adjust_prs = adjustVapPres;

    if (adjust_prs == (EOS_REAL) 0.0) {
      n = 0;
      while (fudge_mat2[n] > 0) {
        if (fudge_mat2[n] == mat) {
          adjust_prs = fudge_matdef2[n];
          break;
        }
        n = n + 1;
      }
    }

    if (adjust_prs != (EOS_REAL) 0.0) {
      //              if (doedit .and. mype.eq.iope) then
      //               write(*,"(/,1x,a,i8,1p,2e12.4)") &
      //                     'Adjusting vapor pressures: mat, matdef(2,mat), adjust_prs = ', &
      //                      sesame_mat,matdef(2,1),adjust_prs
      _eos_DEBUG_PRINT
        (" Adjusting vapor pressures: mat, matdef(2,mat), adjust_prs = %8i %12.4e%12.4e\n",
         mat, adjustVapPres, adjust_prs);
      for (it = 0; it < *nt401; it++)
        (*p401)[it] =
          (*p401)[it] - adjust_prs * ((EOS_REAL) 1.0 -
                                      (*p401)[it] / (*p401)[*nt401 - 1]);
    }

    itadd = 0;
    while (itadd < *nt401 - 1) {
      if ((*p401)[itadd] <= (EOS_REAL) 0.0)
        itadd = itadd + 1;
      else
        break;
    }

    if (itadd > 0)
      itadd -= 1;

    vapor_num = *nt401 - itadd;

    if (vapor_num < 0) {
      //            call global_error('SESAME_VAPOR_BUILD: vapor_num.le.0')
      *err = EOS_UNDEFINED;
      eos_SetCustomMsg_str (errMsg,
			    "eos_UtilsRage::_eos_vapor_build ERROR vapor_num (%i) < 0",
			    vapor_num);
      EOS_FREE (savep);
      EOS_FREE (idome);
      return;
    }

    *vapor_t = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_p = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_rl = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_rg = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_vl = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_vg = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_el = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_eg = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));

    _eos_DEBUG_PRINT ("%2s \t%12s %12s %12s %12s %12s %12s %12s %12s\n",
                      "it",
                      "vapor_t", "vapor_p",
                      "vapor_rl", "vapor_rg",
                      "vapor_vl", "vapor_vg", "vapor_el", "vapor_eg");
    for (it = 0; it < vapor_num; it++) {
      (*vapor_t)[it] = (*t401)[it + itadd];
      (*vapor_p)[it] = (*p401)[it + itadd];
      (*vapor_rl)[it] = MAX (RSMALL, (*rl401)[it + itadd]);
      (*vapor_rg)[it] = MAX (RSMALL, (*rg401)[it + itadd]);
      (*vapor_vl)[it] = (EOS_REAL) 1.0 / (*vapor_rl)[it];
      (*vapor_vg)[it] = (EOS_REAL) 1.0 / (*vapor_rg)[it];
      (*vapor_el)[it] = (*el401)[it + itadd];
      (*vapor_eg)[it] = (*eg401)[it + itadd];
      _eos_DEBUG_PRINT
        ("%2i.\t%12.5e %12.5e %12.5e %12.5e %12.5e %12.5e %12.5e %12.5e\n",
         it + 1, (*vapor_t)[it], (*vapor_p)[it], (*vapor_rl)[it],
         (*vapor_rg)[it], (*vapor_vl)[it], (*vapor_vg)[it], (*vapor_el)[it],
         (*vapor_eg)[it]);
    }

    /* .....insure decreasing liquid densities */

    for (ir = vapor_num - 2; ir >= 0; ir--) {
      (*vapor_rl)[ir] =
        MAX ((*vapor_rl)[ir], (EOS_REAL) 1.00000001 * (*vapor_rl)[ir + 1]);
      (*vapor_vl)[ir] = (EOS_REAL) 1.0 / (*vapor_rl)[ir];
    }

    if (table_type == 0) {
      for (it = 0; it < nT; it++) {
        //  _eos_sesame_vapor_t(t301[it], plg, rg, rl, vg, vl, eg, el, dedvl)
        _eos_sesame_vapor_t (T[it], vapor_num, *vapor_p, *vapor_t, *vapor_vl,
                             *vapor_vg, *vapor_rl, *vapor_rg, *vapor_el,
                             *vapor_eg, &plg, &rg, &rl, &vg, &vl, &eg, &el,
                             &dedvl);
        if (rl > rg) {
          for (ir = 0; ir < nR; ir++) {
            if (R[ir] >= rg && R[ir] < rl) {
              irlo301[it] = MIN (irlo301[it], ir);
              irhi301[it] = MAX (irhi301[it], ir);
            }
          }                     /* nr loop */
        }
      }                         /* nt loop */
    }
  }                             /* if found401 */
  else if (table_type < 0) {
    _eos_find_critical_pt (P, E, nR, nT, R, T, mat, savep, idome, &tcrit,
                           &pcrit, &rcrit, &ecrit, err);

    if (tcrit <= T[1]) {
      *err = EOS_UNDEFINED;
      eos_SetCustomMsg_str (errMsg,
			    "eos_UtilsRage::_eos_vapor_build ERROR tcrit (%.8e) <= T[1] (%.8e)",
			    tcrit, T[1]);
      EOS_FREE (savep);
      EOS_FREE (idome);
      //                    call global_error('SESAME_VAPOR_BUILD: tcrit.le.t301(2)')
      return;
    }

    if (tcrit >= T[nT - 2]) {
      *err = EOS_UNDEFINED;
      eos_SetCustomMsg_str (errMsg,
			    "eos_UtilsRage::_eos_vapor_build ERROR tcrit (%.8e) >= T[nT-2] (%.8e)",
			    tcrit, T[nT - 2]);
      EOS_FREE (savep);
      EOS_FREE (idome);
      //                    call global_error('SESAME_VAPOR_BUILD: tcrit.ge.t301(nT-1)')
      return;
    }

    itlo = 0;
    ithi = nT - 1;

    while (ithi - itlo > 1) {
      it = (itlo + ithi) / 2;
      if (tcrit >= T[it])
        itlo = it;
      else
        ithi = it;
    }

    vapor_num = itlo + 1;

    /*
       if (mype.eq.iope) then
       write(*,"(a,i6,1p,3e12.4)")'$$$ SESAME_VAPOR_BUILD: num, tcrit, pcrit, rcrit = ', &
       vapor_num, tcrit, pcrit, rcrit
     */
    _eos_DEBUG_PRINT
      ("$$$ SESAME_VAPOR_BUILD: num, tcrit, pcrit, rcrit = %6i %12.4e%12.4e%12.4e\n",
       vapor_num, tcrit, pcrit, rcrit);

    *vapor_t = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_p = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_rl = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_rg = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_vl = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_vg = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_el = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
    *vapor_eg = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));

    for (i = 0; i <= vapor_num; i++) {
      (*vapor_t)[i] = (EOS_REAL) 0.0;
      (*vapor_p)[i] = (EOS_REAL) 0.0;
      (*vapor_rg)[i] = (EOS_REAL) 0.0;
      (*vapor_rl)[i] = (EOS_REAL) 0.0;
      (*vapor_eg)[i] = (EOS_REAL) 0.0;
      (*vapor_el)[i] = (EOS_REAL) 0.0;
      (*vapor_vg)[i] = (EOS_REAL) 0.0;
      (*vapor_vl)[i] = (EOS_REAL) 0.0;
      if (i < vapor_num)
        (*vapor_t)[i] = T[i];
    }

    (*vapor_t)[vapor_num - 1] = tcrit;
    (*vapor_p)[vapor_num - 1] = pcrit;
    (*vapor_rl)[vapor_num - 1] = rcrit;
    (*vapor_rg)[vapor_num - 1] = rcrit;
    (*vapor_el)[vapor_num - 1] = ecrit;
    (*vapor_eg)[vapor_num - 1] = ecrit;

    irlo = 0;
    irhi = nR - 1;
    while (irhi - irlo > 1) {
      ir = (irlo + irhi) / 2;
      if (rcrit >= R[ir])
        irlo = ir;
      else
        irhi = ir;
    }
    irhi = irlo + 1;

    for (it = 0; it < vapor_num; it++) {
      pmax = (EOS_REAL) 0.0;
      for (ir = 0; i <= irlo; i++)
        pmax = MAX (pmax, P[it][ir]);
      pmin = P[it][nR - 1];
      for (ir = irhi; i < nR; i++)
        pmin = MIN (pmin, P[it][ir]);
      pmin = MAX ((EOS_REAL) 0.0, pmin);
      (*vapor_p)[it] = (EOS_REAL) 0.5 *(pmin + pmax);
    }

    for (it = 0; it < vapor_num; it++) {
      for (ir = 1; i <= irlo; i++) {
        if (P[it][ir] > (*vapor_p)[it]) {
          s = ((*vapor_p)[it] - P[it][ir - 1]) / (P[it][ir] - P[it][ir - 1]);
          (*vapor_rg)[it] = R[ir - 1] + s * (R[ir] - R[ir - 1]);
          (*vapor_eg)[it] = E[it][ir - 1] + s * (E[it][ir] - E[it][ir - 1]);
        }
      }
    }

    for (it = 0; it < vapor_num; it++) {
      for (ir = irhi + 1; i < nR; i++) {
        if (P[it][ir] > (*vapor_p)[it]) {
          s = ((*vapor_p)[it] - P[it][ir - 1]) / (P[it][ir] - P[it][ir - 1]);
          (*vapor_rg)[it] = R[ir - 1] + s * (R[ir] - R[ir - 1]);
          (*vapor_eg)[it] = E[it][ir - 1] + s * (E[it][ir] - E[it][ir - 1]);
        }
      }
    }

    for (it = vapor_num - 1; it >= 0; it--) {
      if ((*vapor_rl)[it] == (EOS_REAL) 0.0) {
        (*vapor_rl)[it] = (*vapor_rl)[it + 1];
        (*vapor_el)[it] = (*vapor_el)[it + 1];
      }

      if ((*vapor_rg)[it] == (EOS_REAL) 0.0) {
        (*vapor_rg)[it] = (*vapor_rg)[it + 1];
        (*vapor_eg)[it] = (*vapor_eg)[it + 1];
      }
    }

    //            if (mype.eq.iope) then
    //              write(*,"('     n    t',11x,'p',11x,'rl',10x,'rg',10x,'el',10x,'eg')")
    _eos_DEBUG_PRINT
      ("sesame_vapor_build (!found_401 && table_type < 0): mat=%i\n", mat);
    _eos_DEBUG_PRINT
      ("     n    t           p           rl          rg          el          eg\n");

    for (it = 0; it <= vapor_num; it++) {
      (*vapor_vl)[it] = (EOS_REAL) 1.0 / (*vapor_rl)[it];
      (*vapor_vg)[it] = (EOS_REAL) 1.0 / (*vapor_rg)[it];
      //                    if (mype.eq.iope) then
      //                    write(*,"(i6,1p,6e12.4)")it, vapor_t[it], vapor_p[it], vapor_rl[it], vapor_rg[it], &
      //                                         vapor_el[it], vapor_eg[it]
      _eos_DEBUG_PRINT ("%6i  %12.4e%12.4e%12.4e%12.4e%12.4e%12.4e\n", it + 1,
                        (*vapor_t)[it], (*vapor_p)[it], (*vapor_rl)[it],
                        (*vapor_rg)[it], (*vapor_el)[it], (*vapor_eg)[it]);
    }
  }                             /* if (table_type < 0) */
  else if (table_type == 0) {
    for (it = 0; it < nT; it++) {
      for (ir = 1; ir < nR; ir++) {
        if (P[it][ir] <= P[it][ir - 1]) {
          irlo301[it] = MIN (irlo301[it], ir - 1);
          irhi301[it] = MAX (irhi301[it], ir);
        }
        else if (irhi301[it] > 0)
          break;
      }

      _eos_DEBUG_PRINT ("irlo301[%i] = %i, irhi301[%i] = %i\n", it + 1,
                        BOUND (0, nR, irlo301[it] + 1), it + 1, BOUND (0, nR,
                                                                       irhi301
                                                                       [it] +
                                                                       1));

      if (irhi301[it] == -1)
        break;
      else if (it > 0) {
        if (irlo301[it] >= irhi301[it - 1] || irhi301[it] <= irlo301[it - 1])
          break;
        else
          vapor_num = it + 1;
      }
    }                           /* it - loop */

    /* .....     build the liquid/vapor tables */

    if (vapor_num >= 0) {
      *vapor_t = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
      *vapor_p = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
      *vapor_rl = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
      *vapor_rg = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
      *vapor_vl = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
      *vapor_vg = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
      *vapor_el = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));
      *vapor_eg = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (vapor_num + 1));

      for (it = 0; it < vapor_num + 1; it++) {  /* intialize arrays to zero */
        (*vapor_t)[it] = 0.0;
        (*vapor_p)[it] = 0.0;
        (*vapor_rl)[it] = 0.0;
        (*vapor_rg)[it] = 0.0;
        (*vapor_vl)[it] = 0.0;
        (*vapor_vg)[it] = 0.0;
        (*vapor_el)[it] = 0.0;
        (*vapor_eg)[it] = 0.0;
      }

      for (it = 0; it < vapor_num; it++) {
        ir = irlo301[it];
        (*vapor_t)[it] = T[it];
        (*vapor_p)[it] = P[it][ir];

        ir1 = irlo301[it];
        ir2 = irhi301[it];
        if (ir2 - ir1 > 2) {
          ir1 = irlo301[it] + 1;
          ir2 = irhi301[it] - 1;
        }

        /* .....         least square fit the e-v two phase data (e = cnst+dedv*v) */

        sum_1 = (EOS_REAL) 0.0;
        sum_v = (EOS_REAL) 0.0;
        sum_e = (EOS_REAL) 0.0;
        sum_vv = (EOS_REAL) 0.0;
        sum_ve = (EOS_REAL) 0.0;

        for (ir = ir1; ir <= ir2; ir++) {
          sum_1 = sum_1 + (EOS_REAL) 1.0;
          sum_v = sum_v + V[ir];
          sum_e = sum_e + E[it][ir];
          sum_vv = sum_vv + V[ir] * V[ir];
          sum_ve = sum_ve + V[ir] * E[it][ir];

          _eos_DEBUG_PRINT ("*** it= %3i, ir= %3i: sum_1 = %23.14e\n", it + 1,
                            ir + 1, sum_1);
          _eos_DEBUG_PRINT ("                    : sum_v = %23.14e\n", sum_v);
          _eos_DEBUG_PRINT ("                    : sum_e = %23.14e\n", sum_e);
          _eos_DEBUG_PRINT ("                    : sum_vv = %23.14e\n",
                            sum_vv);
          _eos_DEBUG_PRINT ("                    : sum_ve = %23.14e\n",
                            sum_ve);
          _eos_DEBUG_PRINT ("                    : v301(ir) = %23.14e\n",
                            V[ir]);
          _eos_DEBUG_PRINT ("                    : e301(ir,it) = %23.14e\n",
                            E[it][ir]);

        }

        _eos_DEBUG_PRINT ("*** it= %3i         : sum_1 = %23.14e\n", it + 1,
                          sum_1);
        _eos_DEBUG_PRINT ("                    : sum_v = %23.14e\n", sum_v);
        _eos_DEBUG_PRINT ("                    : sum_e = %23.14e\n", sum_e);
        _eos_DEBUG_PRINT ("                    : sum_vv = %23.14e\n", sum_vv);
        _eos_DEBUG_PRINT ("                    : sum_ve = %23.14e\n", sum_ve);
        _eos_DEBUG_PRINT ("                    : ir1 = %i\n", ir1 + 1);
        _eos_DEBUG_PRINT ("                    : ir2 = %i\n", ir2 + 1);

        cnst =
          (sum_vv * sum_e - sum_v * sum_ve) / (sum_1 * sum_vv -
                                               sum_v * sum_v);
        dedv =
          (sum_1 * sum_ve - sum_v * sum_e) / (sum_1 * sum_vv - sum_v * sum_v);
        avg_e = sum_e / sum_1;

        _eos_DEBUG_PRINT ("                    : cnst = %23.14e\n", cnst);
        _eos_DEBUG_PRINT ("                    : dedv = %23.14e\n", dedv);
        _eos_DEBUG_PRINT ("                    : avg_e = %23.14e\n", avg_e);

        sigma = (EOS_REAL) 0.0;
        for (ir = ir1; ir <= ir2; ir++) {
          e = cnst + dedv * V[ir];
          sigma = sigma + (E[it][ir] - e) * (E[it][ir] - e);

          _eos_DEBUG_PRINT ("    it= %3i, ir= %3i: sigma^2 = %23.14e\n",
                            it + 1, ir + 1, sigma);

        }
        sigma = sqrt (sigma / sum_1);

        build301[it] = (sigma < (EOS_REAL) 0.1 * FABS (avg_e))?EOS_TRUE:EOS_FALSE;
        _eos_DEBUG_PRINT
          ("build301[%i]=%s, sigma=%23.14e < (0.1*FABS(avg_e))=%23.14e\n",
           it + 1, ((build301[it]) ? "T" : "F"), sigma,
           (EOS_REAL) 0.1 * FABS (avg_e));

        /* .....            determine the liquid density and volume */

        ir = irhi301[it];

        nfit = MIN (NFIT_MAX, nR - (ir + 1));
        if (nfit >= 3) {
          _eos_DEBUG_PRINT
            ("calling _eos_sesame_fit2_c(%i, %23.14e, %23.14e)\n...", nfit,
             R[ir + 1], P[it][ir + 1]);
          _eos_sesame_fit2_c (0, nfit, &(R[ir + 1]) /*r301(ir+1:ir+nfit) */ ,
                              &(P[it][ir + 1]) /*P(ir+1:ir+nfit,it) */ ,
                              coef);
          _eos_DEBUG_PRINT ("P[%i][%i] -> %23.14e\n\n", it + 1, ir + 2,
                            P[it][ir + 1]);
          test =
            coef[1] * coef[1] + (EOS_REAL) 4.0 *coef[2] * (P[it][ir] -
                                                           coef[0]);
          if (test < (EOS_REAL) 0.0)
            coef[2] = (EOS_REAL) 0.0;
        }
        else
          coef[2] = (EOS_REAL) 0.0;

        if (coef[2] != (EOS_REAL) 0.0) {
          (*vapor_rl)[it] =
            MAX (R[ir],
                 (-coef[1] + sqrt (test)) / ((EOS_REAL) 2.0 * coef[2]));
          (*vapor_vl)[it] = (EOS_REAL) 1.0 / (*vapor_rl)[it];
        }
        else if (ir + 2 <= nR - 1) {
          drdp = (R[ir + 2] - R[ir + 1]) / (P[it][ir + 2] - P[it][ir + 1]);
          (*vapor_rl)[it] = R[ir + 1] + drdp * (P[it][ir] - P[it][ir + 1]);
          (*vapor_vl)[it] = (EOS_REAL) 1.0 / (*vapor_rl)[it];
        }
        else {
          (*vapor_rl)[it] = R[ir + 1];
          (*vapor_vl)[it] = (EOS_REAL) 1.0 / MAX (RSMALL, (*vapor_rl)[it]);
        }

        /* .....            determine the vapor density and volume */

        ir1 = irlo301[it];
        ir2 = irlo301[it] - 1;
        ir3 = irlo301[it] - 2;
        if (ir3 >= 0) {
          dedv_g = (E[it][ir3] - E[it][ir2]) / (V[ir3] - V[ir2]);
          (*vapor_vg)[it] = ((E[it][ir1] - E[it][ir2]) +
                             (dedv_g * V[ir2] - dedv * V[ir1])) / (dedv_g -
                                                                   dedv);
          (*vapor_eg)[it] = E[it][ir2] + dedv_g * ((*vapor_vg)[it] - V[ir2]);
        }
        else {
          (*vapor_vg)[it] = V[ir1];
          (*vapor_eg)[it] = E[it][ir1];
        }

        (*vapor_rg)[it] = (EOS_REAL) 1.0 / (*vapor_vg)[it];

        /*    .....         determine the liquid energy */

        if (build301[it])
          (*vapor_el)[it] = cnst + dedv * (*vapor_vl)[it];
        else {
          if ((*vapor_rl)[it] < R[0])
            (*vapor_el)[it] = E[it][0];
          else if ((*vapor_rl)[it] >= R[nR - 1])
            (*vapor_el)[it] = E[it][nR - 1];
          else {
            irlo = 0;
            irhi = nR - 1;
            while (irhi - irlo > 1) {
              ir = (irlo + irhi) / 2;
              if ((*vapor_rl)[it] >= R[ir])
                irlo = ir;
              else
                irhi = ir;
            }

            irhi = irlo + 1;
            sr = ((*vapor_rl)[it] - R[irlo]) / (R[irhi] - R[irlo]);
            (*vapor_el)[it] = E[it][irlo] + sr * (E[it][irhi] - E[it][irlo]);
          }
        }
      }                         /* it - loop */

      /* .....       test and add approximate critical point (if possible) */

      if (vapor_num >= nT) {

        //              if (mype.eq.iope) then
        //               write(*,"(a, 2i8)")'SESAME_VAPOR_BUILD: vapor_num, nT = ', &
        //                                                       vapor_num,nT
        _eos_DEBUG_PRINT ("SESAME_VAPOR_BUILD: vapor_num, nT = %8i%8i\n",
                          vapor_num, nT);
      }
      else {
        itlo = vapor_num - 1;
        ithi = itlo + 1;

        vapor_num = vapor_num + 1;

        t = sqrt (T[ithi] * T[itlo]);

        r = sqrt ((*vapor_rl)[itlo] * (*vapor_rg)[itlo]);
        v = (EOS_REAL) 1.0 / r;

        for (ir = 0; ir < nR; ir++) {
          if (v >= V[ir]) {
            dedv = (E[ithi][ir] - E[ithi][ir - 1]) / (V[ir] - V[ir - 1]);
            ehi = E[ithi][ir - 1] + dedv * (v - V[ir - 1]);
            break;
          }
        }

        dedv =
          ((*vapor_eg)[itlo] - (*vapor_el)[itlo]) / ((*vapor_vg)[itlo] -
                                                     (*vapor_vl)[itlo]);
        elo = (*vapor_el)[itlo] + dedv * (v - (*vapor_vl)[itlo]);

        dedt = (ehi - elo) / (T[ithi] - T[itlo]);
        e = elo + dedt * (t - T[itlo]);

        /* interpolate vapor_num'th row of P at r, tc to get p */
        //_eos_RageRationalInterp(1, nR, 1, R, T, &(P[vapor_num-1]), &r, &T[vapor_num-1], &p, &tmp_dpdr, &tmp_dpdt, &xyBounds, err);
        _eos_RageRationalInterp (1, nR, R, P[vapor_num - 1], &r, &p,
                                 &tmp_dpdr, &xyBounds, err);
        dpdt = (p - (*vapor_p)[itlo]) / (T[ithi] - T[itlo]);
        p = (*vapor_p)[itlo] + dpdt * (t - T[itlo]);

        (*vapor_t)[vapor_num - 1] = t;
        (*vapor_p)[vapor_num - 1] = p;
        (*vapor_rg)[vapor_num - 1] = r;
        (*vapor_vg)[vapor_num - 1] = v;
        (*vapor_eg)[vapor_num - 1] = e;
        (*vapor_rl)[vapor_num - 1] = r;
        (*vapor_vl)[vapor_num - 1] = v;
        (*vapor_el)[vapor_num - 1] = e;

        for (it = 0; it < vapor_num; it++)
          _eos_DEBUG_PRINT ("vapor_rl[%i]= %22.12e, vapor_rg[%i]=  %22.12e\n",
                            it + 1, (*vapor_rl)[it], it + 1, (*vapor_rg)[it]);

      }                         /* if (vapor_num < nT-1) */
    }                           /* if (vapor_num >= 0) */

    /* .....    insure decreasing liquid densities */

    for (ir = vapor_num - 2; ir >= 0; ir--) {
      (*vapor_rl)[ir] =
        MAX ((*vapor_rl)[ir], (EOS_REAL) 1.00000001 * (*vapor_rl)[ir + 1]);
      (*vapor_vl)[ir] = (EOS_REAL) 1.0 / (*vapor_rl)[ir];
    }
  }                             /* if (table_type == 0) */

  *num_vapor = vapor_num;

  if (doedit && vapor_num > 0) {
    for (loop = 1; loop <= 2; loop++) {
      bad_dome = EOS_FALSE;
      _eos_DEBUG_PRINT
        ("\nsesame_vapor_build (doedit && vapor_num >= 0): mat=%i\n", mat);
      _eos_DEBUG_PRINT
        ("     n    t              p              rg             rl             eg             el\n");

      for (it = 0; it < vapor_num; it++) {
        bad = EOS_FALSE;
        if ((*vapor_rg)[it] > (*vapor_rl)[it])
          bad = EOS_TRUE;
        if ((*vapor_eg)[it] < (*vapor_el)[it])
          bad = EOS_TRUE;
        if (it > 0) {
          if ((*vapor_p)[it] < (*vapor_p)[it - 1])
            bad = EOS_TRUE;
          if ((*vapor_t)[it] < (*vapor_t)[it - 1])
            bad = EOS_TRUE;
        }

        bad_point = " ";
        if (bad) {
          bad_dome = EOS_TRUE;
          bad_point = "bad";
        }

        _eos_DEBUG_PRINT ("%6i %15.7e%15.7e%15.7e%15.7e%15.7e%15.7e %s\n",
                          it + 1, (*vapor_t)[it], (*vapor_p)[it],
                          (*vapor_rg)[it], (*vapor_rl)[it], (*vapor_eg)[it],
                          (*vapor_el)[it], bad_point);
        /*
           write(*,"(i6,1p,6e15.7,2x,a)") &
           it, vapor_t[it],vapor_p[it], &
           vapor_rg[it],vapor_rl[it], &
           vapor_eg[it],vapor_el[it],bad_point
           }
         */
      }                         /* it - loop */

      if (loop == 1 && bad_dome) {
        for (it = 0; it < vapor_num; it++) {
          (*vapor_rg)[it] = MIN ((*vapor_rg)[it], (*vapor_rl)[it]);
          (*vapor_eg)[it] = MAX ((*vapor_eg)[it], (*vapor_el)[it]);
        }
      }
      else
        break;
    }                           /* loop */

    if (bad_dome) {
      *err = EOS_UNDEFINED;
      eos_SetCustomMsg_str (errMsg,
			    "eos_UtilsRage::_eos_vapor_build ERROR bad dome");
    }

    s = (EOS_REAL) 1.0;
    for (n = 0; n < fudge_num; n++) {
      if (fudge_mat[n] == mat) {
        s = fudge_scale[n];

        if (s > (EOS_REAL) 1.0) {
          // call global_error('SESAME_VAPOR_BUILD: s.gt.1.0')
	  eos_SetCustomMsg_str (errMsg,
				"eos_UtilsRage::_eos_vapor_build ERROR fudge scale > 1");
          *err = EOS_UNDEFINED;
        }

        if (s <= (EOS_REAL) 0.0) {
          // call global_error('SESAME_VAPOR_BUILD: s.le.0.0')
          *err = EOS_UNDEFINED;
	  eos_SetCustomMsg_str (errMsg,
				"eos_UtilsRage::_eos_vapor_build ERROR fudge scale <= 0");
        }
      }
    }                           /* n-loop */

    if (scale > (EOS_REAL) 0.0 && scale < (EOS_REAL) 1.0) {
      bad = EOS_FALSE;
      //                            write(*,"(/,a,2i6,1p,e12.4)")'$$$ fudge dome: n, mid301, scale = ', &
      //                      n, mid301, scale
      //                            write(*,"(5x,'n    t',14x,'p',14x,'rg',13x,'rl',13x,'eg',13x,'el')")
      _eos_DEBUG_PRINT
        ("($$$ fudge dome: n, mid301, scale = %6i%6i %12.4e)\n", n, mat,
         scale);
      _eos_DEBUG_PRINT
        ("     n    t              p              rg             rl             eg             el\n");

      for (it = 0; it < vapor_num; it++) {
        for (n = 0; n < fudge_num; n++) {
          (*vapor_p)[it] =
            (*vapor_p)[vapor_num - 1] + ((*vapor_p)[it] -
                                         (*vapor_p)[vapor_num - 1]) / scale;
          /*
             write(*,"(i6,1p,6e15.7)") &
             it, vapor_t[it],vapor_p[it], &
             vapor_rg[it],vapor_rl[it], &
             vapor_eg[it],vapor_el[it]
           */
          _eos_DEBUG_PRINT ("%6i %15.7e%15.7e%15.7e%15.7e%15.7e%15.7e %s\n",
                            it + 1, (*vapor_t)[it], (*vapor_p)[it],
                            (*vapor_rg)[it], (*vapor_rl)[it], (*vapor_eg)[it],
                            (*vapor_el)[it], bad_point);

          if (it > 0) {
            if ((*vapor_p)[it] < (*vapor_p)[it - 1])
              bad = EOS_TRUE;
            if ((*vapor_t)[it] < (*vapor_t)[it - 1])
              bad = EOS_TRUE;
          }
        }                       /* n - loop */
      }                         /* it-loop */

      if (bad) {
        *err = EOS_UNDEFINED;
	eos_SetCustomMsg_str (errMsg,
			      "eos_UtilsRage::_eos_vapor_build ERROR bad fudged dome");
        // call global_error('SESAME_VAPOR_BUILD: bad fudged dome')
      }
    }                           /* if scale .. */
  }                             /* if (doedit && vapor_num > 0) */

  EOS_FREE (savep);
  EOS_FREE (idome);

  _eos_DEBUG_PRINT ("*** LEAVING SESAME_VAPOR_BUILD\n");
}

void _eos_sesame_vapor_crit (EOS_REAL *t, EOS_REAL *p, EOS_REAL *r,
                             EOS_REAL *v, EOS_REAL *e, EOS_INTEGER vapor_num,
                             EOS_REAL *vapor_t, EOS_REAL *vapor_p,
                             EOS_REAL *vapor_vl, EOS_REAL *vapor_el)
{
  //_eos_DEBUG_PRINT("\n*** ENTERING SESAME_VAPOR_CRIT\n");
  //_eos_DEBUG_PRINT("vapor_num = %i\n",vapor_num);

  *t = (EOS_REAL) 0.0;
  *p = (EOS_REAL) 0.0;
  *r = (EOS_REAL) 0.0;
  *v = (EOS_REAL) 0.0;
  *e = (EOS_REAL) 0.0;

  if (vapor_num >= 0) {
    *t = vapor_t[vapor_num - 1];
    *p = vapor_p[vapor_num - 1];
    *v = vapor_vl[vapor_num - 1];
    *e = vapor_el[vapor_num - 1];

    *r = (EOS_REAL) 1.0 / *v;
  }

  //_eos_DEBUG_PRINT("*** LEAVING SESAME_VAPOR_CRIT\n");
}

/************************************************************************
 * 
 * Makes data of class eos_RecordType1 pt smooth
 * 
 * Returned Values: 
 * EOS_INTEGER *err - output error code
 * EOS_BOOLEAN *found_401 is passed in, it's true when 401 table was found and loaded, false otherwise. 
 *             if 401 is not found, but generated (by build_dome) 401 data, then found_401 is set to EOS_TRUE
 *
 * Input Values (301 table):
 *    nR       = input  integer number of density values.
 *    nT       = input  integer number of temperature values.
 *    R[nR]    = input  real X array.
 *    T[nT]    = input  real Y array.
 *    P        = in/out real F array (nR * nT elements).
 *    E        = in/out real E array (nR * nT elements)..
 *    cP[nR]   = cold curve for P, P at t=0
 *    cE[nR]   = cold curve for E, E at t=0
 *    mat      = material id.
 *    
 *    from 401 table (input/output args):
 *    nt401              // Number of temperature values
 *    vaporArrayOffset   // (nt401 - num_vapor) offset
 *    avgAtomicNumber401 // mean atomic number
 *    avgAtomicWgt401    // mean atomic mass
 *    refDensity401      // normal solid density
 *    p401[nt401]        // Vapor Pressure
 *    t401[nt401]        // Temperature
 *    rg401[nt401]       // Vapor Density on Coexistence Line
 *    rl401[nt401]       // Density of Liquid or Solid on Coexistence Line
 *    eg401[nt401]       // Internal Energy of Vapor on Coexistence Line
 *    el401[nt401]       // Internal Energy of Liquid on Coexistence Line
 *    ag401[nt401]       // Free Energy of Vapor on Coexistence Line
 *    al401[nt401]       // Free Energy of Liquid on Coexistence Line
 * 
 ************************************************************************/
void _eos_FixTable (EOS_INTEGER nR, EOS_INTEGER nT, EOS_REAL *R, EOS_REAL *T,
                    EOS_REAL **P, EOS_REAL **E, EOS_INTEGER mat, EOS_REAL *cP,
                    EOS_REAL *cE, EOS_INTEGER *nt401,
                    EOS_INTEGER *vaporArrayOffset,
                    EOS_REAL *avgAtomicNumber401, EOS_REAL *avgAtomicWgt401,
                    EOS_REAL *refDensity401, EOS_REAL **p401, EOS_REAL **t401,
                    EOS_REAL **rg401, EOS_REAL **rl401, EOS_REAL **eg401,
                    EOS_REAL **el401, EOS_REAL **ag401, EOS_REAL **al401,
                    EOS_INTEGER table_type, EOS_BOOLEAN *found_401,
                    EOS_REAL adjustVapPres, EOS_INTEGER *err, EOS_CHAR **errMsg)
{
  EOS_INTEGER num_vapor = 0;
  EOS_BOOLEAN flat, down;
  EOS_INTEGER it, ir, k, irlo, irhi, *irlo301, *irhi301, i;
  EOS_REAL pavg, dpdrlo, dpdrhi, plo, rlo, phi, rhi;
  EOS_REAL tcrit, pcrit, rcrit, vcrit, ecrit, plg, rg, rl, vg, vl, eg, el,
    dedvl, power;
  EOS_REAL dedtsum, dedt, w8t;
  EOS_INTEGER table_good = 0;
  EOS_REAL *V;
  EOS_BOOLEAN *build301;
  EOS_REAL *vapor_t = NULL, *vapor_p = NULL, *vapor_rl = NULL, *vapor_rg =
    NULL, *vapor_vl = NULL, *vapor_vg = NULL, *vapor_el = NULL, *vapor_eg =
    NULL;

  V = (EOS_REAL *) malloc (nR * sizeof (EOS_REAL));
  build301 = (EOS_BOOLEAN *) malloc (nT * sizeof (EOS_BOOLEAN));
  for (it = 0; it < nT; it++)
    build301[it] = EOS_FALSE;
  irlo301 = (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * nT);
  irhi301 = (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * nT);

  _eos_DEBUG_PRINT ("\n*** ENTERING FIX_TABLE\n");

  table_good = EOS_FALSE;

  /* .....   determine type of table */
  down = EOS_FALSE;
  flat = EOS_FALSE;

  /*     if (doedit .and. mype.eq.iope) then
     write(*,"(/,a)")' ---- before loop removal'
     do it = 1,nt301
     do ir = 1,nr301
     write(*,"(' p301(',i3,',',i3,')=',es20.12)") ir,it,p301(ir,it)
     enddo ! ir
     enddo ! it
     write(*,"(a)")' ----'
     write(*,"(/,a)")'     n    tev'
     endif
   */
  /*
     _eos_DEBUG_PRINT("\n ---- before loop removal\n");
     for(it=0; it < nT; it++)
     for(ir=0; ir < nR; ir++)
     _eos_DEBUG_PRINT(" p301(%i,%i)=%20.12e\n",ir+1,it+1,P[it][ir]);
     _eos_DEBUG_PRINT(" ----\n");
   */
  _eos_DEBUG_PRINT ("\n     n    tev\n");

  for (it = 0; it < nT; it++) {
    /*          if (doedit .and. mype.eq.iope) then
       write(*,"(i6,1p,e12.4)")it,t301(it)
       endif
     */
    _eos_DEBUG_PRINT ("%6i%12.4e\n", it + 1, T[it]);

    /* .....  remove unstable points that intersect the high rho boundary */
    _eos_DEBUG_PRINT
      ("it=%i: remove unstable points that intersect the high rho boundary... \n",
       it + 1);

    for (ir = nR - 1; ir > 0; ir--) {
      irhi = ir;
      if (P[it][ir] > P[it][ir - 1])
        break;
    }

    for (ir = irhi + 1; ir < nR; ir++) {
      _eos_DEBUG_PRINT ("P[%i][%i]: %23.14e -> %23.14e\n", it + 1, ir + 1,
                        P[it][ir],
                        P[it][ir - 1] + BLKMODMIN * (R[ir] - R[ir - 1]));
      P[it][ir] = P[it][ir - 1] + BLKMODMIN * (R[ir] - R[ir - 1]);
    }

    /* .....     remove some single point loops */
    _eos_DEBUG_PRINT ("it=%i: remove some single point loops...\n", it + 1);

    for (ir = 0; ir < nR - 3; ir++) {
      if (P[it][ir] < P[it][ir + 3] &&
          P[it][ir] < P[it][ir + 1] &&
          P[it][ir + 1] > P[it][ir + 2] && P[it][ir + 2] < P[it][ir + 3]) {
        pavg = (EOS_REAL) 0.5 *(P[it][ir + 1] + P[it][ir + 2]);
        if (pavg <= P[it][ir] || pavg >= P[it][ir + 3])
          pavg = (EOS_REAL) 0.5 *(P[it][ir] + P[it][ir + 3]);
        _eos_DEBUG_PRINT ("P[%i][%i]: %23.14e -> %23.14e\n", it + 1, ir + 2,
                          P[it][ir + 1], pavg);
        _eos_DEBUG_PRINT ("P[%i][%i]: %23.14e -> %23.14e\n", it + 1, ir + 3,
                          P[it][ir + 2], pavg);
        P[it][ir + 1] = pavg;
        P[it][ir + 2] = pavg;
      }
    }                           /* single point loops */

    /* .....     remove some double point loops */
    _eos_DEBUG_PRINT ("it=%i: remove some double point loops...\n", it + 1);

    for (ir = 0; ir < nR - 3; ir++) {
      if (P[it][ir] < P[it][ir + 4] &&
          P[it][ir] < P[it][ir + 1] &&
          P[it][ir + 1] > P[it][ir + 2] &&
          P[it][ir + 2] > P[it][ir + 3] && P[it][ir + 3] < P[it][ir + 4]) {
        pavg =
          ((EOS_REAL) 1.0 / (EOS_REAL) 3.0) * (P[it][ir + 1] + P[it][ir + 2] +
                                               P[it][ir + 3]);
        if (pavg <= P[it][ir] || pavg >= P[it][ir + 4])
          pavg = (EOS_REAL) 0.5 *(P[it][ir] + P[it][ir + 4]);
        _eos_DEBUG_PRINT ("P[%i][%i]: %23.14e -> %23.14e\n", it + 1, ir + 2,
                          P[it][ir + 1], pavg);
        _eos_DEBUG_PRINT ("P[%i][%i]: %23.14e -> %23.14e\n", it + 1, ir + 3,
                          P[it][ir + 2], pavg);
        _eos_DEBUG_PRINT ("P[%i][%i]: %23.14e -> %23.14e\n", it + 1, ir + 4,
                          P[it][ir + 3], pavg);
        P[it][ir + 1] = pavg;
        P[it][ir + 2] = pavg;
        P[it][ir + 3] = pavg;
      }
    }                           /* double point loops */

    /* .....     make almost flat into flat */
    _eos_DEBUG_PRINT ("it=%i: make almost flat into flat...\n", it + 1);
    for (ir = 0; ir < nR - 1; ir++) {
      if (P[it][ir + 1] > (EOS_REAL) 0.0 && P[it][ir + 1] > (EOS_REAL) 0.0) {
        if (P[it][ir + 1] < P[it][ir]
            && P[it][ir + 1] > (EOS_REAL) 0.9999 * P[it][ir]) {
          _eos_DEBUG_PRINT ("P[%i][%i]: %23.14e -> %23.14e\n", it + 1, ir + 2,
                            P[it][ir + 1], P[it][ir]);
          P[it][ir + 1] = P[it][ir];
        }

      }
    }

    /* .....     determine if the loops have been removed */
    k = 0;
    for (ir = 1; ir < nR; ir++) {
      if (P[it][ir] <= (EOS_REAL) 0.0)
        k = 0;
      else if (P[it][ir] != P[it][ir - 1])
        k = 0;
      else {
        k = k + 1;
        if (k >= 3 && P[it][ir] <= PDOMEMAX) {
          flat = EOS_TRUE;
          /* _eos_DEBUG_PRINT(" p301(%i,%i)=%20.12e, p301(%i,%i)=%20.12e ---  FLAT\n",ir,it+1,P[it][ir-1],ir+1,it+1,P[it][ir]); */
          break;
        }
      }
      /* _eos_DEBUG_PRINT(" p301(%i,%i)=%20.12e, p301(%i,%i)=%20.12e\n",ir,it+1,P[it][ir-1],ir+1,it+1,P[it][ir]); */
    }

    /* .....     determine if the loops exist */
    for (ir = 1; ir < nR; ir++) {
      if (P[it][ir - 1] > (EOS_REAL) 0.0 &&
          P[it][ir] > (EOS_REAL) 0.0 && P[it][ir] < P[it][ir - 1]) {
        down = EOS_TRUE;
        /* _eos_DEBUG_PRINT(" p301(%i,%i)=%20.12e, p301(%i,%i)=%20.12e ---  DOWN\n",ir,it+1,P[it][ir-1],ir+1,it+1,P[it][ir]); */
        break;
      }
    }
  }                             /* it - loop */

  /*     if (doedit .and. mype.eq.iope) then
     write(*,"(/,a)")' ---- after loop removal'
     do it = 1,nt301
     do ir = 1,nr301
     write(*,"(' p301(',i3,',',i3,')=',es20.12)") ir,it,p301(ir,it)
     enddo ! ir
     enddo ! it
     write(*,"(a,/)")' ----'
     endif

     _eos_DEBUG_PRINT("\n ---- after loop removal\n");

     for(it=0; it < nT; it++)
     for(ir=0; ir < nR; ir++)
     _eos_DEBUG_PRINT(" p301(%i,%i)=%20.12e\n",ir+1,it+1,P[it][ir]);
     _eos_DEBUG_PRINT(" ----\n\n");
   */

  /* .....   you can not have it both ways */
  if (flat && down) {
    //      call global_error('SESAME_FIX_TABLE: flat.and.down')
    //      print *, " >--> WARNING:  Both flat and down are true, mat_id=", matrl_ident
    //      print *, "      This may lead to problems in the Teos file."
    //      print *, "      This is a temporary fix."
    //      flat = .false.
    *err = EOS_UNDEFINED;
    eos_SetCustomMsg_str (errMsg,
			  "WARNING: In eos_UtilsRage::_eos_FixTable the table is both flat and down, mat_id=%i\n%s\n%s",
			  mat,
			  "         This may lead to problems in the Teos file.",
			  "         This is a temporary fix.");
    flat = EOS_FALSE;
    _eos_DEBUG_PRINT
      ("WARNING: In eos_UtilsRage::_eos_FixTable the table is both flat and down, mat_id=%i\n%s\n%s\n",
       mat, "         This may lead to problems in the Teos file.",
       "         This is a temporary fix.");
/*       EOS_FREE(V); */
/*       EOS_FREE(build301); */
/*       EOS_FREE(irlo301); */
/*       EOS_FREE(irhi301); */
/*       return; */
  }

  /*.....   set table type */
  if (down)
    table_type = -1;
  else if (flat)
    table_type = 0;
  else
    table_type = 1;

  /*.....   insure dpdr.ge.0 if no loops */
  _eos_DEBUG_PRINT ("\ninsure dpdr.ge.0 if no loops...\n");
  if (table_type >= 0) {
    for (it = 0; it < nT; it++)
      for (ir = nR - 2; ir >= 0; ir--) {
        if (P[it][ir] > P[it][ir + 1]) {
          _eos_DEBUG_PRINT ("P[%i][%i]: %23.14e -> %23.14e\n", it + 1, ir + 1,
                            P[it][ir], P[it][ir + 1]);
        }
        P[it][ir] = MIN (P[it][ir], P[it][ir + 1]);
      }
  }

  /* .....   force energies to increase (constant density) */
  _eos_DEBUG_PRINT ("\nforce energies to increase (constant density)...\n");
  for (ir = 0; ir < nR; ir++) {
    for (it = 1; it < nT; it++) {
      if (E[it][ir] < E[it - 1][ir]) {
        //                if (doedit .and. mype.eq.iope .and. &
        //                    abs(e301(ir,it)-e301(ir,it-1)).gt.1.0e-4_REAL8*abs(e301(ir,it)+e301(ir,it-1))) then
        //                  write(*,620)ir,it,r301(ir),t301(it),e301(ir,it-1),e301(ir,it)
        //620     format('  Adjust energies: ir, it, rho, tev, sielo, siehi = ', &
        //                  2i5,1p,2e12.4,2e20.12)
        if (FABS (E[it][ir] - E[it - 1][ir]) >
            1.0e-4 * FABS (E[it][ir] + E[it - 1][ir]))
          _eos_DEBUG_PRINT
            ("  Adjust energies: ir, it, rho, tev, sielo, siehi = %5i%5i %12.4e%12.4e%20.12e%20.12e\n",
             ir + 1, it + 1, R[ir], T[it], E[it - 1][ir], E[it][ir]);

        /* .....         pick average cv around this point */

        dedtsum = (EOS_REAL) 0.0;
        w8t = (EOS_REAL) 0.0;
        for (i = ir - 1; i <= ir + 1; i++) {
          if (i >= 0) {
            if (i < nR) {
              dedt = E[it][i] - E[it - 1][i];
              if (dedt > (EOS_REAL) 0.0) {
                dedt = dedt / (T[it] - T[it - 1]);
                dedtsum = dedtsum + dedt;
                w8t = w8t + (EOS_REAL) 1.0;
              }
            }
          }
        }                       /* i */

        if (it < nT - 1) {
          for (i = ir - 1; i <= ir + 1; i++) {
            if (i >= 0) {
              if (i < nR) {
                dedt = E[it + 1][i] - E[it][i];
                if (dedt > (EOS_REAL) 0.0) {
                  dedt = dedt / (T[it + 1] - T[it]);
                  dedtsum = dedtsum + dedt;
                  w8t = w8t + (EOS_REAL) 1.0;
                }
              }
            }
          }                     /* i loop */
        }
        if (w8t > (EOS_REAL) 0.0)
          dedt = dedtsum / w8t;
        else
          dedt = (EOS_REAL) 1.e4;
        _eos_DEBUG_PRINT ("E[%i][%i]: %23.14e -> %23.14e\n", it + 1, ir + 1,
                          E[it][ir],
                          E[it - 1][ir] + dedt * (T[it] -
                                                  T[it -
                                                    1]) / (EOS_REAL) 100.0);
        E[it][ir] =
          E[it - 1][ir] + dedt * (T[it] - T[it - 1]) / (EOS_REAL) 100.0;
      }                         /* if (E[it][ir] < e301E[it-1][ir])  */
    }                           /* it */
  }                             /* ir */

  /* .....   check data */

  for (ir = 0; ir < nR - 1; ir++) {
    if (R[ir + 1] <= R[ir]) {
      *err = EOS_UNDEFINED;
      eos_SetCustomMsg_str (errMsg,
			    "eos_UtilsRage::_eos_FixTable ERROR densities not in order");
      EOS_FREE (V);
      EOS_FREE (build301);
      EOS_FREE (irlo301);
      EOS_FREE (irhi301);
      return;
      //             call global_error('SESAME_FIX_TABLE: densities not in order')
    }
  }

  for (it = 0; it < nT - 1; it++) {
    if (T[it + 1] <= T[it]) {
      *err = EOS_UNDEFINED;
      eos_SetCustomMsg_str (errMsg,
			    "eos_UtilsRage::_eos_FixTable ERROR temperatures not in order");
      EOS_FREE (V);
      EOS_FREE (build301);
      EOS_FREE (irlo301);
      EOS_FREE (irhi301);
      return;
      //             call global_error('SESAME_FIX_TABLE: temperatures not in order')
    }
  }

  for (it = 0; it < nT - 1; it++) {
    for (ir = 0; ir < nR; ir++) {
      if (E[it + 1][ir] < E[it][ir]) {
        *err = EOS_UNDEFINED;
        eos_SetCustomMsg_str (errMsg,
			      "eos_UtilsRage::_eos_FixTable ERROR energies not in order");
        EOS_FREE (V);
        EOS_FREE (build301);
        EOS_FREE (irlo301);
        EOS_FREE (irhi301);
        return;
        //             call global_error('SESAME_FIX_TABLE: energies not in order')
      }
    }
  }

  /* .....   set V */
  for (ir = 0; ir < nR; ir++)
    V[ir] = (EOS_REAL) 1.0 / R[ir];
  _eos_DEBUG_PRINT (" ---- before sesame_vapor_build\nr301:\n");
  for (ir = 0; ir < nR; ir = ir + 4) {
    for (it = ir; it < MIN (ir + 4, nR); it++)
      _eos_DEBUG_PRINT ("%20.12e ", R[it]);
    _eos_DEBUG_PRINT ("\n");
  }
  _eos_DEBUG_PRINT (" ----\nv301:\n");
  for (ir = 0; ir < nR; ir = ir + 4) {
    for (it = ir; it < MIN (ir + 4, nR); it++)
      _eos_DEBUG_PRINT ("%20.12e ", V[it]);
    _eos_DEBUG_PRINT ("\n");
  }

  /* .....   build vapor curves */
  *err = EOS_OK;
  _eos_sesame_vapor_build (P, E, nR, nT, R, T, V, build301, mat, nt401,
                           avgAtomicNumber401, avgAtomicWgt401, refDensity401,
                           p401, t401, rg401, rl401, eg401, el401, ag401,
                           al401, &num_vapor, table_type, &vapor_t, &vapor_p,
                           &vapor_rl, &vapor_rg, &vapor_vl, &vapor_vg,
                           &vapor_el, &vapor_eg, found_401, irlo301, irhi301,
                           adjustVapPres, err, errMsg);
  if (*err) {
    EOS_FREE (V);
    EOS_FREE (build301);
    EOS_FREE (irlo301);
    EOS_FREE (irhi301);
    EOS_FREE (vapor_t);
    EOS_FREE (vapor_p);
    EOS_FREE (vapor_rl);
    EOS_FREE (vapor_rg);
    EOS_FREE (vapor_vl);
    EOS_FREE (vapor_vg);
    EOS_FREE (vapor_el);
    EOS_FREE (vapor_eg);
    return;
  }

  _eos_DEBUG_PRINT (" ---- after sesame_vapor_build\nr301:\n");
  for (ir = 0; ir < nR; ir = ir + 4) {
    for (it = ir; it < MIN (ir + 4, nR); it++)
      _eos_DEBUG_PRINT ("%20.12e ", R[it]);
    _eos_DEBUG_PRINT ("\n");
  }
  _eos_DEBUG_PRINT (" ----\nv301:\n");
  for (ir = 0; ir < nR; ir = ir + 4) {
    for (it = ir; it < MIN (ir + 4, nR); it++)
      _eos_DEBUG_PRINT ("%20.12e ", V[it]);
    _eos_DEBUG_PRINT ("\n");
  }

  /* .....   build fake loops */
  if (flat) {
    _eos_DEBUG_PRINT ("\nbuild fake loops...\n");

    _eos_sesame_vapor_crit (&tcrit, &pcrit, &rcrit, &vcrit, &ecrit, num_vapor,
                            vapor_t, vapor_p, vapor_vl, vapor_el);

    _eos_DEBUG_PRINT
      ("AFTER SESAME_VAPOR_CRIT: tcrit=%20.11e pcrit=%20.11e rcrit=%20.11e vcrit=%20.11e ecrit=%20.11e\n",
       tcrit, pcrit, rcrit, vcrit, ecrit);

    if (tcrit <= (EOS_REAL) 0.0) {
      *err = EOS_UNDEFINED;
      eos_SetCustomMsg_str (errMsg,
			    "eos_UtilsRage::_eos_FixTable ERROR tcrit=%e <= 0",
			    tcrit);
      EOS_FREE (V);
      EOS_FREE (build301);
      EOS_FREE (irlo301);
      EOS_FREE (irhi301);
      EOS_FREE (vapor_t);
      EOS_FREE (vapor_p);
      EOS_FREE (vapor_rl);
      EOS_FREE (vapor_rg);
      EOS_FREE (vapor_vl);
      EOS_FREE (vapor_vg);
      EOS_FREE (vapor_el);
      EOS_FREE (vapor_eg);
      return;
      //              call global_error('SESAME_FIX_TABLE: tcrit.le.0')
    }

    for (it = 0; it < nT; it++)
      _eos_DEBUG_PRINT ("irlo301[%3i]=%5i irhi301[%3i]=%5i\n",
                        it + 1, irlo301[it], it + 1, irhi301[it]);

    for (it = nT - 1; it >= 0; it--) {
      _eos_sesame_vapor_t (T[it], num_vapor, vapor_p, vapor_t, vapor_vl,
                           vapor_vg, vapor_rl, vapor_rg, vapor_el, vapor_eg,
                           &plg, &rg, &rl, &vg, &vl, &eg, &el, &dedvl);

      power = (EOS_REAL) 0.0;
      if (el > (EOS_REAL) 0.0 && eg > (EOS_REAL) 0.0)
        power = log (eg / el) / log (rg / rl);

      _eos_DEBUG_PRINT
        ("AFTER SESAME_VAPOR_T: rg=%25.16e rl=%25.16e eg=%25.16e el=%25.16e",
         rg, rl, eg, el);
      if (el > (EOS_REAL) 0.0 && eg > (EOS_REAL) 0.0 && rl > (EOS_REAL) 0.0
          && rg > (EOS_REAL) 0.0)
        _eos_DEBUG_PRINT (" log(eg/el)=%25.16e log(rg/rl)=%25.16e\n",
                          log (eg / el), log (rg / rl));
      _eos_DEBUG_PRINT ("\n");


      if (T[it] < tcrit) {
        _eos_DEBUG_PRINT ("*** T[%i]=%23.14e < tcrit=%23.14e\n", it + 1,
                          T[it], tcrit);

        irlo = irlo301[it];
        irhi = irhi301[it];

        if (irlo >= 2) {
          dpdrlo =
            (P[it][irlo - 1] - P[it][irlo - 2]) / (R[irlo - 1] - R[irlo - 2]);
          plo = P[it][irlo - 1];
          rlo = R[irlo - 1];
        }
        else if (irlo == 1) {
          dpdrlo = (P[it][irlo] - P[it][irlo - 1]) / (R[irlo] - R[irlo - 1]);
          plo = P[it][irlo - 1];
          rlo = R[irlo - 1];
        }
        else {
          dpdrlo = (EOS_REAL) 0.0;
          plo = P[it][0];
          rlo = R[0];
        }

        if (irhi <= nR - 3) {
          dpdrhi =
            (P[it][irhi + 2] - P[it][irhi + 1]) / (R[irhi + 2] - R[irhi + 1]);
          phi = P[it][irhi + 1];
          rhi = R[irhi + 1];
        }
        else if (irhi <= nR - 2) {
          dpdrhi = (P[it][irhi + 1] - P[it][irhi]) / (R[irhi + 1] - R[irhi]);
          phi = P[it][irhi + 1];
          rhi = R[irhi + 1];
        }
        else {
          dpdrhi = P[it][nR - 1] / R[nR - 1];
          phi = P[it][nR - 1];
          rhi = R[nR - 1];
        }

        _eos_DEBUG_PRINT ("rlo=%23.14e rhi=%23.14e plo=%23.14e phi=%23.14e\n",
                          rlo, rhi, plo, phi);
        for (ir = irlo; ir <= irhi; ir++) {
          if (R[ir] < rcrit) {
            _eos_DEBUG_PRINT
              ("R[%i]=%23.14e <  rcrit --- P[%i][%i]: %23.14e -> %23.14e\n",
               ir + 1, R[ir], it + 1, ir + 1, P[it][ir],
               plo + dpdrlo * (R[ir] - rlo));
            P[it][ir] = plo + dpdrlo * (R[ir] - rlo);
          }
          else {
            _eos_DEBUG_PRINT
              ("R[%i]=%23.14e >= rcrit --- P[%i][%i]: %23.14e -> %23.14e\n",
               ir + 1, R[ir], it + 1, ir + 1, P[it][ir],
               phi + dpdrhi * (R[ir] - rhi));
            P[it][ir] = phi + dpdrhi * (R[ir] - rhi);
          }

          if (build301[it] && power != (EOS_REAL) 0.0) {
            _eos_DEBUG_PRINT
              ("build301[%i]=%s: E[%i][%i]: %23.14e -> %23.14e\n", it + 1,
               ((build301[it]) ? "T" : "F"), it + 1, ir + 1, E[it][ir],
               el * pow (R[ir] / rl, power));
            E[it][ir] = el * pow (R[ir] / rl, power);
          }
        }                       /* ir - loop */
      }                         /* T[it] < tcrit */
      else {
        _eos_DEBUG_PRINT ("T[%i]=%23.14e >= tcrit=%23.14e\n", it + 1, T[ir],
                          tcrit);
      }
    }                           /* it- loop */
  }                             /* if flat */

  /* .....   modify the loops

     if (.false. .and. down) then

     call sesame_vapor_crit(tcrit, pcrit, rcrit, vcrit, ecrit)

     if (tcrit.le.ZERO) then
     call global_error('SESAME_FIX_TABLE: tcrit.le.0')
     endif

     do it = 1,nt301
     if (t301(it).lt.tcrit) then
     do ir = 2,nr301
     if (r301(ir).ge.rcrit) exit
     p301(ir,it) = max(p301(ir,it), p301(ir-1,it))
     enddo ! ir

     do ir = nr301-1,1,-1
     if (r301(ir).lt.rcrit) exit
     p301(ir,it) = min(p301(ir,it), p301(ir+1,it))
     enddo ! ir
     endif
     enddo ! it
     endif
   */

  table_good = (num_vapor > 0 || table_type > 0);

  EOS_FREE (V);
  EOS_FREE (build301);
  EOS_FREE (irlo301);
  EOS_FREE (irhi301);

  if (!vapor_t)
    return;                     /* then none of the vapor_* arrays are allocated */
  if (!*found_401 && !*p401) {  /* now we MUST store the vapor_* arrays into *401 arrays */
    *nt401 = num_vapor;
    *p401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);
    *t401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);
    *rg401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);
    *rl401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);
    *eg401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);
    *el401 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *nt401);

    for (i = 0; i < *nt401; i++) {      /* store generated 401 data in rt2_handle data object */
      (*p401)[i] = vapor_p[i];
      (*t401)[i] = vapor_t[i];
      (*rg401)[i] = vapor_rg[i];
      (*rl401)[i] = vapor_rl[i];
      (*eg401)[i] = vapor_eg[i];
      (*el401)[i] = vapor_el[i];
    }
  }

  *vaporArrayOffset = *nt401 - num_vapor;

  _eos_DEBUG_PRINT ("%s\n\n\n",
                    ((table_good) ? "FIX_TABLE: table GOOD" :
                     "FIX_TABLE: table BAD"));
  _eos_DEBUG_PRINT ("     %23s %23s\n", "vapor_t", "t401");
  for (i = 0; i < *vaporArrayOffset; i++)
    _eos_DEBUG_PRINT ("%4i %23s %23.14e\n", i + 1, " ", (*t401)[i]);
  for (i = *vaporArrayOffset; i < *nt401; i++)
    _eos_DEBUG_PRINT ("%4i %23.14e %23.14e %s\n", i + 1,
                      vapor_t[i - *vaporArrayOffset], (*t401)[i],
                      (vapor_t[i - *vaporArrayOffset] - (*t401)[i] !=
                       0.0) ? "different" : " ");
  _eos_DEBUG_PRINT ("     %23s %23s\n", "vapor_p", "p401");
  for (i = 0; i < *vaporArrayOffset; i++)
    _eos_DEBUG_PRINT ("%4i %23s %23.14e\n", i + 1, " ", (*p401)[i]);
  for (i = *vaporArrayOffset; i < *nt401; i++)
    _eos_DEBUG_PRINT ("%4i %23.14e %23.14e %s\n", i + 1,
                      vapor_p[i - *vaporArrayOffset], (*p401)[i],
                      (vapor_p[i - *vaporArrayOffset] - (*p401)[i] !=
                       0.0) ? "different" : " ");
  _eos_DEBUG_PRINT ("     %23s %23s\n", "vapor_rl", "rl401");
  for (i = 0; i < *vaporArrayOffset; i++)
    _eos_DEBUG_PRINT ("%4i %23s %23.14e\n", i + 1, " ", (*rl401)[i]);
  for (i = *vaporArrayOffset; i < *nt401; i++)
    _eos_DEBUG_PRINT ("%4i %23.14e %23.14e %s\n", i + 1,
                      vapor_rl[i - *vaporArrayOffset], (*rl401)[i],
                      (vapor_rl[i - *vaporArrayOffset] - (*rl401)[i] !=
                       0.0) ? "different" : " ");
  _eos_DEBUG_PRINT ("     %23s %23s\n", "vapor_rg", "rg401");
  for (i = 0; i < *vaporArrayOffset; i++)
    _eos_DEBUG_PRINT ("%4i %23s %23.14e\n", i + 1, " ", (*rg401)[i]);
  for (i = *vaporArrayOffset; i < *nt401; i++)
    _eos_DEBUG_PRINT ("%4i %23.14e %23.14e %s\n", i + 1,
                      vapor_rg[i - *vaporArrayOffset], (*rg401)[i],
                      (vapor_rg[i - *vaporArrayOffset] - (*rg401)[i] !=
                       0.0) ? "different" : " ");
  _eos_DEBUG_PRINT ("     %23s %23s\n", "vapor_el", "el401");
  for (i = 0; i < *vaporArrayOffset; i++)
    _eos_DEBUG_PRINT ("%4i %23s %23.14e\n", i + 1, " ", (*el401)[i]);
  for (i = *vaporArrayOffset; i < *nt401; i++)
    _eos_DEBUG_PRINT ("%4i %23.14e %23.14e %s\n", i + 1,
                      vapor_el[i - *vaporArrayOffset], (*el401)[i],
                      (vapor_el[i - *vaporArrayOffset] - (*el401)[i] !=
                       0.0) ? "different" : " ");
  _eos_DEBUG_PRINT ("     %23s %23s\n", "vapor_eg", "eg401");
  for (i = 0; i < *vaporArrayOffset; i++)
    _eos_DEBUG_PRINT ("%4i %23s %23.14e\n", i + 1, " ", (*eg401)[i]);
  for (i = *vaporArrayOffset; i < *nt401; i++)
    _eos_DEBUG_PRINT ("%4i %23.14e %23.14e %s\n", i + 1,
                      vapor_eg[i - *vaporArrayOffset], (*eg401)[i],
                      (vapor_eg[i - *vaporArrayOffset] - (*eg401)[i] !=
                       0.0) ? "different" : " ");
  _eos_DEBUG_PRINT ("     %23s\n", "vapor_vl");
  for (i = 0; i < *vaporArrayOffset; i++)
    _eos_DEBUG_PRINT ("%4i %23s\n", i + 1, " ");
  for (i = *vaporArrayOffset; i < *nt401; i++)
    _eos_DEBUG_PRINT ("%4i %23.14e\n", i + 1,
                      vapor_vl[i - *vaporArrayOffset]);
  _eos_DEBUG_PRINT ("     %23s\n", "vapor_vg");
  for (i = 0; i < *vaporArrayOffset; i++)
    _eos_DEBUG_PRINT ("%4i %23s\n", i + 1, " ");
  for (i = *vaporArrayOffset; i < *nt401; i++)
    _eos_DEBUG_PRINT ("%4i %23.14e\n", i + 1,
                      vapor_vg[i - *vaporArrayOffset]);

#if 0
  printf ("%s\n\n\n",
          ((table_good) ? "FIX_TABLE: table GOOD" : "FIX_TABLE: table BAD"));
  if (*nt401 != num_vapor) {
    printf ("     %23s %23s\n", "vapor_t", "t401");
    for (i = 0; i < *vaporArrayOffset; i++)
      printf ("%4i %23s %23.14e\n", i + 1, " ", (*t401)[i]);
    for (i = *vaporArrayOffset; i < *nt401; i++)
      printf ("%4i %23.14e %23.14e %s\n", i + 1,
              vapor_t[i - *vaporArrayOffset], (*t401)[i],
              (vapor_t[i - *vaporArrayOffset] - (*t401)[i] !=
               0.0) ? "different" : " ");
    printf ("     %23s %23s\n", "vapor_p", "p401");
    for (i = 0; i < *vaporArrayOffset; i++)
      printf ("%4i %23s %23.14e\n", i + 1, " ", (*p401)[i]);
    for (i = *vaporArrayOffset; i < *nt401; i++)
      printf ("%4i %23.14e %23.14e %s\n", i + 1,
              vapor_p[i - *vaporArrayOffset], (*p401)[i],
              (vapor_p[i - *vaporArrayOffset] - (*p401)[i] !=
               0.0) ? "different" : " ");
    printf ("     %23s %23s\n", "vapor_rl", "rl401");
    for (i = 0; i < *vaporArrayOffset; i++)
      printf ("%4i %23s %23.14e\n", i + 1, " ", (*rl401)[i]);
    for (i = *vaporArrayOffset; i < *nt401; i++)
      printf ("%4i %23.14e %23.14e %s\n", i + 1,
              vapor_rl[i - *vaporArrayOffset], (*rl401)[i],
              (vapor_rl[i - *vaporArrayOffset] - (*rl401)[i] !=
               0.0) ? "different" : " ");
    printf ("     %23s %23s\n", "vapor_rg", "rg401");
    for (i = 0; i < *vaporArrayOffset; i++)
      printf ("%4i %23s %23.14e\n", i + 1, " ", (*rg401)[i]);
    for (i = *vaporArrayOffset; i < *nt401; i++)
      printf ("%4i %23.14e %23.14e %s\n", i + 1,
              vapor_rg[i - *vaporArrayOffset], (*rg401)[i],
              (vapor_rg[i - *vaporArrayOffset] - (*rg401)[i] !=
               0.0) ? "different" : " ");
    printf ("     %23s %23s\n", "vapor_el", "el401");
    for (i = 0; i < *vaporArrayOffset; i++)
      printf ("%4i %23s %23.14e\n", i + 1, " ", (*el401)[i]);
    for (i = *vaporArrayOffset; i < *nt401; i++)
      printf ("%4i %23.14e %23.14e %s\n", i + 1,
              vapor_el[i - *vaporArrayOffset], (*el401)[i],
              (vapor_el[i - *vaporArrayOffset] - (*el401)[i] !=
               0.0) ? "different" : " ");
    printf ("     %23s %23s\n", "vapor_eg", "eg401");
    for (i = 0; i < *vaporArrayOffset; i++)
      printf ("%4i %23s %23.14e\n", i + 1, " ", (*eg401)[i]);
    for (i = *vaporArrayOffset; i < *nt401; i++)
      printf ("%4i %23.14e %23.14e %s\n", i + 1,
              vapor_eg[i - *vaporArrayOffset], (*eg401)[i],
              (vapor_eg[i - *vaporArrayOffset] - (*eg401)[i] !=
               0.0) ? "different" : " ");
    printf ("     %23s\n", "vapor_vl");
    for (i = 0; i < *vaporArrayOffset; i++)
      printf ("%4i %23s\n", i + 1, " ");
    for (i = *vaporArrayOffset; i < *nt401; i++)
      printf ("%4i %23.14e\n", i + 1, vapor_vl[i - *vaporArrayOffset]);
    printf ("     %23s\n", "vapor_vg");
    for (i = 0; i < *vaporArrayOffset; i++)
      printf ("%4i %23s\n", i + 1, " ");
    for (i = *vaporArrayOffset; i < *nt401; i++)
      printf ("%4i %23.14e\n", i + 1, vapor_vg[i - *vaporArrayOffset]);
  }
#endif

  /*
     Copy vapor_[p,t,rl,rg,el,eg] array data into [p,t,rl,rg,el,eg]401 arrays
   */
  for (i = *vaporArrayOffset; i < *nt401; i++)
    (*t401)[i] = vapor_t[i - *vaporArrayOffset];
  for (i = *vaporArrayOffset; i < *nt401; i++)
    (*p401)[i] = vapor_p[i - *vaporArrayOffset];
  for (i = *vaporArrayOffset; i < *nt401; i++)
    (*rl401)[i] = vapor_rl[i - *vaporArrayOffset];
  for (i = *vaporArrayOffset; i < *nt401; i++)
    (*rg401)[i] = vapor_rg[i - *vaporArrayOffset];
  for (i = *vaporArrayOffset; i < *nt401; i++)
    (*el401)[i] = vapor_el[i - *vaporArrayOffset];
  for (i = *vaporArrayOffset; i < *nt401; i++)
    (*eg401)[i] = vapor_eg[i - *vaporArrayOffset];

  /* Free local arrays' memory */
  EOS_FREE (vapor_t);
  EOS_FREE (vapor_p);
  EOS_FREE (vapor_rl);
  EOS_FREE (vapor_rg);
  EOS_FREE (vapor_vl);
  EOS_FREE (vapor_vg);
  EOS_FREE (vapor_el);
  EOS_FREE (vapor_eg);

  _eos_DEBUG_PRINT ("*** LEAVING FIX_TABLE\n");
}

/************************************************************************
 * 
 * Converts selected data of class eos_RecordType1 to specified units
 * 
 * Returned Values: 
 *    EOS_INTEGER *err - output error code
 *
 * Input Values:
 *    EOS_INTEGER convertFlag - flag to specify desired units conversion
 *                              (_EOS_SESAME_TO_CGS or_EOS_CGS_TO_SESAME)
 *
 * Input/Output Values (301 table):
 *    nR       = input  integer number of X values.
 *    nT       = input  integer number of Y values.
 *    R[nR]    = in/out  real X array.
 *    T[nT]    = in/out  real Y array.
 *    P        = in/out real F array (nR * nT elements).
 *    E        = in/out real E array (nR * nT elements)..
 *    
 * Input/Output Values (401 table):
 *    nt401              // Number of temperature values
 *    p401[nt401]        // Vapor Pressure
 *    t401[nt401]        // Temperature
 *    rg401[nt401]       // Vapor Density on Coexistence Line
 *    rl401[nt401]       // Density of Liquid or Solid on Coexistence Line
 *    eg401[nt401]       // Internal Energy of Vapor on Coexistence Line
 *    el401[nt401]       // Internal Energy of Liquid on Coexistence Line
 *    ag401[nt401]       // Free Energy of Vapor on Coexistence Line
 *    al401[nt401]       // Free Energy of Liquid on Coexistence Line
 *    avgAtomicNumber401 // mean atomic number
 *    avgAtomicWgt401    // mean atomic mass
 *    refDensity401      // normal solid density
 * 
 ************************************************************************/
void _eos_ConvertUnits (EOS_INTEGER convertFlag,
                        EOS_INTEGER nR, EOS_INTEGER nT, EOS_REAL *R,
                        EOS_REAL *T, EOS_REAL **P, EOS_REAL **E,
                        EOS_INTEGER nt401, EOS_REAL *p401, EOS_REAL *t401,
                        EOS_REAL *rg401, EOS_REAL *rl401, EOS_REAL *eg401,
                        EOS_REAL *el401, EOS_REAL *ag401, EOS_REAL *al401,
                        EOS_REAL *avgAtomicNumber401,
                        EOS_REAL *avgAtomicWgt401, EOS_REAL *refDensity401,
                        EOS_REAL *adjustVapPres, EOS_INTEGER *err, EOS_CHAR **errMsg)
{
  EOS_INTEGER i, j;
  EOS_REAL SCALEP, SCALEE, SCALET;

  *err = EOS_OK;

  /* barye = dynes/cm^2 */

  switch (convertFlag) {
  case _EOS_SESAME_TO_CGS:
    SCALEP = 1.0e10;            /* barye / (GPa)      */
    SCALEE = 1.0e10;            /* (ergs/g) / (MJ/kg) */
    SCALET = 1.0 / 11604.505;   /* eV / Kelvin        */
    break;

  case _EOS_CGS_TO_SESAME:
    SCALEP = 1.0e-10;           /* GPa / barye        */
    SCALEE = 1.0e-10;           /* (MJ/kg) / (ergs/g) */
    SCALET = 11604.505;         /* Kelvin / eV        */
    break;

  default:
    *err = EOS_FAILED;
    eos_SetCustomMsg_str (errMsg,
			  "eos_RecordType1::_eos_ConvertUnits ERROR, invalid convertFlag, %i",
			  convertFlag);
    return;
  }

  for (j = 0; j < nT; j++) {
    if (T)
      T[j] *= SCALET;
    for (i = 0; i < nR; i++) {
      if (P)
        P[j][i] *= SCALEP;
      if (E)
        E[j][i] *= SCALEE;
    }
  }
  for (j = 0; j < nt401; j++) {
    if (p401)
      p401[j] *= SCALEP;
    if (t401)
      t401[j] *= SCALET;
    if (eg401)
      eg401[j] *= SCALEE;
    if (el401)
      el401[j] *= SCALEE;
    if (ag401)
      ag401[j] *= SCALEE;
    if (al401)
      al401[j] *= SCALEE;
  }
  *adjustVapPres *= SCALEP;

  return;
}
