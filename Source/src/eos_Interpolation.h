/*********************************************************************
 * Class Name : eos_Interpolation
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/
#ifndef  EOS_INTERPOLATION_H
#define  EOS_INTERPOLATION_H
#include "eos_Access.h"

typedef struct
{
  EOS_INTEGER interpolationType;        /* currently selected interpolation type */
  EOS_INTEGER derivativeType;   /* currently selected partial derivative type */
  EOS_INTEGER lastErrorCode;    /* EOS_OK, EOS_INTERP_EXTRAPOLATED, EOS_INTERP_EXTRAP_TBAL, EOS_INTERP_EXTRAP_PBAL */
  EOS_BOOLEAN disableGhostNodes;
  EOS_BOOLEAN saveSpeciesData;
  EOS_BOOLEAN enableDiscontinuousDerivatives; /* for linear and bilnear interpolation only */
  EOS_BOOLEAN enableXYpassthru; /* store status of EOS_XY_PASSTHRU option */
  EOS_BOOLEAN enableXYmodify; /* store status of EOS_XY_MODIFY option */
  EOS_INTEGER *xyBounds;        /* xyBounds array stores extrap error per point for each XY Pair */
  EOS_INTEGER nXYPairs;         /* number of XY pairs for which errors are stored in xyBounds */
  EOS_INTEGER nAlloc;           /* allocated size of xyBounds. */
  EOS_REAL *xSpecies;       /* Species-specific X values for each XY Pair.
			       Used only by eos_Mix functions if EOS_SAVE_SPECIES_DATA option is set. */
  EOS_REAL *ySpecies;       /* Species-specific Y values for each XY Pair.
			       Used only by eos_Mix functions if EOS_SAVE_SPECIES_DATA option is set. */
  EOS_REAL *FSpecies;       /* Species-specific F values for each XY Pair.
			       Used only by eos_Mix functions if EOS_SAVE_SPECIES_DATA option is set. */
  EOS_REAL *dFxSpecies;     /* Species-specific dF/dX values for each XY Pair.
			       Used only by eos_Mix functions if EOS_SAVE_SPECIES_DATA option is set. */
  EOS_REAL *dFySpecies;     /* Species-specific dF/dY values for each XY Pair.
			       Used only by eos_Mix functions if EOS_SAVE_SPECIES_DATA option is set. */

  /* Virtual function prototypes */
  void (*Interpolate) (void *me, EOS_INTEGER th);

} eos_InterpolationData;

typedef struct
{
  eos_Access eosAccess;         /* must be the FIRST, DO NOT MOVE! */
  EOS_INTEGER numberOfHandles;  /* number of tables handles */
  eos_InterpolationData **interpolationDataList;        /* interpolation data for each handle (array size=numberOfHandles) */
} eos_Interpolation;

#include "eos_Interpolation.proto.h"

#endif

