/*********************************************************************
 * Class Name : eos_Utils
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

#ifndef EOS_UTILS_H
#define EOS_UTILS_H

#include "ses_defines.h"
#include "eos_types.h"
#include "eos_ErrorHandler.h"
#include "eos_universal_types.h"
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "eos_SaferMemoryAllocation.h"

#ifndef EXCLUDE_COLD_CURVE_IN_INVERTED_GRID
#  define __INCLUDE_COLD_CURVE_IN_INVERTED_GRID__
#endif
#ifdef __INCLUDE_COLD_CURVE_IN_INVERTED_GRID__
#  define __ADD_VAL__(v) v
#else
#  define __ADD_VAL__(v) 0
#endif

/* The following define enables debug output in eos_RecordType1 packing/unpacking functions */
//#define _DEBUG_PACKING_FUNCTIONS

/* The following define enables debug output in eos_DumpExpandedGridRecordType1 function */
//#define DEBUG_EOS_EXPANDGRIDINTERPOLATE


/************************************************************************
 * 
 * Data members
 * 
 ************************************************************************/

/* Declare the global storage to contain available Sesame filenames with
 * absolute pathnames.
 * Define the default network-dependent paths in which to find the
 * sesameFilesDir.txt that is a text file containing a list of available
 * Sesame files.
 */
#ifdef _EOS_UTILS_INTERNAL_

EOS_CHAR **sesameIndexLocations = NULL;
EOS_INTEGER sesameIndexLocationsL = 0;
EOS_CHAR **sesameFiles = NULL;
EOS_INTEGER sesameFilesL;
EOS_INTEGER _EOS_STOP_FILE_SEARCH = 999999;
EOS_BOOLEAN fileListPrinted = EOS_FALSE;

#define EOS_NUMDEFAULTSESAMEPATHSL 7
static const EOS_CHAR *defaultSesamePathList[EOS_NUMDEFAULTSESAMEPATHSL] = {
  "/usr/projects/data/eos",     /* LANL Production data path        */
  "/usr/local/codes/data/eos",  /* LANL X-Div LAN data path         */
  "/opt/local/codes/data/eos",  /* LANL X-Div LAN data path         */
  "/usr/local/udata/ses",       /* LANL Cray unclassified data path */
  "/usr/local/cdata",           /* LANL Cray classified data path   */
  "/usr/gapps/lanl-data/eos",   /* LLNL Production data path        */
  "/projects/lanl-data/eos"     /* SANDIA Production data path      */
};

/* Define a list of default Sesame file names to locate. This list
 * was taken from EOSPAC v5, and it is used here as an alternative
 * source of file names to locate.
 */
#define EOS_NUMDEFAULTFILENAMES 41
static const EOS_CHAR *defaultSesameFileNames[EOS_NUMDEFAULTFILENAMES] = {
  "sesameu", "sesameu1", "sesameu2", "sesameu3", "sesameu4",
  "sesamea", "sesamea1", "sesamea2", "sesameb", "sesamec",
  "sesame", "sesame1", "sesame2", "sesame3", "sesame4",
  "sesep", "sesep1", "sesep2", "sesep3", "sesep4",
  "sesou", "sesou1", "sesou2", "sesou3", "sesou4",
  "sesop", "sesop1", "sesop2", "sesop3", "sesop4",
  "sescu", "sescu1", "sescu2", "sescu3", "sescu4",
  "sescu9", "sescp", "sescp1", "sescp2", "sescp3",
  "sescp4"
};

#else // if not _EOS_UTILS_INTERNAL_

extern EOS_CHAR **sesameIndexLocations;
extern EOS_INTEGER sesameIndexLocationsL;
extern EOS_CHAR **sesameFiles;
extern EOS_INTEGER sesameFilesL;
extern EOS_BOOLEAN fileListPrinted;

#endif

/*
 * Define/declare the global storage to contain the cached data file information
 */


typedef struct
{
  EOS_INTEGER nmats;
  ses_file_handle sesFile;
  ses_material_id_reference sesMats;
  ses_boolean materialListLoaded;


} SesameFileCache;

typedef struct{
    EOS_REAL x_threshold_index; /* index of x_threshold value in xvals */
    EOS_REAL x_threshold; /* value below which only one loop is valid */
    EOS_REAL *xvals;      /* independent variable table */
    EOS_REAL fmin;        /* dependent variable minimum at xvals[i] where i<x_threshold_index */
    EOS_REAL fmin_index;  /* xvals index corresponding to fmin value */
} _EXCEPTIONS_T;

#ifdef _EOS_UTILS_INTERNAL_

SesameFileCache *sesameFileCache = NULL;
EOS_INTEGER sesameFileCacheL;

#else // if not _EOS_UTILS_INTERNAL_

extern SesameFileCache *sesameFileCache;
extern EOS_INTEGER sesameFileCacheL;

#endif

/*
 * Define/declare the global storage to contain the matid-to-file
 * cross-reference information
 */
typedef struct
{
  EOS_INTEGER matid;   /* material id number */
  EOS_INTEGER jfile;   /* index into both sesameFileCache and sesameFiles arrays */
} MatidMap;

#ifdef _EOS_UTILS_INTERNAL_

MatidMap *gMatidMap = NULL;
EOS_INTEGER gMatidMapL;

#else // if not _EOS_UTILS_INTERNAL_

extern MatidMap *gMatidMap;
extern EOS_INTEGER gMatidMapL;

#endif

/* Miscellaneous macro definitions */
#define AVERAGE(_ab_type_,a,b) (a+b)/((_ab_type_)2.0)
/* The following 4 defines can have values of 0 or 1; otherwise code failure occurs in _eos_CreateGhostData() */
#define _EOS_CREATEGHOSTDATA_X_LO 1
#define _EOS_CREATEGHOSTDATA_X_HI 1
#define _EOS_CREATEGHOSTDATA_Y_LO 1
#define _EOS_CREATEGHOSTDATA_Y_HI 1

#include "eos_Utils.proto.h"

#endif

