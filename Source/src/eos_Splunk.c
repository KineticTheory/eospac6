/*********************************************************************
 * Class Name : eos_Splunk
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <assert.h>

#include "eos_types_internal.h"
#define _EOS_SPLUNK_INTERNAL_
#include "eos_Splunk.h"

/*!**********************************************************************
 *
 * function _eos_splunk_get_acc_disable
 * Routine to get acc_disable value for a specific function identified by key
 * 
 * Returned Values: 
 * EOS_BOOLEAN     - ouput acc_disable value for key
 *
 * Input Values:
 * EOS_INTEGER key - input key name tied to a specific API function
 *
 ************************************************************************/
inline EOS_BOOLEAN
_eos_splunk_get_acc_disable(EOS_INTEGER key)
{
#ifdef USE_SPLUNK

  assert(key >= EOS_SPLUNK_ACCUMULATOR_MIN_KEY);
  assert(key <= EOS_SPLUNK_ACCUMULATOR_MAX_KEY);

  return(eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].acc_disable);

#else

  return(EOS_TRUE);

#endif
}

/*!**********************************************************************
 *
 * function _eos_splunk_get_acc_active
 * Routine to get acc_active value for a specific function identified by key
 * 
 * Returned Values: 
 * EOS_BOOLEAN     - ouput acc_active value for key
 *
 * Input Values:
 * EOS_INTEGER key - input key name tied to a specific API function
 *
 ************************************************************************/
inline EOS_BOOLEAN
_eos_splunk_get_acc_active(EOS_INTEGER key)
{
#ifdef USE_SPLUNK

  assert(key >= EOS_SPLUNK_ACCUMULATOR_MIN_KEY);
  assert(key <= EOS_SPLUNK_ACCUMULATOR_MAX_KEY);

  return(eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].acc_active);

#else

  return(EOS_FALSE);

#endif
}

/*!**********************************************************************
 *
 * function _eos_splunk_flush
 * Routine to flush the accumulated messages for a specific function identified by key
 * 
 * Returned Values: 
 * none
 *
 * Input Values:
 * EOS_INTEGER key - input key name tied to a specific API function
 *
 ************************************************************************/
void
_eos_splunk_flush(EOS_INTEGER key)
{
#ifdef USE_SPLUNK

  EOS_CHAR *command_args = NULL;
  EOS_INTEGER ibuf;

  assert(key >= EOS_SPLUNK_ACCUMULATOR_MIN_KEY);
  assert(key <= EOS_SPLUNK_ACCUMULATOR_MAX_KEY);

  if (_eos_splunk_get_acc_disable(key) || ! _eos_splunk_get_acc_active(key)) {
    /* Do nothing, because feature is disabled or unused for this key */
    return;
  }

  if (eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].ibuf >= 0 &&
      strlen(eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].msg_buffer[0]) > 0) {

    /* the message buffer accumulator is full for functionName; time to flush */
    EOS_INTEGER i;

    ibuf = eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].ibuf;

    for (i=0; i<ibuf; i++) {

      command_args = eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].msg_buffer[i];

#if 1
#if defined(CRAY_CPU_TARGET)

      /* Crays do not use syslog. We must use their xtlog command to log our info. */
      enum { N = EOS_SPLUNK_ACCUMULATOR_STR_LEN + 100 };
      EOS_CHAR command[N];
      strcpy(command, "/opt/cray/llm/default/bin/xtlog syslog 6 1 \"");
      strcat(command, command_args);
      strcat(command, "\"");
      system(command);

#else

      openlog("syslog-test", LOG_PID | LOG_CONS | LOG_NDELAY, LOG_USER);
      syslog(LOG_INFO, "%s\n", command_args);
      sleep(0.1);
      closelog();

#endif
#else
      {
        FILE *fp = NULL;
        fp = fopen("/usr/projects/eos/scratch/simulated.syslog.txt", "a");
        if (fp) fprintf(fp, "%s\n", command_args);
        fclose(fp);
      }

#endif

      strcpy(command_args, "");

    }

    eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].ibuf = 0;
  }
#endif
}

/*!**********************************************************************
 *
 * function _eos_splunk
 * Routine to capture metric data of runs and write to either a buffer or the syslog for the Splunk server
 * 
 * Returned Values: 
 * none
 *
 * Input Values:
 * EOS_INTEGER key - input key name tied to a specific API function
 * char *val       - input associated value(s) to send to syslog
 *
 ************************************************************************/

/* The following is locally-defined in eos_FC_Interface.MixedCase.c, which is created from eos_FC_Interface.c */
/* using the logic defined in the following target in src/Makefile: $(MANGLED_SRCS): %.c : $(INTERFACE_SRCS)  */
EOS_CHAR* _eos_get_version_MixedCase();

void
_eos_splunk(EOS_INTEGER key, EOS_CHAR *val)
{
#ifdef USE_SPLUNK

  assert(key >= EOS_SPLUNK_ACCUMULATOR_MIN_KEY);
  assert(key <= EOS_SPLUNK_ACCUMULATOR_MAX_KEY);

  if (_eos_splunk_get_acc_disable(key)) {
    /* Do nothing, because feature is disabled for this key */
    return;
  }
  else {

    EOS_CHAR *command_args = NULL;
    EOS_CHAR *functionName = NULL;
    EOS_CHAR *eos_version_info = NULL;
    EOS_CHAR *job_id = NULL;
    EOS_CHAR *cluster_name = NULL;
    EOS_CHAR *job_partition = NULL;
    EOS_CHAR mypid[50];
    EOS_CHAR type[50];
    EOS_INTEGER ibuf;

    job_id = getenv("SLURM_JOBID");
    cluster_name = getenv("SLURM_CLUSTER_NAME");
    job_partition = getenv("SLURM_JOB_PARTITION");
    if (!job_id) {
      sprintf(mypid, "%d", (int)getpid());
      job_id = mypid;
    }
    if (!cluster_name) cluster_name = getenv("HOST");

    eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].acc_active = EOS_TRUE;
    functionName = eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].functionName;
    ibuf = eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].ibuf;
    command_args = eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].msg_buffer[ibuf];

    strcpy(type, "METRICS");
    /* strcpy(type, "TESTING"); */

    eos_version_info = _eos_get_version_MixedCase();
    sprintf(command_args,
            "qid=X-EOSPAC eospac_version_info='%s' type=%s jobid=%s user=%s cluster=%s partition=%s function=%s %s",
            eos_version_info, type, job_id, getenv("USER"), cluster_name, job_partition, functionName, val);

    if (ibuf >= EOS_SPLUNK_ACCUMULATOR_BUF_LEN-1) {

      /* the message buffer accumulator is full for functionName; time to flush */
      _eos_splunk_flush(key);

    }
    else {

      eos_SplunkAccumulator[key-EOS_SPLUNK_ACCUMULATOR_MIN_KEY].ibuf++;

    }

  }
#endif
}

