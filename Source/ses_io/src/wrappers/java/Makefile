
.NOTPARALLEL :

#  This is the makefile for ses_io/src/wrappers/java
_realpath = $(shell perl -e 'use Cwd "abs_path"; print abs_path(shift)," ";' $(1))
REL_ROOT := ../../..
ROOT := $(strip $(call _realpath,$(strip $(shell pwd))/$(REL_ROOT)))


ifneq ($(wildcard ../../../../Makefile.compile_opts),)
  include ../../../../Makefile.compile_opts
else
  include ../../../test_makefile/Makefile.compile_opts
endif
include ../../../test_makefile/Makefile.local

#environment variables for the local file

LOCAL_CFLAGS=-I$(JVM_INCLUDE) -I$(JNI_H_LOC)

LD_LIBRARY_PATH=.
LOCAL_LDFLAGS=

ifeq ($(SYSTEMROOT),C:\Windows)
CLASSPATH=.
LD_LIBRARY_PATH=.
else
CLASSPATH=.:./ses_defines:./MySesIO
endif
SES_LIB_PATH = $(ROOT)/src

# Being a cray, things are always in weird locales:
ifeq ($(call match,ct-fe,$(MACHINE)),TRUE)
JAVACC=/opt/java/default/bin/java
JAVA=/opt/java/default/bin/javac
JAVAH=/opt/java/default/bin/javah
else
JAVACC=$(shell which javac)
JAVA=$(shell which java)
JAVAH=$(shell which javah)
endif


#targets

all : $(DYNAMIC_LIBRARY) 

test : $(DYNAMIC_LIBRARY) JavaTests.class MySesIO/SesIO.class
ifeq ($(JAVA),)
	echo "No java found"
else
	$(JAVA) -Djava.library.path=. JavaTests
endif

jar :  SesIO.class ses_defines
	jar -cf jSesIO.jar SesIO.class ses_defines/*.class

################33

CLASS = ses_number.class ses_file_handle.class ses_error_flag.class ses_word_reference.class ses_boolean.class ses_string.class ses_table_id.class ses_label.class ses_material_id.class ses_material_id_reference.class ses_table_id_reference.class ses_open_type.class ses_number_reference.class ses_array_order.class

ses_defines : $(CLASS)

JavaTests.class : JavaTests.java
	$(JAVACC)  -d . JavaTests.java

ses_error_flag.class : ses_defines/ses_error_flag.java
	$(JAVACC)  -d . ses_defines/ses_error_flag.java

ses_number.class : ses_defines/ses_number.java
	$(JAVACC)  -d . ses_defines/ses_number.java

ses_file_handle.class : ses_defines/ses_file_handle.java
	$(JAVACC)  -d . ses_defines/ses_file_handle.java

ses_word_reference.class : ses_defines/ses_word_reference.java
	$(JAVACC)  -d . ses_defines/ses_word_reference.java

ses_boolean.class : ses_defines/ses_boolean.java
	$(JAVACC)  -d . ses_defines/ses_boolean.java

ses_string.class : ses_defines/ses_string.java
	$(JAVACC)  -d . ses_defines/ses_string.java

ses_table_id.class : ses_defines/ses_table_id.java
	$(JAVACC)  -d . ses_defines/ses_table_id.java

ses_label.class : ses_defines/ses_label.java
	$(JAVACC)  -d . ses_defines/ses_label.java

ses_material_id.class : ses_defines/ses_material_id.java
	$(JAVACC)  -d . ses_defines/ses_material_id.java

ses_material_id_reference.class : ses_defines/ses_material_id_reference.java
	$(JAVACC)  -d . ses_defines/ses_material_id_reference.java

ses_table_id_reference.class : ses_defines/ses_table_id_reference.java
	$(JAVACC)  -d . ses_defines/ses_table_id_reference.java

ses_open_type.class : ses_defines/ses_open_type.java
	$(JAVACC)  -d . ses_defines/ses_open_type.java

ses_number_reference.class : ses_defines/ses_number_reference.java
	$(JAVACC)  -d . ses_defines/ses_number_reference.java

ses_array_order.class : ses_defines/ses_array_order.java
	$(JAVACC)  -d . ses_defines/ses_array_order.java

#########################

$(ROOT)/src/libses.a :
	$(MAKE) -C $(ROOT)/src libses.a

#########################

OBJ=SesAccessDirectory.o SesAccessTableIndex.o SesArraySizeNext.o SesChangeNext.o SesClose.o SesCombine.o SesComments.o SesDate.o SesDefineTable.o SesDeleteNext.o SesDeleteTable.o SesGetLabel.o SesGetMaterialID.o SesGetMaterials.o SesGetTableIds.o SesHasNext.o SesIndicatesError.o SesIsValid.o SesOpen.o SesPrintErrorCondition.o SesRead1D.o SesRead2D.o SesRead3D.o SesReadNamedArray.o SesReadNext.o SesReadNumber.o SesReadPairs.o SesReadWord.o SesSetArrayOrder.o SesSetDate.o SesSetFormat.o SesSetGrid.o SesSetLabel.o SesSetMaterialOrder.o SesSetSignificantDigits.o SesSetup.o SesSetTableIndex.o SesSetValidate.o SesSetVersion.o SesSkip.o SesVersion.o SesWrite1D.o SesWrite2D.o SesWrite3D.o SesWriteComments.o SesWriteNext.o SesWriteNumber.o SesWritePairs.o SesWriteWord.o SesExit.o SesFormat.o SesGetComments.o SesGetDate.o SesGetVersion.o SesPrintErrorMessage.o SesGetGrid.o SesGetTableSizes.o SesWriteSetup.o SesGetAvailableFormats.o


$(DYNAMIC_LIBRARY) : MySesIO_SesIO.h $(OBJ) $(ROOT)/src/libses.a SesIO.class 
	gcc $(SHARED_FLAG) $(LOCAL_LDFLAGS) $(JFLAGS) -o $(DYNAMIC_LIBRARY) $(OBJ) -L$(SES_LIB_PATH) -lxml2 -lses -lc;


SesIO.class : MySesIO/SesIO.java 
	$(JAVACC)  -d . MySesIO/SesIO.java

MySesIO_SesIO.h : SesIO.class
	$(JAVAH) -jni MySesIO.SesIO


.c.o :  
	$(CC) $(CFLAGS) $(LOCAL_CFLAGS) -fPIC -o $@ $<	

############33
clean :
	rm libsesj.so; new_filename;*.class;rm SesIO.h; rm MySesIO/SesIO.class; rm MySesIO_SesIO.h; rm *.so;rm *.o; rm *~;rm ses_defines/*.class; rm ses_writer;
