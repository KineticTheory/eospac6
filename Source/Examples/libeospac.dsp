# Microsoft Developer Studio Project File - Name="libeospac" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libeospac - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libeospac.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libeospac.mak" CFG="libeospac - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libeospac - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libeospac - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "libeospac - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "libeospac - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "_LIB" /D "MixedCase" /D "_MKDIR_NOT_AVAIL" /D "WIN32" /D "_DEBUG" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "libeospac - Win32 Release"
# Name "libeospac - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\src\eos_Access.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_Data.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_DataMap.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_ErrorHandler.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_Interface.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_Interface.f90
# PROP Intermediate_Dir "Debugf90"
# End Source File
# Begin Source File

SOURCE=..\src\eos_Interpolation.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType1.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType2.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType3.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType4.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType5.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType6.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_Utils.c
# End Source File
# Begin Source File

SOURCE=..\src\eos_UtilsRage.c
# End Source File
# Begin Source File

SOURCE=..\src\TEST_FUNCTIONS.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\src\eos_Access.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_Data.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_DataMap.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_ErrorHandler.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_Interface.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_Interpolation.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType1.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType2.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType3.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType4.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType5.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_RecordType6.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_types.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_types_internal.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_Utils.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_UtilsRage.h
# End Source File
# Begin Source File

SOURCE=..\src\eos_wrappers.h
# End Source File
# Begin Source File

SOURCE=..\src\TEST_FUNCTIONS.h
# End Source File
# End Group
# End Target
# End Project
