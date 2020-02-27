#-----------------------------------------------------------------------------
# Build system for EOSPAC6
#
# Example:
#
# cmake -DCMAKE_INSTALL_PREFIX=<directory> \
#       -DEOSPAC6_LIBRARY_TYPE=[STATIC|SHARD] \
#    /path/to/eospac6/Source/CMakeLists.txt
#-----------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.11.0 FATAL_ERROR)
project( Eospac6
  VERSION 6.4.0
  DESCRIPTION "Access library for equation of state data (Sesame)"
  LANGUAGES C )

# Do not look for Fortran for
# 1. XCode based Generators, or
# 2. Visual Studio IDE or NMake Generators (MSYS or CYGWIN environments will
#    look for Fortran).
# 3. Ninja, Codeblocks or Eclipse CDT4 generators.
# 4. Unix Makefile generators when $ENV{FC} is not set (clang on Linux)
if( ${CMAKE_GENERATOR} MATCHES "Unix Makefiles" AND
    (DEFINED ENV{FC} OR DEFINED CMAKE_Fortran_COMPILER ))
  enable_language( Fortran OPTIONAL )
endif()

# Build system configuration files are located here.
# set( CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/config )

#-----------------------------------------------------------------------------
# Compiler flags
#-----------------------------------------------------------------------------

set( CMAKE_POSITION_INDEPENDENT_CODE ON )
if( NOT DEFINED EOSPAC6_LIBRARY_TYPE )
  set( EOSPAC6_LIBRARY_TYPE "STATIC" )
 endif()
set( EOSPAC6_LIBRARY_TYPE "${EOSPAC6_LIBRARY_TYPE}" CACHE STRING
     "Keyword for creating new libraries (STATIC or SHARED).")
set_property( CACHE EOSPAC6_LIBRARY_TYPE PROPERTY STRINGS SHARED STATIC)     

if( "${CMAKE_C_COMPILER_ID}" STREQUAL "MSVC" )
  set( _USE_MATH_DEFINES 1 )
  set( CMAKE_C_FLAGS "/W0 /Gy /fp:precise /DWIN32 /D_WINDOWS /MP" )
  set( CMAKE_C_FLAGS_DEBUG " /Od /Zi /MDd" ) #  /DDEBUG /D_DEBUG"
  set( CMAKE_C_FLAGS_RELEASE "/O2 /DNDEBUG /MD" )
  set( CMAKE_C_FLAGS_MINSIZEREL "/O1 /DNDEBUG /MD" )
  set( CMAKE_C_FLAGS_RELWITHDEBINFO "/O2 /Zi /DDEBUG /MDd" )

  # Suppress some MSVC warnings about "unsafe" pointer use.
  if(MSVC_VERSION GREATER 1399)
    string( APPEND CMAKE_C_FLAGS
      " /D_CRT_SECURE_NO_DEPRECATE /D_SCL_SECURE_NO_DEPRECATE /D_SECURE_SCL=0" )
  endif()

  # If building static libraries, include debugging information in the library.
  #if( ${EOSPAC6_LIBRARY_TYPE} MATCHES "STATIC" )
  #  string( APPEND CMAKE_C_FLAGS_DEBUG " /Z7"   )
  #endif()

  # Silence warnings (might be real issues and should be investigated later.
  # string( APPEND CMAKE_C_FLAGS " /wd6385 /wd6308 /wd4251" )
endif()
set( CMAKE_C_FLAGS                "${CMAKE_C_FLAGS}"                CACHE STRING "compiler flags" FORCE )
set( CMAKE_C_FLAGS_DEBUG          "${CMAKE_C_FLAGS_DEBUG}"          CACHE STRING "compiler flags" FORCE )
set( CMAKE_C_FLAGS_RELEASE        "${CMAKE_C_FLAGS_RELEASE}"        CACHE STRING "compiler flags" FORCE )
set( CMAKE_C_FLAGS_MINSIZEREL     "${CMAKE_C_FLAGS_MINSIZEREL}"     CACHE STRING "compiler flags" FORCE )
set( CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO}" CACHE STRING "compiler flags" FORCE )

set(eospac_std_target_props_C
  C_STANDARD 11                # Force strict ANSI-C 11 standard
  C_EXTENSIONS OFF
  C_STANDARD_REQUIRED ON)

#-----------------------------------------------------------------------------
# Sources -> Library
#-----------------------------------------------------------------------------

file( GLOB libsources 
  ${Eospac6_SOURCE_DIR}/src/*.c 
  ${Eospac6_SOURCE_DIR}/ses_io/src/user_interface/*.c
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/*.c
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/ascii/*.c
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/binary/*.c
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/json_parser/*.c
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/llnl_xml/*.c
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/xml/*.c)
list( REMOVE_ITEM libsources ${Eospac6_SOURCE_DIR}/src/TEST_FUNCTIONS.c )
file( GLOB libincludes 
  ${Eospac6_SOURCE_DIR}/src/*.h 
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/*.h
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/ascii/*.h
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/binary/*.h
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/json_parser/*.h
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/llnl_xml/*.h
  ${Eospac6_SOURCE_DIR}/ses_io/src/internals/xml/*.h
  ${Eospac6_SOURCE_DIR}/ses_io/include/*.h)
list( REMOVE_ITEM libsources ${Eospac6_SOURCE_DIR}/src/TEST_FUNCTIONS.h )
if( MSVC )
  list(APPEND libsources ${libincludes}) # provide links to files in the IDE.
endif()
add_library( libeospac6 ${EOSPAC6_LIBRARY_TYPE} ${libsources} )
target_include_directories( libeospac6
  PUBLIC
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/src>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/include>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/src/internals>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/src/internals/ascii>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/src/internals/binary>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/src/internals/json_parser>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/src/internals/llnl_xml>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/src/internals/xml>
    $<INSTALL_INTERFACE:include> )
set_target_properties( libeospac6 PROPERTIES 
  OUTPUT_NAME "eospac6" 
  WINDOWS_EXPORT_ALL_SYMBOLS ON
  ${eospac_std_target_props_C} )
if( MSVC )
  set_source_files_properties( ${libsources} PROPERTIES 
  COMPILE_DEFINITIONS "EOSPAC6_VERSION_FUNCTIONS_REDEFINED=1;MixedCase=1:_WIN32=1" ) # PC=1
endif()

#-----------------------------------------------------------------------------
# Install
#-----------------------------------------------------------------------------
if( MSVC )
  install( TARGETS libeospac6 
    CONFIGURATIONS ${CMAKE_CONFIGURATION_TYPES}
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin
    ARCHIVE DESTINATION lib )
  # For Debug under VS, also install the pdb files.
  install( FILES ${PROJECT_BINARY_DIR}/libeospac6.dir/Debug/libeospac6.pdb
    CONFIGURATIONS Debug 
    DESTINATION lib)
else()
  install( TARGETS libeospac6 
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin
    ARCHIVE DESTINATION lib )
endif()
install(
  FILES 
    ${Eospac6_SOURCE_DIR}/src/eos_Interface.h
    ${Eospac6_SOURCE_DIR}/src/eos_Interface.proto.h
    ${Eospac6_SOURCE_DIR}/src/eos_universal_types.h    
    ${Eospac6_SOURCE_DIR}/ses_io/include/ses_defines.h
    ${Eospac6_SOURCE_DIR}/src/TEST_FUNCTIONS.h 
    ${Eospac6_SOURCE_DIR}/src/eos_Interface2003.f90
    ${Eospac6_SOURCE_DIR}/src/eos_Interface.f90
    ${Eospac6_SOURCE_DIR}/src/eos_Interface.fi
    # .mod files?
  DESTINATION include )

