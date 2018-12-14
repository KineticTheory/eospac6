#-----------------------------------------------------------------------------
# Build system for EOSPAC6
# 
# Example:
# 
# cmake -DCMAKE_INSTALL_PREFIX=<directory> \
#    /path/to/eospac6/Source/CMakeLists.txt
#-----------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.11.0 FATAL_ERROR)
project( Eospac6
  VERSION 6.3.1
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

if( "${CMAKE_C_COMPILER_ID}"   STREQUAL "MSVC" )
  set( _USE_MATH_DEFINES 1 )
  # set( MD_or_MT "/MD" )
  # set( MD_or_MT_debug "${MT_or_MD}d" )
  set( CMAKE_C_FLAGS "/W0 /Gy /fp:precise /DWIN32 /D_WINDOWS /MP" )
  set( CMAKE_C_FLAGS_DEBUG " /Od /Zi" ) #  /DDEBUG /D_DEBUG"
  set( CMAKE_C_FLAGS_RELEASE "/O2 /DNDEBUG" )
  set( CMAKE_C_FLAGS_MINSIZEREL "/O1 /DNDEBUG" )
  set( CMAKE_C_FLAGS_RELWITHDEBINFO "/O2 /Zi /DDEBUG" )

  # Suppress some MSVC warnings about "unsafe" pointer use.
  if(MSVC_VERSION GREATER 1399)
    string( APPEND CMAKE_C_FLAGS
      " /D_CRT_SECURE_NO_DEPRECATE /D_SCL_SECURE_NO_DEPRECATE /D_SECURE_SCL=0" )
  endif()

  # If building static libraries, inlcude debugging information in the library.
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

#-----------------------------------------------------------------------------
# Sources -> Library
#-----------------------------------------------------------------------------

file( GLOB libsources ${Eospac6_SOURCE_DIR}/src/*.c )
list( REMOVE_ITEM libsources "${Eospac6_SOURCE_DIR}/src/TEST_FUNCTIONS.c" )
list( REMOVE_ITEM libsources "${Eospac6_SOURCE_DIR}/src/eos_FC_Interface.c" )
add_library( libeospac6 ${libsources} )
target_include_directories( libeospac6
  PUBLIC 
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/src>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/include>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/src/internals/ascii>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/src/internals/binary>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/src/internals/llnl_xml>
    $<BUILD_INTERFACE:${Eospac6_SOURCE_DIR}/ses_io/src/internals/xml>
    $<INSTALL_INTERFACE:include> )
#set_target_properties( libeospac6
#  PROPERTIES COMPILE_DEFINITIONS "")
    
    