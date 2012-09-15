// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/Config.h,v 1.22 2005/04/09 19:26:42 kommer Exp $

#ifndef acdk_Config_h
#define acdk_Config_h

#include "Compiler.h"

#ifdef DOXYGENONLY // documentation only
/**
  compile acdk with no multithreading support
  @ingroup acdkplatformmacros
*/
#define ACDK_ST 

/* ============================== Meta Information ============================== */
/**
  if ACDK_NOMETAINFO is defined, all functionality regarding Metainformation,
  acdk/lang/Class and acdk/lang/reflect/ * will be deactivated.
  In normal case you should not define this, until you want to have a 
  tiny static version of the acdk library.
  @ingroup acdkplatformmacros
  */
#define ACDK_NOMETAINFO

/**
  If defined ACDK provides some interface functions to Standard C++ library
  @note currently not implemented
  @ingroup acdkmacros
*/
#define ACDK_STD_CPP_CONVERTERS

/* ============================== Debugging ============================== */

/** 
  enables debugging information 
  @ingroup acdkmacros
*/
#define ACDK_DEBUG

/*
  ACDK_NO_NULLPOINTER_CHECKING if active, no NullpointerException will be thrown 
  Set this only, if you are sure, that your code is very proofed.
  @ingroup acdkplatformmacros
*/
#define ACDK_NO_NULLPOINTER_CHECKING 

/**
  if ACDK_NO_BADCAST_CHECKING is defined, no ClassCastException will be thrown.
  Set this only, if you are sure, that your code has no casting errors.
  @ingroup acdkplatformmacros
*/
# define ACDK_CHECKBADCAST(condition, excode) do { } while(false)


#endif // DOXYGENONLY end documentation

#if !defined(ACDK_ST) && !defined(ACDK_MT)
# define ACDK_MT
#endif



/* ============================== Memory Management ============================== */



#ifdef ACDK_NO_BADCAST_CHECKING
# define ACDK_CHECKBADCAST(condition, excode) do { } while(false)
#else //ACDK_NO_BADCAST_CHECKING
# define ACDK_CHECKBADCAST(condition, excode) if (condition) excode
#endif //ACDK_NO_BADCAST_CHECKING

// ACDK_NO_BADCAST_CHECKING 


 
#ifdef ACDK_USE_EXT_REFERER
# ifdef ACDK_USE_GC
#   undef ACDK_USE_GC
# endif
#endif



#ifdef ACDK_NO_SIZE_T
typedef unsigned int size_t;
#endif //ACDK_NO_SIZE_T

/**
  define of a 16bit unicode char
  @ingroup acdkkeywords
*/
typedef unsigned short ucchar;
/**
  alias to ucchar
  define of a 16bit unicode char
  @ingroup acdkkeywords
*/
typedef unsigned short uc2char;
/**
  defines a 32bit unicode character
  @ingroup acdkkeywords
*/
typedef unsigned long uc4char;

#ifdef DOXYGENONLY
/**
  defines a 64 bit long
  @ingroup acdkkeywords
*/
typedef long long jlong;
/**
  defines a byte type
  @ingroup acdkkeywords
*/
typedef unsigned char byte;

/** 
  use this to define constant jlong 
  @code
  jlong l = JLONG_CONSTANT(123456789234);
  @endcode
*/
#define JLONG_CONSTANT(theconst) theconst##ULL

#endif //DOXYGENONLY

#if defined(_MSC_VER)
  typedef __int64 jlong;
  typedef unsigned char byte;
# define JLONG_CONSTANT(theconst) theconst
#elif defined(__BORLANDC__)
  typedef __int64 jlong;
  typedef unsigned char byte;
# define JLONG_CONSTANT(theconst) theconst##i64
#elif defined(__GNUG__)
  typedef long long jlong;
  typedef unsigned char byte;
# define JLONG_CONSTANT(theconst) theconst##ULL
#elif defined(__sparc) // 32bit sparc
 typedef long long jlong;
 typedef unsigned char byte;
# define JLONG_CONSTANT(theconst) theconst##ULL
#elif defined(__i386) && defined(__SUNPRO_CC)
 typedef long long jlong;
 typedef unsigned char byte;
# define JLONG_CONSTANT(theconst) theconst##ULL
#elif defined(__sparcv9) // 64 bit sparc
 typedef long jlong;
 typedef unsigned char byte;
# define JLONG_CONSTANT(theconst) theconst
#elif defined(__MWERKS__)
  typedef long long jlong;
  typedef unsigned char byte;
#define JLONG_CONSTANT(theconst) theconst
#elif defined(__INTEL_COMPILER) // also defined: __ICC -dryrun
  typedef long long jlong;
  typedef unsigned char byte;
# define JLONG_CONSTANT(theconst) theconst##ULL
#else
# error unsuported plattform for jlong
#endif 


#ifdef ACDK_MT
/** before including solaris standard c header to use the reentrant version */
# ifndef _REENTRANT
#  define _REENTRANT
# endif
#endif


#ifndef _UNICODE
# define _UNICODE 
#endif
#ifndef UNICODE
# define UNICODE 
#endif

#endif //acdk_Config_h

