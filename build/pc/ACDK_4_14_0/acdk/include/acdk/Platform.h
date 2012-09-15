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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/Platform.h,v 1.32 2005/04/09 19:26:42 kommer Exp $

#ifndef acdk_Platform_h
#define acdk_Platform_h

#ifdef DOXYGENONLY // documentation only
/**
  Compiled under Microsoft Windows platform
  @ingroup acdkplatformmacros
*/
#define ACDK_OS_WIN32
/**
  Compiled under Microsoft Windows platform using gcc and the cygwin runtime environment
  @ingroup acdkplatformmacros
*/
#define ACDK_OS_CYGWIN32

/**
  Compiled FreeBSD
  @ingroup acdkplatformmacros
*/
#define ACDK_OS_BSD


/**
  Compiled under Linux
  @ingroup acdkplatformmacros
*/
#define ACDK_OS_LINUX

/**
  Compiled under Sun Solaris
  @ingroup acdkplatformmacros
*/
#define ACDK_OS_SOLARIS

/**
  Compiled under Mac OS X/Darwin
  @ingroup acdkplatformmacros
*/
#define ACDK_OS_DARWIN


/**
  Compiled under a unix flavor operation system (Linux, Solaris, FreeBSD)
  @ingroup acdkplatformmacros
*/
#define ACDK_OS_UNIX

/**
  Compiled under cpu platform with big endian byte order
  @ingroup acdkplatformmacros
*/
#define ACDK_BIGENDIAN 1
/**
  Compiled under cpu platform with little endian byte order
  @ingroup acdkplatformmacros
*/
#define ACDK_LITTLEENDIAN 

/**
  maximum character length of a path name

  @ingroup acdkplatformmacros
*/
#define ACDK_MAX_PATH 1024

/**
  number of wide character supports by native os calls
  is 2 or 4
  @ingroup acdkplatformmacros
*/
#define ACDK_UCLITERAL_WIDE 2

/**
  Using posix thread library
  @ingroup acdkplatformmacros
*/
#define POSIX_THREADS
/**
  Using win32 thread api
  @ingroup acdkplatformmacros
*/
#define WIN32_THREADS

/**
  is defined if the platform pointer has 64 bit
  @ingroup acdkplatformmacros
*/
#define ACDK_64_BIT_PTR

#endif // 0 documentation only

#if defined(_MSC_VER) || defined(WIN32) || defined(_WIN32) || (defined(__BORLANDC__) && !defined(__linux__))
# if !defined(ACDK_OS_WIN32)
#  define ACDK_OS_WIN32
# endif
#endif


#ifdef OS_CYGWIN32
//#  define ACDK_OS_LINUX
#  define ACDK_OS_WIN32
#  define ACDK_OS_CYGWIN32
#  if !defined(WIN32)
#   define WIN32
#  endif
#endif

#ifdef ACDK_MINGW
# define ACDK_OS_MINGW
# if !defined(ACDK_OS_WIN32)
#   define ACDK_OS_WIN32
# endif
#endif

#if defined(__FreeBSD__)
# define ACDK_OS_BSD
#endif

#if defined(OS_LINUX) || defined(linux)
# define ACDK_OS_LINUX
# ifndef _GNU_SOURCE
#  define _GNU_SOURCE
# endif
# ifndef __USE_UNIX98
#  define __USE_UNIX98
# endif
# ifndef _REENTRANT
#  define _REENTRANT
# endif
# if defined(__alpha__)
#  define ACDK_ATOMIC_USE_PTHREAD
# endif 
#endif

#if defined(OS_SOLARIS) || defined(sun)
# define ACDK_OS_SOLARIS
// solaris has enhancements pthread_mutexattr_settype()
# define ACDK_HAS_PTHREAD_RECURSIVE_MUTEX
# define ACDK_HAS_PTHREAD_MUTEX_ATTR_SETTYPE
#endif

#ifdef OS_DARWIN
# define ACDK_OS_DARWIN
# define ACDK_MAX_PATH 1024
# define ACDK_OS_UNIX
# define POSIX_THREADS
# define _ANSI_LIBRARY
# define ACDK_HAS_PTHREAD_MUTEX_ATTR_SETTYPE
#endif

#if defined(ACDK_OS_LINUX) || defined(ACDK_OS_SOLARIS) || defined(ACDK_OS_BSD) || defined(ACDK_OS_DARWIN)
# ifndef ACDK_OS_UNIX
#  define ACDK_OS_UNIX
# endif
# ifndef ACDK_OS_DARWIN
#   define ACDK_HAS_PTHREAD_RECURSIVE_MUTEX
# endif
# if defined(ACDK_OS_BSD)
#  define ACDK_HAS_PTHREAD_MUTEX_ATTR_SETTYPE
# endif
# define ACDK_HAS_PTHREAD_MUTEX_SETKIND
#endif

#if defined(ACDK_OS_UNIX) && !defined(ACDK_OS_CYGWIN32)
# define POSIX_THREADS
#endif


#if defined(ACDK_OS_WIN32) || defined(ACDK_OS_CYGWIN32)
#  define ACDK_MAX_PATH 1024
#endif //ACDK_OS_WIN32

#ifdef ACDK_OS_UNIX
#  define ACDK_MAX_PATH 1024
# ifndef ACDK_OS_CYGWIN32
#    define HAS_FNMATCH_H 1
# endif
#endif //ACDK_OS_UNIX



#if defined(__sparc__) || defined(__alpha__)
# define ACDK_BIGENDIAN 1
#else
# define ACDK_LITTLEENDIAN 1
#endif

#if defined(__amd64__)
# define ACDK_64_BIT_PTR
#endif

#if defined(ACDK_OS_UNIX) && !defined HAVE_UNISTD_H
#  define HAVE_UNISTD_H
#endif

#define ACDK_UCCHAR_SIZE 2

#if defined(ACDK_OS_WIN32)
# define ACDK_UCLITERAL_WIDE 2
#else //defined(ACDK_OS_WIN32)
# define ACDK_UCLITERAL_WIDE 4
#endif //defined(ACDK_OS_WIN32)

#endif //acdk_Platform_h

