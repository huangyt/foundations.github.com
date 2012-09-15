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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/sys.h,v 1.14 2005/04/10 12:52:40 kommer Exp $

#ifndef acdk_lang_sys_sys_h
#define acdk_lang_sys_sys_h

#include <stddef.h> 
#include "core_threadsys.h"

/**
  @defgroup acdkmacros Common Macros
  @htmlonly
  <pageintro>
    A list of macros commonly used in ACDK code.
  </pageintro>
  @endhtmlonly
  Please refer also to gw_ref[acdk_hb_lang].
*/
/**
  @defgroup acdkmetainfo Macros parsed by Metainfo compiler
  @htmlonly
  <pageintro>
    A list of macros parsed by the ACDK metacompiler.
  </pageintro>
  @endhtmlonly
  Please refer also to gw_ref[acdk_hb_mi].
  @see acdkkeywords
*/
/**
  @defgroup acdksmartptr SmartPointers and Garbage Collection
   @htmlonly
  <pageintro>
    Types and Macros used in connection of smart pointer and garbage collection.
  </pageintro>
  @endhtmlonly
  Please refer also to gw_ref[acdk_hb_lang].
  
*/
/**
  @defgroup acdkkeywords  ACDK pseudo keywords
   @htmlonly
  <pageintro>
    Extended/pseudo keywords known by ACDK.
  </pageintro>
  @endhtmlonly
  Please refer also to gw_ref[acdk_hb_lang].
  @see acdkmetainfo
  @see acdkmacros
*/

/**
  @defgroup acdkplatformmacros ACDK Platform macros
   @htmlonly
  <pageintro>
     Platform macros identifies the used compiler or platform
    under which ACDK will be compiled.
  </pageintro>
  @endhtmlonly
*/

/**
  @defgroup acdkstring ACDK String macros/function/classes
  Please refer also to gw_ref[acdk_hb_lang_string]
  @htmlonly
  <pageintro>
     Classes, functions and macros used to handle Strings.
  </pageintro>
  @endhtmlonly
*/
namespace acdk {
namespace lang {

/** 
  System internal classes to support basic Language Features
*/
namespace sys {
  // nothing here, because declared just for documentation

} // sys
} // lang
} // acdk

#if defined(DOXYGENONLY)
/**
  internal macro to cast from pointer to a int
  @ingroup acdkmacros
  @ingroup acdkplatformmacros
*/
# define ACDK_CAST_PTR2INT(ptr) (int)(jlong)((void*)ptr)

/**
  internal macro to cast from an integer type (jlong) 
  to a pointer.
  @ingroup acdkmacros
  @ingroup acdkplatformmacros
*/
# define ACDK_CAST_INT2PTR(T, ival) (T*)(jlong)(ival)

/** 
  returned the size with aligment. On sparc pointer should be aligned to 8 bytes.
  @ingroup acdkmacros
  @ingroup acdkplatformmacros
*/
#define aligned(size) (((size) + (8 - 1)) & ~(8 - 1))

#endif //defined(DOXYGENONLY)

#if defined(ACDK_64_BIT_PTR)
# define ACDK_CAST_PTR2INT(ptr) (int)(jlong)((void*)ptr)
# define ACDK_CAST_INT2PTR(T, ival) (T*)(jlong)(ival)
#else 
# define ACDK_CAST_PTR2INT(ptr) (int)((void*)ptr)
# define ACDK_CAST_INT2PTR(T, ival) (T*)(ival)
#endif


// don't include anything here, because RefHolder.h needs above namespace-declaration



// this is used for internal pointer-arithmetic
// maybe it should return jlong
/*
inline
unsigned int
aligned(int size)
{
#if defined(ACDK_OS_SOLARIS) && defined(__sparc__)
  return ((size + (8 - 1)) & ~(8 - 1));
#else
  return size;
#endif
}
*/
#if defined(ACDK_OS_SOLARIS) && defined(__sparc__)
#define aligned(size) (((size) + (8 - 1)) & ~(8 - 1))
#else
#define aligned(size) size
#endif
/**
  returns the machine aligned size
  @see aligned(size)
  @ingroup acdkmacros
  @ingroup acdkplatformmacros
*/
#define ALIGNEDSIZEOF(o) aligned(sizeof(o))



#endif //acdk_lang_sys_sys_h










