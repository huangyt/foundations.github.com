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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/lang.h,v 1.14 2005/02/05 10:44:57 kommer Exp $

#ifndef acdk_lang_lang_h
#define acdk_lang_lang_h

#ifndef acdk_h
#include <acdk.h>
#endif // acdk_h


namespace acdk {
/**
  Corresponds to the java.lang package.
  Implements basic Types.
*/
 namespace lang {

  class String;
  class Comparable;
  class RString;
  /*
  ACDK_DECL_CLASS(Boolean);
  ACDK_DECL_CLASS(Byte);
  ACDK_DECL_CLASS(Character);
  ACDK_DECL_CLASS(Class);
  ACDK_DECL_CLASS(ClassLoader);
  ACDK_DECL_CLASS(Double);
  ACDK_DECL_CLASS(Float);
  ACDK_DECL_CLASS(Integer);
  ACDK_DECL_CLASS(Long);
  ACDK_DECL_CLASS(Number);
  ACDK_DECL_CLASS(Object);
  ACDK_DECL_CLASS(Package);
  ACDK_DECL_CLASS(Process);
  ACDK_DECL_CLASS(Process_PipeReader);
  ACDK_DECL_CLASS(Runtime);
  ACDK_DECL_CLASS(Short);
  ACDK_DECL_CLASS(StringBuffer);
  ACDK_DECL_CLASS(SystemImpl);
  ACDK_DECL_CLASS(Thread);
  ACDK_DECL_CLASS(ThreadGroup);
  ACDK_DECL_CLASS(ThreadLocal);
  ACDK_DECL_CLASS(Void);

  ACDK_DECL_INTERFACE(Cloneable);
  ACDK_DECL_INTERFACE(Comparable);
  ACDK_DECL_INTERFACE(Initializer);
  ACDK_DECL_INTERFACE(Runnable);
  
  ACDK_DECL_THROWABLE(Throwable, Object);
   ACDK_DECL_THROWABLE(Error, Throwable);
    ACDK_DECL_THROWABLE(SystemError, Error);
   ACDK_DECL_THROWABLE(Exception, Throwable);
    ACDK_DECL_THROWABLE(ClassCastException, Exception);
    ACDK_DECL_THROWABLE(DmiException, Exception);
      ACDK_DECL_THROWABLE(NoSuchDmiElementException, DmiException);
        ACDK_DECL_THROWABLE(ClassNotFoundException, NoSuchDmiElementException);

    ACDK_DECL_THROWABLE(CloneNotSupportedException, Exception);
    ACDK_DECL_THROWABLE(IllegalAccessException, Exception);
    ACDK_DECL_THROWABLE(InstantiationException, Exception);
    ACDK_DECL_THROWABLE(InterruptedException, Exception);
    ACDK_DECL_THROWABLE(RuntimeException, Exception);
     ACDK_DECL_THROWABLE(IllegalArgumentException, RuntimeException);
      ACDK_DECL_THROWABLE(IllegalThreadStateException, IllegalArgumentException);
      ACDK_DECL_THROWABLE(NumberFormatException, IllegalArgumentException);
     ACDK_DECL_THROWABLE(IllegalMonitorStateException, RuntimeException);
     ACDK_DECL_THROWABLE(IllegalStateException, RuntimeException);
     ACDK_DECL_THROWABLE(IndexOutOfBoundsException, RuntimeException);
      ACDK_DECL_THROWABLE(ArrayIndexOutOfBoundsException, IndexOutOfBoundsException);
      ACDK_DECL_THROWABLE(StringIndexOutOfBoundsException, IndexOutOfBoundsException);
     ACDK_DECL_THROWABLE(NullPointerException, RuntimeException);
     ACDK_DECL_THROWABLE(UnsupportedOperationException, RuntimeException);
  */
  namespace sys {
    // no public declarations at all
  } // acdk::lang::sys

  namespace ref {
/*
   ACDK_DECL_CLASS(PhantomReference);
   ACDK_DECL_CLASS(Reference);
   ACDK_DECL_CLASS(ReferenceQueue);
   ACDK_DECL_CLASS(Reference);
   ACDK_DECL_CLASS(ReferenceQueue);
   ACDK_DECL_CLASS(SoftReference);
   ACDK_DECL_CLASS(WeakReference);

   ACDK_DECL_INTERFACE(Runnable);
*/
  } // acdk::lang::ref

  namespace reflect {
/*
   ACDK_DECL_CLASS(AccessibleObject);
   ACDK_DECL_CLASS(Constructor);
   ACDK_DECL_CLASS(Field);
   ACDK_DECL_CLASS(Method);
   
   ACDK_DECL_INTERFACE(Member);

   ACDK_DECL_THROWABLE(InvocationTargetException, Exception);
  */ 
  } // acdk::lang::reflect


 } // lang
} //acdk



#endif // acdk_lang_lang_h

