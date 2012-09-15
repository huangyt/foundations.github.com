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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ThreadGroup.h,v 1.10 2005/02/05 10:44:57 kommer Exp $

#ifndef acdk_lang_ThreadGroup_h
#define acdk_lang_ThreadGroup_h

#include <acdk.h>
#include "Thread.h"
#include <acdk/util/Vector.h>


namespace acdk {
namespace lang {


ACDK_DECL_CLASS(Thread);

ACDK_DECL_CLASS(ThreadGroup);

/** 
  A ThreadGroup controls a group of threads
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:44:57 $
*/  
class ACDK_CORE_PUBLIC ThreadGroup
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(ThreadGroup)
public:
  /**
    create a new thread group on given name
  */
  ThreadGroup(IN(RString) name);
  /**
    create a new thread group as child for given parent
  */
  ThreadGroup(IN(RThreadGroup) parent, IN(RString) name);
  /**
    return the root thread group (in which also the main-thread
    is running
  */
  static RThreadGroup getRoot();
  /**
    return the number of active threads in this thread group
  */
  int activeCount();
  /**
    return the number of active child threads groups in this thread group
  */
  int activeGroupCount();
  /**
    @deprecated has no functionality
  */
  void checkAccess();
  /**
    @throw IllegalThreadStateException if (activeCount() != 0)
  */
  void destroy();
  /**
    Fill list with all threads of this thread group
  */
  int enumerate(IN(RThreadArray) list, bool recursive = false);
  /**
    Fill list with all child threads groups of this thread group
  */
  int enumerate(IN(RThreadGroupArray) list, bool recursive = false);
  /**
    return the maximum priority of this thread group
  */
  int getMaxPriority();
  /**
    return the name of this thread group
  */
  RString getName();
  /**
    return the parent thread group.
    It may return Nil if this thread group has no parent
  */
  RThreadGroup getParent();
  /**
    return true if this thread group is a deamon thread group
  */
  bool isDaemon();
  /**
    @deprecated unsupported call
  */
  bool isDestroyed();
  /**
    @deprecated unsupported call
  */
  void interrupt();
  /**
    Print threads to System::out
  */
  void list();
  /**
    return true if group is parent of this group
  */
  bool parentOf(IN(RThreadGroup) group);
  /**
    set this thread group as deamon
  */
  void setDaemon(bool on);
  /**
    set the maximum priority of 
    this thread group.
    @todo has no functionality
  */
  void setMaxPriority(int prio);
  virtual RString toString();
  /**
    will be called if a thread ends with uncatched exception
  */
  virtual void uncaughtException(IN(RThread) t, IN(RThrowable) e);

private:
  ThreadGroup();
  void _removeThread(IN(RThread) thread);
  void _addThread(IN(RThread) thread);
  RThreadGroup _parent;
  RString _name;
  ::acdk::util::RVector _children; // contains RThreadGroup
  ::acdk::util::RVector _threads; // contains RThread
  int _maxPriority;
  bool _isDaemon;
  static RThreadGroup _root;
  friend class Thread;
};

} // lang
} // acdk

#endif //acdk_lang_ThreadGroup_h

