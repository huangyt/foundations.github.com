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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ThreadGroup.cpp,v 1.21 2005/04/30 14:29:14 kommer Exp $


#include <acdk.h>
#include <acdk/lang/sys/core_specific.h>

#include "Thread.h"
#include "ThreadLocal.h"
#include "System.h"
#include <acdk/io/PrintWriter.h>

#include "IllegalThreadStateException.h"
#include "UnsupportedOperationException.h"

namespace acdk {
namespace lang {

USING_CLASS(::acdk::util::, Vector);
USING_CLASS(::acdk::util::, Iterator);

RThreadGroup ThreadGroup::_root = Nil;

//static 
RThreadGroup 
ThreadGroup::getRoot()
{
  if (_root == Nil) 
  {
    _root = new ThreadGroup();
    acdk::lang::System::registerStaticReference(_root);
  }
  return _root;
}
  

//private
ThreadGroup::ThreadGroup()
: _parent(Nil),
  _name("Root Thread Group"),
  _children(new Vector()),
  _threads(new Vector()),
  _maxPriority(0),
  _isDaemon(false)
{
  
}

ThreadGroup::ThreadGroup(IN(RString) name)
: Object(),
  _parent(Nil),
  _name(name),
  _children(new Vector()),
  _threads(new Vector()),
  _maxPriority(0),
  _isDaemon(false)
{
  _parent = Thread::currentThread()->getThreadGroup();
}

ThreadGroup::ThreadGroup(IN(RThreadGroup) parent, IN(RString) name)
: Object(),
  _parent(parent),
  _name(name),
  _children(new Vector()),
  _threads(new Vector()),
  _maxPriority(0),
  _isDaemon(false)
{
}

int 
ThreadGroup::activeCount()
{
  RIterator it = _threads->iterator();
  int ac = 0;
  while (it->hasNext() == true) {
    RThread t = RThread(it->next());
    if (t != Nil && t->isAlive() == true)
      ++ac;
  }
  RIterator git = _children->iterator();
  while (git->hasNext() == true)
    ac += RThreadGroup(git->next())->activeCount();
  return ac;
}
 
int 
ThreadGroup::activeGroupCount()
{
  int i = 1;
  RIterator git = _children->iterator();
  while (git->hasNext() == true)
    i += RThreadGroup(git->next())->activeGroupCount();
  return i;
}

void 
ThreadGroup::checkAccess()
{
}
 
void 
ThreadGroup::destroy()
{
  if (activeCount() != 0)
    THROW0(IllegalThreadStateException);

}
 
int 
ThreadGroup::enumerate(IN(RThreadArray) list, bool recursive)
{
  int i = 0;
  for (; i < list->length() && list[i] != Nil; i++)
    ;
  RIterator it = _threads->iterator();
  for (i = 0; it->hasNext() == true && i < list->length(); )  {
    RThread t = RThread(it->next());
    if (t->isAlive() == true)
      list[i++] = RThread(it->next());

  }
  if (recursive == false || _parent == Nil)
    return i;
  RIterator git = _children->iterator();
  while (git->hasNext() == true)
    i += RThreadGroup(git->next())->enumerate(list, recursive);
  return i;
}

int 
ThreadGroup::enumerate(IN(RThreadGroupArray) list, bool recursive)
{
  int i = 0;
  for (; i < list->length() && list[i] != Nil; i++)
    ;
  RIterator it = _children->iterator();
  for (i = 0; it->hasNext() == true && i < list->length(); i++) 
    list[i] = RThreadGroup(it->next());
  if (recursive == false || _parent == Nil)
    return i;
  return _parent->enumerate(list, recursive);
}
 
int 
ThreadGroup::getMaxPriority()
{
  return _maxPriority;
}
 
RString 
ThreadGroup::getName()
{
  return _name;
}
 
RThreadGroup 
ThreadGroup::getParent()
{
  return _parent;
}
 
bool 
ThreadGroup::isDaemon()
{
  return _isDaemon;
}

bool 
ThreadGroup::isDestroyed()
{
  THROW0(UnsupportedOperationException);
  return false;
}

void 
ThreadGroup::interrupt()
{
  RIterator it = _threads->iterator();
  while (it->hasNext() == true )  {
    RThread t = RThread(it->next());
    if (t != Nil && t->isAlive() == true)
      t->interrupt();
  }
}

void 
ThreadGroup::list()
{
  System::out->println(toString() + ":");
  RIterator it = _threads->iterator();
  while (it->hasNext() == true )  {
    RThread t = RThread(it->next());
    if (t != Nil) {
      System::out->println(t->toString());
    }    
  }
}
 
bool 
ThreadGroup::parentOf(IN(RThreadGroup) group)
{
  if (RThreadGroup(this) == group)
    return true;
  if (_parent == Nil)
    return false;
  return _parent->parentOf(group);
}
 
void 
ThreadGroup::setDaemon(bool on)
{
  _isDaemon = on;
}
 
void 
ThreadGroup::setMaxPriority(int prio)
{
  _maxPriority = prio;
}

//virtual 
RString 
ThreadGroup::toString()
{
  return "ThreadGroup: " + _name;
}

// virtual
void 
ThreadGroup::uncaughtException(IN(RThread) t, IN(RThrowable) ex)
{
  ex->printStackTrace();
  System::err->println("Uncaught Exception: [" + ex->toString() + "] in Thread [" + t->toString() + "]");
}

void 
ThreadGroup::_addThread(IN(RThread) thread)
{
  _threads->add((RObject)thread);
}

void 
ThreadGroup::_removeThread(IN(RThread) thread)
{
  if (_threads->contains(&thread))
    _threads->remove(&thread);
}


//static 
void 
ThreadLocal::threadEnd()
{
  if (System::isInMain() == false)
    return;
  ::acdk::lang::sys::core_specific::thread_cleanup();
}

} // lang
} // acdk


