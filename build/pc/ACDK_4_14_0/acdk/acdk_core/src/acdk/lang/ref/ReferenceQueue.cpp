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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/ReferenceQueue.cpp,v 1.12 2005/03/07 13:53:35 kommer Exp $

#include <acdk.h>
#include "ref.h"

namespace acdk {
namespace lang {
namespace ref {

using namespace acdk::lang;

ReferenceQueue::ReferenceQueue()
: Object(),
  _first(Nil)
{
}

RReference 
ReferenceQueue::poll()
{ 
  SYNCTHIS();
  return _dequeue();
}

RReference 
ReferenceQueue::remove(jlong timeout/* = 0*/)// throw(RInterruptedException, RThrowable)
{
  SYNCTHIS();
  if (_first == Nil) {
    wait(timeout);
  }
  return _dequeue();
}

void 
ReferenceQueue::enqueue(IN(RReference) ref)
{
  SYNCTHIS();
  //sys::coreout << "E " << (void*) ref.iptr() << " " << ref->refCount() << sys::eofl;
  if (_first == Nil)
  {
    _first = ref;
    return;
  }
  ref->_next = _first;
  _first = ref;
  notify();
}

RReference 
ReferenceQueue::_dequeue()
{
  
  if (_first == Nil)
    return Nil;
  if (_first->_next == Nil)
  {
    RReference result = _first;
    _first = Nil;
    result->_isDequeued = true;
    //sys::coreout << "D: " << (void*) result.iptr() << " " << result->refCount() << sys::eofl;
    return result;
  }
  RReference fn = _first->_next;
  RReference result = _first;
  _first = _first->_next;
  result->_next = Nil;
  result->_isDequeued = true;
  //sys::coreout << "D: " << (void*) result.iptr() << " " << result->refCount() << sys::eofl;
  return result;
}
  
} // ref
} // lang
} // acdk


