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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Throwable.cpp,v 1.33 2005/04/18 20:19:25 kommer Exp $


#include <acdk.h>

#include "Throwable.h"
#include "System.h"

#include "CloneNotSupportedException.h"
#include "InterruptedException.h"
#include "ClassCastException.h"
#include <acdk/lang/dmi/ClazzAttributesRes.h>

#include <acdk/io/PrintWriter.h>

namespace acdk {
namespace lang {

OUT(RThrowListenerArray) getListener()
{
  static RThrowListenerArray listner;
  return listner;
}

//static 
void 
Throwable::registerThrowListener(IN(RThrowListener) listner)
{
  OUT(RThrowListenerArray) listners = getListener();
  if (listners == Nil)
    listners = new ThrowListenerArray(0);
  listners->append(listner);
}

//static 
void 
Throwable::unregisterThrowListener(IN(RThrowListener) listner)
{
  OUT(RThrowListenerArray) listners = getListener();
  if (listners == Nil)
  {
    return;
  }
  for (int i = 0; i < listners->length(); ++i)
  {
    if (listners[i] == listner)
    {
      if (listners->length() == 1)
        listners = Nil;
      else
        listners->remove(i);
      break;
    }
  }
}

//static 
bool 
Throwable::onThrow(IN(RThrowable) ex, int line, const char* file)
{
  OUT(RThrowListenerArray) listners = getListener();
  if (listners == Nil)
    return true;
  RString l = file;
  for (int i = 0; i < listners->length(); ++i)
  {
    if (listners[i]->onThrow(ex, line, l) == false)
      return false;
  }
  return true;
}

Throwable::Throwable()
:  _stackFrame(1) 
{
}


Throwable::Throwable(IN(RString) what, IN(RThrowable) cause)
:  _what(what)
, _cause(cause)
, _stackFrame(1)
{
}

Throwable::Throwable(IN(RThrowable) cause)
: _what(Nil)
, _cause(cause)
, _stackFrame(1)
{
	if (cause != Nil)
		_what = cause->toString();
}

Throwable::~Throwable()
{

}

//virtual
RString
Throwable::getMessage()
{
  return _what;
}

RThrowable 
Throwable::getCause()
{
  return _cause;
}

RThrowable 
Throwable::initCause(IN(RThrowable) cause)
{
  _cause = cause;
  return this;
}

RStackFrameArray
Throwable::getStackFrames()
{
  if (_stackFrames != Nil)
    return _stackFrames;
  _stackFrames = _stackFrame.getStackFrames();
  return _stackFrames;
}

void 
Throwable::writeObject(IN(acdk::io::RObjectWriter) out, IN(RClass) cls)
{
  if (cls == GetClass())
  {
    getStackFrames();
    out->defaultWriteObject(cls, this);
  }
}



//virtual
void
Throwable::printStackTrace()
{
  printStackTrace(System::err);
}



//virtual
void
Throwable::printStackTrace(IN(acdk::io::RPrintWriter) out)
{
  System::printStackTrace(out, getStackFrames());
  if (_cause != Nil)
  {
    out->println("Caused by: " + _cause->getMessage());
    _cause->printStackTrace(out);
  }
}


void
Throwable::throwException(bool onlyIfHasMeta)
{
  /*
  acdk::lang::dmi::ClazzAttributesRes::ResTableEntry rv =
    acdk::lang::dmi::ClazzAttributesRes::getAttribute((acdk::lang::dmi::MetaInfo*)getClazzInfo(), "__throwExceptionFunc");
  if (rv.value.data == 0)
    throw RThrowable(this);
  */
  const acdk::lang::dmi::ClazzInfo* ci = getClazzInfo()->loadFullClazzInfo();
  if (ci == 0)
    return;
  acdk::lang::dmi::ClazzAttributeResValue carv
    = acdk::lang::dmi::ClazzAttributesRes::getAttribute((acdk::lang::dmi::MetaInfo*)ci, "__throwExceptionFunc");
  DispatchThrowableFunc func = (DispatchThrowableFunc)carv.data;
  if (func != 0)
    func(this);
}

IndexOutOfBoundsException::IndexOutOfBoundsException(IN(RString) what, int idx, int length)
: RuntimeException()
, _accessIndex(idx)
, _length(length)
{
  _what = what + " AccessIndex=[" +  idx + "]; MaxIndex=[" + length + "]; ";
}

CloneNotSupportedException::CloneNotSupportedException()
: Exception()
{
}

CloneNotSupportedException::CloneNotSupportedException(IN(RString) what)
: Exception(what)
{
}
/*
BadCastException::BadCastException(IN(RObject) fromObject, IN(RString) toClass)
: Exception(),
  _fromObject(fromObject),
  _toClass(toClass)
{
  RString fromClassName;
  RString toClassName;

  if (_fromObject == Nil) {
    fromClassName = RString("Nil");
  } else {
    fromClassName = fromObject->getName();
  }
  if (_toClass == Nil) {
    toClassName = RString("Nil");
  } else {
    toClassName = _toClass;
  }
  _what = RString("Bad Cast from ") + fromClassName + RString(" to Class ") + toClassName;
}

ClassCastException::ClassCastException(IN(RObject) fromObject, IN(RObject) toObject)
: Exception(),
  _fromObject(fromObject),
  _toObject(toObject)
{
}
*/

#if defined(__BORLANDC__) //### ACDK_NEED_TEMPLATE_INSTANTIATION
static void __instantiateMissingTemplates()
{
  RIllegalAccessException ex1;
  ex1 = new IllegalAccessException();
  RInterruptedException ex2;
  ex2 = new InterruptedException();
  RClassCastException bce;
  bce = new ClassCastException();
}
#endif //defined(__BORLANDC__)

} // lang
} // acdk
