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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/DmiDelegate.cpp,v 1.9 2005/04/13 13:23:09 kommer Exp $

#include "DmiDelegate.h"
#include "AcdkStdWeakTypeDmiClient.h"

namespace acdk {
namespace lang {
namespace dmi {

ScriptVar 
DmiDelegate::call(INOUT(ScriptVarArray) args)
{
  RDmiObjectArray dargs = new DmiObjectArray(args.size());
  for (int i = 0; i < args.size(); ++i)
    dargs[i] = new DmiObject(args[i]);
  return *call(dargs);
}

RDmiObject 
StdDmiDelegate::call(IN(RDmiObjectArray) args)
{
  ScriptVarArray sargs(args->length());
  for (int i = 0; i < sargs.size(); ++i)
  {
    sargs[i] = *(args[i]);
  }
  return new DmiObject(StdDmiDelegate::call(sargs));
}

RDmiObject 
StdDmiDelegate::call(IN(RDmiNamedArgArray) namedArgs)
{
  ScriptVarArray sargs(namedArgs->length());
  RStringArray names = new StringArray(namedArgs->length());

  for (int i = 0; i < sargs.size(); ++i)
  {
    sargs[i] = *(namedArgs[i]->value);
    names[i] = namedArgs[i]->name;
  }
  DmiClient& dmiClient = AcdkStdWeakTypeDmiClient::getDmiClient();
  if (_object != Nil)
    return new DmiObject(_object->invokeMethod(_methodName, sargs, dmiClient, names));
  return new DmiObject(invokeStaticMethod(_class->getName(), _methodName, sargs, dmiClient, names));
}

//foreign 
ScriptVar 
StdDmiDelegate::call(INOUT(ScriptVarArray) args)
{
  if (_object != Nil)
    return _object->invokeMethod(_methodName, args);
  return invokeStaticMethod(_class->getName(), _methodName, args);
}


} // namespace dmi
} // namespace lang
} // namespace acdk


