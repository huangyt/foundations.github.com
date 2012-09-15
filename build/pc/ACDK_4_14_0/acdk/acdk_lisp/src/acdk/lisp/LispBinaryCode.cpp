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



#include "LispBinaryCode.h"
#include <acdk/io/FileWriter.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/BinaryObjectWriter.h>
#include <acdk/io/BinaryObjectReader.h>

#include <acdk/lang/System.h>

namespace acdk {
namespace lisp {


void 
LispEnvironment::storeCompiled(IN(RString) file)
{
  ::acdk::io::FileWriter fout(file);
  storeCompiled(&fout);
}

int LispSerializeFlags = 
                     SerializeNamed 
                   | SerializeReduced
                   | SerializeJoinedStrings
                   | SerializeOnlySerializable //SerializeAll //SerializeOnlySerializable
                   | SerializeIgnoreNotSerializable
                   ;

void 
LispEnvironment::storeCompiled(IN(::acdk::io::RWriter) out)
{
  ::acdk::io::BinaryObjectWriter bout(&out, LispSerializeFlags);
  LispBinaryCode lbc;
  lbc.globals = _globals;
  lbc.macros = _macros;
  lbc.defuns = _defuns;
  bout.writeObject(&lbc);
}

void 
LispEnvironment::loadCompiled(IN(RString) file, bool replace)
{
  ::acdk::io::FileReader fin(file);
  loadCompiled(&fin, replace);
}

void 
LispEnvironment::loadCompiled(IN(::acdk::io::RReader) in, bool replace)
{
 
  
  ::acdk::io::BinaryObjectReader bin(&in, LispSerializeFlags);
  RLispBinaryCode lbc = (RLispBinaryCode)bin.readObject();
  if (lbc->globals == Nil)
    lbc->globals = new acdk::util::HashMap();
  if (lbc->macros == Nil)
    lbc->macros = new acdk::util::HashMap();
  if (lbc->defuns == Nil)
    lbc->defuns = new acdk::util::HashMap();

  if (replace == true)
  {
    _globals = lbc->globals;
    _macros = lbc->macros;
    _defuns = lbc->defuns;
    
  } else {
    ::acdk::util::RIterator it = lbc->globals->iterator();
    while (it->hasNext() == true)
    {
      ::acdk::util::RMapEntry entry = (::acdk::util::RMapEntry)it->next();
      _globals->put(entry->getKey(), entry->getValue());
    }
    it = lbc->macros->iterator();
    while (it->hasNext() == true)
    {
      ::acdk::util::RMapEntry entry = (::acdk::util::RMapEntry)it->next();
      _macros->put(entry->getKey(), entry->getValue());
    }
    it = lbc->defuns->iterator();
    while (it->hasNext() == true)
    {
      ::acdk::util::RMapEntry entry = (::acdk::util::RMapEntry)it->next();
      _defuns->put(entry->getKey(), entry->getValue());
    }
  }
}


} // namespace lisp
} // namespace acdk



