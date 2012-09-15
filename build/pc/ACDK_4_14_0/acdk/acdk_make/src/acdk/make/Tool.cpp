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


#include "Tool.h"
#include <acdk/util/HashMap.h>

namespace acdk {
namespace make {

USING_CLASS(acdk::util::, HashMap);

namespace {

OUT(RHashMap)
getToolClassMap()
{
  static RHashMap _hm = new HashMap();
  return _hm;
}
/*
OUT(RHashMap)
getToolTribeMap()
{
  static RHashMap _hm = new HashMap();
  return _hm;
}
*/
} // anon namespace

//static 
void 
Tool::registerTool(IN(RTool) tool)
{
  RHashMap trm = (RHashMap)getToolClassMap()->get(&tool->getToolClass());
  if (trm == Nil)
  {
    trm = new HashMap();
  }
  trm->put(&tool->getToolTribe(), (RObject)tool);
  getToolClassMap()->put(&tool->getToolClass(), &trm);
}

RTool 
Tool::getTool(IN(RProps) env, IN(RString) toolClass, IN(RString) toolTribe)
{
  RHashMap trm = (RHashMap)getToolClassMap()->get(&toolClass);
  
  if (trm == Nil)
    return Nil;
  if (toolTribe != Nil)
    return (RTool)trm->get(&toolTribe);
  acdk::util::RIterator it = trm->keySet()->iterator();
  while (it->hasNext() == true)
  {
    RTool t = (RTool)trm->get(it->next());
    if (t->configure(env) == true)
      return t;
  }
  return Nil;
}

RToolArray 
Tool::getTools(IN(RProps) env, IN(RString) toolClass)
{
  RHashMap trm = (RHashMap)getToolClassMap()->get(&toolClass);
  RToolArray ta = new ToolArray(0);
  if (trm == Nil)
    return Nil;
  acdk::util::RIterator it = trm->keySet()->iterator();
  while (it->hasNext() == true)
  {
    RTool t = (RTool)trm->get(it->next());
    if (t->configure(env) == true)
      ta->append(t);
  }
  return ta;
}

acdk::util::RMap 
Tool::getToolsMap()
{
  return (acdk::util::RMap)getToolClassMap();
}


} // namespace make
} // namespace acdk 
  
