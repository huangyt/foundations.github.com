
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


#include "ClassInitAttribute.h"
#include "StringTagAttribute.h"
#include "MetaCompiler.h"
#include <acdk/locale/Encoding.h>
#include "FieldInfo.h"

namespace acdk {
namespace tools {
namespace mc {


ClassInitAttribute::ClassInitAttribute(IN(RString) initFunction, IN(RString) deinitFunction)
: _initFunction(initFunction)
, _deinitFunction(deinitFunction)
{
}



//static 
void 
ClassInitAttribute::initAttribute(IN(RMetaCompiler) mc)
{
  //mc->registerAttribute("StringTag", "acdk.tools.mc.ClassInitAttribute");
}

//foreign virtual 
bool 
ClassInitAttribute::apply(IN(RCodeInfo) cm)
{
  if (instanceof(cm, ClassInfo) == false)
    return false;
  RClassInfo ci(cm);
  if (_initFunction != Nil)
  {
    
    StringTagAttribute sta("__acdk_classinitfunction", _initFunction);
    sta.apply(cm);
  }
   RString identifier = ci->getMetaInfoCIdentifier();
  cm->addCode("    " + identifier + "->callClassInitializer();\n", ModuleInit);
  if (_deinitFunction != Nil)
  {
    StringTagAttribute sta("__acdk_classdeinitfunction", _deinitFunction);
    sta.apply(cm);
    // ### TODO: currently not suppoerted cm->addCode("::acdk::lang::dmi::ClazzInfo_callClassDeinitializer(" + identifier + ");", ModuleDeInit);
  }
  return true;
}

} // namespace mc
} // namespace tools
} // namespace acdk


