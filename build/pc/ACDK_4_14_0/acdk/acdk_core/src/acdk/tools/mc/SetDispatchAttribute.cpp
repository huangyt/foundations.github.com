
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


#include "SetDispatchAttribute.h"
#include "MetaCompiler.h"
#include <acdk/locale/Encoding.h>
#include "FieldInfo.h"

namespace acdk {
namespace tools {
namespace mc {


SetDispatchAttribute::SetDispatchAttribute(IN(RString) name, bool staticCall)
: _functionSignature(name)
, _staticCall(staticCall)
{
}

//static 
void 
SetDispatchAttribute::initAttribute(IN(RMetaCompiler) mc)
{
  mc->registerAttribute("DispatchAttribute", "acdk.tools.mc.SetDispatchAttribute");
}
//foreign virtual 
bool 
SetDispatchAttribute::apply(IN(RCodeInfo) cm)
{
  return attachAttribute(cm);
}

//virtual 
bool SetDispatchAttribute::apply(IN(RModuleInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool SetDispatchAttribute::apply(IN(RClassInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool SetDispatchAttribute::apply(IN(RMethodInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool SetDispatchAttribute::apply(IN(RArgumentInfo) cm) { return attachAttribute((RCodeInfo)cm); }
//virtual 
bool SetDispatchAttribute::apply(IN(RFieldInfo) cm) { return attachAttribute(&cm); }

bool 
SetDispatchAttribute::attachAttribute(IN(RCodeInfo) ci)
{
 
  return true;
}

} // namespace mc
} // namespace tools
} // namespace acdk


