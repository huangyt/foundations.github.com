
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


#include "StringTagAttribute.h"
#include "MetaCompiler.h"
#include "FieldInfo.h"
#include "ArgumentInfo.h"
#include <acdk/locale/Encoding.h>

namespace acdk {
namespace tools {
namespace mc {


//static 
void 
CodeAttribute::initAttribute(IN(RMetaCompiler) mc)
{
  mc->registerAttribute("CodeAttribute", "acdk/tools/mc/CodeAttribute");
}
//static 
int 
CodeAttribute::getCounter()
{
  static int counter = 0;
  return ++counter;
}


//virtual 
bool 
CodeAttribute::apply(IN(RCodeInfo) cm)
{
  return attachAttribute(cm);
}


bool 
CodeAttribute::attachAttribute(IN(RCodeInfo) ci)
{
  /*
  if (instanceof(RObject(this), ::acdk::io::Serializable) == false)
    return false;
  acdk::io::MemWriter mout;
  acdk::io::BinaryObjectWriter bout(&mout, acdk::io::SerializeNamed | 
                                           acdk::io::SerializeReduced | 
                                           acdk::io::SerializeOnlySerializeable);

  bout.writeObject(this);
  */
  return false;
}

} // namespace mc
} // namespace tools
} // namespace acdk


