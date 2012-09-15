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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/mc/OrbSetRepId.cpp,v 1.5 2005/02/05 10:45:40 kommer Exp $


#include "OrbSetRepId.h"

namespace acdkx {
namespace orb {
namespace mc {

//virtual 
bool 
OrbSetRepId::apply(IN(RCodeInfo) codeinfo) 
{ 
  if (instanceof(codeinfo, ClassInfo) == false)
    return false;
  RClassInfo ci(codeinfo);
  ci->addCode("#include <acdkx/orb/AORB.h>\n", acdk::tools::mc::ModuleInclude);
  StringBuffer sb;
  sb << "\n::acdkx::orb::RegisterRepId _register_" << ci->name  << "_repId(" << ci->name << "::clazzInfo(), \"" 
        << _repId << "\", " << _major << ", " << _minor << ");\n";
  ci->addCode(sb.toString(), acdk::tools::mc::ModuleBeforeDispatch);

  return true;
}


} // namespace mc
} // namespace orb
} // namespace acdkx

