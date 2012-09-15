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


#include "AcdkLibTask.h"
#include <acdk/util/Arrays.h>
#include <acdk/io/GlobFilenameFilter.h>

namespace acdk {
namespace make {

using namespace acdk::cfgscript;

void
AcdkLibTask::setupAcdkDeps(IN(RProps) props)
{
  props->appendStringArrayVal("COMPILE_TYPE", "LIB", PropsNoStringDups);
  AcdkBinaryTask::setupAcdkDeps(props);
}

//virtual 
bool 
AcdkLibTask::execute(IN(RString) exec, IN(RProps) props)
{
  props->appendStringArrayVal("COMPILE_TYPE", "LIB", PropsNoParentWrite | PropsNoStringDups);
  return AcdkBinaryTask::execute(exec, props);

}

} // namespace make
} // namespace acdk


