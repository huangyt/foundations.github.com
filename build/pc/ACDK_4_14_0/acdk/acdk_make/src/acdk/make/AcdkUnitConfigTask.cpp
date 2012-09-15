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


#include "AcdkUnitConfigTask.h"

namespace acdk {
namespace make {

using namespace acdk::cfgscript;

void 
AcdkUnitConfigTask::addDefine(IN(RString) str)
{
  _props->appendStringArrayVal("CCC_EXT_DEFINES_LIST", str, PropsParentRead | PropsNoParentWrite | PropsNoWarnRead);
}

void 
AcdkUnitConfigTask::addIncludePath(IN(RString) str)
{
  _props->appendStringArrayVal("CCC_INCLUDE_LIST", str, PropsParentRead | PropsNoParentWrite | PropsNoWarnRead);
}

void 
AcdkUnitConfigTask::addLibraryPath(IN(RString) str)
{
  _props->appendStringArrayVal("CCC_EXT_LIBPATH_LIST", str, PropsParentRead | PropsNoParentWrite | PropsNoWarnRead);
}

void 
AcdkUnitConfigTask::addExecPath(IN(RString) str)
{
  _props->appendStringArrayVal("SHELL_EXT_PATH_LIST", str, PropsParentRead | PropsNoParentWrite | PropsNoWarnRead);
  
}

//virtual 
bool 
AcdkUnitConfigTask::execute(IN(RString) exec, IN(RProps) props)
{
  {
    RStringArray sa = _props->getStringArrayVal("CCC_EXT_DEFINES_LIST", PropsParentRead | PropsNoParentWrite | PropsNoWarnRead);
    for (int i = 0; i < sa->length(); ++i)
    {
      props->appendStringArrayVal("CCC_EXT_DEFINES_LIST", sa[i], PropsParentRead | PropsNoParentWrite | PropsNoWarnRead);
    }
  }
  {
    RStringArray sa = _props->getStringArrayVal("CCC_INCLUDE_LIST", PropsParentRead | PropsNoParentWrite | PropsNoWarnRead);
    for (int i = 0; i < sa->length(); ++i)
    {
      props->appendStringArrayVal("CCC_INCLUDE_LIST", sa[i], PropsParentRead | PropsNoParentWrite | PropsNoWarnRead);
    }
  }
  return true;
}



} // namespace make
} // namespace acdk



