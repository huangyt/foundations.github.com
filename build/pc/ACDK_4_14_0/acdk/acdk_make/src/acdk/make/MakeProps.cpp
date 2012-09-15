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


#include "MakeProps.h"
#include <acdk/io/File.h>


namespace acdk {
namespace make {

using namespace acdk::cfgscript;

void 
MakeProps::addExtInclude(IN(RString) file_or_path)
{
  RString dirname = file_or_path;
  ::acdk::io::File f(file_or_path);
  if (f.isDirectory() == false)
  {
    dirname = f.getParent();
  }
  setStringVal(".IncludeFile", dirname, PropsNoFlags);
  appendStringVal("CCC_EXT_INCLUDES", eval("${CCC_IncludeFileMask}", PropsEvalRecursive | PropsParentRead), " ", PropsParentRead);
}

void 
MakeProps::addExtLibPath(IN(RString) file_or_path)
{
  RString dirname = file_or_path;
  acdk::io::File f(file_or_path);
  if (f.isDirectory() == false)
  {
    dirname = f.getParent();
  }
  setStringVal(".LinkPath", dirname, PropsNoFlags);
  appendStringVal("CCC_EXT_LDFLAGS", eval("${CCC_LinkPathMask}", PropsEvalRecursive | PropsParentRead), " ", PropsParentRead);
}

void 
MakeProps::addDefine(IN(RString) define, IN(RString) value)
{
  RString definetoken = define;
  if (value != Nil && value->length() > 0)
    definetoken = definetoken + "=" + value;
  setStringVal(".DefineToken", definetoken, PropsNoFlags);
  appendStringVal("CCC_EXT_DEFINES", eval("${CCC_DefineMask}", PropsEvalRecursive | PropsParentRead), " ", PropsParentRead);
  
}

} // namespace make
} // namespace acdk



