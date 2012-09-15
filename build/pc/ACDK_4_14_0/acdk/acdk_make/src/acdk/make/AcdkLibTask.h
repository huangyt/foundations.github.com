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

#ifndef acdk_make_AcdkLibTask_h
#define acdk_make_AcdkLibTask_h

#include "AcdkBinaryTask.h"

namespace acdk {
namespace make {

ACDK_DECL_CLASS(AcdkLibTask);

/**
  Create a the given Library.
  execute fails if directory cannot be created.
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC AcdkLibTask
: extends AcdkBinaryTask
{
  ACDK_WITH_METAINFO(AcdkLibTask)
public:
 
  AcdkLibTask(IN(RString) name)
  : AcdkBinaryTask(name, "ACDK library")
  {
  }
  AcdkLibTask(IN(RString) name, IN(RString) desc)
  : AcdkBinaryTask(name, desc)
  {
  }
 
  virtual bool execute(IN(RString) exec, IN(RProps) props);
  void setupAcdkDeps(IN(RProps) props);
};



} // namespace make
} // namespace acdk


#endif //acdk_make_AcdkLibTask_h
