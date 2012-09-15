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

#ifndef acdk_make_AcdkTestExeTask_h
#define acdk_make_AcdkTestExeTask_h

#include "AcdkExeTask.h"

namespace acdk {
namespace make {

ACDK_DECL_CLASS(AcdkTestExeTask);

/**
  Create a the given Library.
  execute fails if directory cannot be created.
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC AcdkTestExeTask
: extends AcdkExeTask
{
  ACDK_WITH_METAINFO(AcdkTestExeTask)
public:
 
  AcdkTestExeTask(IN(RString) name);
  AcdkTestExeTask(IN(RString) name, IN(RString) desc);
  
private:
  foreign void _init();
};



} // namespace make
} // namespace acdk


#endif //acdk_make_AcdkTestExeTask_h
