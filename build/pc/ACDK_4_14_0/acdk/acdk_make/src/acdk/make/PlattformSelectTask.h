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

#ifndef acdk_make_PlattformSelectTask_h
#define acdk_make_PlattformSelectTask_h

#include "AbstractTask.h"



namespace acdk {
namespace make {

ACDK_DECL_CLASS(PlattformSelectTask);

/**
  Select and set current platform.
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC PlattformSelectTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(PlattformSelectTask)
protected:
  RProps _targetProps;
public:
  
  PlattformSelectTask()
  : AbstractTask("configure", "", "Check and configure Platform")
  , _targetProps(Nil)
  {
  }
  
  /**
    if successfull sets also env "_PlattformSelected" to "TRUE"
    */
  virtual bool execute(IN(RString) exec, IN(RProps) props);
  RStringArray getAvailableTargets(IN(RProps) globals);
  RProps getTargetSpecs(IN(RProps) globals, IN(RString) targetname);
private:
  void initTargetProps(IN(RProps) props);
};



} // namespace make
} // namespace acdk


#endif //acdk_make_PlattformSelectTask_h
