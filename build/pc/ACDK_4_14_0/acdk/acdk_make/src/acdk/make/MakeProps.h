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

#ifndef acdk_make_MakeProps_h
#define acdk_make_MakeProps_h

#include "Config.h"
#include <acdk.h>
#include <acdk/cfgscript/Props.h>



namespace acdk {
namespace make {

USING_CLASS(acdk::cfgscript::, Props);


ACDK_DECL_CLASS(MakeProps);

/**

*/
class ACDK_ACDK_MAKE_PUBLIC MakeProps
: extends Props
{
  ACDK_WITH_METAINFO(MakeProps)
public:
    
  MakeProps(IN(RString) name, IN(RProps) parent = Nil, bool private_props = true)
    : Props(name, acdk::cfgscript::PropsParentRead, parent, private_props)
  {
  }
  /**
    Add external include directory or include file
  */
  void addExtInclude(IN(RString) file_or_path);
  void addExtLibPath(IN(RString) file_or_path);
  void addDefine(IN(RString) define, IN(RString) value = Nil);
};




} // namespace make
} // namespace acdk


#endif //acdk_make_MakeProps_h
