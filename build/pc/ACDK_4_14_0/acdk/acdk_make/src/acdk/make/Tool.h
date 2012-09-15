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

#ifndef acdk_make_Tool_h
#define acdk_make_Tool_h

#include "AbstractTask.h"
#include <acdk/util/Map.h>

namespace acdk {
namespace make {

USING_CLASS(::acdk::cfgscript::, Props);

ACDK_DECL_INTERFACE(Tool);

class ACDK_ACDK_MAKE_PUBLIC Tool
: implements Task
{
  ACDK_WITH_METAINFO(Tool)
public:
  virtual RString getToolClass() = 0;
  virtual RString getToolTribe() = 0;
  virtual bool configure(IN(RProps) env) = 0;
  /**
    returns an array of 
      TOOLKEY -> description
  */
  virtual RStringArrayArray getToolDescription() = 0;
  static void registerTool(IN(RTool) tool);
  static RTool getTool(IN(RProps) env, IN(RString) toolClass, IN(RString) toolTribe = Nil);
  /**
    ToolClass(String) -> ( HashMap: ToolTribe(String) -> Tool )
  */
  static RToolArray getTools(IN(RProps) env, IN(RString) toolClass);
  
  static acdk::util::RMap getToolsMap();

};


ACDK_DECL_CLASS(AbstractTool);

class ACDK_ACDK_MAKE_PUBLIC AbstractTool
: extends AbstractTask
, implements Tool
{
 ACDK_WITH_METAINFO(AbstractTool)
private:
  RString _toolClass;
  RString _toolTribe;
  RStringArrayArray _toolDescription;
public:
  AbstractTool(IN(RString) toolClass, IN(RString) toolTribe)
  : _toolClass(toolClass)
  , _toolTribe(toolTribe)
  , _toolDescription(new StringArrayArray(0))
  {
  }
  virtual RString getToolClass() { return _toolClass; }
  virtual RString getToolTribe() { return _toolTribe; }
  virtual bool configure(IN(RProps) env) = 0;
  RStringArrayArray getToolDescription()
  {
    return _toolDescription;
  }
  void setToolDescription(IN(RStringArrayArray) desc)
  {
    _toolDescription = desc;
  }

};

} // namespace make
} // namespace acdk 
  
#endif //acdk_make_Tool_h
