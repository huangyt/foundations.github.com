
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


#ifndef acdk_tools_mc_EnumInfo_h
#define acdk_tools_mc_EnumInfo_h


#include "CodeInfo.h"
#include <acdk/util/ArrayList.h>
#include <acdk/io/StreamTokenizer.h>

namespace acdk {
namespace tools {
namespace mc {

USING_CLASS(::acdk::io::, PrintWriter);
USING_CLASS(::acdk::io::, StreamTokenizer);
USING_CLASS(::acdk::util::, ArrayList);

ACDK_DECL_CLASS(EnumInfo);
ACDK_DECL_CLASS(ClassInfo);
ACDK_DECL_CLASS(EnumInfoValue);

class ACDK_TOOLS_MC_PUBLIC EnumInfoValue
: extends Object
{
public:
  RString name;
  bool valueDefined;
  int value;
  RString stringValue;
  
  EnumInfoValue(IN(RString) name_, int value_ = 0, bool valDefined = false)
    : name(name_)
    , valueDefined(valDefined)
    , value(value_)
  {
  }
  void setIntValue(int val)
  {
    value = val;
    valueDefined = true;
  }
};

class ACDK_TOOLS_MC_PUBLIC EnumInfo
: extends CodeInfo
{
public:
  RModuleInfo _module;
  RArrayList _namespace;
  RArrayList _usings;
  /**
    if inside a class, this is the ClassInfo which owns
    the enum
  */
  RClassInfo _class;
  REnumInfoValueArray _values;
  /// wether enum is only declared (no values) or defined (with values)
  bool _isDefined;
  bool hasMetinfDef;
  EnumInfo(IN(RModuleInfo) module, IN(RArrayList) thenamespace, IN(RArrayList) usings, IN(RClassInfo) cls = Nil);
  RString getMetaInfoCIdentifier();
  bool parse(IN(RStreamTokenizer) in);
  bool detectEnd(int tk, IN(RStreamTokenizer) in);
  REnumInfoValue getValue(IN(RString) str);
  /// called internally after parse to resolve string values
  bool resolveStringValues();
  void writeEnumInfo(IN(RPrintWriter) out);
};



} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_EnumInfo_h
