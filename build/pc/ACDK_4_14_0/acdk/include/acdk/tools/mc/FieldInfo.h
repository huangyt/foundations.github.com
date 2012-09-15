
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


#ifndef acdk_tools_mc_FieldInfo_h
#define acdk_tools_mc_FieldInfo_h

#include "mc.h"
#include "CodeAttribute.h"
#include "CodeInfo.h"
#include <acdk/io/StreamTokenizer.h>

namespace acdk {
namespace tools {
namespace mc {

USING_CLASS(acdk::io::, StreamTokenizer);
USING_CLASS(::acdk::io::, PrintWriter);

ACDK_DECL_CLASS(ModuleInfo);
ACDK_DECL_CLASS(ClassInfo);

ACDK_DECL_CLASS(FieldInfo);

class ACDK_TOOLS_MC_PUBLIC FieldInfo
: extends CodeInfo
, implements acdk::lang::Comparable
{
  DECL_ACDK_DEFAULT_METACLASS(Object)
public:
  bool serializable;
  bool cloneable;
  bool isstatic;
  
private:
  RString type;
public:
  FieldInfo(IN(RTypeScope) parent, int tflags)
  : CodeInfo(tflags, "", parent)
  , serializable(true)
  , cloneable(true)
  , isstatic(false)
  {
    
  }
  // from Comparable
  virtual int compareTo(IN(RObject) other)
  {
    return name->compareTo(RFieldInfo(other)->name);
  }
  virtual RString getMetaInfoCIdentifier();
  bool parse(IN(RStreamTokenizer) in);
  void dump(IN(RPrintWriter) out, IN(RString) ind);
  bool isBasicType()
  {
    return TypeScope::isBasicType(type) || isEnum();
  }
  bool isKnownType()
  {
   return isBasicType() || isObjectType();
  }
  bool isObjectType()
  {
    return TypeScope::isObjectType(type);
  }
  bool isObject()
  {
    return isObjectType();
  }
  bool isEnum()
  {
    return hasType(type) == TsEnum;
  }
  bool isStatic() { return flags & acdk::lang::dmi::MiStatic; }
  RString fieldType()
  {
    return ::acdk::lang::dmi::MetaInfo::flagsToString((flags | ::acdk::lang::dmi::MiFieldInfo) & ~::acdk::lang::dmi::MiMcKnownType, 
                                                      acdk::lang::dmi::FieldInfoExtFlags(0), 
                                                      acdk::lang::dmi::TpFtFqName | acdk::lang::dmi::TpFtAcdkType);
  
  }
  
  char getTypeChar()
  {
    RString mt = getMappedType();
    if (mt->equals("char") == true)
      return 'C';
    if (mt->equals("ucchar") == true || mt->equals("uc2char") == true)
      return 'U';
    if (mt->equals("bool") == true)
      return 'Z';
    if (mt->equals("short") == true)
      return 'S';
    if (mt->equals("int") == true)
      return 'I';
    /*if (mt->equals("long") == true)
      return 'I';*/
    if (mt->equals("jlong") == true)
      return 'J';
    if (mt->equals("float") == true)
      return 'S';
    if (mt->equals("double") == true)
      return 'D';
    if (mt->length() > 3 &&
        mt->charAt(0) == 'R' && Character::isUpperCase(mt->charAt(1)) == true)
      return 'L';
    return 'X';
  }
  RString getJTypeName()
  {
    return parseJTypeName(getMappedType()->trim());
  }
  RString getOrgType()
  {
    return type;
  }
  RString getMappedType()
  {
    if (hasType(type) == TsEnum)
      return "int";
    return type;
  }
  static RString parseJTypeName(IN(RString) tpname);
  
  bool needFieldInfo() { return true; }
  bool invokeCodeAttributes(IN(RModuleInfo) cm, IN(RClassInfo) ci);
  virtual bool generateMetaInfo(bool defaultValue)
  {
    if (CodeInfo::generateMetaInfo(true) == false)
      return false;
    return RCodeInfo(_parent)->getMcConfigAttribute()->genFields(defaultValue);
  }
};


} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_FieldInfo_h
