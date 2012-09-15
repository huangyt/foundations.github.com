
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


#ifndef acdk_tools_mc_CodeInfo_h
#define acdk_tools_mc_CodeInfo_h

#include "mc.h"
#include <acdk/cfgscript/Props.h>
#include "CodeAttribute.h"
#include "TypeScope.h"
#include "McConfigAttribute.h"
#include <acdk/lang/dmi/MetaAttribute.h>

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(CodeAttribute);

enum CodeWhere
{
  /**
    insert include statements at the beginning of the file
  */
  ModuleInclude,
  /** 
    free standing Module unit
  */
  ModuleBeforeDispatch,

  ModuleAfterDispatch,
  /** 
    before ModuleInit, but behind type lib
  */
  ModuleBeforeInit,
  /**
    will be executed while shared library will be 
    loaded
  */
  

  ModuleInit,
  /**
    Will be executed while Class of Object will be requested
    the first time
  */
  ClassInit
  /*
  ModuleAfterDispatch,
  ClassAttributeInitBefore,
  ClassAttributeInitAfter,
  ClassStaticMethodBeforeFindMethods,
  ClassStaticMethodBeforeAfterFindMethods,
  ClassStaticMethodBeforeAfterInvokeMethods
  */
};
ACDK_DEF_LIB_ENUM(ACDK_TOOLS_MC_PUBLIC, CodeWhere);

ACDK_DECL_CLASS(CodeInsertion);

class ACDK_TOOLS_MC_PUBLIC CodeInsertion
: extends acdk::lang::Object
{
public:
  CodeWhere insertPoint;
  RString code;
  CodeInsertion(IN(RString) c, CodeWhere w)
  : insertPoint(w)
  , code(c)
  {
  }
};


ACDK_DECL_CLASS(CodeInfo);

class ACDK_TOOLS_MC_PUBLIC CodeInfo
: extends TypeScope
{
public:
  
  /** @see acdk::lang::dmi::MetaInfoFlags */
  int flags;
  /** must be legal identifier */
  RString name;
  /** 
    name of Metainfo used to registed 
    if Nil name will be used
  */
  RString dmiName;
  acdk::cfgscript::RProps _props;
  /** 
    should code be generated
    0 -> unspec
    -1 -> don't generate
    1 -> generate
  */
  char _genCode;
  RMcConfigAttribute _mcconfig;
  int _localMcFlags;
  CodeInfo(int fl, IN(RString) nam, IN(RTypeScope) parent = Nil)
  : TypeScope(parent)
  , flags(fl)
  , name(nam)
  , _props(new acdk::cfgscript::Props())
  , _genCode(0)
  , _localMcFlags(0)
  {
  }
  /// will be evaluated by generation time
  void addCodeAttribute(IN(RCodeAttribute) ca);
  RCodeAttributeArray getCodeAttributes()
  {
    RCodeAttributeArray caa = (RCodeAttributeArray)(RObjectArray)_props->getObjectVal("_ca", acdk::cfgscript::PropsNoWarnRead);
    if (caa == Nil)
    {
      caa = new CodeAttributeArray(0);
      _props->setObjectVal("_ca", &caa);
    }
    return caa;
  }
  RMcConfigAttribute getMcConfigAttribute()
  {
    if (_mcconfig != Nil)
      return _mcconfig;
    _mcconfig = (RMcConfigAttribute)getCodeAttribute("acdk/tools/mc/McConfigAttribute");
    if (_mcconfig == Nil)
      _mcconfig = new McConfigAttribute(0);
    return _mcconfig;
  }
  virtual bool generateMetaInfo(bool defaultValue)
  {
    if (_localMcFlags & McConfNoMetaInfo)
      return false;
    return getMcConfigAttribute()->genMetaInfo(defaultValue);
  }
  void addCodeAttributes(IN(RCodeAttributeArray) ca)
  {
    for (int i = 0; i < ca->length(); ++i)
      addCodeAttribute(ca[i]);
  }
  RCodeAttribute getCodeAttribute(IN(RString) clsname);

  /*
  // will be added to code/metainfo
  void addMetaAttribute(IN(acdk::lang::dmi::RMetaAttribute) ma)
  {
    _props->appendObjectList("_ma", &ma);
  }
  void addStringAttribute(IN(RString) key, IN(RString) val, bool persist);
  */
  void addCode(IN(RString) code, CodeWhere where);
  RCodeInsertionArray getCode()
  {
    return (RCodeInsertionArray)(RObjectArray)_props->getObjectVal("_code", acdk::cfgscript::PropsNoWarnRead);
  }
  void writeCode(IN(::acdk::io::RPrintWriter) out, CodeWhere where);
  //void addCodeAttribute(String key, String initcode);
  /**
    Returns the identifier to the dmi::ClazzInfo, dmi::MethodInfo/etc.
  */
  virtual RString getMetaInfoCIdentifier() = 0;

  /**
    returns the namespace part of name
  */
  RString getNamespace();
  /**
    returns the basse name (without namespace) of name
  */
  RString getBaseName();
  /**
    returns the super qualified name using ACDK_FQ_SUPER_QUALIFIER macro
  */
  RString getSuperName();

  bool invokeCodeAttributes();
  RString getDmiName() { return dmiName == Nil ? name : dmiName; }
  /**
    return the string of signature of this method/member/class
  */
  RString getDispatchSignature(bool isStatic);
};

#define STDDSP_STRING_TYPE "IN(::acdk::lang::RString)"
#define STDDSP_STRING_CASTPLS(stringval)  stringval
#define STDDSP_STRING_CAST(stringval) stringval 

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_CodeInfo_h
