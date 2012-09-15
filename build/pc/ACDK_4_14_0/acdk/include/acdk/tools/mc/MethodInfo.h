
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


#ifndef acdk_tools_mc_MethodInfo_h
#define acdk_tools_mc_MethodInfo_h

#include "mc.h"
#include "ClassInfo.h"
#include "CodeInfo.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(MethodInfo);

class ACDK_TOOLS_MC_PUBLIC MethodInfo
: extends CodeInfo
, implements acdk::util::Comparable
{
  DECL_ACDK_DEFAULT_METACLASS(TypeScope)
public:
  RString returnType;
  RClassInfo _classInfo;
  RArrayList args; //RArgumentInfo
  /**
    Fixed argument count (ignoreing default value arguments
  */
  int argcount;
  RString _javaSignature;
  RArrayList _throws; // RString
  RString _altName;
  RString _operatorName;
  MethodInfo(IN(RTypeScope) parent, IN(RClassInfo) clsInfo, int accessflags);
  /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc);
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  virtual RString getMetaInfoCIdentifier();

  bool isPublic() { return acdk::lang::dmi::MetaInfo::isPublic(flags); }
  bool isPrivate() { return acdk::lang::dmi::MetaInfo::isPrivate(flags); }
  bool isDestructor() { return flags & acdk::lang::dmi::MiMiDestructor; }
  bool isVirtual() { return flags & acdk::lang::dmi::MiMiVirtual; }
  bool isStatic() { return (flags & acdk::lang::dmi::MiMiConstructor) || (flags & acdk::lang::dmi::MiStatic); }
  bool isConstructor() { return (flags & acdk::lang::dmi::MiMiConstructor); }
  bool isAbstract() { return flags & acdk::lang::dmi::MiMiAbstract; }
  bool parse(IN(RStreamTokenizer) in);
  bool isCreateInstance();
  // from Comparable
  virtual int compareTo(IN(RObject) other)
  {
    return name->compareTo(RMethodInfo(other)->name);
  }
  bool invokeCodeAttributes(IN(RModuleInfo) cm, IN(RClassInfo) ci);
  void writeInfo(IN(RPrintWriter) out, IN(RClassInfo) clsinfo);
  /** 
    if arguments as default initializer argcount != args->length();
    */
  void writeInfo(IN(RPrintWriter) out, IN(RClassInfo) clsinfo, int argcount);
  RString getJavaSignature(bool cidentifier, int argcount = -1);
  bool  detectPureVirtualMethod(IN(RStreamTokenizer) in);
  void writeMethodList(IN(RPrintWriter) out, IN(RClassInfo) clsinfo);
  void writeDispatchBody(IN(RPrintWriter) out, IN(RClassInfo) clsinfo);
  void writeDispatchBodyEx(IN(RPrintWriter) out, IN(RClassInfo) clsinfo);
  void writeDispatchBody(IN(RPrintWriter) out, IN(RClassInfo) clsinfo, int argcount);
  void writeDispatchBodyEx(IN(RPrintWriter) out, IN(RClassInfo) clsinfo, int argcount);
  void writeProxyConstructor(IN(RPrintWriter) out, IN(RClassInfo) clsinfo);
  void writeProxyMethod(IN(RPrintWriter) out, IN(RClassInfo) clsinfo);

  //deleted: void writeOrbDispatchBody(IN(RPrintWriter) out, IN(RClassInfo) clsinfo);
  void writeThrowDispatch(IN(RPrintWriter) out, IN(RClassInfo) clsinfo, int argcount);
  void writeDispatchBody2(IN(RPrintWriter) out, IN(RClassInfo) clsinfo, int argcount, bool novirtual);
  /**
    return all arguments, including default values.
    use ai->argcount for current real arg count
  */
  int getArgCount() { return args->size(); }
  int getNoDefaultArgCount();
  void writeOrbProxy(IN(RPrintWriter) out, IN(RClassInfo) clsinfo);
  virtual RString toString();
  RString orgReturnType() { return returnType; }
  RString mappedReturnType()
  {
     if (hasType(returnType) == TsEnum)
      return "int";
    return returnType;
  }
  /**
    If the method has default parameter, multiple 
    Methods with corresponding argument count will 
    be returned
  */
  RMethodInfoArray getFixedParametersMethods();

  /// ACDK2IDL
  void generateIdlIfInterface(IN(RPrintWriter) out);
  
  bool checkModifier(IN(RStreamTokenizer) in);
  void writeCodes(IN(RPrintWriter) out, CodeWhere where);
  bool needMethodInfo();
  virtual bool generateMetaInfo(bool defaultValue)
  {
    if (CodeInfo::generateMetaInfo(true) == false)
      return false;
    if (_classInfo->getMcConfigAttribute()->genMethods(defaultValue) == false)
      return false;
    return true;
  }
};



} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_MethodInfo_h


