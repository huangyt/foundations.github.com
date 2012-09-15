
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


#ifndef acdk_tools_mc_ClassInfo_h
#define acdk_tools_mc_ClassInfo_h

#include "mc.h"
#include "ModuleInfo.h"

namespace acdk {
namespace tools {
namespace mc {

USING_CLASS(::acdk::io::, PrintWriter);

ACDK_DECL_CLASS(EnumInfo);

ACDK_DECL_CLASS(ClassInfo);

class ACDK_TOOLS_MC_PUBLIC ClassInfo
: extends CodeInfo
{
public:
  RModuleInfo _module;
  RArrayList _namespace;
  RArrayList _usings;
  
  /** label to export from shared library (windows) */
  RString _publicDecl;
  //RString _super;
  RArrayList _derivides; // RSuperInfo, first is super
  RArrayList _fields; // RFieldInfo
  /** orginal method definition with possible default parameter initializer */
  RArrayList _orgMethods; //RMethodInfo
  /** methods no default parameter initializer */
  RArrayList _methods; //RMethodInfo
  
  //bool _isInterface;
  bool _hasCreator;
  //bool _isCloneable;
  //bool _isSerializable;
  bool _hasMetaInfo;  
  bool _hasDmiProxy;
  //bool _isCorbaInterface;
  //bool _isCorbaStruct;
  bool _hasGcInterface;
  //bool _isAbstract;
  
  /** generate standardDispatch() function */
  bool _hasScriptable;
  bool _hasScriptableEx;
  /** 
    the current access private/public/access 
    @see acdk::lang::dmi::MetaInfoFlags
  */
  int _currentAccess; 
  bool _detectedIncompatibleField;
  bool _isThrowable;
  bool _generateDmiProxy;
  ClassInfo(IN(RModuleInfo) module, IN(RArrayList) thenamespace, IN(RArrayList) usings, bool isclass, int flags);
  virtual RString getMetaInfoCIdentifier();
  /** 
    return false if class is only predeclaraed, but defined 
  */
  bool parse(IN(RStreamTokenizer) in);
  /** 
    return false if class is only predeclaraed, but defined 
  */
  bool parseHeader(IN(RStreamTokenizer) in);
  bool detectField(IN(RStreamTokenizer) in);
  void dump(IN(RPrintWriter) out, IN(RString) ind);
  bool invokeCodeAttributes(IN(RModuleInfo) cm);
  void writeMIH(RPrintWriter out, IN(::acdk::io::RPrintWriter) stubout, bool inheader);
  void writeFieldInfo(IN(RPrintWriter) out);
  void writeMethodInfo(IN(RPrintWriter) out);
  void writeClassInfo(IN(RPrintWriter) out, IN(::acdk::io::RPrintWriter) stubout, bool with_fieldInfo);
  void writeClazzInfo(IN(RPrintWriter) out, IN(::acdk::io::RPrintWriter) stubout, bool with_fieldInfo, bool stubOnly);
  void writeExternalMetaInfoInitializer(IN(RPrintWriter) out, bool with_fieldInfo);
  void writeInterfacesInfo(IN(RPrintWriter) out);
  void writeOpenNamespace(IN(RPrintWriter) out);
  void writeCloseNamespace(IN(RPrintWriter) out);
  void generateDispatch(IN(RPrintWriter) out);
  void generateDispatchMethods(IN(RPrintWriter) out);
  void generateFieldAccessor(IN(RPrintWriter) out);
  //void generateDispatchEx(IN(RPrintWriter) out);
  //void generateDmiProxy(IN(RPrintWriter) out);
  //void generateORB(IN(RPrintWriter) out);
  void generateDispatchBody(IN(RPrintWriter) out, bool statics);
  void generateDispatchBodyEx(IN(RPrintWriter) out, bool statics);
  void generateProxyMethods(IN(RPrintWriter) out);
  //void generateOrbProxy(IN(RPrintWriter) out);
  //void generateOrbDispatch(IN(RPrintWriter) out);

  RString getNamespaceAccessor();
  RString getJTypeName();
  RString getFlags() { return getFlags(flags | (_hasMetaInfo == true ? acdk::lang::dmi::MiClazzInfo : 0)); }
  RString getClazzFlags() { return getFlags(flags | (_hasMetaInfo == true ? acdk::lang::dmi::MiClazzInfo : 0)); }
  static RString getFlags(int flags);
  /** 
    @return last token 
    */
  int skipStatementOrFunction(IN(RStreamTokenizer) in);
  bool isCreateInstance(IN(RStreamTokenizer) in);
  bool isInterface();
  bool isAbstract() { return (flags & acdk::lang::dmi::MiCiAbstract) != 0; }
  bool checkContext(IN(RModuleInfo) module); // checks if Serializable, etc is valid
  /**
    set alternative names for polymorphic methods
  */
  void setMethodAltnames();
  void addStandardMethods();
  int getCollectableMemberCount();
  void sortMethodsAndFields();
  /** checks the current token if it is public/private/etc. */
  void detectAccessModifier(IN(RStreamTokenizer) in);

  /// ACDK2IDL group
  //void generateORBIdl(IN(RPrintWriter) out);
  void writeOpenModule(IN(RPrintWriter) out);
  void writeCloseModule(IN(RPrintWriter) out);
  void generateIdlIfInterface(IN(RPrintWriter) out);
  void generateIdlIfStruct(IN(RPrintWriter) out);

  bool needMethodInfo() { return _hasScriptable; }
  bool needFieldInfo() { return _hasScriptable || _hasGcInterface || _hasMetaInfo; }
  void writeCodes(IN(RPrintWriter) out, CodeWhere where);
  void writeClazzAndClassInitializer(IN(RPrintWriter) out);
  RString getSerialVersionUID();

  static inline bool isCompatibleType(int flags) { return flags & acdk::lang::dmi::MiMcKnownType; }
};




} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_ClassInfo_h

