
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


#ifndef acdk_tools_mc_ModuleInfo_h
#define acdk_tools_mc_ModuleInfo_h

#include "mc.h"
#include "CodeInfo.h"
#include "EnumInfo.h"
#include <acdk/util/ArrayList.h>
#include <acdk/io/StreamTokenizer.h>

namespace acdk {
namespace tools {
namespace mc {

USING_CLASS(acdk::util::, ArrayList);
USING_CLASS(acdk::util::, Iterator);
USING_CLASS(acdk::io::, StreamTokenizer);


ACDK_DECL_CLASS(ModuleInfo);

class ACDK_TOOLS_MC_PUBLIC ModuleInfo 
: extends CodeInfo
{
protected:
  
  /// list of Strings
  RArrayList _curNameSpace;
  
  bool _hasMetaInfo;
  RStringArray _knownExceptions;
public:
  RString _fname;
  /// contains RClassInfo
  RArrayList _classes;
  RArrayList _usings;
  REnumInfoArray _enums;
  ModuleInfo(IN(RString) fname)
  : CodeInfo(0, "")
  , _curNameSpace(new ArrayList())  
  , _hasMetaInfo(true)
  , _knownExceptions(new StringArray(0))
  , _fname(fname)
  , _classes(new ArrayList())
  , _usings(new ArrayList())
  , _enums(new EnumInfoArray(0))
  {
  }
  virtual RString getMetaInfoCIdentifier();
  bool hasMetaInfo();
  
  void dump(IN(::acdk::io::RPrintWriter) out, IN(RString) ind);
  void analyseFile(RString file);
  RString fname() { return _fname; }
  RString baseFilename();
  RString getParent();
  bool checkContext();
  bool invokeCodeAttributes();


  bool parse();
  bool parse(IN(RStreamTokenizer) in);
  
  void writeCodes(IN(::acdk::io::RPrintWriter) out, CodeWhere where);

  // not used: void writeMIH();
  // not used: void writeMIH(IN(::acdk::io::RPrintWriter) out, bool inheader);
  void writeClsInfoCPP(IN(::acdk::io::RPrintWriter) out, IN(::acdk::io::RPrintWriter) stubout, bool with_field_info);
  void writeModuleHeaderInclude(IN(::acdk::io::RPrintWriter) out, IN(::acdk::io::RPrintWriter) stubout);
  /// ACDK2IDL
  void generateORBIdl(IN(::acdk::io::RPrintWriter) out);
  static RString getNameSpace(IN(RArrayList) ns, IN(RString) separator = "/");
  friend class ModuleInfoCompiler;
  bool isKnownThrowable(IN(RString) name);
  static void writeOpenNamespace(IN(RPrintWriter) out, IN(RArrayList) nslist, IN(RArrayList) usings);
  static void writeCloseNamespace(IN(RPrintWriter) out, IN(RArrayList) nslist);
  REnumInfo getEnumInfo(IN(RString) name);
};




} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_ModuleInfo_h
