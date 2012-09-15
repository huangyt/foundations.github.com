
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


#ifndef acdk_tools_mc_MetaCompiler_h
#define acdk_tools_mc_MetaCompiler_h

#include "mc.h"
#include "ArgumentInfo.h"
#include "EnumInfo.h"
#include "TokenStack.h"
#include "UnitInfo.h"
#include <acdk/util/TreeMap.h>

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(CodeAttribute);

ACDK_DECL_CLASS(MetaCompiler);
/**
  Compiles Meta info
  @todo externalMetaInfo is aways true. Cleanup code
*/
class ACDK_TOOLS_MC_PUBLIC MetaCompiler
: extends ::acdk::lang::Object
{
public:
  /**
    RString name -> RString classname
  */
  acdk::util::RTreeMap _registeredAttributes;
  /// contains RUnitInfo
  RArrayList _units;
  /// list of enum found 
  REnumInfoArray _enums;
  bool _unitDefsWritten;
  RString _baseOutname;
  /// only used to write stup code
  RString _baseStupOutname;
  /**
    generate meta info as external shared library
    always true
    
  */
  static bool externalMetaInfo; 
  static bool generateProxies;
  bool baseMetaInfoHeaderWritten;
  bool dummyExportForExternalMIWritten;
  MetaCompiler();
  static RMetaCompiler getMetaCompiler();
  /**
    Register a given CodeAttribute by name
  */
  void registerAttribute(IN(RString) name, IN(RString) clsname);
  RString getRegisteredAttribute(IN(RString) name);
  RCodeAttribute readParseCodeAttribute(IN(RString) code);
  /**
    adds a unit in the notation acdk_tools_aunit
  */
  RUnitInfo addUnit(IN(RString) unitdeclarator);
  void writeUnitDefinitions(IN(RPrintWriter) out);
  bool hasUnit(IN(RString) unit);
  RUnitInfo getUnit(IN(RString) unit);
  static bool skipUntilToken(IN(RStreamTokenizer) in, int tk);
  static void skipStatement(IN(RStreamTokenizer) in);
  static void skipTypeDeclarator(IN(RStreamTokenizer) in);
  static void skipWS(IN(RStreamTokenizer) in);
  static void skipPreprocessorStatement(IN(RStreamTokenizer) in);
  static RString readComponentIndentifier(IN(RStreamTokenizer) in, RTokenStack tkstack = Nil);
  static RString readCodeAttributeCode(IN(RStreamTokenizer) in);
  static bool isAccessToken(IN(RString) str);
  static bool checkCompatibleType(RString str);

};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_MetaCompiler_h
