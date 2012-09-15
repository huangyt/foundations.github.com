
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




#include "FieldInfo.h"
#include "ArgumentInfo.h"
#include "MetaCompiler.h"
#include "CMCException.h"

namespace acdk {
namespace tools {
namespace mc {

using namespace acdk::lang::dmi;

//virtual 
RString 
FieldInfo::getMetaInfoCIdentifier()
{
  RClassInfo cls = (RClassInfo)_parent;
  return cls->name + "_fields_" + name;
}

bool 
FieldInfo::parse(IN(RStreamTokenizer) in)
{
  int tk;
  enum FieldExcect
  {
    Type,
    Label,
    End
  };
  FieldExcect expect = Type;
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) 
  {
    if (tk == StreamTokenizer::TT_WORD) {
      if (in->sval->equals("ACDK_FIELD") == true)
        continue;
      if (in->sval->equals("foreign") == true)
        return false;
      if (in->sval->equals("ACDK_MUTABLE") == true || 
          in->sval->equals("mutable") == true ||
          in->sval->equals("transient") == true) {
        serializable = false;
        flags |= MiIvTransientCall;
        continue;
      }
      if (in->sval->startsWith("ACDK_") == true || in->sval->startsWith("ACDKX_") == true )
        continue;
    
      if (in->sval->equals("static") == true) {
        isstatic = true;
        flags |= MiStatic;
        continue;
      }
      in->pushBack();
      RString str = MetaCompiler::readComponentIndentifier(in);
      if (expect == Type) {
        type = str;

        expect = Label;
      } else if (expect == Label) {
        name = str;
        expect = End;
      } else {
        THROW_CMC(CMCException, in, RString("Expecting ';' at end of field. instead reading: [") + name + "]");
      } 
    } else if (tk == ';') {
      if (expect != End) 
        THROW_CMC(CMCException, in, RString("Unexpected ';' while reading field"));
      return checkCompatibleType(type);
    } else {
      THROW_CMC(CMCException, in, RString("Unexpected token while reading field"));
    }
  }
  return false;
}

void 
FieldInfo::dump(IN(RPrintWriter) out, IN(RString) ind)
{
  out->print(ind);
  if (serializable == true)
    out->print("serializable ");
  if (cloneable == true)
    out->print("cloneable ");
  if (isstatic == true)
    out->print("isstatic ");
  out->println(RString("[") + type + " | " + name + "]");
}


//static 
RString 
FieldInfo::parseJTypeName(IN(RString) tpname) 
{
  /*
    acdk::lang::RObjectArrayImpl<acdk::util::RSet>  => [Lacdk/util/Set;
    acdk::lang::RObjectArrayImpl<RSet>  => [Lacdk/util/Set;
    RObjectArrayImpl<RSet>  => [Lacdk/util/Set;
    RObjectArrayImpl<acdk::lang::RshortArray> => [[S
    RString => LString;
    // acdk::util::RHashTable<::CORBA::Object> => L[acdk/util/HashTable;LCORBA/Object;
  */
  int idx;
  if ((idx = tpname->indexOf('<')) != -1) {
    RString templateType = tpname->substr(0, idx);
    if (tpname->charAt(tpname->length() - 1) != '>')
      THROW1(Exception, "Malformed type: " + tpname);
    RString itpname = tpname->substr(idx + 1, tpname->length() - 1);
    // Asuming all <> has underlying Array
    return "[" + parseJTypeName(itpname->trim());
  }
  RString ns;
  RString cname  = tpname;
  if (tpname->indexOf(":") != -1) {
    int idx = tpname->lastIndexOf(":");
    ns = tpname->substr(0, idx + 1)->replace("::", "/");
    if (ns->charAt(0) == '/')
      ns = ns->substr(1);
    cname = tpname->substr(idx + 1);

  }
  if (cname->endsWith("Array")) {
    if (cname->equals((RString)"RcharArray")) 
      return "[C";
    if (cname->equals((RString)"RbyteArray")) 
      return "[B";
    if (cname->equals((RString)"RshortArray")) 
      return "[S";
    if (cname->equals((RString)"RintArray")) 
      return "[I";
    if (cname->equals((RString)"RlongArray")) 
      return "[J";
    if (cname->equals((RString)"RFloatArray")) 
      return "[F";
    if (cname->equals((RString)"RDoubleArray")) 
      return "[D";
    if (cname->equals((RString)"RBoolArray") || cname->equals((RString)"RbooleanArray")) 
      return "[Z";
    return cname;
  }
  
  if (cname->equals((RString)"char")) 
    return "C";
  if (cname->equals((RString)"ucchar")) 
    return "U";
  if (cname->equals((RString)"byte")) 
    return "B";
  if (cname->equals((RString)"short")) 
    return "S";
  if (cname->equals((RString)"int")) 
    return "I";
  /*if (cname->equals((RString)"long")) 
    return "I";*/
  if (cname->equals((RString)"jlong")) 
    return "J";
  if (cname->equals((RString)"float")) 
    return "F";
  if (cname->equals((RString)"double")) 
    return "D";
  if (cname->equals((RString)"bool")) 
    return "Z";
  if (cname->equals((RString)"RObjectArray"))
    return "[Object";
  if (cname->charAt(0) == 'R' && Character::isUpperCase(cname->charAt(1)) == true)
    cname = cname->substr(1);
  return "L" + (ns == Nil ? String::emptyString() : ns) +  cname + ";";
}

bool 
FieldInfo::invokeCodeAttributes(IN(RModuleInfo) cm, IN(RClassInfo) ci) 
{
  return CodeInfo::invokeCodeAttributes();
}

} // namespace mc
} // namespace tools
} // namespace acdk
