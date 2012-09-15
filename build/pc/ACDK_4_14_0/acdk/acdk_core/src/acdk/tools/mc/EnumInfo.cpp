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



#include "EnumInfo.h"
#include "ModuleInfo.h"

namespace acdk {
namespace tools {
namespace mc {

EnumInfo::EnumInfo(IN(RModuleInfo) module, IN(RArrayList) thenamespace, IN(RArrayList) usings, IN(RClassInfo) cls)
: CodeInfo(0, "") // ### FIXME Modifier::IsEnumInfo
, _module(module)
, _namespace(RArrayList(thenamespace->clone()))
, _usings(RArrayList(usings->clone()))
, _class(cls)
, _values(new EnumInfoValueArray(0))
, _isDefined(false)
, hasMetinfDef(false)
{
}

RString 
EnumInfo::getMetaInfoCIdentifier()
{
  return name;
}


bool 
EnumInfo::detectEnd(int tk, IN(RStreamTokenizer) in)
{
  if (tk != StreamTokenizer::TT_WORD && tk == '}')
  {
    tk = in->nextToken();
    resolveStringValues();
    return true;
  }
  return false;
}


bool 
EnumInfo::parse(IN(RStreamTokenizer) in)
{
  enum Expect
  {
    EnumName,
    Name,
    Value
  };
  Expect expect = EnumName;
  int tk;
  REnumInfoValue curValue;
  int curIntValue = 0;
  bool curIntValueDefined = true;
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) 
  {
    switch (expect)
    {
    case EnumName:
    {
      if (tk != StreamTokenizer::TT_WORD)
        return false;
      name = in->sval;
      tk = in->nextToken();
      if (tk == ';')
        return true;
      if (tk != '{')
        return false;
      _isDefined = true;
      expect = Name;
      break;
    }
    case Name:
    {
      if (detectEnd(tk, in) == true)
        return true;
      curValue = new EnumInfoValue(in->sval);
      expect = Value;
      break;
    }
    case Value:
    {
      if (tk == ',')
      {
        _values->append(curValue);
        expect = Name;
        break;
      }
      do {
        if (detectEnd(tk, in) == true)
        {
          _values->append(curValue);
          return true;
        }
        tk = in->nextToken();
        if (tk == ',')
        {
          _values->append(curValue);
          expect = Name;
          break;
        }
      } while (true);
      /*
      if (detectEnd(tk, in) == true)
      {
        if (curValue != Nil)
        {
          curValue->setIntValue(curIntValue);
          _values->append(curValue);
        }
        return true;
      }
      if (tk == ',')
      {
        if (curIntValueDefined == false)
          ACDK_LOG(Error, "Parsing enumeration " + name + ": Eannot determine this enumeration value: " + curValue->name);
        curValue->setIntValue(curIntValue);
        ++curIntValue;
         _values->append(curValue);
        expect = Name;
        break;
      }
      if (tk == '=')
        tk = in->nextToken();
      bool neg = false;
      if (tk == '-')
      {
        neg = true;
        tk = in->nextToken();
      }
      try {
        //curValue->setIntValue(Integer::parseInt(in->sval));
        //curIntValue = curValue->value + 1;
        curIntValueDefined = true;
      } catch (RNumberFormatException ex) {
        REnumInfoValue vi = getValue(in->sval);
        if (vi != Nil)
        {
          curValue->setIntValue(vi->value);
          curIntValueDefined = true;
        }
        else
        {
          curValue->stringValue = in->sval;
          curIntValueDefined = false;
        }
      }
      tk = in->nextToken();
      if (detectEnd(tk, in) == true)
        return true;
      expect = Name;
      // tk == ','
      */
      break;
    }
    }
  }
  return false;
}

REnumInfoValue  
EnumInfo::getValue(IN(RString) str)
{
  for (int i = 0; i < _values->length(); ++i)
  {
    if (_values[i]->name->equals(str) == true)
      return _values[i];
  }
  return Nil;
}

bool 
EnumInfo::resolveStringValues()
{
  bool ret = true;
  for (int i = 0; i < _values->length(); ++i)
  {
    if (_values[i]->stringValue != Nil && _values[i]->valueDefined == false)
    {
      REnumInfoValue v = getValue(_values[i]->stringValue);
      if (v != Nil)
        _values[i]->setIntValue(v->value);
      else
      {
        ret = false;
        ACDK_LOG(Error, "Parsing enumeration " + name + ": Cannot resolve enum value: " + _values[i]->stringValue);
      }
    }
  }
  return ret;
}

void 
EnumInfo::writeEnumInfo(IN(RPrintWriter) out)
{
  if (_isDefined == false)
    return;
  ModuleInfo::writeOpenNamespace(out, _namespace, _usings);
  int i;
  for (i = 0; i < _values->length(); ++i)
  {
    REnumInfoValue ev = _values[i];
    out->print("\n");
    out->print(RString("::acdk::lang::dmi::ClazzEnumValueInfo ") + name + "_" + ev->name + 
      " = \n{\n  ::acdk::lang::dmi::MiEnumValInfo, // flags\n  0, // attributeRes\n  \"" 
        + ev->name + "\", // name\n" 
        + "  -1, // hashCode\n"
        + "  \"\", // ns\n  0, // _scopeParent\n  0, // _nextSibling\n  "
        + "0,  // ClazzEnum definition\n  "
        + ModuleInfo::getNameSpace(_namespace, "::") + "::" + ev->name + ", // value\n"
        + "\n};\n\n"
        );
  }
  
  out->print("::acdk::lang::dmi::ClazzEnumValueInfo* " + name + "_enumValues[] = {\n");
  for (i = 0; i < _values->length(); ++i)
  {
    REnumInfoValue ev = _values[i];
    out->print("  &" + name + "_" + ev->name + ",\n");
  }
  out->print("0\n};\n\n");
  if (hasMetinfDef == true)
    out->print("::acdk::lang::dmi::ClazzEnumInfo* " + name + "MetaInf::GetEnumInfo()\n{\nstatic ");
  
  out->print("::acdk::lang::dmi::ClazzEnumInfo " + name + "_enumInfo = {\n");
  out->print("  ::acdk::lang::dmi::MiEnumInfo, // flags\n"
             "  0, // attribute rest\n");
  out->print("  \"" + name + "\", // name\n");
  out->print("  -1, // hashCode\n");
  out->print("  \"" + ModuleInfo::getNameSpace(_namespace, "/") + "\", // ns\n");
  out->print("  0, // _scopeParent\n");
  out->print("  0, // _nextSibling\n");
  out->print("  " + name + "_enumValues, // values\n");
  out->print("  0, // internal next link\n");
  out->print("};\n\n");
  if (hasMetinfDef == true)
    out->print("  static ");
  out->print("::acdk::lang::dmi::RegisterEnumInfo _register_" + name + "(&" + name + "_enumInfo);\n\n");
  if (hasMetinfDef == true)
  {
    out->print("  return &" + name + "_enumInfo;\n}\n");
    out->print("static ::acdk::lang::dmi::RegisterEnumInfo _register_" + name + "EnumInfo(" + name + "MetaInf::GetEnumInfo());\n\n");

  }
  ModuleInfo::writeCloseNamespace(out, _namespace);
}

} // namespace mc
} // namespace tools
} // namespace acdk


