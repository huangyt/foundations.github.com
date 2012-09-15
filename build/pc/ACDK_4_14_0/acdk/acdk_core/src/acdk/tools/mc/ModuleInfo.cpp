
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




#include "ModuleInfo.h"
#include "ArgumentInfo.h"
#include "MetaCompiler.h"
#include "CMCException.h"
#include "DmiProxyAttribute.h"
#include "DmiProxyGenerator.h"

#include <acdk/io/File.h>
#include <acdk/locale/Encoding.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/util/StringTokenizer.h>

namespace acdk {
namespace tools {
namespace mc {

//virtual
RString
ModuleInfo::getMetaInfoCIdentifier()
{
  return "";
}

RString
ModuleInfo::baseFilename()
{
  acdk::io::File tf(_fname);
  return tf.getName();
}

RString
ModuleInfo::getParent()
{
  acdk::io::File tf(_fname);
  return tf.getParent();
}

bool
ModuleInfo::checkContext()
{
  bool berg = true;
  RIterator it = _classes->iterator();
  while (it->hasNext() == true)
    berg = RClassInfo(it->next())->checkContext(const_cast<ModuleInfo*>(this)) && berg;
  return berg;
}

//static
void
ModuleInfo::writeOpenNamespace(IN(RPrintWriter) out, IN(RArrayList) nslist, IN(RArrayList) usings)
{
  out->print("\n");
  RIterator it = nslist->iterator();
  while (it->hasNext() == true)
  {
    RString ns = RString(it->next());
    out->print("namespace ");
    out->print(ns);
    out->print(" { \n");
  }
  out->print("\n");
  if (usings != Nil)
  {
    RIterator it = usings->iterator();
    while (it->hasNext() == true) {
      RString ns = RString(it->next());
      out->print("using namespace ");
      out->print(ns);
      out->print(";\n");
    }
  }
}

//static
void
ModuleInfo::writeCloseNamespace(IN(RPrintWriter) out, IN(RArrayList) nslist)
{
  RIterator it = nslist->iterator();
  out->print("\n");
  while (it->hasNext() == true)
  {
    RString ns = RString(it->next());
    out->print("} // namespace ");
    out->print(ns);
    out->print("\n");
  }
  out->print("\n");
}

void
ModuleInfo::writeClsInfoCPP(IN(RPrintWriter) out, IN(RPrintWriter) stubout, bool withFieldInfo)
{
  RPrintWriter o = stubout;
  if (stubout == Nil)
    o = out;
  if (MetaCompiler::generateProxies == false)
  {
    for (int i = 0; i < _enums->length(); ++i)
    {
      _enums[i]->writeEnumInfo(o);
    }
  }
  RIterator it = _classes->iterator();
  while (it->hasNext() == true)
  {
    RClassInfo(it->next())->writeClassInfo(out, stubout, withFieldInfo);
  }
}


bool
ModuleInfo::hasMetaInfo()
{
  if (_enums->length() > 0)
    return true;
  RIterator it = _classes->iterator();
  while (it->hasNext() == true) {
    if (RClassInfo(it->next())->_hasMetaInfo == true)
      return true;
  }
  return false;
}

void
ModuleInfo::dump(IN(RPrintWriter) out, IN(RString) ind)
{
  out->println(ind + "Modul: [" + _fname + "]");
  RIterator it = _classes->iterator();
  while (it->hasNext() == true) {
    RClassInfo ci = RClassInfo(it->next());
    out->println(ind + "class->");
    ci->dump(out, ind + " ");
  }
}

void
ModuleInfo::writeModuleHeaderInclude(IN(RPrintWriter) out, IN(::acdk::io::RPrintWriter) stubout)
{
  out->print("#include \"");
  if (stubout != Nil || MetaCompiler::generateProxies)
  {
    out->print("../");
    if (stubout != Nil && MetaCompiler::getMetaCompiler()->baseMetaInfoHeaderWritten == false)
    {
      stubout->print("#include \"");
      stubout->println(baseFilename() + "\"");
    }
  }
  out->println(baseFilename() + "\"");
  writeCodes(out, ModuleInclude);
}

bool
ModuleInfo::parse()
{
  ACDK_LOG(Info, "Analyse file: " + _fname);
  RStreamTokenizer in = new StreamTokenizer(
                          new acdk::io::ByteToCharReader(
                            new ::acdk::io::FileReader(_fname),
                            acdk::locale::Encoding::getAsciiEncoding()->getDecoder()));
  return parse(in);
}

// 
RArrayList asContainer(IN(RStringArray) arr) // #### remove this later
{
  RArrayList la = new ArrayList();
  for (int i = 0; i < arr->length(); ++i)
    la->add(&arr[i]);
  return la;
}



bool
ModuleInfo::parse(IN(RStreamTokenizer) in)
{
  int tk;
  bool attn_class = true; // ## for debugging, otherwise it should be false
  bool isclass = true;
  acdk::tools::mc::CodeAttributeArray clsAttributes(0);
  acdk::tools::mc::CodeAttributeArray unitAttributes(0);
  int currentFlags = 0;
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    //RString msg = RString("Readed: [") + ": " + in->getDeviceName() + in->lineno() + "] " +
    //                "Token: " + in->toString();

    if (tk ==  '#') // c-preprocessor
    {
      MetaCompiler::skipPreprocessorStatement(in);
      if (in->ttype == StreamTokenizer::TT_EOF)
        break;
    }
    else if (tk == StreamTokenizer::TT_WORD || tk == ':')
    {
      if (in->sval->equals("ACDK_NO_METAINFO_HEADER") == true)
        return false;
      if (in->sval->equals("namespace") == true)
      {
        tk = in->nextToken();
        RString ns = in->sval;
        if (tk == StreamTokenizer::TT_WORD)
          ;//cerr << "namespace: " << in->sval->c_str() << endl;
        else
          THROW_CMC(CMCException, in, "Expect identifier after \'namespace\'");
        tk = in->nextToken();
        if (tk != '{')
          THROW_CMC(CMCException, in, "Expect identifier after \'namespace\'");
        _curNameSpace->add((RObject)ns);
      } else if (in->sval->equals("using") == true) {
        tk = in->nextToken();
        if (tk != StreamTokenizer::TT_WORD)
          THROW_CMC(CMCException, in, "Expect identifier after \'using\'");
        if (in->sval->equals("namespace") == true) {
          RString nm = MetaCompiler::readComponentIndentifier(in);
          if (in->ttype == StreamTokenizer::TT_EOF)
            break;
          tk = in->nextToken();
          if (tk != ';')
            THROW_CMC(CMCException, in, "Expect \';\' after \'using namespace blabla\'");
          _usings->add((RObject)nm);
        } else {
          MetaCompiler::skipStatement(in);
          if (in->ttype == StreamTokenizer::TT_EOF)
            break;
        }
      } else if (in->sval->equals("ACDK_DECL_UNIT") == true) {
        RString unit = MetaCompiler::readCodeAttributeCode(in);
        RUnitInfo ui = MetaCompiler::getMetaCompiler()->addUnit(unit);
        ui->addCodeAttributes(&unitAttributes);
      } else if (in->sval->equals("ACDK_DECL_THROWABLE") == true || in->sval->equals("ACDK_DECL_THROWABLE_FQ") == true) {
        RString unit = MetaCompiler::readCodeAttributeCode(in);
        _knownExceptions->append(unit->substr(0, unit->indexOf(","))->trim());
        tk = in->nextToken(); // consume ';'
      }
      else if (in->sval->equals("ACDK_DECL_ENUM_FQ") == true ||
               in->sval->equals("ACDK_DECL_ENUM") == true)
      {
        bool isfq = false;
        if (in->sval->equals("ACDK_DECL_ENUM_FQ") == true)
          isfq = true;
        acdk::util::RList ns;
        RString enname;
        RString nsname;
        tk = in->nextToken(); // (
        tk = in->nextToken();
        if (isfq == true)
        {
          nsname  = in->sval;
          ns = (acdk::util::RList)asContainer(acdk::util::StringTokenizer(in->sval, "::").allToken());
          //ns = acdk::util::StringTokenizer(in->sval, "::").allToken()->asContainer();
          in->nextToken(); // ,
          in->nextToken(); 
          enname = in->sval;
        }
        else
        {
          enname = in->sval;
        }
        addType(TsEnum, enname);
        if (nsname != Nil)
        {
          if (nsname->endsWith("::") == true)
            addType(TsEnum, nsname + enname);
          else
            addType(TsEnum, nsname + "::" + enname);
        }
        /*
        REnumInfo ei;
        if (ns == Nil)
          ei = new EnumInfo(this, _curNameSpace, _usings);
        else
          ei = new EnumInfo(this, ns, _usings);
        ei->name = enname;
        MetaCompiler::getMetaCompiler()->_enums->append(ei);
        */
        tk = in->nextToken(); // )
        tk = in->nextToken(); // ;
        continue;
      }
      else if (in->sval->equals("ACDK_DEF_ENUM") == true || in->sval->equals("ACDK_DEF_LIB_ENUM") == true)
      {
        bool withExport = false;
        if (in->sval->equals("ACDK_DEF_LIB_ENUM") == true)
          withExport = true;
        tk = in->nextToken();
        tk = in->nextToken();
        RString xexport = "";
        if (withExport == true)
        {
          xexport = in->sval;
          tk = in->nextToken();
          tk = in->nextToken();
        }
        RString enumName = in->sval;
        REnumInfo ei = getEnumInfo(enumName);
        if (ei == Nil)
        {
          ACDK_LOG(Error, "Failed to parse enum definition: " + enumName);
          MetaCompiler::skipStatement(in);
          continue;
        }
        ei->hasMetinfDef = true;
        MetaCompiler::skipStatement(in);
        continue;
      }
      else if (in->sval->equals("ACDK_CLASSATTRIBUTE") == true ||
                 in->sval->equals("ACDK_UNITATTRIBUTE") == true )
      {
        bool unitinfo = in->sval->equals("ACDK_UNITATTRIBUTE");
        RString code = MetaCompiler::readCodeAttributeCode(in);
        acdk::tools::mc::RCodeAttribute ca = MetaCompiler::getMetaCompiler()->readParseCodeAttribute(code);
        if (ca != Nil)
        {
          if (unitinfo == true)
            unitAttributes.append(ca);
          else
            clsAttributes.append(ca);
        }
        else
        {
          // ingnore it! THROW_CMC(CMCException, in, "Parsed Attribute failed: " + code);
        }
      } else if (in->sval->equals("ACDK_CLASS") == true) {
        attn_class = true;
      } else if (in->sval->equals("ACDK_INTERFACE") == true) {
        attn_class = true;
        isclass = false;
        currentFlags |= acdk::lang::dmi::MiCiInterface;
      }
      else if (in->sval->equals("final") == true)
      {
        currentFlags |= acdk::lang::dmi::MiNoDmiProxy;
      }
      else if (in->sval->equals("class") == true)
      {
        if (attn_class == true)
        {
          RClassInfo ci = new ClassInfo(this, _curNameSpace, _usings, isclass, currentFlags);
          currentFlags = 0;
          if (ci->parse(in) == true)
          {
            ci->addCodeAttributes(&clsAttributes);

            // ### experimental
            DmiProxyGenerator::addToClass(ci);
            _classes->add((RObject)ci);
            clsAttributes.resize(0);
            ci->_isThrowable = isKnownThrowable(ci->name);
          }
          if (in->ttype == StreamTokenizer::TT_EOF)
            break;
        }
        isclass = true; //## debugging
      } else if (in->sval->equals("const") == true ||
                 in->sval->equals("extern") == true) {
        // 'const int val = 42;'
        MetaCompiler::skipStatement(in);
        if (in->ttype == StreamTokenizer::TT_EOF)
          break;
      }
      else if (in->sval->equals("enum") == true)
      {
        REnumInfo ei = new EnumInfo(this, _curNameSpace, _usings);
        if (ei->parse(in) == true)
        {
          MetaCompiler::getMetaCompiler()->_enums->append(ei);
          addType(TsEnum, ei->name);
          _enums->append(ei);
        }
        else
        {
          //tk = in->nextToken();
          //### addType(Enum, in->sval);
          ACDK_LOG(Warn, "Failed to parse enum: " + ei->name);
          MetaCompiler::skipTypeDeclarator(in);
        }
          if (in->ttype == StreamTokenizer::TT_EOF)
          break;
        continue;
      } else if (in->sval->equals("typedef") == true ||
                 in->sval->equals("friend") == true ||
                 in->sval->equals("template") == true ||
                 in->sval->equals("union") == true ||
                 in->sval->equals("struct") == true) {
        MetaCompiler::skipTypeDeclarator(in);
        if (in->ttype == StreamTokenizer::TT_EOF)
          break;
      } else {
        // hoply only function definitions.
        MetaCompiler::skipStatement(in);
        if (in->ttype == StreamTokenizer::TT_EOF)
          break;
      }
    } else if (tk == '}') { // This is namespace closure
      _usings = new ArrayList();
      if (_curNameSpace->size() == 0)
        THROW_CMC(CMCException, in, "Unexpecting closing namespace");
      _curNameSpace->removeRange(_curNameSpace->size() - 1, _curNameSpace->size());
    } else {
      THROW_CMC(CMCException, in, "Unexpected Token in ModuleInfo::parse()");
    }
  }
  if (name == Nil)
    return false;
  return true;
}


bool
ModuleInfo::invokeCodeAttributes()
{
  RIterator it = _classes->iterator();
  while (it->hasNext() == true)
    if (RClassInfo(it->next())->invokeCodeAttributes(this) == false)
      return false;
  CodeInfo::invokeCodeAttributes();
  return true;
}

void
ModuleInfo::writeCodes(IN(RPrintWriter) out, CodeWhere where)
{
  RIterator it = _classes->iterator();
  while (it->hasNext() == true)
    RClassInfo(it->next())->writeCodes(out, where);
  writeCode(out, where);
}

//static
RString
ModuleInfo::getNameSpace(IN(RArrayList) ns, IN(RString) separator)
{
  RIterator it = ns->iterator();
  StringBuffer ret(20);
  while (it->hasNext() == true)
  {
    RString ns = RString(it->next());
    if (ret.length() > 0)
      ret.append(separator);
    ret.append(ns);
  }
  return ret.toString();
}

REnumInfo
ModuleInfo::getEnumInfo(IN(RString) name)
{
  for (int i = 0; i < _enums->length(); ++i)
    if (_enums[i]->getBaseName()->equals(name) == true)
      return _enums[i];
  return Nil;
}

bool
ModuleInfo::isKnownThrowable(IN(RString) name)
{
  for (int i = 0; i < _knownExceptions->length(); ++i)
  {
    if (_knownExceptions[i]->equals(name) == true)
      return true;
  }
  return false;
}

} // namespace mc
} // namespace tools
} // namespace acdk

