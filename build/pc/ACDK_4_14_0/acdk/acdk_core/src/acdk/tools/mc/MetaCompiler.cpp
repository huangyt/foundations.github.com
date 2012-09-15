
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



#include "MetaCompiler.h"
//#include "CodeAttribute.h"
#include <acdk/cfgscript/Script.h>
#include "CMCException.h"
#include "McConfigAttribute.h"
#include <acdk/io/StringWriter.h>

namespace acdk {
namespace tools {
namespace mc {
  
using namespace acdk::cfgscript;

static RMetaCompiler gMetaCompiler;
bool MetaCompiler::externalMetaInfo = false;
bool MetaCompiler::generateProxies = false;

MetaCompiler::MetaCompiler()
: _registeredAttributes(new acdk::util::TreeMap())
, _units(new ArrayList())
, _enums(new EnumInfoArray(0))
, _unitDefsWritten(false)
, baseMetaInfoHeaderWritten(false)
, dummyExportForExternalMIWritten(false)
{
  gMetaCompiler = this;
  McConfigAttribute::initAttribute(this);
}

//static 
RMetaCompiler 
MetaCompiler::getMetaCompiler()
{
  return gMetaCompiler;
}

void 
MetaCompiler::registerAttribute(IN(RString) name, IN(RString) clsname)
{
  _registeredAttributes->put(&name, &clsname);
}

RString 
MetaCompiler::getRegisteredAttribute(IN(RString) name)
{
  RObject obj = _registeredAttributes->get(&name);
  if (obj != Nil)
    return (RString)obj;
  return name;
}

RUnitInfo 
MetaCompiler::addUnit(IN(RString) unitdeclarator)
{
  RUnitInfo ui = new UnitInfo(unitdeclarator);
  _units->add(&ui);
  return ui;
}
bool 
MetaCompiler::hasUnit(IN(RString) unit)
{
  return getUnit(unit) != Nil;
}

RUnitInfo 
MetaCompiler::getUnit(IN(RString) unit)
{
  RIterator it = _units->iterator();
  while (it->hasNext() == true)
  {
    RUnitInfo ui(it->next());
    if (ui->name->equals(unit) == true)
      return ui;
  }
  return Nil;
}

RCodeAttribute 
MetaCompiler::readParseCodeAttribute(IN(RString) code)
{
  RString name = code;
  RString parameter = "()";
  int startparam;
  if ((startparam = name->indexOf('(')) != -1)
  {
    parameter = name->substr(startparam);
    name = name->substr(0, startparam);
  }
  name = getRegisteredAttribute(name);
  ACDK_LOG(Info, "Parse MetaCode: [" + name + "][" + parameter + "]");
  RString evalcode = "attr = new " + name + parameter + ";";
  ACDK_LOG(Info, "Parse MetaCode: [" + evalcode + "]");
  RProps props = new Props();
  RObject erg ;
  try { 
    RScript script = new Script("<mem>");
    script->eval(evalcode, props, ScriptReadWriteParent);
    erg = props->getObjectVal("attr");
  } catch (RThrowable ex) {
    ACDK_LOG(Error, "Failed execute Attribute Code: " + evalcode + " with " + ex->getMessage());
  }
  return (RCodeAttribute)erg;
}


//static
bool
MetaCompiler::skipUntilToken(IN(RStreamTokenizer) in, int tk)
{
  int rtk;
  while ((rtk = in->nextToken()) != StreamTokenizer::TT_EOF) 
    if (rtk == tk)
      return true;
  return false;
}

//static 
void 
MetaCompiler::skipWS(IN(RStreamTokenizer) in)
{
  int rtk;
  while ((rtk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    if (rtk != StreamTokenizer::TT_WS) {
      in->pushBack();
      return;
    }
  }
}

//static
void
MetaCompiler::skipPreprocessorStatement(IN(RStreamTokenizer) in)
{
  int tk;
  bool breakLineAttn = false;
  WantWhiteSpaceOnStack wantws(in, true);
  WantNLOnStack wantln(in, true);
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF)  
  {
    if (tk == '\\') 
    {
      breakLineAttn = true;
      continue;
    } 
    else if (tk == StreamTokenizer::TT_EOL)  // || tk == '\n' || tk == '\r'
    {
      if (breakLineAttn == true) 
      {
        breakLineAttn = false;
        continue;
      } 
      else
        break;
    } 
    breakLineAttn = false;
  }
}

//static
void
MetaCompiler::skipStatement(IN(RStreamTokenizer) in)
{
  int tk;
  int brackets = 0;
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    if (tk == ';' && brackets == 0)
      return;
    if (tk == '{') {
      ++brackets;
      continue;
    }
    if (tk == '}') {
      --brackets;
      if (brackets == 0) {
        tk = in->nextToken();
        if (tk != ';')
          in->pushBack();
        return;
      }
    }
  }
}


//static
void
MetaCompiler::skipTypeDeclarator(IN(RStreamTokenizer) in)
{
  int tk;
  int openBrackets = 0;

  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF)  {
    if (tk == ';' && openBrackets == 0)
      return;
    if (tk == '{') {
      ++openBrackets;
      continue;
    }
    if (tk == '}') {
      --openBrackets;
      continue;
    }
  }
}


//static
RString
MetaCompiler::readComponentIndentifier(IN(RStreamTokenizer) in, RTokenStack tkstack)
{
  int tk = 0;
  bool hadtkstack = true;
  if (tkstack == Nil) {
    tkstack = new TokenStack(in);
    hadtkstack = false;
  }
  int bracketcount = 0;
  bool skipNextWS = false;
  RStringBuffer sb = new StringBuffer(20);
  WantWhiteSpaceOnStack _wantWs(in, true);
  skipWS(in);
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    tkstack->push();
    if (tk == StreamTokenizer::TT_WORD) {
      sb->append(in->sval);
      if (in->sval->equals("unsigned") == true)
        skipNextWS = true;
      continue;
    }
    if (tk == StreamTokenizer::TT_WS) {
      if (bracketcount == 0 && skipNextWS == false)
        break;
      sb->append(in->sval);
      skipNextWS = false;
      continue;
    }
    if (tk == '<') {
      sb->append(char(tk));
      bracketcount++;
      continue;
    }
    if (tk == '>') {
      if (bracketcount < 1) 
        THROW_CMC(CMCException, in, "Unexpected '>' while reading componound identifier");
      --bracketcount;
      sb->append(char(tk));
      if (bracketcount == 0)
        break;
      continue;
    }
    if (tk == ':') {
      sb->append(char(tk));
      continue;
    }
    // all other token types will break reading identifier
    tkstack->pop();
    break;
  }
  if (hadtkstack == false)
    tkstack->flush();
  return sb->toString();
}


//static 
RString 
MetaCompiler::readCodeAttributeCode(IN(RStreamTokenizer) in)
{
  StringBuffer sb;
  WantWhiteSpaceOnStack wantws(in, true);
  int parantescount = 1;
  int tk = in->nextToken();
  if (tk != '(')
    THROW_CMC(CMCException, in, "Expect '('");
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) 
  {
    if (tk == '(')
    {
      ++parantescount;
    }
    else if (tk == ')')
    {
      if (--parantescount == 0)
      {
        return sb.toString();
      }
    }
    sb.append(in->toCode());
  }
  THROW_CMC(CMCException, in, "Unexpected End while reading CodeAttribute definition");
  return sb.toString();
}

bool
MetaCompiler::isAccessToken(IN(RString) str)
{
  return (str->equals(":") == true ||
          str->equals("public:") == true ||
          str->equals("public") == true ||
          str->equals("private") == true ||
          str->equals("private:") == true ||
          str->equals("protected:") == true ||
          str->equals("protected") == true);
}

bool 
MetaCompiler::checkCompatibleType(RString str)
{
  if (str->length() < 2)
    return false;
  if (str->lastIndexOf(':') != -1)
    str = str->substr(str->lastIndexOf(':') + 1);
  if (str->charAt(0) == 'R' && Character::isUpperCase(str->charAt(1)) == true)
    return true;
  if (str->equals("void") == true ||
      str->equals("bool") == true ||
      str->equals("char") == true ||
      str->equals("ucchar") == true ||
      str->equals("uc2char") == true ||
      str->equals("byte") == true ||
      str->equals("short") == true ||
      str->equals("int") == true ||
      str->equals("jlong") == true ||
      str->equals("float") == true ||
      str->equals("double") == true)
    return true;
  
  if (str->startsWith("RboolArray") == true ||
      str->startsWith("RcharArray") == true ||
      str->startsWith("RuccharArray") == true ||
      str->startsWith("RbyteArray") == true ||
      str->startsWith("RshortArray") == true ||
      str->startsWith("RintArray") == true ||
      str->startsWith("RlongArray") == true ||
      str->startsWith("RfloatArray") == true ||
      str->startsWith("RdoubleArray") == true)
    return true;
  // ## test ::acdk::lang::RObject
  return false;
}


void 
writeUnitDefinition(IN(RPrintWriter) out, IN(RUnitInfo) unitdef)
{
  RString ns = "";
  RString name = unitdef->name->replace('_', '/');
  int idx = name->lastIndexOf('/');
  if (idx != -1)
  {
    ns = name->substr(0, idx);
    name = name->substr(idx + 1);
  }
  out->print("\n\nstruct acdk::lang::dmi::UnitInfo " + unitdef->name + "_unitInfo = {\n  ::acdk::lang::dmi::MiUnitInfo, // flags\n  0, //attributeRes\n  ");
  RString unitdecl = unitdef->name->replace('_', '/');
  
  out->print("\"" + name + "\", // name of unit\n"
        + "  -1, // hashCode\n"
      + "  \"" + ns + "\", // ns\n  0, // _scopeParent\n  0, // _nextScopeSibling\n  0 // _firstChild first ClazzInfo of this unit\n};\n\n");
  out->print("static ::acdk::lang::dmi::RegisterUnitInfo _register_" + unitdef->name + "_unitInfo(&" + unitdef->name + "_unitInfo);\n\n");
}

void 
MetaCompiler::writeUnitDefinitions(IN(RPrintWriter) out)
{
  if (_unitDefsWritten == true)
    return;
  _unitDefsWritten = true;
  acdk::io::StringWriter mout;
  PrintWriter pout((acdk::io::RCharWriter)&mout);
  //writeCodes(&pout, ModuleInit);
  RIterator it = _units->iterator();
  while (it->hasNext() == true)
  {
    RUnitInfo ui(it->next());
    writeUnitDefinition(out, ui);
    ui->writeCode(&pout, ModuleInit);
  }
  RString str = mout.getString();
  if (str->length() == 0)
    return;
  RString structidentifier = RString("_ModuleInitializer") + CodeAttribute::getCounter();
  StringBuffer sb("\nstruct ");
  sb << structidentifier
  << "\n{\n  " << structidentifier << "()\n  {\n";
  str = str->convert(CCAscii);
  sb << str;
  sb << "  }\n};\n\nstatic " << structidentifier << " " << structidentifier << "_instance;\n\n\n";
  out->print(sb.toString());
}

} // namespace mc
} // namespace tools
} // namespace acdk


