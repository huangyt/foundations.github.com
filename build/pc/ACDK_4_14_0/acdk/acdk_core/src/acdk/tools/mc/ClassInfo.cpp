
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


#include "ClassInfo.h"
#include "SuperInfo.h"
#include "FieldInfo.h"
#include "ArgumentInfo.h"
#include "MetaCompiler.h"
#include "CMCException.h"
#include "McConfigAttribute.h"
#include "ThrowableAttribute.h"
#include "DmiProxyGeneratorExt.h"

#include <acdk/util/TreeMap.h>
#include <acdk/util/Collections.h>
#include <acdk/io/MemWriter.h>
#include <acdk/locale/Encoding.h>

namespace acdk {
namespace tools {
namespace mc {

using namespace acdk::lang::dmi;


ClassInfo::ClassInfo(IN(RModuleInfo) module, IN(RArrayList) thenamespace, IN(RArrayList) usings, bool isclass, int flags)
: CodeInfo(flags, "", (RTypeScope)module)
, _module(module)
, _namespace(RArrayList(thenamespace->clone()))
, _usings(RArrayList(usings->clone()))
, _derivides(new ArrayList())
, _fields(new ArrayList())
, _orgMethods(new ArrayList())
, _methods(new ArrayList())
, _hasCreator(false)
, _hasMetaInfo(false)
, _hasDmiProxy(false)
,  _hasScriptable(false)
, _hasScriptableEx(false)
, _currentAccess(MiPrivate)
, _detectedIncompatibleField(false)
, _isThrowable(false)
, _generateDmiProxy(true)
{
  if (isclass == false)
    flags |= MiCiInterface;
}


//virtual 
RString 
ClassInfo::getMetaInfoCIdentifier()
{
  return name + "::clazzInfo()";
}


RString
ClassInfo::getNamespaceAccessor()
{
  return ModuleInfo::getNameSpace(_namespace, "/");
}

RString 
ClassInfo::getJTypeName()
{
  RIterator it = _namespace->iterator();
  StringBuffer ret(20);
  while (it->hasNext() == true) {
    RString ns = RString(it->next());
    if (ret.length() > 0)
      ret.append("/");
    ret.append(ns);
  }
  if (ret.length() > 0)
    ret.append("/");
  ret.append(name);
  return ret.toString();
}

bool
ClassInfo::isInterface() 
{
  if (flags & MiCiInterface)
    return true;
  if (_derivides->size() == 0 || RSuperInfo(_derivides->get(0))->name->length() == 0) {
    flags |= MiCiInterface;
    return true;
  }
  RIterator it = _derivides->iterator();
  while (it->hasNext() == true) 
  {
    if ((RSuperInfo(it->next())->flags & MiMiVirtual) == 0)
      return false;
  }
  flags |= MiCiInterface;
  return true;
}

RString
getBaseClassName(IN(RString) name)
{
  int pos = name->lastIndexOf(':');
  if (pos == -1)
    return name;
  return name->substring(pos + 1);
}

//static
RString 
ClassInfo::getFlags(int flags)
{
  return MetaInfo::flagsToString(flags & ~MiMcKnownType, ClazzInfoExtFlags(0), TpFtFqName | TpFtAcdkType);
}

bool 
isMacro(IN(RString) str)
{
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end; ++it)
  {
    if (*it != '_' && Character::isUpperCase(*it) == false)
      return false;
  }
  return true;
}

bool 
ClassInfo::parseHeader(IN(RStreamTokenizer) in)
{
  enum HeaderExpect 
  {
    Name,
    Super,
    Interface
  };
  HeaderExpect expect = Name;
  int deraccess = 0;
  bool isforeign = false;
  int tk;
  bool hasSuperImpl = false; // has class an extends
  RString leadingGlobalScope = "";
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    if (tk == '{') 
      break;
    if (tk == StreamTokenizer::TT_WORD) 
    {
      if (in->sval->equals("ACDK_INTERFACEBASE") == true)
      {
        flags |= MiCiInterface;
        continue;
      }
      if (expect == Name && isMacro(in->sval) == true && _publicDecl == Nil) // because class URL
      {
        _publicDecl = in->sval;
        continue;
      }
      if (in->sval->equals("public") == true) {
        deraccess |= MiPublic;
        if (expect == Super)
          hasSuperImpl = true;
        continue;
      }
      if (in->sval->equals("protected") == true) {
        deraccess |= MiProtected;
        continue;
      }
      if (in->sval->equals("private") == true) {
        deraccess |= MiPrivate;
        continue;
      }

      if (in->sval->equals("virtual") == true) {
        deraccess |= MiMiVirtual;
        continue;
      }
      if (in->sval->equals("implements") == true)
      {
         deraccess = MiMiVirtual | MiPublic;
         continue;
      }
      if (in->sval->equals("extends") == true)
      {
         deraccess = MiPublic;
         hasSuperImpl = true;
         continue;
      }
      if (in->sval->equals("final") == true)
      {
         deraccess = MiNoDmiProxy;
         continue;
      }
      if (in->sval->equals("foreign") == true) 
      {
        isforeign = true;
        continue;
      }
      if (in->sval->equals("inline") == true || in->sval->equals("explicit") == true) 
      {
        continue;
      }
      
      if (in->sval->equals("friend") == true || in->sval->equals("typedef") == true || in->sval->equals("template") == true)
      {
        tk = skipStatementOrFunction(in);
        continue;
      }
      if (expect == Name) {
        name = in->sval;
        expect = Super;
      } else if (expect == Super) {
        in->pushBack();
        RString name = leadingGlobalScope + MetaCompiler::readComponentIndentifier(in);
        if (isforeign == true)
          isforeign = false;
        else
          _derivides->add(new SuperInfo(deraccess, name, this));
        deraccess = 0; //MiPrivate;
        expect = Interface;
      } else { // Interface
        in->pushBack();
        RString name = MetaCompiler::readComponentIndentifier(in);
        if (isforeign == true)
          isforeign = false;
        else
          _derivides->add(new SuperInfo(deraccess | MiCiInterface, name, this));
        if (name->endsWith("::acdk::io::Serializable") || 
            name->equals("acdk::io::Serializable") ||
            name->equals("Serializable"))
        {
          flags |= MiCiSerializable;
        }
        deraccess = 0; //MiPrivate;
      }
    } else if (tk == ':') {
      int stk = in->nextToken();
      if (stk == ':') 
        leadingGlobalScope = "::";
      else 
        in->pushBack();
      if (expect != Super && stk != ':') // class A : B or class A : ::ns::B
        THROW_CMC(CMCException, in, "Syntax error, readed \':\' but not expext super");
    } else if (tk == ',') {
      if (expect == Name)
        THROW_CMC(CMCException, in, "Expect label of this class");
      if (expect == Super)
        THROW_CMC(CMCException, in, "Expect label of super class");
    } else if (tk == ';' && expect == Super) {
      return false; // only predeclaration of the class
    } else {
      THROW_CMC(CMCException, in, "Unexpexted token");
    }
  }
  if (hasSuperImpl == false)
    flags |= MiCiInterface;
  return true;
}

bool isTypeDeclarator(IN(RString) val)
{
  return val->equals("class") == true ||
         val->equals("struct") == true ||
         val->equals("enum") == true ||
         val->equals("union") == true;
}       

void
ClassInfo::detectAccessModifier(IN(RStreamTokenizer) in)
{
  if (in->sval->equals("public") == true ||
      in->sval->equals("public:") == true)
    _currentAccess = MiPublic;
  else if (in->sval->equals("protected") == true ||
           in->sval->equals("protected:") == true)
    _currentAccess = MiProtected;
  else if (in->sval->equals("private") == true ||
           in->sval->equals("private:") == true)
    _currentAccess = MiPrivate;
}

/** try to parse statetem, until ';' or '(' */
bool
ClassInfo::detectField(IN(RStreamTokenizer) in)
{
  RTokenStack tkstack = new TokenStack(in);
  _detectedIncompatibleField = false;
  int tk;
  RString label;
  RString type;
  WantWhiteSpaceOnStack _wantWS(in, true);
  bool bret = false;
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    tkstack->push();
    if (tk == StreamTokenizer::TT_WS)
      continue;
    if (tk == '~') // ignore destructor
      break;
    if (tk == StreamTokenizer::TT_WORD) {
      if (in->sval->startsWith("ACDK_") == true)
        continue;
      if (isInvalidDeclStartIdentifier(in->sval) == true)
        return false;
      detectAccessModifier(in);
      if (in->sval->equals("virtual") == true ||
          in->sval->equals("implements") == true ||
          in->sval->equals("extends") == true ||
          in->sval->equals("public") == true ||
          in->sval->equals("public:") == true ||
          in->sval->equals("protected") == true ||
          in->sval->equals("protected:") == true ||
          in->sval->equals("private") == true ||
          in->sval->equals("private:") == true ||
          in->sval->equals("typedef") == true ||
          in->sval->equals("friend") == true ||
          in->sval->equals("inline") == true ||
          in->sval->equals("template") == true ||
          in->sval->equals("operator") == true ||
          in->sval->equals("const") == true || // not really a field
          isTypeDeclarator(in->sval) == true ||
          in->sval->equals("ACDK_WITH_METAINFO") == true ||
          in->sval->equals("ACDK_WITH_METAINFO2") == true ||
          in->sval->equals("ACDK_WITH_METAINFO_EX") == true ||
          in->sval->equals("ACDK_ACDK_SCRIPTABLE") == true ||
          in->sval->equals("ACDK_WITH_MI_MEMBER") == true ||
          in->sval->equals("ACDK_WITH_DMIPROXY") == true
          ) 
          break;
        
      if (in->sval->equals("ACDK_FIELD") == true ||
          in->sval->equals("transient") == true ||
          in->sval->equals("mutable") == true ||
          in->sval->equals("ACDK_MUTABLE") == true
          ) 
      {
        continue;
        /*
        while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
          // tkstack->push();
          if (tk == ';') {
            tkstack->pop();
            return false;
          }
        }
        continue;
        */
      }
      if (in->sval->equals("static") == true)
        continue;
      if (in->sval->startsWith("ACDK") == true)
        continue;
      tkstack->pop();
      RString tstr = MetaCompiler::readComponentIndentifier(in, tkstack);
      if (type == Nil)
        type = tstr;
      else if (label == Nil)
        label = tstr;
      else {
        break;
        THROW_CMC(CMCException, in, "Unexpexted token");
      }
    } else if (tk == ';') {
      if (type != Nil && label != Nil) {
        if (_detectedIncompatibleField == true) {
            if (needFieldInfo())
              ACDK_LOG(Warn, in->getStreamPos() + ": Found field after incompatible type in class: " + name);
          bret = false;
        } else {
          bret = true;
        }
      }
      break;
    } else if (tk == '(' // begin of functions args
               || tk == ':' // public:
               ) {
      break;
    } else if (tk == '*' || tk == '&' || tk == '[' || tk == '{') { // this may a field, but not compatible with MM -> ignore it
      if (needFieldInfo())
        ACDK_LOG(Info,  in->getStreamPos() + ": Found incompatible field type [" + type + String::valueOf((char)tk) + " | " + label + "] in class: " + name);
      _detectedIncompatibleField = true;
      break;
    } else {
      THROW_CMC(CMCException, in, "Unexpexted token" + in->sval);
    }
  }
  return bret;
}


int
ClassInfo::skipStatementOrFunction(IN(RStreamTokenizer) in)
{
  int tk;
  int openbrackets = 0;
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    if (in->sval->equals("public") == true || 
        in->sval->equals("extends") == true) {
      _currentAccess = MiPublic;
      return in->nextToken(); 
    }
             
    if (in->sval->equals("private") == true) {
      _currentAccess = MiPrivate;
      return in->nextToken(); 
    }
    if (in->sval->equals("private") == true) {
      _currentAccess = MiPrivate;
      return tk;
    }
    if (in->sval->equals("protected") == true) {
      _currentAccess = MiProtected;
      return in->nextToken(); 
    }
    if (in->sval->equals("protected") == true) {
      _currentAccess = MiProtected;
      return tk;
    }
    if (tk == ';' && openbrackets == 0)
      return tk;
    if (tk == '}') {
      if (openbrackets == 1)
        return tk;
      else
        --openbrackets;
    }
    if (tk == '{')
      ++openbrackets;
  }
  return tk;
}


bool
ClassInfo::isCreateInstance(IN(RStreamTokenizer) in)
{
  /*### no longer needed */
  int tk;
  int openbrackets = 0;
  enum Expect
  {
    Modifier,
    ReturnValue,
    FuncName
  };
  Expect expect = Modifier;
  RTokenStack tkstack = new TokenStack(in);
  WantWhiteSpaceOnStack _wantWs(in, true);
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    tkstack->push();
    if (tk == ';'  || tk == '(' || tk == '{') 
      return false;
    if (tk == ':')
      continue;
    if (tk != StreamTokenizer::TT_WORD)
      return false;
    if (expect == Modifier) {
      if (in->sval->equals("static") != true)
        return false;
      expect = ReturnValue;
      continue;
    }
    if (expect == ReturnValue) {
      if (in->sval->equals("RObject") != true)
        return false;
      expect = FuncName;
      continue;
    }
    if (expect == FuncName) {
      if (in->sval->equals("create_instance") != true)
        return false;
      return true;
    }
  }
  return false;
}


void
ClassInfo::addStandardMethods()
{
  RMethodInfo mi = new MethodInfo(this, this, MiPublic | MiStatic);
  mi->name = "GetClass";
  mi->returnType = "::acdk::lang::RClass";
  _methods->add((RObject)mi); 
  /*
  mi = new MethodInfo(this, this, MiPublic| MiMiVirtual);
  mi->name = "getClass";
  mi->returnType = "::acdk::lang::RClass";
  _methods->add((RObject)mi); 
  */
  if (_hasDmiProxy == true)
  {
    mi = new MethodInfo(this, this, MiPublic | MiStatic);
    mi->name = "getDmiProxy";
    mi->returnType = "::acdk::lang::dmi::RDmiProxy";
    
    RArgumentInfo arg = new ArgumentInfo(&mi, "::acdk::lang::RObject", "dmidelegate", MiAiIn);
    mi->args->add(&arg);
    mi->argcount = 1;
    _methods->add(&mi); 
  }
}

bool 
ClassInfo::parse(IN(RStreamTokenizer) in)
{
  if (parseHeader(in) == false)
    return false;
  
  _currentAccess = MiPrivate;
  int tk;
  int openblocks = 1;
  acdk::tools::mc::CodeAttributeArray methodAttributes(0);
  acdk::tools::mc::CodeAttributeArray fieldAttributes(0);

  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    if (tk == StreamTokenizer::TT_WORD || tk == '~') {
      in->pushBack();
      if (in->sval->equals("ACDK_ACDK_SCRIPTABLE") == true) {
        _hasMetaInfo = true;
        _hasScriptable = true;
        in->nextToken();
        continue;
      }
      if (in->sval->equals("ACDK_WITH_METAINFO") == true ||
          in->sval->equals("ACDK_WITH_METAINFO2") == true || 
          in->sval->equals("ACDK_WITH_MI_MEMBER") == true) 
      {
        _hasMetaInfo = true;
        if (in->sval->equals("ACDK_WITH_METAINFO") == true ||
            in->sval->equals("ACDK_WITH_METAINFO2") == true)
          _hasScriptable = true;

        in->nextToken();
        in->nextToken();
        if (in->sval->equals("(") == true)
        {
          in->nextToken();
          in->nextToken();
          //in->nextToken();
        } else
          in->pushBack();
        continue;
      }
      if (in->sval->equals("ACDK_WITH_DMIPROXY") == true) {
        _hasDmiProxy = true;
        in->nextToken();
        continue;
      }
      
      if (in->sval->equals("ACDK_WITH_METAINFO_EX") == true) {
        _hasScriptableEx = true;
        in->nextToken();
        continue;
      }
      /*      
      if (in->sval->equals("ACDK_CORBA_INTERFACE") == true) {
        _hasMetaInfo = true;
        _isCorbaInterface = true;
        in->nextToken(); // (
        in->nextToken(); // classname
        in->nextToken(); // )
        continue;
      }

      if (in->sval->equals("ACDK_CORBA_STRUCT") == true) {
         //_isCorbaStruct = true;
         in->nextToken(); // ?? why
         continue;
      }*/
      if (in->sval->equals("ACDK_METHODATTRIBUTE") == true || in->sval->equals("ACDK_FIELDATTRIBUTE") == true) 
      {
        bool isMethodAttr = false;
        if (in->sval->equals("ACDK_METHODATTRIBUTE") == true)
          isMethodAttr = true;
        in->nextToken();
        RString code = MetaCompiler::readCodeAttributeCode(in);
        acdk::tools::mc::RCodeAttribute ca = MetaCompiler::getMetaCompiler()->readParseCodeAttribute(code);
        if (ca != Nil)
        {
          if (isMethodAttr == true)
            methodAttributes.append(ca);
          else
            fieldAttributes.append(ca);
        } else {
          // ### warning
        }
        continue;
      }
      if (in->sval->startsWith("ACDK_") == true)
      {
        tk = in->nextToken();
        tk = in->nextToken(); // (
        if (tk == '(')
        {
          in->unread('(');
          RString code = MetaCompiler::readCodeAttributeCode(in);
          continue;
        } 
        else
        {
          in->unread(in->sval);
          continue;
        }
      }
      if (tk == ':')
        continue;
      if (tk == StreamTokenizer::TT_WORD) 
      {
        RString str = in->sval;
        if (str->equals("enum") == true) 
        {
          tk = in->nextToken();
          tk = in->nextToken();
          addType(TsEnum, in->sval);
          tk = skipStatementOrFunction(in);
          if (tk == StreamTokenizer::TT_EOF)
            return true;
          tk = in->nextToken(); // read ';' at the end of enum e { };
          continue;
        }

        if (str->compareTo((RString)"public:") == 0  || str->compareTo((RString)"public") == 0) {
          _currentAccess = MiPublic;
          in->nextToken();
          continue;
        }
        if (str->compareTo((RString)"private") == 0  || str->compareTo((RString)"private:") == 0) {
          _currentAccess = MiPrivate;
          in->nextToken();
          continue;
        }
        if (str->compareTo((RString)"protected:") == 0 || str->compareTo((RString)"protected") == 0) {
          _currentAccess = MiProtected;
          in->nextToken();
          continue;
        }
        if (str->equals("using") == true) {
          //log(Info, "detected using");
          tk = skipStatementOrFunction(in);
          continue;
        }
      }
      
      if (detectField(in) == true) {
        RFieldInfo fi = new FieldInfo(this, _currentAccess);
        fi->addCodeAttributes(&fieldAttributes);
        fieldAttributes.resize(0);
        if (fi->parse(in) == true)
          _fields->add((RObject)fi);
        continue;
      } else {
        
        RMethodInfo mi = new MethodInfo(this, this, _currentAccess);
        mi->addCodeAttributes(&methodAttributes);
        methodAttributes.resize(0);
        if (mi->parse(in) == true) 
        {
          _orgMethods->add((RObject)mi);
          RMethodInfoArray ma = mi->getFixedParametersMethods();
          for (int i = 0; i < ma->length(); ++i)
          {
            RMethodInfo mi = ma[i];
            if (mi->flags & MiCiAbstract)
              flags |= MiCiAbstract;
            _methods->add((RObject)mi);
            if (mi->isCreateInstance() == true)
              _hasCreator = true;
          }
        } else {
          if (mi->flags & MiCiAbstract || mi->detectPureVirtualMethod(in) == true)
            flags |= MiCiAbstract;
          tk = skipStatementOrFunction(in);
          if (tk == StreamTokenizer::TT_EOF)
            return true;
          continue;
        }
      }
    }
    if (tk == '#') {
      MetaCompiler::skipPreprocessorStatement(in);
      continue;
    }
    if (tk == '{') {
      openblocks++;
      continue;
    }
    if (tk == '}') {
      --openblocks;
      if (openblocks == 0) {
        tk = in->nextToken();
        //if (tk != ';') acdk::sql::Array will not work here.
        //  THROW_CMC(CMCException, in, "expext ';' after closing '}' of the class definition");
        break;
      }
    } 
  }
  addStandardMethods();
  setMethodAltnames();
  
  return true;
}



void 
ClassInfo::writeMIH(RPrintWriter out, IN(::acdk::io::RPrintWriter) stubout, bool inheader)
{
  if (_hasMetaInfo == false)
    return;
  /*
  out->print("\n//virtual\n");
  if (inheader == true)
    out->print("inline\n");
  out->print("::acdk::lang::RClass\n");
  out->print(name + "::getClass()\n"
"{\n"
"  ::acdk::lang::RClass tclass = GetClass();\n");
*/
  /* Classes will not be deleted, so the given instance of will also be stored longer than
     needed
    if (isInterface() == false)
    out->print("  tclass->setObjectInstance(::acdk::lang::RObject(const_cast<" + name + "*>(this)));\n");
  */
  /* out->print("  return tclass;\n" "}\n" ); */
  if (inheader == true)
    out->print("inline\n");
  {
    RPrintWriter twriter = out;
    if (stubout != Nil)
      twriter = stubout;
    twriter->print("::acdk::lang::RClass\n");
    twriter->print(name + "::GetClass()\n"
  "{\n"
  "  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());\n"
  "}\n\n"
  );
  }
  RMcConfigAttribute mcconfig = (RMcConfigAttribute)getCodeAttribute("acdk/tools/mc/McConfigAttribute");
  if (stubout == Nil)
  {
  out->print("\n//static\n");
  
  /* ============================       getInternalFields()       ==================================== */
  out->print("//virtual\n");
  if (inheader == true)
    out->print("inline\n");
  out->print("::acdk::lang::dmi::SysFields\n");
  out->print(name + "::getInternalFields()\n");
  
  

  if (isInterface() == true) {
    out->print("{\n  ::acdk::lang::dmi::SysFields _sys_fields;\n");
    out->print("  return _sys_fields;\n}\n");
  } else {
    //ACDK_FQ_SUPER_QUALIFIER(
    out->print("{\n  ::acdk::lang::dmi::SysFields _sys_fields = " + RSuperInfo(_derivides->get(0))->getSuperName() + "::getInternalFields();\n");
    bool genrateFields = true;
    if (mcconfig != Nil)
      genrateFields = mcconfig->genFields(genrateFields);
    
    RIterator it = _fields->iterator();
    while (it->hasNext() == true) 
    {
      RFieldInfo fi = RFieldInfo(it->next());
      RMcConfigAttribute mcfieldconfig = (RMcConfigAttribute)fi->getCodeAttribute("acdk/tools/mc/McConfigAttribute");
      bool genrateField = genrateFields;
      if (mcfieldconfig != Nil)
        genrateFields = mcfieldconfig->genFields(genrateField);

      if (genrateFields == true && fi->isKnownType() == true) 
      {
        out->print("  _sys_fields.push_back(::acdk::lang::dmi::SysField(& ");
        out->print(name);
        out->print("_fields_");
        out->print(fi->name + ", ");
        if (fi->isEnum() == true)
          out->print("(int*)&");
        else if (fi->isBasicType() == true)
          out->print("&");
        out->print(fi->name);
        if (fi->isObjectType() == true)
          out->print("._ref_this()");
        out->print(")); // " + fi->getOrgType() + " " + fi->name + " \n");
        //out->print("  _sys_fields.push_back(::acdk::lang::dmi::SysField(\"")->print(fi->name + "\", &" + fi->name + ")); // " + fi->type + " " + fi->name + " \n");
      }
    }
    if (isInterface() == true)
      out->print("  return _sys_fields;\n");
    else
      out->print("  return getImplFields(_sys_fields);\n");
    out->print("}\n\n");
  } 
  } // if (stubout == Nil)
  
  // 
  /* ============================       getCollectableFields()       ==================================== */
  
  //virtual void getCollectableFields(FieldReferences& fields) { } 
  if (stubout != Nil)
    out = stubout;
  if (mcconfig == Nil || ((mcconfig->attributes & McConfOwnCollectableFields) == 0))
  //if ((flags & MiMcOwnCollectableFields) == 0)
  {
    out->print("//virtual\n");
    if (inheader == true)
      out->print("inline\n");
    out->print("void\n" + name + "::getCollectableFields(FieldReferences& fields)\n");
    if (isInterface() == true) {
      out->print("{\n}\n\n");
      return;
    }
    
    out->print("{\n  " + RSuperInfo(_derivides->get(0))->getSuperName() + "::getCollectableFields(fields);\n");
    RIterator it = _fields->iterator();
    while (it->hasNext() == true) 
    {
      RFieldInfo fi = RFieldInfo(it->next());
      if (fi->isObject() == true && fi->isstatic == false) 
      {
        out->print("  fields.push_back((::acdk::lang::RObject*)this->" + fi->name + "._ref_this()); // " + fi->getOrgType() + " " + fi->name + " \n");
      }
    }
    out->print("}\n\n");
  } // if ((flags & MiMcOwnCollectableFields) == 0)
}

void 
ClassInfo::dump(IN(RPrintWriter) out, IN(RString) ind)
{
// printing namespace
  if (_namespace->size() == 0) {
    out->println(ind + "namespace: ::");
  } else {
    out->print(ind + "namespace: ");
    RIterator it = _namespace->iterator();
    while (it->hasNext() == true) {
      RString s = RString(it->next());
      out->print(RString("::") + s);
    }
    out->println("");
  }

// RString name;
  if (isInterface() == true)
    out->println(ind + "interface: " + name);
  else
    out->println(ind + "class: " + name);

// RArrayList _derivided;
  if (_derivides->size() > 0) {
    out->println(ind + "deriveded:");
    RIterator it = _derivides->iterator();
    while (it->hasNext() == true) {
      RString name = RSuperInfo(it->next())->name;
      out->println(ind + " " + name);
    }
  }

// RArrayList _fields;
  if (isInterface() == true)
    return;
  out->println(ind + "fields:");
  RIterator it = _fields->iterator();
  while (it->hasNext() == true) {
    RFieldInfo fi = RFieldInfo(it->next());
     fi->dump(out, ind + " ");
  }
  out->println(ind + "methods:");
  it = _methods->iterator();
  while (it->hasNext() == true) {
    RMethodInfo mi = (RMethodInfo)it->next();
    out->println(ind + " " + mi->_javaSignature);
  }
}

/** 
  @param name "acdk::io::Serialize"
  @return "ACDK__io__Serialize"
*/
RString
asIntentifier(RString name)
{
  return name->replace("::", "__");
}



void 
ClassInfo::writeOpenNamespace(IN(RPrintWriter) out)
{
  ModuleInfo::writeOpenNamespace(out, _namespace, _usings);
}



void 
ClassInfo::writeCloseNamespace(IN(RPrintWriter) out)
{
  ModuleInfo::writeCloseNamespace(out, _namespace);
}


void
ClassInfo::writeInterfacesInfo(IN(RPrintWriter) out)
{
  {
  RIterator ifaces = _derivides->iterator();
  while (ifaces->hasNext() == true) {
    RSuperInfo si = RSuperInfo(ifaces->next());
    out->print("::acdk::lang::dmi::ClazzSuperInfo _" + name + "_super_" + asIntentifier(si->name) + " =\n{\n");
    out->print("  "); 
    out->print(MetaInfo::flagsToString(si->flags & ~MiMcKnownType, ClazzInfoExtFlags(0), TpFtFqName | TpFtAcdkType)); 

    out->print(",\n  0, //AttributesRes\n");
    out->print("  " + si->name + "::clazzInfo()\n};\n\n");
  }
  }
  out->print("::acdk::lang::dmi::ClazzSuperInfo* _"); 
  out->print(name + "_interfaces[] =\n{\n");
  RIterator ifaces = _derivides->iterator();
  while (ifaces->hasNext() == true) {
    RSuperInfo si = RSuperInfo(ifaces->next());
    out->print("  &_" + name + "_super_" + asIntentifier(si->name) + ",\n");
  }
  out->print("  0\n};\n\n");
}


bool 
ClassInfo::checkContext(IN(RModuleInfo) module) 
{
  bool ret = true;
  if (_hasMetaInfo == false) {
    RIterator it = _fields->iterator();
    while (it->hasNext() == true) {
      RFieldInfo fi = RFieldInfo(it->next());
      if (fi->isObject() == true) {
        ACDK_LOG(Warn, "In Class " + name + " the Member: [" + 
          fi->getOrgType() + " " + fi->name + "] is an Reference, but The class itself has no Metainfo");
        ret = false;
      }
      
    } 
    return ret;
    
  }
  //### test if implemented interfaces are virtual
  //### test if super are NOT virtual
  //### test if cloneable/serializable has create_instance-function
  return ret;
}



void 
ClassInfo::writeFieldInfo(IN(RPrintWriter) out)
{
  bool generateFields = true;
  RMcConfigAttribute mcconfig = (RMcConfigAttribute)getCodeAttribute("acdk/tools/mc/McConfigAttribute");
  if (mcconfig != Nil)
    generateFields = mcconfig->genFields(generateFields);
 {
    RStringBuffer loffset = new StringBuffer();
    RIterator interfaces = _derivides->iterator();
    while (interfaces->hasNext() == true) 
    {
      loffset->append(" + sizeof(" + RSuperInfo(interfaces->next())->name + ")");
    }
    
    RIterator it = _fields->iterator();
    while (it->hasNext() == true) 
    {
      RFieldInfo fi = RFieldInfo(it->next());

      RMcConfigAttribute fieldmcconfig = (RMcConfigAttribute)fi->getCodeAttribute("acdk/tools/mc/McConfigAttribute");
      bool generateField = generateFields;
      if (fieldmcconfig != Nil)
        generateField = fieldmcconfig->genFields(generateField);
      if (generateField == false)
        continue;

      out->print("::acdk::lang::dmi::ClazzFieldInfo " + name + "_fields_" +  fi->name + " = \n{\n");
      out->print("  " + fi->fieldType() + ",\n");
      out->print("  0, //AttributesRes\n");
      out->print("  \"" + fi->getDmiName() + "\", // name\n ");
      out->print("  -1, // hashCode\n ");
      out->print("  \"\", // ns\n");
      out->print("  0, // _scopeParent\n");
      out->print("  0, // _nextSibling\n");
      //out->print("  \"" + fi->getJTypeName() + "\", // org: " + fi->getOrgType() + " \n");
      //out->print("  \'"); 
      //out->print(fi->getTypeChar()); 
      //out->print("\',\n");
      out->print("  " + clazzInfoExpr(fi->getMappedType()) + ",\n");
      //out->print(RString("  0 // address") + "\n"); 
      if (MetaCompiler::externalMetaInfo == true)
      {
        RString sig = fi->getDispatchSignature(fi->flags & MiStatic);
        if (sig == Nil)
          out->print("  " + name + "_MetainfoWrapper::" + fi->getMetaInfoCIdentifier() + "_accessor, // read/write access to this fields\n");
        else
          out->print("  " + sig + ", // read/write access to this fields\n");
      }
      if (MetaInfo::isStatic(fi->flags) == false) 
      {
        //out->print("  (void*)" + loffset->toString());
        out->print("  (void*)0");
      } 
      else 
      {
        if (MetaInfo::isPublic(fi->flags) == true) 
        {
          if (fi->isEnum() == true)
            out->print("  (void*)(int*)&");
          else if (fi->isBasicType() == true)
            out->print("  (void*)&");
          else
              out->print("  (void*)");
          out->print(name + "::" + fi->name);
          if (fi->isObjectType() == true)
            out->print("._ref_this()");
        
          //out->print("  (void*)&" + name + "::" + fi->name);
        } else
          out->print("  (void*)0");
      }
      //if (MetaInfo::isStatic(fi->flags) == false)
      //  loffset->append(" + sizeof(" + fi->type + ")");
      out->print(" // address of field\n};\n\n");
    }
  }
  {
    out->print("::acdk::lang::dmi::ClazzFieldInfo* _" + name + "_fields[] = \n{\n");
    RIterator it = _fields->iterator();
    while (it->hasNext() == true) 
    {
      RFieldInfo fi = RFieldInfo(it->next());
      RMcConfigAttribute fieldmcconfig = (RMcConfigAttribute)fi->getCodeAttribute("acdk/tools/mc/McConfigAttribute");
      bool generateField = generateFields;
      if (fieldmcconfig != Nil)
        generateField = fieldmcconfig->genFields(generateField);
      if (generateField == false)
        continue;
      out->print("  &" + name + "_fields_" +  fi->name + ",\n");
    }
    out->print("  0\n};\n\n");
  }
}

void 
ClassInfo::writeMethodInfo(IN(RPrintWriter) out)
{
  bool generateMethods = true;
  generateMethods = getMcConfigAttribute()->genMethods(generateMethods);

  RIterator it = _methods->iterator();
  while (it->hasNext() == true) 
  {
    RMethodInfo mi = RMethodInfo(it->next());

    bool generateMethod = generateMethods;
    generateMethod = mi->getMcConfigAttribute()->genMethods(generateMethod);
    if (generateMethod == true)
    {
      mi->_genCode = 1;
      mi->writeInfo(out, this);
    } 
    else
      mi->_genCode = -1;
  }
  {
    out->print("::acdk::lang::dmi::ClazzMethodInfo* _" + name + "_methods[] = \n{\n");
    RIterator it = _methods->iterator();

    while (it->hasNext() == true) 
    {
      RMethodInfo mi = RMethodInfo(it->next());
      if (ClassInfo::isCompatibleType(mi->flags) == false || mi->isDestructor() == true)
        continue;

      bool generateMethod = mi->generateMetaInfo(generateMethods);
      if (generateMethod == true)
        mi->writeMethodList(out, this);
    }
    out->print("  0\n};\n\n");
  }
} 

int
ClassInfo::getCollectableMemberCount()
{
  RIterator it = _fields->iterator();
  int count = 0;
  while (it->hasNext() == true) {
    RFieldInfo fi = RFieldInfo(it->next());
    if (fi->isObject() == true)
      ++count;
  }
  return count;
}


void 
ClassInfo::sortMethodsAndFields()
{
  //RArrayList
  acdk::util::Collections::sort((acdk::util::RList)_fields);
  acdk::util::Collections::sort((acdk::util::RList)_methods);
}
/*
RString 
ClassInfo::getSerialVersionUID()
{
  for (int i = 0; i < _fields->size(); ++i)
  {
    RFieldInfo fi = (RFieldInfo)_fields->get(i);
    if ((fi->flags & MiStatic) && 
        fi->name->equals("serialVersionUID") == true)
    {
      return name + "::serialVersionUID";
    }
  }
  return "JLONG_CONSTANT(" + Long::toString(calculateClassIDHash()) + ")";
}
*/
void
ClassInfo::writeClazzInfo(IN(RPrintWriter) out, IN(::acdk::io::RPrintWriter) stubout, bool with_fieldInfo, bool stubOnly)
{
  if (stubout != Nil)
  {
    out->print("::acdk::lang::dmi::ClazzInfo* " + name + "::clazzInfo()\n{\n  static ");
  }
  out->print("::acdk::lang::dmi::ClazzInfo "); 
  if (stubout == Nil)
    out->print(name + "::"); 
  RString clazzaccessor = "::";
  out->print("_clazzInfo =\n{\n");
  out->print("  ");
  out->print(getClazzFlags());
  out->print(", // clazz-flags\n");
  out->print("  0, //AttributesRes\n");
  out->print("  \"");
  out->print(getDmiName() + "\", // name of class\n");
  out->print("  -1, // hashCode\n");
  out->print("  \"");
  out->print(getNamespaceAccessor() + "\", // the namespace\n");
  out->print("  0, // _scopeParent\n");
  out->print("  0, // _nextSibling\n");
  out->print("  0, // type\n");
  out->print("  0, // _firstChild\n");
  
  out->print("  _");
  out->print(name + "_interfaces, // pointer to Array of ClazzInfo references\n");
  out->print("  0, // count of Super / Interfaces\n");
  
  if (stubOnly == false)
    out->print("  _" + name + "_fields, // pointer to Array of fields\n");
  else
    out->print("  0, // pointer to Array of fields\n");
  out->print("  0, // count of Fields\n");
  if (stubOnly == false)
    out->print("  _" + name + "_methods, // pointer to Array of Methods\n");
  else
    out->print("  0, // pointer to Array of Methods\n");
  out->print("  0, // count of Methods\n");
  if (_hasCreator == true) {
    out->print("  &");
    out->print(name + "::create_instance, // create-function for cloning/serializing\n");
  } else {
    out->print("  0, // create-function for cloning/serializing\n");
  }
  if (stubOnly == false && _hasScriptable == true) {
    out->print("  &");
    out->print(name + clazzaccessor + "create_array, // create-function for cloning/serializing arrays\n");
    out->print("  &");
    out->print(name + clazzaccessor + "create_array_array, // create-function for cloning/serializing arrays\n");
  } else {
    out->print("  0, // create-function for cloning/serializing arrays\n");
    out->print("  0, // create-function for cloning/serializing arrays\n");
  }

  out->print("  0, // Class* thisClass; chaching instance\n");
  
  //out->print(RString("  ") + getSerialVersionUID() + ", // jlong serialVersionUID; for serialization\n");
  out->print("  0, // jlong serialVersionUID; for serialization\n");
  if (_hasScriptable == true) 
  {
    //if (stubout != Nil)
    if (MetaCompiler::externalMetaInfo == true)
    {
      RString sig = getDispatchSignature(false);
      if (sig != Nil)
        out->print("  " + sig + ", // dynamic_dispatch\n");
      else
        out->print("  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch\n");
      sig = getDispatchSignature(true);
      if (sig != Nil)
        out->print("  " + sig + ", // static_dispatch\n");
      else
        out->print("  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch\n");
    }
    else
    {
      out->print(RString("  &") + name + clazzaccessor + "StandardDispatch, // static_dispatch\n");
    }
  } 
  else 
  {
    if (stubout != Nil)
      out->print("  0, // dynamic_dispatch\n");
    out->print("  0, // static_dispatch\n");
  }
  out->print(RString(" ") + getCollectableMemberCount() + ", // count off all collectable members in this class\n");
  RString module = ModuleInfo::getNameSpace(_namespace, "_");
  out->print("  0, // member type info for arrays or typed container\n  ");
  /**
    typedef void* (*CastToInterfacePtrFunc)(Object* obj);
    void* _castToInterfacePtr(Object* obj)
    {
      return reinterpret_cast<void*>(dynamic_cast<MyClass*>(obj));
    }
    RefHolder::_setInterfacePtr(void* ptr)
    {
      _iptr = reinterpret_cast<T>(ptr);
    }
  */
  out->print("  " + name + "::_castToInterfacePtr, // cast object to interface pointer\n");
  out->print("0 // next ClazzInfo in chain\n");
  out->print("};\n");
  if (stubout != Nil)
  {
    out->print("  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);\n");
    out->print("  return &_clazzInfo;\n};\n\n");
  }
  out->print("static ::acdk::lang::dmi::RegisterClazzInfo _register_");
  out->print(name + "(" + name + clazzaccessor + "clazzInfo());\n\n");

   
}

void 
ClassInfo::writeClassInfo(IN(RPrintWriter) out, IN(::acdk::io::RPrintWriter) stubout, bool with_fieldInfo)
{
  if (_hasMetaInfo == false)
    return;
  sortMethodsAndFields();
  writeOpenNamespace(out);
  if (stubout != Nil)
    writeOpenNamespace(stubout);
  if (MetaCompiler::generateProxies == false)
  {
    if (stubout == Nil)
    {
      writeFieldInfo(out);
      if (with_fieldInfo == true)
        writeMIH(out, stubout, false);
      writeMethodInfo(out);
    }
    writeInterfacesInfo(stubout != Nil ? stubout : out);

  
    RString clazzaccessor = "::";
    if (stubout != Nil)
      clazzaccessor = "_";
  
    writeClazzInfo(stubout == Nil ? out : stubout, stubout, with_fieldInfo, stubout == Nil ? false : true);

    if (_hasScriptable == true) 
    {
      out->print("\n//static\n::acdk::lang::RObject\n");
      out->print(name + clazzaccessor + "create_array(int length)\n{\n");
      out->print("  return new ObjectArrayImpl<R" + name + ">(length);\n}\n");
    
      out->print("\n//static\n::acdk::lang::RObject\n");
      out->print(name + clazzaccessor + "create_array_array(int firstLength, int secondLength)\n{\n");
      out->print("  return Nil;//not implemented yet\n}\n"); // ### implement me
    }
  
    if (_hasScriptable == true)
      generateDispatch(out);

    if (stubout != Nil)
    {
    
      writeFieldInfo(out);
      if (with_fieldInfo == true)
        writeMIH(out, stubout, false);
      writeMethodInfo(out);
    
    }
    if (stubout != Nil)
      writeExternalMetaInfoInitializer(out, with_fieldInfo);
    writeClazzAndClassInitializer(out);
  }
  else
  {
    DmiProxyGeneratorExt dmiext;
    dmiext.generateProxy(getJTypeName(), out);
  }
  if (stubout != Nil)
    writeCloseNamespace(stubout);
  writeCloseNamespace(out);
}



void
ClassInfo::generateDispatchBody(IN(RPrintWriter) out, bool statics)
{
  // ## TODO cases MetaCompiler::externalMetaInfo == true will not be used, delete them
  /*
  bool generateMethods = true;
  RMcConfigAttribute mcconfig = (RMcConfigAttribute)getCodeAttribute("acdk/tools/mc/McConfigAttribute");
  if (mcconfig != Nil)
    generateMethods = mcconfig->genMethods(generateMethods);
  */
  out->print("{\n");
  if (statics == false)
  {
    if (MetaCompiler::externalMetaInfo == false)
      out->print("  " + name + "* This = this;\n");
    else
    {
      out->print("  " + name + "* This = dmi_cast<" + name + ">(This_);\n");
    }
  }

  if (statics == false)
    out->print("  {\n    bool __forward = false;\n    ::acdk::lang::Object* __forwardPtr = This->_getObjectPtr()->getDmiTarget(__forward, clazzinfo);\n"
               "    if (__forward == true)\n      return __forwardPtr->standardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);\n  }\n"
              );
  
  if (MetaCompiler::externalMetaInfo == true)
  {
    out->print(
      "  if (methinf == 0)\n"
      "  {\n"
      "     const ::acdk::lang::dmi::ClazzInfo* sicClazz_ = clazzinfo;\n"
      "     methinf =  ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags);\n"
      "     if (clazzinfo != sicClazz_)\n"
      );
    if (statics == false)
      out->print("       return clazzinfo->dynamic_dispatch(This, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);\n");
    else
      out->print("       return clazzinfo->static_dispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);\n");
    out->print(
      "  }\n"
      );
  }
  else
  {
    out->print(
      "  if (methinf == 0)\n"
      "     methinf =  ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags);\n"
      );
  }
  RIterator it = _methods->iterator();
  while (it->hasNext() == true) 
  {
    RMethodInfo mi = RMethodInfo(it->next()); 
    if (flags & MiCiAbstract && mi->isConstructor() == true)
      continue;
    if (ClassInfo::isCompatibleType(mi->flags) == false || mi->isDestructor() == true)
      continue;
    if (mi->isStatic() != statics)
      continue;

    //bool generateMethod = generateMethods;
    bool generateMethod = mi->generateMetaInfo(true);
    /*
    if (mi->flags & MiMcNoMetaInfo)
      generateMethod = false;
    RMcConfigAttribute methodmcconfig = (RMcConfigAttribute)mi->getCodeAttribute("acdk/tools/mc/McConfigAttribute");
    if (methodmcconfig != Nil)
      generateMethod = methodmcconfig->genMethods(generateMethod);
    */
    if (generateMethod == true)
    {
      mi->writeDispatchBody(out, this);
    }
  }
  it = _derivides->iterator();
  if (MetaCompiler::externalMetaInfo == false)
  {
    while (it->hasNext() == true) 
    {
      RSuperInfo si = (RSuperInfo)it->next();
      RString sname = si->name;
      if (sname->lastIndexOf(':') != -1)
        sname = sname->substr(sname->lastIndexOf(':') + 1);
      if (sname->compareTo((RString)"ObjectBase") != 0) 
      {
        out->print(RString("  if ((methinf = ") + si->getSuperName() + "::" + (statics == true ? "S" : "s") 
          + "tandardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf)) != 0)\n");
        out->print("    return methinf;\n");
      }
    }
    if (statics == false) 
    {
      out->print("  return StandardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);\n");
    }
    else
      out->print("  return 0;\n");
  } 
  
  else // MetaCompiler::externalMetaInfo 
  {
    if (statics == false)
    {
      out->print("  if ((methinf = ::acdk::lang::dmi::StdDispatch::_invoke_dynamic_super(This_, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf)) != 0)\n"
        "    return methinf;\n"
        "  return ::acdk::lang::dmi::StdDispatch::_invoke_static(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);\n"
        );
    } else
      out->print("  return ::acdk::lang::dmi::StdDispatch::_invoke_static_super(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);\n");
  }
  out->print("}\n");
}

void
ClassInfo::generateDispatchMethods(IN(RPrintWriter) out)
{
  /*
  bool generateMethods = true;
  RMcConfigAttribute mcconfig = (RMcConfigAttribute)getCodeAttribute("acdk/tools/mc/McConfigAttribute");
  if (mcconfig != Nil)
    generateMethods = mcconfig->genMethods(generateMethods);
  */
  RIterator it = _methods->iterator();
  while (it->hasNext() == true) 
  {
    RMethodInfo mi = RMethodInfo(it->next()); 
    if (flags & MiCiAbstract && mi->isConstructor() == true)
      continue;
    if (ClassInfo::isCompatibleType(mi->flags) == false || mi->isDestructor() == true)
      continue;
    /*
    bool generateMethod = generateMethods;
    if (mi->flags & MiMcNoMetaInfo)
      generateMethod = false;    
    generateMethod = mi->getMcConfigAttribute()->genMethods(generateMethod);
    */
    bool generateMethod = mi->generateMetaInfo(true);
    if (generateMethod == false)
      continue;
    out->print(
          "  static const ::acdk::lang::dmi::ClazzMethodInfo* \n  " +
          mi->getMetaInfoCIdentifier() + "_dispatch(::acdk::lang::Object* This_, " STDDSP_STRING_TYPE " fname, "
          "::acdk::lang::dmi::ScriptVar& ret, "
          "::acdk::lang::dmi::ScriptVarArray& args, "
          "::acdk::lang::dmi::DmiClient& dc, "
          "IN(::acdk::lang::RStringArray) namedArgs, "
          "int flags, "
          "const ::acdk::lang::dmi::ClazzInfo* clazzinfo, "
          "const ::acdk::lang::dmi::ClazzMethodInfo* methinf)\n  {\n");
    if (mi->isStatic() == false)
      out->print("    " + name + "* This = dmi_cast<" + name + ">(This_);\n");
     mi->writeDispatchBody(out, this);
     out->print("  }\n");
  }
}

RString getVarConverter(IN(RString) tn, IN(RString) vn, int flags, bool isEnum);

void
ClassInfo::generateFieldAccessor(IN(RPrintWriter) out)
{
  //if (isInterface() == true)
  //  return;
  RIterator it = _fields->iterator();
  while (it->hasNext() == true) 
  {
    RFieldInfo fi = RFieldInfo(it->next());
    
    out->print("  static const ::acdk::lang::dmi::ClazzFieldInfo*\n  " +
               fi->getMetaInfoCIdentifier() + "_accessor(::acdk::lang::Object* This_, "
                                      "IN(::acdk::lang::RString) fname, "
                                      "::acdk::lang::dmi::ScriptVar& var, "
                                      "::acdk::lang::dmi::DmiClient& dc, "
                                      "int flags, "
                                      "const ::acdk::lang::dmi::ClazzInfo* clazzinfo, "
                                      "const ::acdk::lang::dmi::ClazzFieldInfo* fieldinf)\n  {\n"
                                      );
    if (fi->isStatic() == false)
      out->print("    " + name + "* This = dmi_cast<" + name + ">(This_);\n");
    out->print("    if (flags & ::acdk::lang::dmi::MiReadOnly)\n"
                "     var = ");
    if (fi->isEnum() == true)
    {
      if (fi->isStatic() == false)
        out->print("::acdk::lang::getScriptVarOf(*((int*)&This->" + fi->name + "), flags);\n"); 
      else
        out->print("::acdk::lang::getScriptVarOf(*((int*)&" + name + "::" + fi->name + "), flags);\n");
    }
    else
    {
      if (fi->isStatic() == false)
        out->print("::acdk::lang::getScriptVarOf(This->" + fi->name + ", flags);\n"); 
      else
        out->print("::acdk::lang::getScriptVarOf(" + name + "::" + fi->name + ", flags);\n");
    }
    
    out->print("    else\n      ");
    if (fi->isStatic() == false)
      out->print("This->" + fi->name + " = "); 
    else
      out->print(name + "::" + fi->name + " = ");
    out->print(getVarConverter(fi->getOrgType(), "var", 0, fi->isEnum()));// ### TODO handle OutParam
    out->print(";\n    return fieldinf;\n  }\n");
  }
}

void
ClassInfo::generateProxyMethods(IN(RPrintWriter) out)
{

  RIterator it = _methods->iterator();
  while (it->hasNext() == true) 
  {
    RMethodInfo mi = RMethodInfo(it->next()); 
    if (ClassInfo::isCompatibleType(mi->flags) == false || mi->isDestructor() == true)
      continue;
    if (mi->isConstructor() == true && mi->isPrivate() == false)
    {
      mi->writeProxyConstructor(out, this);
      continue;
    }
    else if (mi->isVirtual() == true)
    {
      //mi->writeProxyMethod(out, this);
      continue;
    }
  }
  // ### TODO write virtual getDmiClient
  // ### TODO write static_dispatch to forward DMI-Proxy constructors.
}

void
ClassInfo::generateDispatch(IN(RPrintWriter) out)
{
  if (MetaCompiler::externalMetaInfo == false)
    writeCodes(out, ModuleBeforeDispatch);
  if (_hasScriptable == false )
    return;
  if (MetaCompiler::externalMetaInfo == true)
  {
    // ### TODO write forwards of ClazzMethodInfo used in generateProxyMethods
    out->print("class " + name + "_MetainfoWrapper " //" : extends " + name + 
               //", public ::acdk::lang::dmi::DmiProxyBase"
                "\n{\n");
    out->print("\npublic:\n");
    generateDispatchMethods(out);
    generateFieldAccessor(out);
    //generateProxyMethods(out);
    
    out->print("};\n\n");
    writeCodes(out, ModuleBeforeDispatch);
    return;
    /*
    out->print("struct " + name + "_MetainfoWrapper : public " + name + "\n{\n");
    out->print("static const ::acdk::lang::dmi::ClazzMethodInfo* \n");
    out->print("standardDispatch(::acdk::lang::Object* This_, " STDDSP_STRING_TYPE " fname, "
      "::acdk::lang::dmi::ScriptVar& ret, "
      "::acdk::lang::dmi::ScriptVarArray& args, "
      "::acdk::lang::dmi::DmiClient& dc, "
      "IN(::acdk::lang::RStringArray) namedArgs, "
      "int flags, "
      "const ::acdk::lang::dmi::ClazzInfo* clazzinfo, "
      "const ::acdk::lang::dmi::ClazzMethodInfo* methinf)\n");
      generateDispatchBody(out, false);
  
      out->print("static const ::acdk::lang::dmi::ClazzMethodInfo* \n");
      out->print("StandardDispatch(" STDDSP_STRING_TYPE " fname, "
      "::acdk::lang::dmi::ScriptVar& ret, "
      "::acdk::lang::dmi::ScriptVarArray& args, "
      "::acdk::lang::dmi::DmiClient& dc, "
      "IN(::acdk::lang::RStringArray) namedArgs, "
      "int flags, "
      "const ::acdk::lang::dmi::ClazzInfo* clazzinfo, "
      "const ::acdk::lang::dmi::ClazzMethodInfo* methinf)\n");
    generateDispatchBody(out, true);
    out->print("};\n\n");
    */

  } 
  else
  {
  out->print("//virtual\n");
  out->print("const ::acdk::lang::dmi::ClazzMethodInfo* \n");
  out->print(name + "::standardDispatch(" STDDSP_STRING_TYPE " fname, "
    "::acdk::lang::dmi::ScriptVar& ret, "
    "::acdk::lang::dmi::ScriptVarArray& args, "
    "::acdk::lang::dmi::DmiClient& dc, "
    "IN(::acdk::lang::RStringArray) namedArgs, "
    "int flags, "
    "const ::acdk::lang::dmi::ClazzInfo* clazzinfo, "
    "const ::acdk::lang::dmi::ClazzMethodInfo* methinf)\n");
  
  generateDispatchBody(out, false);
  
  out->print("//static\n");
  out->print("const ::acdk::lang::dmi::ClazzMethodInfo* \n");
  
  out->print(name + "::StandardDispatch(" STDDSP_STRING_TYPE " fname, "
    "::acdk::lang::dmi::ScriptVar& ret, "
    "::acdk::lang::dmi::ScriptVarArray& args, "
    "::acdk::lang::dmi::DmiClient& dc, "
    "IN(::acdk::lang::RStringArray) namedArgs, "
    "int flags, "
    "const ::acdk::lang::dmi::ClazzInfo* clazzinfo, "
    "const ::acdk::lang::dmi::ClazzMethodInfo* methinf)\n");
  generateDispatchBody(out, true);
  
  
  
  }
}

void
ClassInfo::writeClazzAndClassInitializer(IN(RPrintWriter) out)
{
  acdk::io::MemWriter mout;
  
  writeCodes(out, ModuleBeforeInit);
  RPrintWriter tout = new PrintWriter(&mout, acdk::locale::Encoding::getAsciiEncoding()->getEncoder());
  writeCodes(tout, ModuleInit);
  RbyteArray code = mout.getBuffer();
  //RString code = new String(mout.getBuffer());
  if (code->length() > 0)
  {
    RString strcode = new String((const char*)code->data(), code->length(), NormalSST | CCAscii); // ## UTF
    RString identifier = getMetaInfoCIdentifier();
    RString structidentifier = identifier->replace("::", "_")->replace("()", "") + "_ClazzAttributesResInitializer" + CodeAttribute::getCounter();
    StringBuffer sb("\nstruct ");
    sb << structidentifier
     << "\n{\n  " << structidentifier << "()\n  {\n";
    sb << strcode;
    sb << "  }\n};\n\n" << structidentifier << " " << structidentifier << "_instance;\n\n\n";
    out->print(sb.toString());
  }
  writeCodes(out, ModuleAfterDispatch);
}
void 
ClassInfo::writeExternalMetaInfoInitializer(IN(RPrintWriter) out, bool with_fieldInfo)
{
  RString structname = name + "_MetaInfoInitializer";
  out->print("\nstruct " + structname + "\n{  " + structname + "()\n  {\n");
  RString wrappername = name + "_MetainfoWrapper";

  out->print("    ::acdk::lang::dmi::ClazzInfo* ci = " + name + "::clazzInfo();\n");
  out->print("    ci->fields =  _" + name + "_fields;\n");

  out->print("    ci->methods =  _" + name + "_methods;\n");
  //out->print("    ci->dynamic_dispatch = " + wrappername + "::standardDispatch;\n");
  //out->print("    ci->static_dispatch = " + wrappername + "::StandardDispatch;\n");
  RString module = ModuleInfo::getNameSpace(_namespace, "_");
  if (MetaCompiler::getMetaCompiler()->hasUnit(module) == true)
  {
    out->print("    ci->_scopeParent = " + module + "_unitInfo.getMetaInfo();\n");
  }
  out->print("    ci->registerClazzInfo(); // make sure clazzinfo is registered\n");
  out->print("    ci->_resolveMemberParents();\n");
  out->print("    ci->flags |= ::acdk::lang::dmi::MiResolved;\n");
  out->print("  }\n};\n" + structname + " " + structname + "_staticinstance__;\n\n");
}

bool 
ClassInfo::invokeCodeAttributes(IN(RModuleInfo) cm)
{
  RIterator it = _fields->iterator();
  while (it->hasNext() == true) 
  {
    if (RFieldInfo(it->next())->invokeCodeAttributes(cm, this) == false)
      return false;
  }
  it = _methods->iterator();
  while (it->hasNext() == true) 
  {
    RMethodInfo mi(it->next());
    if (mi->invokeCodeAttributes(cm, this) == false)
      return false;
  }
  CodeInfo::invokeCodeAttributes();
  if (_isThrowable == true)
  {
    ThrowableAttribute().apply(this);
  }
  return true;
}

void 
ClassInfo::writeCodes(IN(RPrintWriter) out, CodeWhere where)
{
  // ### write code for super/methods/ fields
  
  RIterator it;
  it = _derivides->iterator();
  while (it->hasNext() == true)
  {
    RSuperInfo(it->next())->writeCode(out, where);
  }
  it = _fields->iterator();
  while (it->hasNext() == true)
  {
    RFieldInfo(it->next())->writeCode(out, where);
  }
  it = _orgMethods->iterator();
  while (it->hasNext() == true)
  {
    RMethodInfo(it->next())->writeCodes(out, where);
  }
  writeCode(out, where);
}


void
getAltMethodPrefix(StringBuffer& sb, int i)
{
   if (i < 10)
    sb.append(i);
  else if (i < 26 + 10)
    sb.append(char('a' + i - 10));
  else if (i < 2 * 26 + 10)
    sb.append(char('A' + i - (10 + 26)));
  else
  {
    int next = i / (2 * 26 + 10);
    getAltMethodPrefix(sb, next);
    int rest = i % (2 * 26 + 10);
    getAltMethodPrefix(sb, rest);
  }
}

RString getAltMethodPrefix(int i)
{
  StringBuffer sb;
  getAltMethodPrefix(sb, i);
 
  sb.append("_");
  return sb.toString();
}

void 
ClassInfo::setMethodAltnames()
{
  RIterator it = _methods->iterator();
  int i = 0;
  while (it->hasNext() == true)
  {
    RMethodInfo mi(it->next()); 
    if (mi->_altName == Nil)
      mi->_altName = "_" + getAltMethodPrefix(i) + mi->name;
    ++i;
  }
  /* old implementation
  acdk::util::TreeMap methodsMap; // RString methodname -> Integer last Number
  while (it->hasNext() == true) 
  {
    RMethodInfo mi(it->next()); 
    int lci = 0;
    RInteger lc = (RInteger)methodsMap.get(&mi->name);
    if (lc != Nil)
    {
      lci = lc->intValue() + 1;
      mi->_altName = mi->name + lci;
    }
    methodsMap.put(&mi->name, new Integer(lci));
  }    
  */
}

} // namespace mc
} // namespace tools
} // namespace acdk

