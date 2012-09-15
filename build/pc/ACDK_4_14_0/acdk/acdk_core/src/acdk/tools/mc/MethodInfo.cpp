
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




#include "MethodInfo.h"
#include "ArgumentInfo.h"
#include "MetaCompiler.h"
#include "EnumArgAttribute.h"

#include "SetDispatchAttribute.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace tools {
namespace mc {

using namespace acdk::lang::dmi;

MethodInfo::MethodInfo(IN(RTypeScope) parent, IN(RClassInfo) clsInfo, int accessflags)
: CodeInfo(accessflags, "", parent)
, _classInfo(clsInfo)
, args(new ArrayList())
, argcount(-1)
, _throws(new ArrayList())
, _altName(Nil)
{
  flags |= MiMcKnownType;
}

//foreign 
RObject 
MethodInfo::clone(sys::Allocator* alc)
{
  RMethodInfo ni = new (alc) MethodInfo(_parent, _classInfo, flags);
  ni->_props = _props;
  ni->name = name;
  ni->flags = flags;
  ni->returnType = returnType;
  RIterator it = args->iterator();
  ni->args = new ArrayList();
  while (it->hasNext() == true)
  {
    RObject na = it->next()->clone(alc);
    RArgumentInfo(na)->_parent = &ni;
    ni->args->add(na);
  }
  /*ni->args = (RArrayList)args->clone(alc); 
  RIterator it = ni->args->iterator();
  while (it->hasNext() == true)
  {
    RArgumentInfo(it->next())->_parent = ni;
  }
  */
  ni->argcount = argcount;
  ni->_javaSignature = _javaSignature;
  ni->_throws = _throws;
  return &ni;
}

//virtual 
RString 
MethodInfo::getMetaInfoCIdentifier()
{
   return _classInfo->name + "_method_" +  getJavaSignature(true, argcount); 
}

bool 
MethodInfo::needMethodInfo() 
{ 
  return //(flags & ClazzIsKnownType) &&
            _classInfo->needMethodInfo(); 
}


RString
getJavaSigOfTypename(IN(RString) tp)
{
  if (tp->equals("void") == true)
    return "V";
  if (tp->equals("bool") == true)
    return "Z";
  if (tp->equals("byte") == true)
    return "B";
  if (tp->equals("char") == true)
    return "C";
  if (tp->equals("ucchar") == true || tp->equals("ucchar") == true)
    return "UC";
  if (tp->equals("short") == true)
    return "S";
  if (tp->equals("int") == true)
    return "I";
  /*if (tp->equals("long") == true)
    return "L";*/
  if (tp->equals("jlong") == true)
    return "J";
  if (tp->equals("float") == true)
    return "F";
  if (tp->equals("double") == true)
    return "D";
  return "L" + tp->replace("::", "/") + ";";
}

RString 
getJavaSigOfAsCIdentifier(IN(RString) typ)
{
  return typ->replace(":", "_")
            ->replace(".", "_")
            ->replace(".", "_")
            ->replace("/", "_")
            ->replace("(", "_")
            ->replace(")", "_")
            ->replace(";", "_")
            ->replace("<", "_") // templates
            ->replace(">", "_") // templates
            ->replace(" ", "_"); // templates
}

RString
MethodInfo::getJavaSignature(bool cidentifier, int argcount)
{
  if (_javaSignature != Nil && argcount == -1) {
    return cidentifier ? getJavaSigOfAsCIdentifier(_javaSignature) : _javaSignature;
  }
  StringBuffer sig(256);
  sig.append(name); 
  sig.append("(");
  int ac = argcount;
  if (ac == -1)
    ac = args->size();
  RIterator it = args->iterator();
  while (it->hasNext() == true && ac-- != 0) 
  {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (ai->flags & MiAiByval)
      sig.append("BYVAL");
    if (ai->flags & MiAiIn)
      sig.append("IN");
    if (ai->flags & MiAiOut)
      sig.append("OUT");
    sig.append(getJavaSigOfTypename(ai->getMappedType()));
  }
  sig.append(")");
  if (returnType != Nil) {
    sig.append(getJavaSigOfTypename(returnType));
  }
  if (argcount == -1) {
    _javaSignature = sig.toString();
    return cidentifier ? getJavaSigOfAsCIdentifier(_javaSignature) : _javaSignature;
  } 
  RString tstr = sig.toString();
  return cidentifier ? getJavaSigOfAsCIdentifier(tstr) : tstr;
}

RMethodInfoArray 
MethodInfo::getFixedParametersMethods()
{
  int nodefargslength = getNoDefaultArgCount();
  int allargslength = args->size();
  if (allargslength == nodefargslength)
  {
    RMethodInfoArray mia = new MethodInfoArray(1);
    argcount = nodefargslength;
    mia[0] = this;
    flags |= MiMiOrgPoly;
    return mia;
  }
  int difmethodcount = allargslength - nodefargslength + 1;
  RMethodInfoArray mia = new MethodInfoArray(difmethodcount);
  argcount = allargslength;
  mia[0] = this;
  for (int i = 1; i < difmethodcount; ++i)
  {
    RMethodInfo ni = (RMethodInfo)clone();
    ni->argcount = nodefargslength + i - 1;
    mia[i] = ni;
  }
  flags |= MiMiOrgPoly;
  return mia;
}



//virtual 
RString 
MethodInfo::toString()
{
  if (_javaSignature == Nil)
    _javaSignature = "";
  if (name == Nil)
    name = "<unknonwn name>";
  return returnType + " " + _classInfo->name + "::" + name + "(" + _javaSignature + ")";
}

int 
MethodInfo::getNoDefaultArgCount()
{
  RIterator it = args->iterator();
  int nodefargslength = 0;
  while (it->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (ai->hasDefaultInitializer == true)
      break;
    ++nodefargslength;
  } 
  return nodefargslength;
}

bool 
MethodInfo::isCreateInstance()
{
  if ((MetaInfo::isStatic(flags) == true) && (name->equals("create_instance") == true)) 
  {
    return true;
  }
  return false;
}


bool 
MethodInfo::checkModifier(IN(RStreamTokenizer) in)
{ 
  if (in->sval->equals("oneway") == true)  
  {
    flags |= MiMiOneway;
    return true;
  }
  if (in->sval->equals("static") == true)  
  {
    flags |= MiStatic;
    return true;
  }
  if (in->sval->equals("virtual") == true)
  {
    flags |= MiMiVirtual;
    return true;
  }
  if (in->sval->equals("overwrite") == true)
  {
    flags |= MiMiVirtual;
    _localMcFlags |= McConfNoMetaInfo;
    return true;
  }
  if (in->sval->equals("inline") == true || in->sval->equals("explicit") == true)
      return true;
  return false;
}

RString 
parseOperator(IN(RStreamTokenizer) in)
{
  int tk;
  bool first = true;
  StringBuffer sb;
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) 
  {
    if (tk == StreamTokenizer::TT_WORD) // operator new() operator delete
    {
      sb.append(in->sval);
      first = false;
      continue;
    }
    if (tk == '(')
    {
      if (first == false)
      {
        in->pushBack();
        return sb.toString();
      }
      else
      {
        sb.append(char(tk));
      }
    }
    else
    {
      sb.append(char(tk));
    }
    first = false;
  }
  return "";
}

int 
getFlag(IN(RString) cargtype)
{
  int argflag = 0;
  if (cargtype->equals("IN") == true || cargtype->equals("INP") == true)
    argflag |= MiAiIn;
  else if (cargtype->equals("OUT") == true || cargtype->equals("OUTP") == true)
    argflag |= MiAiOut;
  else if (cargtype->equals("INOUT") == true || cargtype->equals("INOUTP") == true)
    argflag |= MiAiOut | MiAiIn;
  else if (cargtype->equals("READONLY") == true)
    argflag |= MiReadOnly;
  else if (cargtype->equals("BYVAL") == true || cargtype->equals("BYVALP") == true)
    argflag |= MiAiByval;
  else if (cargtype->equals("BYVALIN") == true || cargtype->equals("BYVALINP") == true)
    argflag |= MiAiByval | MiAiIn;
  else if (cargtype->equals("BYVALOUT") == true || cargtype->equals("BYVALOUTP") == true)
    argflag |= MiAiByval | MiAiOut;
  else if (cargtype->equals("BYVALINOUT") == true || cargtype->equals("BYVALINOUTP") == true)
    argflag |= MiAiByval | MiAiIn | MiAiOut;
  else if (cargtype->equals("BYREF") == true || cargtype->equals("BYREFIN") == true)
    argflag |= MiAiByref | MiAiIn;
  else if (cargtype->equals("BYREFOUT") == true)
    argflag |= MiAiByref | MiAiOut;
  else if (cargtype->equals("BYREFINOUT") == true)
    argflag |= MiAiByref | MiAiOut | MiAiIn;
  return argflag;
}


RString
readSkipExpression(IN(RStreamTokenizer) in)
{
  StringBuffer sb;
  int tk;
  int openBrackets = 0;
  do {
    tk = in->nextToken();
    if (tk == StreamTokenizer::TT_EOF)
      return sb.toString();
    if (tk == '(')
      ++openBrackets;
    else if (tk == ')')
    {
      if (openBrackets == 0)
        break;
      --openBrackets;
    }
    else if (tk == ',' && openBrackets == 0)
      break;
    sb.append(in->toCode());
  } while (true);
  in->pushBack();
  return sb.toString();
}

bool 
MethodInfo::parse(IN(RStreamTokenizer) in)
{
  int tk;
  int openbrackets = 0;
  enum Expect
  {
    Modifier,
    //ReturnValue,
    FuncName,
    OperatorOps,
    OpenArgBracket,
    ArgType,
    ArgName,
    BeginBody,
    StartExceptions,
    ExceptionsDeclarator,
    EndExceptionsDeclarator
  };
  RString cargtype;

  Expect expect = Modifier;
  RTokenStack tkstack = new TokenStack(in);
  WantWhiteSpaceOnStack _wantWS(in, true);
  bool hasDefaultInitializer = false;
  RArgumentInfo lastargument = Nil;
  int argflag = 0;
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    tkstack->push();
    if (tk == StreamTokenizer::TT_WS)
      continue;
    if (in->sval->startsWith("ACDK_") == true)
        continue;
    switch (expect) {
    case Modifier :
      if (tk == '~') 
      {
        tk = in->nextToken();
        flags |= MiMiDestructor;
        returnType = "void";
        expect = FuncName;
        continue;
      }
      if (tk == ':')
        continue;
      if (tk != StreamTokenizer::TT_WORD) {
        return false;
      }
      if (MetaCompiler::isAccessToken(in->sval)) 
        continue;
      if (checkModifier(in) == true)
         continue;
      if (isInvalidDeclStartIdentifier(in->sval) == true)
        return false;

      if (in->sval->equals(_classInfo->name) == true && (flags & MiMiDestructor) == 0) 
      {
        flags |= MiMiConstructor;
        returnType = "R" + _classInfo->name;
        name = in->sval;
        expect = OpenArgBracket;
      } 
      else   
      {
        int argflag = getFlag(in->sval);
        if (argflag != 0)
        {
          flags |= argflag;
          tk = in->nextToken();
          if (tk != '(')
            return false;
          returnType = MetaCompiler::readComponentIndentifier(in);
          tk = in->nextToken();
          if (tk != ')')
            return false;
        }
        else
        {
          in->pushBack();
          returnType = MetaCompiler::readComponentIndentifier(in);
        }
        if (checkCompatibleType(returnType) == false) 
        {
          if (needMethodInfo())
            ACDK_LOG(Note, in->getStreamPos() + ": Ignore Method because incompatible return type: [" + returnType + "]: " + toString());
          checkCompatibleType(returnType);
          flags &= ~MiMcKnownType;
        }
        else
        {
          if (hasType(returnType) == TsEnum)
            addCodeAttribute(new EnumArgAttribute(returnType));

        }
        expect = FuncName;
      }
      break;
      /*
    case ReturnValue : 
      if (in->sval->equals("oneway") == true)  {
        flags |= MiMiOneway;
        expect = ReturnValue;
        break;
      }
      in->pushBack();
      returnType = MetaCompiler::readComponentIndentifier(in);
      if (checkCompatibleType(returnType) == false) {
        log(Note, in, "Ignore Method because incompatible return type: " + toString());
        checkCompatibleType(returnType);
        flags &= ~MiMcKnownType;
      }
      expect = FuncName;
      break;
    */
    case FuncName :
    {
      if (tk != StreamTokenizer::TT_WORD)
        return false;
      name = in->sval;
      //System::out->println("Parse func name: " + name);
      if (name->equals("operator") == true)
        expect = OperatorOps;
      else
        expect = OpenArgBracket;
      break;
    }
    case OperatorOps:
    {
      in->pushBack();
      _operatorName = parseOperator(in);
      name = acdk::lang::reflect::Method::encodeOperatorToFuncName(_operatorName);
      expect = OpenArgBracket;
      break;
    }
    case OpenArgBracket:
      if (tk != '(') 
        return false;
      expect = ArgType;
      break;
    case ArgType: 
      if (tk == '=') 
      { // default initializer
        StringBuffer defsb(" =");
        if (lastargument != Nil)
          lastargument->hasDefaultInitializer = true;
        defsb.append(readSkipExpression(in));
        tk = in->nextToken();
        if (lastargument != Nil)
        {
          lastargument->defaultInitializer = defsb.toString();
          lastargument->flags |= MiAiHasDefaultInit;
        }
        if (tk == ')') 
        {
          expect = BeginBody;
          break;
        }
        expect = ArgType;
        
        break; 
      }
      hasDefaultInitializer = false;
      if (tk == ',') 
        break;
      if (tk == ')') {
        expect = BeginBody;
        break;
      }
      
      in->pushBack();
      cargtype = MetaCompiler::readComponentIndentifier(in);
      argflag = getFlag(cargtype);
      if (argflag != 0)
      {
        tk = in->nextToken();
        if (tk != '(')
          return false;
        cargtype = MetaCompiler::readComponentIndentifier(in);
        tk = in->nextToken();
        if (tk != ')')
          return false;
      }
      if (checkCompatibleType(cargtype) == false) 
      {
        if (needMethodInfo())
          ACDK_LOG(Note, in->getStreamPos() + ": Ignore Method because incompatible argument type: [" 
                              + cargtype + "] in method: " + toString());
        flags &= ~MiMcKnownType;
      }
      
      expect = ArgName;
      break;
    case ArgName :
      hasDefaultInitializer = false;
      if (tk != StreamTokenizer::TT_WORD)
        return false;
      lastargument = new ArgumentInfo(this, cargtype, in->sval, argflag);
      if (hasType(cargtype) == TsEnum)
      {
        lastargument->addCodeAttribute(new EnumArgAttribute(cargtype));
      }
      args->add((RObject)lastargument);
      expect = ArgType;
      break;
    case BeginBody :
      if (tk == StreamTokenizer::TT_WORD && in->sval->equals("const") == true)
      {
        flags |= MiReadOnly;
        break;
      }
      if (isConstructor() && tk == ':') 
      {
        MetaCompiler::skipUntilToken(in, '{');
        tk = '{';
      }
      if (tk == '=') 
      {
       flags |= MiMiAbstract; 
       do {
         tk = in->nextToken();
       } while (tk == StreamTokenizer::TT_WS);
       if (tk != StreamTokenizer::TT_NUMBER || in->nval->intValue() != 0)
         return false;
       do {
         tk = in->nextToken();
       } while (tk == StreamTokenizer::TT_WS);
       if (tk != ';')
         return false;
       flags |= MiMiAbstract;
       tkstack->flush();
       return true;
      } else if (tk == '{') {
        int bcount = 1;
        while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
          if (tk == '{')
            ++bcount;
          else if (tk == '}') {
            --bcount;
            if (bcount == 0) 
            {
              goto endOfParse;
            }
          }
        }
        return false;
      } else if (tk == ';') {
        tkstack->flush();
        return name != Nil;
      } else if (tk == StreamTokenizer::TT_WORD) {
        if (in->sval->equals("throw") ||
            in->sval->startsWith("THROWS")) 
        {
          expect = StartExceptions;
          if (in->sval->equals("throw") == true)
          {
            TokenStack tklocal(in);
            tk = in->nextToken();
            tklocal.push();
            if (tk == '(')
            {
              tk = in->nextToken();
              tklocal.push();
              if (tk == ')')
              {
                tklocal.flush();
                expect = BeginBody;
                continue;
              }
            }
            ACDK_LOG(Warn, in->getStreamPos() + ": Use the THROWS* macoros instead throw in function specifications");
          }
          break;
        } else {
          return false;
        }
      }
    case StartExceptions :
      if (tk != '(')
        return false;
      expect = ExceptionsDeclarator;
      break;
    case ExceptionsDeclarator:
      if (tk != StreamTokenizer::TT_WORD)
        return false;
      _throws->add((RObject)in->sval);
      expect = EndExceptionsDeclarator;
      break;
    case EndExceptionsDeclarator :
      if (tk == ',')
        expect = ExceptionsDeclarator;
      else if (tk == ')')
        expect = BeginBody;
      break;
    }
  }
endOfParse:
  tkstack->flush();
  return name != Nil;
}

bool 
MethodInfo::detectPureVirtualMethod(IN(RStreamTokenizer) in)
{
  int tk;
  TokenStack tkstack(in);
  WantWhiteSpaceOnStack _wantWS(in, true);
  bool attn = false;
  while ((tk = in->nextToken()) != StreamTokenizer::TT_EOF) {
    tkstack.push();
    if (tk == ';' || tk == '{')
      return false;
    if (tk == ')')
      attn = true;
    if (attn == true && tk == '=') {
      do {
        tk = in->nextToken();
        tkstack.push();
      } while (tk ==  StreamTokenizer::TT_WS);
      if (tk == StreamTokenizer::TT_NUMBER && in->nval->intValue() == 0) {
        do {
          tk = in->nextToken();
          tkstack.push();
        } while (tk ==  StreamTokenizer::TT_WS);
        if (tk == ';')
          return true;
      }
    }
  }
  return false;
}



void
MethodInfo::writeThrowDispatch(IN(RPrintWriter) out, IN(RClassInfo) clsinfo, int argcount)
{
  if (_throws->size() == 0)
    return;
  out->print("void " + clsinfo->name + "_" + getJavaSignature(true, argcount) + "_throwDispatch(IN(::acdk::lang::RThrowable) ex)\n");
  out->print("{\n");
  RIterator it = _throws->iterator();

  while (it->hasNext() == true) 
  {
    RString t = (RString)it->next();
    int idx = t->lastIndexOf("::");
    if (idx == -1)
    {
      out->print(" if (instanceof(ex, " + t->substr(1) + ") == true)\n    THROW_INSTANCE( " + t + "(ex));\n");
    }
    else
    {
      RString ns = t->substr(0, idx + 2);
      RString cls = t->substr(idx + 2);
      out->print(" if (instanceof(ex, " + ns + cls->substr(1) + ") == true)\n    THROW_INSTANCE( " + t + "(ex));\n");
    }
  }
  out->print("}\n\n");
}

void
MethodInfo::writeInfo(IN(RPrintWriter) out, IN(RClassInfo) clsinfo, int argcount)
{
  if (ClassInfo::isCompatibleType(flags) == false || isDestructor() == true)
    return;
  if (generateMetaInfo(true) == false)
    return;
  int ac = argcount;
  if (ac == -1)
    ac = args->size();
  RIterator it = args->iterator();
  RString clsname = clsinfo->name;
  while (it->hasNext() == true && ac-- > 0) 
  {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (ai->_parent != this)
      System::out->println("Ooops");
    out->print("::acdk::lang::dmi::ClazzMethodArgInfo " + clsname + "_methods_" +  getJavaSignature(true, argcount) + "_arg_" + ai->name + " = \n{\n");
    //out->print("::acdk::lang::dmi::ClazzMethodArgInfo " + ai->getMetaInfoCIdentifier() + " = \n{\n");
    
    out->print(RString("  ") + MetaInfo::flagsToString((ai->flags | MiMethodArgInfo) & ~MiMcKnownType, MethodArgInfoExtFlags(0), TpFtFqName | TpFtAcdkType) + ", \n");
    out->print("  0, //AttributesRes\n");
    out->print(RString("  \"") + ai->getDmiName() + "\", // name of arg\n");
    out->print("  -1, // hashCode\n");
    out->print("  \"\", // ns\n");
    out->print("  0, // _scopeParent\n");
    out->print("  0, // _nextSibling\n");
    out->print(RString("  ") + clazzInfoExpr(ai->getMappedType()) + " // type or arg\n");
    out->print("};\n\n");
  }
  out->print("::acdk::lang::dmi::ClazzMethodArgInfo* " + clsname + "_methods_" +  getJavaSignature(true, argcount) + "_args[] = \n{\n");
  it = args->iterator();
  ac = argcount;
  if (ac == -1)
    ac = args->size();
  while (it->hasNext() == true && ac-- > 0) 
  {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    out->print("  &" + clsname + "_methods_" +  getJavaSignature(true, argcount) + "_arg_" + ai->name + ",\n");
    //out->print("  &" + ai->getMetaInfoCIdentifier() + ",\n");
  }
  out->print("  0\n};\n\n");
  
  out->print("::acdk::lang::dmi::ClazzInfo* " + clsname + "_methods_" +  getJavaSignature(true, argcount) + "_exceptions[] =\n{\n");

  it = _throws->iterator();

  while (it->hasNext() == true) 
  {
    RString t = (RString)it->next();
    out->print("  " + t + "::clazzInfo(), \n");
  }
  out->print("  0\n};\n\n");
  
  writeThrowDispatch(out, clsinfo, argcount);

  out->print("::acdk::lang::dmi::ClazzMethodInfo " + clsname + "_method_" +  getJavaSignature(true, argcount) + " = \n{\n");
  out->print(RString("  ") + MetaInfo::flagsToString((flags | MiMethodInfo) & ~MiMcKnownType, MethodInfoExtFlags(0), TpFtFqName | TpFtAcdkType) + ",// class flags, like static, Constructor\n");
  out->print("  0, //AttributesRes\n");

  out->print(RString("  \"") + getDmiName() + "\", // name of method\n");
  out->print("  -1, // hashCode\n");

  out->print("  \"\", // ns\n");
  out->print("  0, // _scopeParent\n");
  out->print("  0, // _nextSibling\n");
  if (isConstructor() == true)
    out->print(RString("  ") + clsname + "::clazzInfo(), // return type\n");
  else
    out->print(RString("  ") + clazzInfoExpr(returnType) + ", // return type\n");

  if (_altName != Nil)
    out->print(RString("  \"") + _altName + "\", // alternative name of method\n");
  else
    out->print("  0, // alternative name of method\n");
  out->print("  -1, // altlabelHashCode\n");

  //out->print(RString("  \"") + getJavaSignature(false, argcount) + "\", //java_signature\n");
  out->print(RString("  ") + clsname + "_methods_" +  getJavaSignature(true, argcount) + "_args, // the arguments\n");
  out->print("  0, //arguments count\n");
  out->print("  " + clsname + "_methods_" +  getJavaSignature(true, argcount) + "_exceptions, // the declared exceptions\n");

  
  if (MetaCompiler::externalMetaInfo == true)
  {
    RString clssig = clsinfo->getDispatchSignature(MetaInfo::isStatic(flags));
    RString sig = getDispatchSignature(MetaInfo::isStatic(flags));
    if ((clsinfo->flags  & MiCiAbstract) != MiCiAbstract || (isConstructor() == false && (clssig == Nil || sig != Nil)))
    {
      if (sig == Nil)
        out->print("  " + clsinfo->name + "_MetainfoWrapper::" + getMetaInfoCIdentifier() + "_dispatch, // invoke this method\n");
      else
        out->print("  " + sig + ", // invoke this method\n");
    }
    else
    {
      
      if (clssig != Nil)
        out->print("  " + clssig + ", // invoke this method");
      else
      {
        //ACDK_LOG(Warn, "This method cannot be invoked directly: " + toString());
        out->print("  0, // this method cannot be invoked because abstract\n");
        /*if (MetaInfo::isStatic(flags) == true)
          out->print("  ::acdk::lang::dmi::StdDispatch::_invoke_static, // invoke this method\n");
        else
          out->print("  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // invoke this method\n");
        */
        //clsinfo->getDispatchSignature(MetaInfo::isStatic(flags));
      }
    }
  }
  if (_throws->size() > 0)
  {
    out->print("  " + clsname + "_" + getJavaSignature(true, argcount) + "_throwDispatch, // dispatch throwing exceptions\n");  
  }
  else
  {
    out->print("  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions\n");  
  }
  out->print("  0 // cached method signature hash\n");
  
  out->print("};\n\n");
}


void
MethodInfo::writeInfo(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)
{
  writeInfo(out, clsinfo, argcount);
  /*
  int allargslength = args->size();
  int nodefargslength = getNoDefaultArgCount();
  
  if (allargslength == nodefargslength) {
    writeInfo(out, clsinfo, -1);
    return;
  }
  while (allargslength >= nodefargslength) 
  {
    writeInfo(out, clsinfo, nodefargslength);
    ++nodefargslength;
  }*/
}

void
MethodInfo::writeMethodList(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)
{
  if (ClassInfo::isCompatibleType(flags) == false || (isDestructor() == true))
    return;
  out->print("  &" + clsinfo->name + "_method_" +  getJavaSignature(true, argcount) + ",\n");
}

static 
RString
getRetConverter(IN(RMethodInfo) mi)
{
  if (mi->flags & MiAiOut)
  {
    if (mi->flags & MiAiIn)
      return "inoutOf(";
    else
      return "outOf(";
  }
  return "";
}



RString 
getVarConverter(IN(RString) tn, IN(RString) vn, int flags, bool isEnum)
{
  if (MiAiOut & flags /*&& !(MiAiByVal & flags)*/)
  {
     if (isEnum == true)
       return "(" + tn + "&)::acdk::lang::dmi::castToRef<int>(" + vn + ", dc)";
    
    if (tn->equals("bool") == true)
      return "::acdk::lang::dmi::castToBoolRef(" + vn + ", dc)";// return vn + ".getBoolRef()";
    if (tn->equals("char") == true)
      return "::acdk::lang::dmi::castToCharRef(" + vn + ", dc)"; //return vn + ".getCharRef()";
    if (tn->equals("ucchar") == true || tn->equals("uc2char") == true)
      return "::acdk::lang::dmi::castToUcCharRef(" + vn + ", dc)"; //return vn + ".getUcCharRef()";
    if (tn->equals("byte") == true)
      return "::acdk::lang::dmi::castToByteRef(" + vn + ", dc)"; //return vn + ".getByteRef()";
    if (tn->equals("short") == true)
      return "::acdk::lang::dmi::castToShortRef(" + vn + ", dc)"; //return vn + ".getShortRef()";
    if (tn->equals("int") == true)
      return "::acdk::lang::dmi::castToIntRef(" + vn + ", dc)"; //return vn + ".getIntRef()";
    if (tn->equals("jlong") == true)
      return "::acdk::lang::dmi::castToLongRef(" + vn + ", dc)"; //return vn + ".getLongRef()";
    if (tn->equals("float") == true)
      return "::acdk::lang::dmi::castToFloatRef(" + vn + ", dc)"; //return vn + ".getFloatRef()";
    if (tn->equals("double") == true)
      return "::acdk::lang::dmi::castToDoubleRef(" + vn + ", dc)"; //return vn + ".getDoubleRef()";
    return "::acdk::lang::dmi::castToObjectRef< " + tn + ">(" + vn + ", dc)"; 
    //return " ::acdk::lang::getTypedObjectRef< " + tn + " >(" + vn + ")";
    
  }   

  if (isEnum == true)
    return "(" + tn + ")::acdk::lang::dmi::castTo<int>(" + vn + ", dc)";
  return "::acdk::lang::dmi::castTo< " + tn + ">(" + vn + ", dc)";
  /*
  //return "(" + tn + ")" + vn + ".getIntVar()";
  if (tn->equals("bool") == true)
    return vn + ".getBoolVar()";
  if (tn->equals("char") == true)
    return vn + ".getCharVar()";
  if (tn->equals("ucchar") == true || tn->equals("uc2char") == true)
    return vn + ".getUcCharVar()";
  if (tn->equals("byte") == true)
    return vn + ".getByteVar()";
  if (tn->equals("short") == true)
    return vn + ".getShortVar()";
  if (tn->equals("int") == true)
    return vn + ".getIntVar()";
  if (tn->equals("jlong") == true)
    return vn + ".getLongVar()";
  if (tn->equals("float") == true)
    return vn + ".getFloatVar()";
  if (tn->equals("double") == true)
    return vn + ".getDoubleVar()";
  return "(" + tn + ")" + vn + ".getObjectVar()";
  */
}


static
RString 
getArgConverter(IN(RArgumentInfo) ai, int argcount)
{
  RString tn = ai->getOrgType();
  int flags = ai->flags;
  return getVarConverter(tn, RString("args[") + argcount + "]", flags, ai->isEnum());
  /*
  if (MiAiOut & flags )
  {
    if (tn->equals("bool") == true)
      return RString("args[") + argcount + "].getBoolRef()";
    if (tn->equals("char") == true)
      return RString("args[") + argcount + "].getCharRef()";
    if (tn->equals("ucchar") == true || tn->equals("uc2char") == true)
      return RString("args[") + argcount + "].getUcCharRef()";
    if (tn->equals("byte") == true)
      return RString("args[") + argcount + "].getByteRef()";
    if (tn->equals("short") == true)
      return RString("args[") + argcount + "].getShortRef()";
    if (tn->equals("int") == true)
      return RString("args[") + argcount + "].getIntRef()";
    if (tn->equals("jlong") == true)
      return RString("args[") + argcount + "].getLongRef()";
    if (tn->equals("float") == true)
      return RString("args[") + argcount + "].getFloatRef()";
    if (tn->equals("double") == true)
      return RString("args[") + argcount + "].getDoubleRef()";
    if (ai->isEnum() == true)
      return "(" + tn + "&)args[" + argcount + "].getIntRef()";
    return " ::acdk::lang::getTypedObjectRef< " + tn + " >(" + RString("args[") + argcount + "])";
  }   

  if (ai->isEnum() == true)
    return "(" + tn + ")args[" + argcount + "].getIntVar()";
  if (tn->equals("bool") == true)
    return RString("args[") + argcount + "].getBoolVar()";
  if (tn->equals("char") == true)
    return RString("args[") + argcount + "].getCharVar()";
  if (tn->equals("ucchar") == true || tn->equals("uc2char") == true)
    return RString("args[") + argcount + "].getUcCharVar()";
  if (tn->equals("byte") == true)
    return RString("args[") + argcount + "].getByteVar()";
  if (tn->equals("short") == true)
    return RString("args[") + argcount + "].getShortVar()";
  if (tn->equals("int") == true)
    return RString("args[") + argcount + "].getIntVar()";
  if (tn->equals("jlong") == true)
    return RString("args[") + argcount + "].getLongVar()";
  if (tn->equals("float") == true)
    return RString("args[") + argcount + "].getFloatVar()";
  if (tn->equals("double") == true)
    return RString("args[") + argcount + "].getDoubleVar()";
  return "(" + tn + ")" + RString("args[") + argcount + "].getObjectVar()";
  */
}

void
MethodInfo::writeDispatchBody2(IN(RPrintWriter) out, IN(RClassInfo) clsinfo, int argcount, bool novirtual)
{
  bool hasret = false;
  
  RString nativeMethodName = name;
  if (_operatorName != Nil)
    nativeMethodName = "operator" + _operatorName; 
  if (MetaCompiler::externalMetaInfo == true)
  {
    if (isStatic() == true)
    {
      nativeMethodName = clsinfo->name + "::" + nativeMethodName;
    }
    else if (novirtual == true)
      nativeMethodName = "This->" +  clsinfo->name + "::" + nativeMethodName;
    else
    {
      nativeMethodName = "This->" + nativeMethodName;
    }
  }
  
  if (isConstructor() == true) 
  {
    if (clsinfo->isAbstract() == true)
      return;
    out->print("    ret = (::acdk::lang::RObject)new " + name + "(");
  }
  else if (returnType->equals("void") == true)
    out->print("    " + nativeMethodName + "(");
  else 
  {
    if (flags & MiAiOut)
    {
      out->print("    ret = " + getRetConverter(this) + nativeMethodName + "(");
    } 
    else if (isBasicType(mappedReturnType()) == true) 
    {
      if (hasType(returnType) == TsEnum)
        out->print("  ret = (" + returnType + ")" + nativeMethodName + "(");
      else
        out->print("  ret = " + nativeMethodName + "(");
      hasret = true;
    } else 
      out->print("    ret = (::acdk::lang::RObject)" + nativeMethodName + "(");
  }
  RIterator ait = args->iterator();
  int ac = 0;
  int argmaxcount = argcount;
  while (ait->hasNext() == true && argmaxcount > 0) {
    if (ac > 0)
      out->print(", ");
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    out->print(getArgConverter(ai, ac));
    ++ac;
    --argmaxcount;
  }
  if (flags & MiAiOut)
    out->print("));\n");
  else
    out->print(");\n");
}
void
MethodInfo::writeDispatchBody(IN(RPrintWriter) out, IN(RClassInfo) clsinfo, int argcount)
{
  if (MetaCompiler::externalMetaInfo == false)
    out->print("  if (&" + clsinfo->name + "_method_" + getJavaSignature(true, argcount) + " == methinf) {\n");
  //out->print("  if (strcmp(methinf->java_signature, \"" + mi->getJavaSignature(false) + "\") == 0) {\n");
  if (/*isVirtual() == true && */isAbstract() == false && isStatic() == false && isConstructor() == false)
  {
    out->print("    if (flags & ::acdk::lang::dmi::MiIvNoWeakBind)\n    ");
    writeDispatchBody2(out, clsinfo, argcount, true);
    out->print("    else\n    ");
    writeDispatchBody2(out, clsinfo, argcount, false);
  }
  else
    writeDispatchBody2(out, clsinfo, argcount, false);

  out->print("    return methinf;\n");
  if (MetaCompiler::externalMetaInfo == false)
    out->print("  }\n");
}


void
MethodInfo::writeDispatchBody(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)
{
  writeDispatchBody(out, clsinfo, argcount);
  /*
  int nodefargslength = getNoDefaultArgCount();
  int allargslength = args->size();
  while (allargslength >= nodefargslength) {
    writeDispatchBody(out, clsinfo, nodefargslength);
    ++nodefargslength;
  }
  */
}

void 
MethodInfo::writeProxyConstructor(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)
{
  if (argcount != args->size())
    return;

  if (flags & MiPublic)
    out->print("  public:\n  ");
  else if (flags & MiProtected)
    out->print("  protected:\n  ");
  else
    out->print("  public:\n  ");
  out->print(clsinfo->name + "_MetainfoWrapper(IN(::acdk::lang::RObject) _acdk_dmi_server_");
  RIterator ait = args->iterator();
  int ac = 0;
  while (ait->hasNext() == true) 
  {
    out->print(", ");
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    out->print(ai->toCode());
    ++ac;
  }
  out->print(", ::acdk::lang::dmi::DmiClient* _acdk_dmi_client_)\n  : ");
  out->print(clsinfo->name + "(");
  ait = args->iterator();
  ac = 0;
  while (ait->hasNext() == true) 
  {
    if (ac != 0)
      out->print(", ");
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    out->print(ai->name);
    ++ac;
  }
  out->print(")\n  , DmiProxyBase(_acdk_dmi_server_, _acdk_dmi_client_)\n  {\n  }\n");


}

void 
MethodInfo::writeProxyMethod(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)
{
  if (argcount != args->size())
    return;
  if (flags & MiPublic)
    out->print("  public:\n  ");
  else if (flags & MiProtected)
    out->print("  protected:\n  ");
  else
    out->print("  public:\n  ");

  StringBuffer sb;
  RMethodInfo mi = this;
  sb.append(mi->returnType + " " + mi->name + "(");

  RIterator ait = mi->args->iterator();
  int ac = 0;
  while (ait->hasNext() == true) 
  {
    if (ac > 0)
      sb.append(", ");
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    sb.append(ai->toCode());
    ++ac;

  }
  sb.append(")");
  if (mi->_throws->size() > 0)
  {
    sb.append(RString(" THROWS") + mi->_throws->size() + "(");
    ac = 0;
    RIterator throws = mi->_throws->iterator();
    while (throws->hasNext() == true)
    {
      RString ct = (RString)throws->next();
      if (ac > 0)
        sb.append(", ");
      sb.append(ct);
      ++ac;
    }
    sb.append(")");
  }
  
  sb.append("\n  {\n");
  sb.append("    ::acdk::lang::dmi::ScriptVar __acdk_retval;\n");
  sb.append("    ::acdk::lang::dmi::ScriptVarArray __acdk_args(");
  sb.append(mi->args->size());
  sb.append(");\n");
  ait = mi->args->iterator();
  int i = 0;
  while (ait->hasNext() == true) 
  {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    sb.append(RString("    __acdk_args[") + i + "] = ");
    if (ai->flags & MiAiOut && ai->flags & MiAiIn)
    {
      sb.append("::acdk::lang::inoutOf(" + ai->name + ");\n");
    } else if (ai->flags & MiAiOut)
      sb.append("::acdk::lang::outOf(" + ai->name + ");\n");
    else
      sb.append("::acdk::lang::inOf(" + ai->name + ");\n");
    ++i;
  }
  sb.append("    _acdk_dmi_server->standardDispatch(" + STDDSP_STRING_CASTPLS(mi->name) + ", __acdk_retval, __acdk_args, DmiProxyBase::getDmiClient(), Nil, 0, _acdk_dmi_server->getClazzInfo(), 0);\n");
  
  if (mi->returnType->equals("void") == false)
  {
      sb.append("    return ");
    if (mi->hasType(mi->returnType) == TsEnum)
    { 
      sb.append("(" + mi->returnType + ")(int)");
    } 
    else if (mi->isBasicType(mi->returnType) == false) 
    {
      sb.append("(" + mi->returnType + ")(::acdk::lang::RObject)");
    }
    sb.append("__acdk_retval;\n");
  }
  sb.append("  }\n");
  out->print(sb.toString());
}



bool 
MethodInfo::invokeCodeAttributes(IN(RModuleInfo) cm, IN(RClassInfo) ci)
{
  int argmaxcount = argcount;
  RIterator it = args->iterator();
  while (it->hasNext() == true && argmaxcount > 0) 
  {
    if (RArgumentInfo(it->next())->invokeCodeAttributes(cm, ci, this) == false)
      return false;
    --argmaxcount;
  }
  CodeInfo::invokeCodeAttributes();
  return true;
}

void 
MethodInfo::writeCodes(IN(RPrintWriter) out, CodeWhere where)
{
  if (_genCode == -1)
    return;
  if ((flags & MiMcKnownType) == 0)
    return;
  int argmaxcount = argcount;
  
  RIterator it = args->iterator();
  while (it->hasNext() == true && argmaxcount > 0) 
  {
    RArgumentInfo(it->next())->writeCode(out, where);
    --argmaxcount;
  }
  writeCode(out, where);
}

} // namespace mc
} // namespace tools
} // namespace acdk

