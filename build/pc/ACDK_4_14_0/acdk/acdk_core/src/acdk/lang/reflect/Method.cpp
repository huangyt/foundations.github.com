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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Method.cpp,v 1.32 2005/02/05 10:44:59 kommer Exp $


#include <acdk.h>
#include "Method.h"
#include <acdk/lang/Character.h>
//#include <acdk/lang/System.h>

#include "../ObjectArrayImpl.h"
#include "../dmi/AcdkStdWeakTypeDmiClient.h"

namespace acdk {
namespace lang {
namespace reflect {

using namespace acdk::lang;
using namespace acdk::lang::dmi;


//virtual
RClass
Method::getDeclaringClass() 
{ 
  return Class::getSingeltonClass(_theClazz); 
}
//virtual 
int
Method::getModifiers() 
{ 
  return _methodInfo->flags;
}

//virtual 
RString 
Method::getName() 
{ 
  return _methodInfo->name; 
}

RString 
Method::getAlternativeName()
{
  if (_methodInfo->altlabel != 0)
    return _methodInfo->altlabel;
  return _methodInfo->name;
}

//virtual 
RClass 
Method::getReturnType()
{
  return Class::getSingeltonClass(_methodInfo->returnType);
}

//virtual 
RClassArray 
Method::getParameterTypes()
{
  int count = _methodInfo->getArgumentCount();
  RClassArray paramsclasses = new ClassArray(count);
  for (int i = 0; i < count; i++) 
  {
    paramsclasses[i] = Class::getSingeltonClass(_methodInfo->methodArgs[i]->type);
  }
  return paramsclasses;
}

int 
Method::getParameterCount()
{
  return _methodInfo->getArgumentCount();
}

RParameterArray 
Method::getParameters()
{
  int count = _methodInfo->getArgumentCount();
  RParameterArray pa = new ParameterArray(count);
  for (int i = 0; i < count; i++) 
  {
    pa[i] = new Parameter(_theClazz, _methodInfo, _methodInfo->methodArgs[i]); 
  }
  return pa;
}
  
RParameter 
Method::getParameter(int idx)
{
  if (idx >= getParameterCount())
    THROW1(IndexOutOfBoundsException, "Method::getParameter");
  return new Parameter(_theClazz, _methodInfo, _methodInfo->methodArgs[idx]);
}
  
RParameter 
Method::getParameter(IN(RString) paramname)
{
  int i;
  int count = _methodInfo->getArgumentCount();
  
  for (i = 0; i < count; i++) 
  {
    if (paramname->equals(_methodInfo->methodArgs[i]->name) == true)
      return new Parameter(_theClazz, _methodInfo, _methodInfo->methodArgs[i]); 
  }
  return Nil;
}

//virtual 
RClassArray 
Method::getExceptionTypes()
{
  int count = _methodInfo->getExceptionsCount();
  RClassArray exs = new ClassArray(count);
  for (int i = 0; i < count; ++i)
  {
    exs[i] = Class::getSingeltonClass(_methodInfo->exceptions[i]);
  }
  return exs;
}

//virtual 
bool 
Method::equals(IN(RObject) obj)
{
  if (instanceof(obj, Method) == false)
    return false;
  RMethod other = (RMethod)obj;
  if (getName()->equals(other->getName()) == false)
    return false;
  RClassArray p = getParameterTypes();
  RClassArray op  = other->getParameterTypes();
  if (p->length() != op->length())
    return false;
  for (int i = 0; i < p->length(); i++) {
    if (p[i]->getName()->equals(op[i]->getName()) == false)
      return false;
  }
  return true;
}

//virtual 
int 
Method::hashCode()
{
  int result = 0;
  result = 31 * result + getDeclaringClass()->hashCode();
  result = 31 * result + getName()->hashCode();
  result = 31 * result + getParameterTypes()->hashCode();
  return result;
}

//virtual 
RString 
Method::toString()
{
  StringBuffer sb(100);
  MetaInfo::flagsToTypeDecl(sb, getModifiers(), TpFtAcdkType);
  sb.append(" ");
  sb.append(getReturnType()->toString());
  sb.append(" ");
  RClass cls = Class::getSingeltonClass(_theClazz);
  sb.append(cls->toString());
  sb.append(".");
  sb.append(getName());
  sb.append("(");
  RClassArray ca = Method::getParameterTypes();
  for (int i = 0; i < ca->length(); i++) {
    if (i > 0)
      sb.append(", ");
    sb.append(ca[i]->toString());
  }
  sb.append(")");
  return sb.toString();
}

//virtual 
RString 
Method::toIndentifier()
{
  StringBuffer sb(100);
  RClass cls = Class::getSingeltonClass(_theClazz);
  sb.append(cls->getName()->replace('/', '_'));
  sb.append("__");
  sb.append(getName()->replace('/', '_'));
  sb.append("__");
  RClassArray ca = Method::getParameterTypes();
  for (int i = 0; i < ca->length(); i++) {
    if (i > 0)
      sb.append("__");
    sb.append(ca[i]->getName()->replace('/', '_'));
  }
  return sb.toString();
}

//virtual 
RObject 
Method::invoke(IN(RObject) obj, IN(RObjectArray) args) THROWS3(::acdk::lang::RIllegalAccessException, 
							                                                 ::acdk::lang::RIllegalArgumentException,
							                                                 ::acdk::lang::reflect::RInvocationTargetException)
{
  dmi::ScriptVarArray sargs(args != Nil ? args->length() : 0);
  for (int i = 0; i < sargs.size(); i++) 
    sargs[i] = args[i];
  if (getModifiers() & dmi::MiStatic) {
    dmi::ScriptVar ret;
    RClass cls = getDeclaringClass();
    if (cls->objectClazzInfo()->static_dispatch == 0)
      THROW0(InvocationTargetException);
    acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient;
    const acdk::lang::dmi::ClazzInfo* ci = cls->objectClazzInfo();
    if (ci->static_dispatch(getName(), ret, sargs, 
                                                dmiclient, Nil, 
                                                MiPublic | MiStatic, 
                                                ci, 0) == 0)
      THROW0(InvocationTargetException);
    return ret.getObjectVar();
  } else {
    dmi::ScriptVar ret;
    acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient;
    if (obj->standardDispatch(getName(), ret, sargs, 
                              dmiclient, Nil, 
                              MiPublic, obj->getClazzInfo(), 0) == 0)
      THROW0(InvocationTargetException);
    return ret.getObjectVar();
  }
  return Nil;  
}


//static 
char* 
Method::encodeOperatorChar(char op)
{
  switch(op)
  {
  case '*': return "as";
  case '@': return "at";
  case '\\': return "bs";
  case '{': return "co";
  case '}': return "cc";
  case ',': return "cm";
  case '.': return "dt";
  case '=': return "eq";
  case '>': return "gt";
  case '#': return "hs";
  case '<': return "lt";
  case '!': return "nt";
  case '+': return "pl";
  case '%': return "ps";
  case '-': return "mi";
  case '/': return "sl";
  case '[': return "bo";
  case ']': return "bc";
  case ':': return "dp";
  case '|': return "vb";
  case '^': return "rf";
  case '?': return "qs";
  case '&': return "la";
  case '(': return "po";
  case ')': return "pc";
  case '~': return "tl";
  case ';': return "pg";
  case '$': return "dl";
  
  default:
    if (Character::isJavaIdentifierPart(op) == true)
    {
      static char retchar[2];
      retchar[0] = op;
      retchar[1] = 0;
      return retchar; // not thread safe
    }
    THROW1(IllegalArgumentException, RString("Unknown operator char:") + op);
  }
  return 0;
}

//static 
RString 
Method::encodeOperatorToFuncName(IN(RString) opchars)
{
  StringBuffer sb("operator");
  String::iterator it = opchars->begin();
  String::iterator end = opchars->end();
  for (; it < end; ++it)
  {
    sb.append("_");
    sb.append(encodeOperatorChar(*it));
  }
  //System::out->println("Operator to func: " + opchars + " = " + sb.toString());
  return sb.toString();
}

/*
RString 
Method::decodeFuncNameToOperator(String::iterator begin, String::iterator end)
{
  //### FIXME
  return Nil;
}
//static 
char 
Method::decodeFuncNameToOpChar(String::iterator& it, String::iterator end)
{
  if (it + 2 >= end || *it != '_')
    THROW1(IllegalArgumentException, RString("Invalid operator encoding:"));
  ++it;
  short v = *(short*)it;
  
  if (v == *(short*)"as")
    return '*';
  else if (v == *(short*)"at")
    return '@';
  else if (v == *(short*)"bs")
    return '\\';
  else if (v == *(short*)"co")
    return '{';
  else if (v == *(short*)"cc")
    return '}';
  else if (v == *(short*)"cm")
    return ',';
  else if (v == *(short*)"dt")
    return '.';
  else if (v == *(short*)"eq")
    return '=';
  else if (v == *(short*)"gt")
    return '>';
  else if (v == *(short*)"hs")
    return '#';
  else if (v == *(short*)"lt")
    return '<';
  else if (v == *(short*)"nt")
    return '!';
  else if (v == *(short*)"pl")
    return '+';
  else if (v == *(short*)"ps")
    return '%';
  else if (v == *(short*)"mi")
    return '-';
  else if (v == *(short*)"sl")
    return '/';
  else if (v == *(short*)"bo")
    return '[';
  else if (v == *(short*)"bc")
    return ']';
  else if (v == *(short*)"dp")
    return ':';
  else if (v == *(short*)"vb")
    return '|';
  else if (v == *(short*)"rf")
    return '^';
  else if (v == *(short*)"qs")
    return '?';
  else if (v == *(short*)"la")
    return '&';
  else if (v == *(short*)"po")
    return '(';
  else if (v == *(short*)"pc")
    return ')';
  else if (v == *(short*)"tl")
    return '~';
  else if (v == *(short*)"pg")
    return '?;
  else if (v == *(short*)"dl")
    return '$';
  THROW1(IllegalArgumentException, RString("Invalid operator encoding:"));
  return 0;
}




//static 
RString 
Method::decodeFuncNameToOperator(String::iterator begin, String::iterator end)
{
  StringBuffer sb;
  String::iterator it = begin;
  if (strstr(it, "operator_") == 0)
    it += strlen("operator_");
  while (it < end)
  {
    sb.append(decodeFuncNameToOpChar(it, end));
  }
  return sb.toString();
}
*/
#if defined(__BORLANDC__) //### ACDK_NEED_TEMPLATE_INSTANTIATION

static void __instantiateMissingTemplates()
{
  RInvocationTargetException ex;
  ex = new InvocationTargetException();

}

#endif 


} // reflect
} // lang
} // acdk



