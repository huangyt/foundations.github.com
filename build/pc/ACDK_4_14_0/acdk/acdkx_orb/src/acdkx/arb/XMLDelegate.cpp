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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/arb/XMLDelegate.cpp,v 1.22 2005/02/05 10:45:39 kommer Exp $


#include "XMLDelegate.h"
#include "AObjectImpl.h"
#include <acdk/io/CharArrayWriter.h>
#include <acdk/io/AbstractFilterReader.h>
#include <acdk/io/TeeWriter.h>

#include <acdk/xml/XMLTokenizer.h>
#include <acdk/xml/XMLObjectWriter.h>
#include <acdk/xml/XMLObjectReader.h>

namespace acdkx {
namespace arb {

using namespace acdk::lang::dmi;

USING_CLASS(acdk::io::, CharArrayWriter);
USING_CLASS(acdk::io::, AbstractFilterReader);
USING_CLASS(acdk::io::, FilterReader);
USING_CLASS(acdk::io::, TeeWriter);

USING_CLASS(acdk::xml::, XMLTokenizer);
USING_CLASS(acdk::xml::, XMLObjectWriter);
USING_CLASS(acdk::xml::, XMLObjectReader);

enum ETagOpen
{
  Open,
  Close, 
  Empty
};

void 
readTag(IN(RXMLTokenizer) tin, IN(RString) name, ETagOpen st)
{
  int tk = tin->nextToken();
  if (tk != XMLTokenizer::TOK_LT) 
    THROW1(Exception, "expect '<'");
  tk = tin->nextToken();
  if (st == Close) {
    if (tk != XMLTokenizer::TOK_SLASH) 
      THROW1(Exception, "expect '/'");
    tk = tin->nextToken();
  }
  if (tk != XMLTokenizer::TOK_SYMBOL) 
    THROW1(Exception, "expect Symbol");
  if (tin->element()->compareTo(name) != 0)
    THROW1(Exception, "expect tag " + name);
  tk = tin->nextToken();
  if (st == Empty) {
    if (tk != XMLTokenizer::TOK_SLASH) 
      THROW1(Exception, "expect '/'");
    tk = tin->nextToken();
  }
  if (tk != XMLTokenizer::TOK_GT) 
    THROW1(Exception, "expect '>'");
}


RString 
readTag(IN(RXMLTokenizer) tin, ETagOpen& st)
{
  st = Open;
  int tk = tin->nextToken();
  if (tk != XMLTokenizer::TOK_LT) 
    THROW1(Exception, "expect '<'");
  tk = tin->nextToken();
  if (tk == XMLTokenizer::TOK_SLASH) {
    
    st = Close;
    tk = tin->nextToken();
  }
  RString erg = tin->element();
  tk = tin->nextToken();
  if (tk == XMLTokenizer::TOK_SLASH) {
    st = Empty;
    tk = tin->nextToken();
  }
  if (tk != XMLTokenizer::TOK_GT) 
    THROW1(Exception, "expect '>'");
  return erg;
}


ScriptVar
readArg(IN(RARB) arb, IN(RXMLTokenizer) tin)
{
  ScriptVar sv = XMLObjectReader(tin).readScriptVar();
  if (sv.type == ScriptVar::ObjectType) {
    RObject obj = sv.getObjectVar();
    if (instanceof(obj, ::acdk::xml::RemoteInterface) == true) {
      sv = arb->string_to_object(::acdk::xml::RRemoteInterface(obj)->objectId());
    }
  }
  return sv;
}

void 
readArgs(IN(RARB) arb, IN(RXMLTokenizer) tin, ScriptVarArray& args, ScriptVar& ex)
{
  int tk = tin->nextToken();
  if (tk != XMLTokenizer::TOK_LT) 
    THROW1(Exception, "expect '<'");
  tk = tin->nextToken();
  if (tk != XMLTokenizer::TOK_SYMBOL) 
    THROW1(Exception, "expect Symbol");
  
  if (tin->element()->equals("exception") == true) {
    tk = tin->nextToken();
    if (tk != XMLTokenizer::TOK_GT) 
      THROW1(Exception, "expect '>'");
    ex = XMLObjectReader(tin).readScriptVar();
    readTag(tin, "exception", Close);
    return;
  } else if (tin->element()->equals("args") == false) 
    THROW1(Exception, "expect 'args' or 'exception as tag");
  tk = tin->nextToken();
  if (tk != XMLTokenizer::TOK_GT) 
    THROW1(Exception, "expect '>'");
  do {
    ETagOpen st;
    RString ntag = readTag(tin, st);
    if (st == Open && ntag->compareTo("arg") == 0) {
      args.push_back(readArg(arb, tin));
      readTag(tin, "arg", Close);
    } else if (st == Close && ntag->compareTo("args") == 0) {
      break;
    }
  } while (true);
}

RString 
readText(IN(RXMLTokenizer) tin)
{
  int tk = tin->nextToken();
  if (tk != XMLTokenizer::TOK_TEXT)
    THROW1(Exception, "expect Text");
  return tin->element();
}


void 
XMLDelegate::readInvoke(IN(RARB) arb, IN(RXMLTokenizer) tin, IN(RString) name, OUT(RString) fromObject, OUT(RString) toObject, 
                        OUT(RString) funcname, ScriptVarArray& args, ::acdk::lang::dmi::ScriptVar& _theEx)
{
  readTag(tin, name, Open);
  readTag(tin, "objectid", Open);
  
  toObject = readText(tin);
  readTag(tin, "objectid", Close);
  readTag(tin, "replyto", Open);
  fromObject = readText(tin);
  readTag(tin, "replyto", Close);
  readTag(tin, "function", Open);
  funcname = readText(tin);
  readTag(tin, "function", Close);
  readArgs(arb, tin, args, _theEx);
  readTag(tin, name, Close);
}


ACDK_DECL_CLASS(TeeReader);
class TeeReader //#### in acdk_core
: public AbstractFilterReader,
  implements FilterReader
{
  RWriter _out;
public:
  TeeReader(IN(RReader) from, IN(RWriter) to)
  : AbstractFilterReader(from),
    _out(to)
  {
  }
  virtual int read()
  {
    int i = _in->read();
    if (i == -1)
      return i;
    _out->write((unsigned char)i);
    return i;
  }
  virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1)
  {
    int erg = _in->read(buffer, offset, len);
    if (erg != 0)
      _out->write(buffer, offset, erg);
    return erg;
  }
  virtual int read(byte* buffer, int offset, int len)
  {
    int erg = _in->read(buffer, offset, len);
    if (erg != 0)
      _out->write(buffer, offset, erg);
    return erg;
  }
};


enum CallDirection
{
  InCall,
  OutCall
};

#define write_string(chars) write((const byte*)chars, 0, strlen(chars)) // ### @todo FIXME

void 
writeArg(IN(RARB) arb, IN(RXMLObjectWriter) xmlout, ScriptVar& arg, int flags)
{
  RWriter out = xmlout->getOut();
  out->write_string("<arg>");
  if (arg.type == ScriptVar::ObjectType) {
    RObject o = arg.getObjectVar();
    if (flags & MiAiByval || o->getClass() == String::GetClass()) {
      xmlout->writeObject( o);
    } else if (instanceof(o, AObjectImpl) == true) {
      out->write_string("<interface>");
      out->write_string(arb->object_to_string(o)->c_str());
      out->write_string("</interface>");
    } else {
      THROW1(Exception, "Unsupported class (not AObjectImpl):" + o->getClass()->getName());
      
    }
    
  } else {
    xmlout->writeScriptVar(arg);
  }
  out->write_string("</arg>");
}

void 
writeArgs(IN(RARB) arb, IN(RXMLObjectWriter) xmlout, ::acdk::lang::dmi::ScriptVarArray& args, 
          const dmi::ClazzMethodInfo* cmi, CallDirection cd)
{
  RWriter out = xmlout->getOut();
  out->write_string("<args>");
  int i = 0;
  int argidx = 0;
  if (cd == InCall && argidx == 0 && cmi->returnType != ::acdk::lang::dmi::ClazzInfo::getVoidClazz()) {
    writeArg(arb, xmlout, args[i], cmi->flags);
  }
  while (cmi->methodArgs[i] != 0) 
  {
    if ((cd == OutCall && cmi->methodArgs[i]->flags & MiAiIn) ||
        (cd == InCall && cmi->methodArgs[i]->flags & MiAiOut)) 
    { 
      writeArg(arb, xmlout, args[i], cmi->methodArgs[i]->flags);
      ++argidx;
    }
    i++;
  }
  out->write_string("</args>");

}

//virtual 
void 
XMLDelegate::invoke(IN(RARB) arb, IN(RObjectID) objid, 
                    const ::acdk::lang::dmi::ClazzMethodInfo* cmi, 
                    ::acdk::lang::dmi::ScriptVarArray& args, 
                    ::acdk::lang::dmi::ScriptVarArray& ergs,
                    ::acdk::lang::dmi::ScriptVar& _theEx)
{
  RConnection con = arb->connect(objid);
  RWriter wout = con->out();
  RReader win = con->in();

  RTeeReader teein = new TeeReader(&con->in(), &System::out->getWriter());
  RTeeWriter teeout = new TeeWriter(&con->out(), &System::out->getWriter());
  wout = (RWriter)teeout;
  win = (RReader)teein;
  /*RWriter wout = System::out;
  RReader win = System::in;
  */
  //RCharArrayWriter rout = new CharArrayWriter(2048);
  Writer& out = *wout.iptr();
  /*
    <invoke>
      <objectid>objid</objectid>
      <replyto>objid</replyto>
      <function>name</function>
      optional <exception>name</exception>
      <args>
        <arg><int>123</int><arg>
      </args>
    </invoke>
  */
  out.write_string("<invoke><objectid>");
  out.write_string(objid->toString()->c_str());
  out.write_string("</objectid><replyto>");
  out.write_string(arb->object_to_string((RObject)arb)->c_str());
  out.write_string("</replyto><function>");
  out.write_string(cmi->name);
  out.write_string("</function>");
    
  XMLObjectWriter xmlout(wout);
  writeArgs(arb, SR(XMLObjectWriter, xmlout), args, cmi, OutCall);

  
  out.write_string("</invoke>");
  out.flush();
  if (cmi->flags & MiMiOneway)
    return;
  /*
    <invoke-reply>
      <objectid>objid</objectid>
      <replyto>objid</replyto>
      <function>name</function>
      <args>
        <arg><int>123</int><arg>
      </args>
    </invoke-reply>
  */
  RXMLTokenizer tin = new XMLTokenizer(win);
  RString fromObject;
  RString toObject;
  RString function;
  readInvoke(arb, tin, "invoke-reply", fromObject, toObject, function, ergs, _theEx);
}




//virtual 
void 
XMLDelegate::dispatch(IN(RARB) arb, IN(::acdk::io::RReader) in, IN(::acdk::io::RWriter) outw)
{
  RWriter out = outw;
  RTeeReader teein = new TeeReader(&in, &System::out->getWriter());
  RTeeWriter teeout = new TeeWriter(&out, &System::out->getWriter());
  RXMLTokenizer tin = new XMLTokenizer(&teein);
  out = (RWriter)teeout;
  ScriptVarArray args(0);
  RString fromObject;
  RString toObject;
  RString funcname;
  ScriptVar dummyEx;
  readInvoke(arb, tin, "invoke", fromObject, toObject, funcname, args, dummyEx); // dummyEx should never readed
  RObject lobj = arb->string_to_object(toObject);
  ScriptVarArray ergs(0);
  ScriptVar ex;
  const ::acdk::lang::dmi::ClazzMethodInfo* cmi = RArbInterface(lobj)->orbDispatch(funcname->c_str(), args, ergs, ex);
  if (cmi->flags & MiMiOneway) {
    return;
  }
  out->write_string("<invoke-reply><objectid>");
  out->write_string(fromObject->c_str());
  out->write_string("</objectid><replyto>");
  out->write_string(toObject->c_str());
  out->write_string("</replyto><function>");
  out->write_string(funcname->c_str());
  out->write_string("</function>");
   
  XMLObjectWriter xmlout(out);
  if (ex.type == ScriptVar::ObjectType) {
    out->write_string("<exception>");
    xmlout.writeObject(ex.getObjectVar());
    out->write_string("</exception>");
  } else 
    writeArgs(arb, &xmlout, ergs, cmi, InCall);

  
  out->write_string("</invoke-reply>");
}


} // namespace arb 
} // namespace acdkx 


