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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/XMLObjectWriter.cpp,v 1.21 2005/03/08 18:50:42 kommer Exp $


#include <acdk.h>
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/dmi/SysFields.h>
#include <acdk/lang/reflect/Modifier.h>

#include <acdk/text/Base64.h>
#include <acdk/net/URLEncoder.h>
#include <acdk/locale/UTF8Encoding.h>

#include <acdk/util/Date.h>
#include "XMLObjectWriter.h"



namespace acdk {
namespace xml {

USING_CLASS(::acdk::net::, URLEncoder);
USING_CLASS(::acdk::io::, AbstractObjectWriter);
using ::acdk::lang::dmi::SysField;
using ::acdk::lang::reflect::Modifier;


XMLObjectWriter::XMLObjectWriter(IN(RWriter) out, int flags /*=::acdk::io::SerializeDefaultFlags*/) 
: AbstractObjectWriter(out, flags),
  //_ptr2id(new HashMap()),
  //_maxLocalRef(0),
  _doIndent(true),
  //_withFieldInfo(false),
  _indent(0)
{
}


RString getObjectName(IN(RObject) obj)
{
  if (obj == Nil)
    return "acdk/lang/Object";
  return obj->getClass()->getName();
}

//static
RString 
XMLObjectWriter::xmlencode(IN(RString) txt)
{
  return 
    txt->replace(">", "&gt;")
       ->replace("<", "&lt;")
       ->replace("&", "&amp;")
       ->replace("\"", "&quot;")
    ;
}

//static
RString 
XMLObjectWriter::xmldecode(IN(RString) txt)
{
  return 
    txt->replace("&gt;", ">")
       ->replace("&lt;", "<")
       ->replace("&amp;", "&")
       ->replace("&quot;", "\"")
    ;
}


void 
XMLObjectWriter::_dumpindend()
{
  if (_doIndent == false)
    return;
  _out->write((const byte*)"\n", 0, 1);
  for (int i = _indent; i >= 0; i--)
    _out->write((const byte*)" ", 0, 1);
}
/*
void 
XMLObjectWriter::writeObject(RObject obj)
{
  writeObject2(obj);
}
*/

/*
//virtual 
void 
XMLObjectWriter::writeObject(RClass cls, RObject obj)
{
  Object::_throwNotImplementedYet("XMLObjectWriter::writeObject(RClass cls, RObject obj)"); // ## implement
}

void 
XMLObjectWriter::writeObject2(RObject obj, RString label)
{
  _dumpindend(); 
  if (obj == Nil) {
    _out->write("<Nil/>");
    return;
  }
  RClass theClass = obj->getClass();
  
  RObject lo = _ptr2id->get(obj);
  if (lo != Nil) {
    RString msg = RString("<object lref=\"") + ((RInteger)lo)->toString() + "\"/>";
    _out->write(msg->c_str());
    return;
  }
  ++_maxLocalRef;
  RString objname = getObjectName(obj);
  
    _ptr2id->put(obj, new Integer(_maxLocalRef));
  _out->write("<object name=\"");
  _out->write(objname->c_str());

  RString msg = RString("\" lref=\"") + _maxLocalRef + "\">";
  
  _out->write(msg->c_str());
  if (objname->compareTo("acdk/lang/String") == 0) {
    _out->write(xmlencode((RString)obj)->c_str());
    _out->write("</object>");
    return;
  } else {
    ::acdk::lang::dmi::SysFields members = obj->getInternalFields();
    bool hasChild = members.size() > 0;
    ++_indent;
    for (int i = 0; i < members.size(); i++) {
      ::acdk::lang::dmi::SysField& f = members[i];

      if (Modifier::isStatic(f.fieldInfo->flags) == true)
        continue;
      if (_withFieldInfo == true) {
        RString msg = RString("<field name=\"") + f.fieldInfo->label + "\">";
        _dumpindend();
        _out->write(msg->c_str());
        ++_indent;
      }
      //_dumpindend();  _out->print("<")->print(f.fieldInfo->label)->print(">\n");
      switch (f.type) {
      case SysField::FT_Bool: _dumpindend(); writeBoolean(*f.cont.bval);  break;
      case SysField::FT_Char : _dumpindend(); writeChar(*f.cont.cval);  break;
      case SysField::FT_Byte : _dumpindend(); writeChar(*f.cont.cval);  break;
      case SysField::FT_Short : _dumpindend(); writeShort(*f.cont.sval);  break;
      case SysField::FT_Int : _dumpindend(); writeInt((int)*f.cont.ival);  break;
        //not valid any more case SysField::FT_Long : _dumpindend(); _out->print(RString("<long value=\"") + (*f.cont.lval) + "\"/>\n");  break; 
      case SysField::FT_JLong : _dumpindend(); writeLong(*f.cont.jlval);  break;
      case SysField::FT_Float : _dumpindend(); writeFloat(*f.cont.fval);  break;
      case SysField::FT_Double : _dumpindend(); writeDouble(*f.cont.dval);  break;
      case SysField::FT_Object : {
        RObject copj = f.cont.oval->impl();
        ++_indent;
        writeObject(copj);
        //dumpAnObject(copj, _indent);
        --_indent;
        break;
                                 }
        //default: _dumpindend(); _out->print(RString("<unknown value=\"") + *f.cont.jlval + "\"/>\n"); break;
      }
      if (_withFieldInfo == true) {
        --_indent;
        _dumpindend();
        _out->write("</field>");
      }
    }
  }
  --_indent;
  //_out->write("<acdkobject name=\"" + theClass->getName());
  _dumpindend(); 
  _out->write("</object>");
}
*/

inline 
void 
_writeString(IN(RWriter) out, IN(RString) str)
{
  URLEncoder().encode(out, str);
}



inline 
void _writeString(IN(RWriter) out, const char* cptr)
{
  out->write((const byte*)cptr, 0, strlen(cptr));
  //THROW1(Exception, "Not implemented yet");
}

inline 
void 
_writeString(IN(RWriter) out, const char* cptr, int len)
{
  out->write((const byte*)cptr, 0, len);
}

//virtual 
void 
XMLObjectWriter::writeBoolean(bool b)
{
  _writeString(_out, Boolean(b).toString());
}

//virtual 
void 
XMLObjectWriter::writeChar(char b)
{
  
  _writeString(_out, URLEncoder().encode(Character::toString(b)));
}

//virtual 
void 
XMLObjectWriter::writeUcChar(uc2char b)
{
  _writeString(_out, URLEncoder().encode(Character::toString(b)));
}

 
//virtual 
void 
XMLObjectWriter::writeShort(short b)
{
  _writeString(_out, Short::toString(b));
}
  
//virtual 
void 
XMLObjectWriter::writeInt(int b)
{
  _writeString(_out, Integer::toString(b));
}

//virtual 
void 
XMLObjectWriter::writeLong(jlong b)
{
  _writeString(_out, Long::toString(b));
}
 
//virtual 
void 
XMLObjectWriter::writeFloat(float b)
{
  _writeString(_out, Float::toString(b));
}
 
//virtual 
void 
XMLObjectWriter::writeDouble(double b)
{
  _writeString(_out, Double::toString(b));
}

//virtual 
void 
XMLObjectWriter::writeStringImpl(IN(RString) str)
{
  _writeString(_out, URLEncoder().encode(str));
}



//virtual 
void 
XMLObjectWriter::writeOpaque(IN(RbyteArray) array)
{
  RbyteArray wr = ::acdk::text::Base64::encode(array);
  writeInt(wr->length());
  _out->write(wr);
}

//virtual 
void 
XMLObjectWriter::write(IN(RbyteArray) array, int offset, int len)
{
  _out->write(array, offset, len);
}

//virtual 
void 
XMLObjectWriter::writeTagStart(IN(RString) key, IN(RString) value/* = Nil*/)
{
  _writeString(_out, "<", 1);
  _writeString(_out, key);
  _writeString(_out, ">", 1);
}

//virtual 
void 
XMLObjectWriter::writeTagEnd(IN(RString) key, IN(RString) value/* = Nil*/)
{
  _writeString(_out, "</", 2);
  _writeString(_out, key);
  _writeString(_out, ">" , 1);
}


//virtual 
void 
XMLObjectWriter::writeClassId(IN(::acdk::lang::RClass) cls) 
{
  if (cls == Nil)
  {
    _writeString(_out, "<class value=\"Nil\"/>");
    return;
  }
  StringBuffer sb;
  sb << "<class value=\"" << cls->getName() << "\"";
  if (withSerialVersionUID() == true)
    sb << " SVUID=\"" << cls->objectClazzInfo()->getSerialVersionUID() << "\"";
  sb << "/>";
  
  _out->write((const byte*)sb.toString()->c_str(), 0, sb.toString()->length());
}



/*
using ::acdk::lang::dmi::ScriptVar;

//virtual 
void 
XMLObjectWriter::write(::acdk::lang::dmi::ScriptVar& arg)
{
  switch (arg.type) {
  case ScriptVar::BoolType : writeBoolean(arg.getBoolVar()); break;
  case ScriptVar::CharType : writeChar(arg.getCharVar()); break;
  case ScriptVar::ByteType : writeChar(arg.getByteVar()); break;
  case ScriptVar::ShortType : writeShort(arg.getShortVar()); break;
  case ScriptVar::IntType : writeInt(arg.getIntVar()); break;
  case ScriptVar::LongType : writeLong(arg.getLongVar()); break;
  case ScriptVar::FloatType : writeFloat(arg.getFloatVar()); break;
  case ScriptVar::DoubleType : writeFloat(arg.getDoubleVar()); break;
  case ScriptVar::ObjectType : writeObject(arg.getObjectVar()); break;
  case ScriptVar::CharPtr : writeString((RString)arg.getObjectVar()); break;
  case ScriptVar::UnknownType: 
  default :
    break;
  }
}
*/

} // xml
} // acdk



