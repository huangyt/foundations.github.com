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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractObjectWriter.cpp,v 1.30 2005/03/14 15:06:15 kommer Exp $


#include <acdk.h>
#include <acdk/lang/reflect/Modifier.h>
#include "AbstractObjectWriter.h"
#include "ObjectStreamException.h"
#include <acdk/lang/System.h>

#include <acdk/io/PrintWriter.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/ObjectStreamException.h>

namespace acdk {
namespace io {

//#define LOCAL_DEBUG 

#ifdef LOCAL_DEBUG
RPrintWriter getDout()
{
    //static RPrintWriter _dout = new PrintWriter(new FileWriter("./AbstractObjectWriter.dout"));
    //return _dout;
  return System::out;
}

#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  for (int _i = 0; _i < identlevel; ++_i) \
    sb.append(" "); \
  sb << strexpr << "\n"; \
  getDout()->print(sb.toString()); \
} while (false)

#else 
# define DOUT(str)
#endif

namespace {
  int identlevel = 0;
  struct Incrementor { Incrementor() { ++identlevel; } ~Incrementor() { --identlevel; }  };
}

#define DOUTLEVEL() Incrementor _inlevel;


//virtual 
void AbstractObjectWriter::writeObject(IN(RObject) obj) 
{ 
  try {
    writeObject2(obj);
    resetLocalRefs();
  } catch (...) {
    throw;
  }
}

//virtual 
void AbstractObjectWriter::writeObject2(IN(RObject) obj) 
{ 
  if (obj == Nil)
    writeObject2(Nil, Nil);
  else
    writeObject2(obj->getClass(), obj);
}

void 
AbstractObjectWriter::writeObject(IN(RClass) cls, IN(RObject) obj)
{
  try {
    writeObject2(cls, obj);
    resetLocalRefs();
  } catch (...) {
    resetLocalRefs();    
    throw;
  }
}

//virtual 
void 
AbstractObjectWriter::writeObject2(IN(RClass) cls, IN(RObject) obj)
{
  if (isTagged() == true) 
    writeTagStart("Object");
  _writeObject(cls, obj);
  if (isTagged() == true)  
    writeTagEnd("Object");
   
}

int 
AbstractObjectWriter::_lookupStringRef(IN(RString) str)
{
  for (int i = 0; i < _lrefs.length(); i++)  
  {
    RObject obj = _lrefs[i];
    if (instanceof(obj, String) == true)
    {
      if (RString(obj)->equals(str) == true)
        return i;
    }
  }
  return -1;
}


bool checkSerializable(IN(RClass) cls, INOUT(RObject) obj, int _serializeFlags)
{
  if (SerializeAll & _serializeFlags)
    return true;
  bool isSerializable = cls->isSerializable();
  
  bool returnVal = true;
  if (SerializeOnlySerializable & _serializeFlags && isSerializable == false)
  {
    returnVal = false;
  }
  
  if (returnVal == false && (SerializeIgnoreNotSerializable & _serializeFlags) != 0)
  {
    obj = Nil;
    return true;
  }
  return returnVal;
}

//virtual 
void 
AbstractObjectWriter::writeClassDescriptor(IN(RClass) cls, IN(RObject) obj)
{
  if (isNamed() == true)
    writeClassId(cls);
  // write here serializedVersion if wantet
  // write here field names if requested
}

void 
AbstractObjectWriter::defaultWriteObject(IN(RClass) cls, IN(RObject) obj)
{
  //doen't, because reader doesn't expect meta info write_string(theClass->getName());
  ::acdk::lang::dmi::SysFields members = obj->getInternalFields(::acdk::lang::dmi::MiNonStatic | ::acdk::lang::dmi::MiIvDeclared, cls->objectClazzInfo());
  int membersize = members.size();
  DOUT("WO: " << cls->toString() << " membersize=" << membersize);

  for (int i = 0; i < members.size(); i++) 
  {
    ::acdk::lang::dmi::SysField& f = members[i];
    
    if (::acdk::lang::dmi::MetaInfo::isStatic(f.fieldInfo->flags) == true ||
        f.fieldInfo->isTransient() == true )
      continue;
    DOUTLEVEL();
    DOUT(RString(f.fieldInfo->name) << " type: " << f.type);
    switch (f.type) 
    {
    case ::acdk::lang::dmi::SysField::FT_Void: break; // nothing
    case ::acdk::lang::dmi::SysField::FT_Bool: writeBooleanElement(*f.cont.bval); break;
    case ::acdk::lang::dmi::SysField::FT_Char : writeCharElement(*f.cont.cval);  break;
    case ::acdk::lang::dmi::SysField::FT_UcChar : writeUcCharElement(*f.cont.ucval);  break;
    case ::acdk::lang::dmi::SysField::FT_Byte : writeCharElement(*f.cont.cval);  break;
    case ::acdk::lang::dmi::SysField::FT_Short : writeShortElement(*f.cont.sval);  break;
    case ::acdk::lang::dmi::SysField::FT_Int : writeIntElement((int)*f.cont.ival);  break;
    case ::acdk::lang::dmi::SysField::FT_JLong : writeLongElement(*f.cont.jlval);  break;
    case ::acdk::lang::dmi::SysField::FT_Float : writeFloatElement(*f.cont.fval);  break;
    case ::acdk::lang::dmi::SysField::FT_Double : writeDoubleElement(*f.cont.dval);  break;
    case ::acdk::lang::dmi::SysField::FT_Object : 
    {
      RObject cobj = f.cont.oval->impl();
      writeObject2(cobj);
      break;
    }
    }
  }
}

void 
AbstractObjectWriter::writeObjectHierarchy(IN(RClass) cls, IN(RObject) obj)
{
  RClassArray supers = cls->getSuperClasses();
  for (int i = 1; i < supers->length(); ++i)
  {
    RClass cclass = supers[i];
    if (cclass->hasWriteObject() == true) 
    {
      obj->writeObject(this, cclass);
    }
    else
    {
      defaultWriteObject(cclass, obj);
    }
  }
}

void 
AbstractObjectWriter::writeUnshared(IN(RClass) cls, IN(RObject) obj)
{
  if (isReduced() == true)
  {
     _lrefs.append(obj);
    writeObjectLocalId(_lrefs.length() - 1);
  }
  writeClassDescriptor(cls, obj);
   
  int nid = _lrefs.length() - 1;
  DOUT("WO: " <<  cls->toString() << " lref=" << nid);
  RObject object = obj;
  if (cls->hasWriteReplace() == true)
    object = object->writeReplace();

  if (instanceof(object, String) == true) 
  {
    writeString((RString)object);
    return;
  }
  if (instanceof(object, byteArray) == true)
  {
    writeOpaque(RbyteArray(object));
    return;
  }
  writeObjectHierarchy(cls, object);
  /*
  RClass curCls = cls;

  defaultWriteObject(curCls, obj);
  
    
  if (cls->hasWriteObject() == true) {
    obj->writeObject(this, cls);
    return;
  }
  if (instanceof(obj, Throwable) == true)
    RThrowable(obj)->getStackFrames();

  //doen't, because reader doesn't expect meta info write_string(theClass->getName());
  ::acdk::lang::dmi::SysFields members = obj->getInternalFields(::acdk::lang::dmi::MiNonStatic);
  int membersize = members.size();
  DOUT("WO: " << cls->toString() << " membersize=" << membersize);

  for (int i = 0; i < members.size(); i++) 
  {
    ::acdk::lang::dmi::SysField& f = members[i];
    
    if (::acdk::lang::dmi::MetaInfo::isStatic(f.fieldInfo->flags) == true ||
        f.fieldInfo->isTransient() == true )
      continue;
    DOUTLEVEL();
    DOUT(RString(f.fieldInfo->name) << " type: " << f.type);
    switch (f.type) 
    {
    case ::acdk::lang::dmi::SysField::FT_Void: break; // nothing
    case ::acdk::lang::dmi::SysField::FT_Bool: writeBooleanElement(*f.cont.bval); break;
    case ::acdk::lang::dmi::SysField::FT_Char : writeCharElement(*f.cont.cval);  break;
    case ::acdk::lang::dmi::SysField::FT_UcChar : writeUcCharElement(*f.cont.ucval);  break;
    case ::acdk::lang::dmi::SysField::FT_Byte : writeCharElement(*f.cont.cval);  break;
    case ::acdk::lang::dmi::SysField::FT_Short : writeShortElement(*f.cont.sval);  break;
    case ::acdk::lang::dmi::SysField::FT_Int : writeIntElement((int)*f.cont.ival);  break;
    case ::acdk::lang::dmi::SysField::FT_JLong : writeLongElement(*f.cont.jlval);  break;
    case ::acdk::lang::dmi::SysField::FT_Float : writeFloatElement(*f.cont.fval);  break;
    case ::acdk::lang::dmi::SysField::FT_Double : writeDoubleElement(*f.cont.dval);  break;
    case ::acdk::lang::dmi::SysField::FT_Object : 
    {
      RObject cobj = f.cont.oval->impl();
      writeObject2(cobj);
      break;
    }
    }
  }
  */
}

void
AbstractObjectWriter::_writeObject(IN(RClass) cls, IN(RObject) tobj)
{
  RObject object = tobj;
  if (cls != Nil)
  {
    RObject elementobject = object;

    RClass elclass = cls;
    while (elclass->isArray() == true)
    {
      elclass = elclass->getArrayElementClass();
    }
    if (elclass != String::GetClass())
    {
      if (checkSerializable(elclass, object, _serializeFlags) == false)
        THROW1(NotSerializableException, "Class " + cls->getName() + " is not serializable");
      
    }
  }

  if (isReduced() == true) 
  {
    for (int i = 0; i < _lrefs.length(); i++)  
    {
      if (object == _lrefs[i]) {
        writeObjectLocalId(i);
        DOUT("WRO: " << (object == Nil ? RString("<Nil>") : object->getClass()->toString()) << " lref=" << Integer::toString(i));
        return;
      }
    }
  }
  
  if (object == Nil) {
    if (isNamed() == false)
      THROW1(StreamCorruptedException, "Cannot write Nil Object in untagged stream");
    DOUT("WO: <NilObject> lref=" << _lrefs.length() - 1);
    _lrefs.append(object);
    writeObjectLocalId(_lrefs.length() - 1);
    writeClassId(Nil);
    return;
  }
  writeUnshared(cls, object);

  
 
}

using ::acdk::lang::dmi::ScriptVar;

//virtual 
void 
AbstractObjectWriter::writeScriptVar(ScriptVar& arg, bool withTypeInfo, bool withFlags)
{
  if (isTagged() == true)  
    writeTagStart("ScriptVar");
  
  if (withTypeInfo == true)
    writeInt(arg.type);
  if (withFlags == true)
    writeInt(arg.flags);
  
  DOUT("SC: type=" << arg.type << ": " << arg.toString());
  DOUTLEVEL();

  switch (arg.type) {
  case ScriptVar::BoolType : writeBoolean(arg.getBoolVar()); break;
  case ScriptVar::CharType : writeChar(arg.getCharVar()); break;
  case ScriptVar::ByteType : writeChar(arg.getByteVar()); break;
  case ScriptVar::ShortType : writeShort(arg.getShortVar()); break;
  case ScriptVar::IntType : writeInt(arg.getIntVar()); break;
  case ScriptVar::LongType : writeLong(arg.getLongVar()); break;
  case ScriptVar::FloatType : writeFloat(arg.getFloatVar()); break;
  case ScriptVar::DoubleType : writeDouble(arg.getDoubleVar()); break;
  case ScriptVar::ObjectType : writeObject2(arg.getObjectVar()); break;
  case ScriptVar::UnknownType: 
  default :
    break;
  }
  if (isTagged() == true)  
    writeTagEnd("ScriptVar");
}

//foreign  virtual 
void 
AbstractObjectWriter::writeString(IN(RString) str)
{
  if (joinStrings() == false)
  {
    writeStringImpl(str);
    return;
  }
  RObject val = _stringCache->get(&str);
  if (val != Nil)
  {
    int lint = RInteger(val)->intValue();
    writeObjectLocalId(lint);
    DOUT("CS: " << str << " sref=" << lint);
    return;
  }
  writeObjectLocalId(_maxStringId);
  DOUT("NS: " << str << " sref=" << _maxStringId);
  _stringCache->put(&str, new (allocator()) Integer(_maxStringId));
  ++_maxStringId;
  writeStringImpl(str);
}

SerializedObjectDescriptor::SerializedObjectDescriptor(IN(RClass) cls)
: name(cls->getName())
, _class(cls)
, _serializedVersion(0)
{
  const acdk::lang::dmi::ClazzInfo* ci = cls->objectClazzInfo();
  ci->loadFullClazzInfo(false, false);

  int c = ci->getFieldsCount();
  fields = new SerializedFieldDescriptorArray(0);
  for (int i = 0; i < c; ++i)
  {
    const acdk::lang::dmi::ClazzFieldInfo* fi = ci->fields[i];
    fields[i] = new SerializedFieldDescriptor(fi->name, Class::getSingeltonClass(fi->type));
  }
}

//static 
RSerializedObjectDescriptor 
SerializedObjectDescriptor::lookup(IN(RClass) cl)
{
  return new SerializedObjectDescriptor(cl);
}

RString 
SerializedObjectDescriptor::toString()
{
  return getName();
}

} // io
} // acdk


