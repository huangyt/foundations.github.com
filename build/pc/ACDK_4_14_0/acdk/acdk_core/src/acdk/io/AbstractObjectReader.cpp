// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" with express or implied warranty.
// Any commercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractObjectReader.cpp,v 1.31 2005/04/15 11:28:47 kommer Exp $


#include "AbstractObjectReader.h"
#include "ObjectStreamException.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace io {

//#define LOCAL_DEBUG

#ifdef LOCAL_DEBUG
#define getDout() ::acdk::lang::System::out

#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  for (int _i = 0; _i < identlevel; ++_i) \
    sb.append(" "); \
  sb << strexpr; \
  getDout()->println(sb.toString()); \
} while (false)
#else
# define DOUT(str) do { } while(false)
#endif


#define READBASARRAY(ClassName,baseType,ShortReader) \
if (elclass == ClassName ::getTYPE()) { \
    R##baseType##Array ba = new baseType##Array(length); \
    for (int i = 0; i < length; i++) { \
      ba[i] = read##ShortReader##Element(); \
    } \
    return (RObject)ba; \
} \


RObject 
AbstractObjectReader::_readBasicArray(IN(RClass) cls, int length)
{
  RClass elclass = Class::getSingeltonClass((const dmi::ClazzInfo*)cls->objectClazzInfo()->userInfo);
  READBASARRAY(Boolean, bool, Boolean);
  READBASARRAY(Character, char, Char);
  READBASARRAY(Short, short, Short);
  READBASARRAY(Integer, int, Int);
  READBASARRAY(Long, long, Long);
  READBASARRAY(Float, float, Float);
  READBASARRAY(Double, double, Double);
  
  //READBASARRAY(Byte, byte, Byte); will be handled seperatelly
  
  return Nil;
}


namespace {
  int identlevel = 0;
  struct Incrementor { Incrementor() { ++identlevel; } ~Incrementor() { --identlevel; }  };
}

#define DOUTLEVEL() Incrementor _inlevel;



//virtual 
RObject 
AbstractObjectReader::readObject()
{
  try {
    RObject obj = readObject2(Nil);
    resetLRefs();
    return obj;
  } catch (...) {
    resetLRefs();
    throw;
  }
}

//virtual 
RObject 
AbstractObjectReader::readObject2()
{
  return readObject2(Nil);
}

//virtual 
RObject 
AbstractObjectReader::readObject(IN(::acdk::lang::RClass) cls)
{
  try {
    RObject obj = readObject2(cls);
    resetLRefs();
    return obj;
  } catch (...) {
    resetLRefs();
    throw;
  }
}

//virtual 
RObject 
AbstractObjectReader::readObject2(IN(::acdk::lang::RClass) cls)
{
  if (isTagged() == true) 
    readTagStart("Object");
  RObject obj = _readObject(cls);
  if (isTagged() == true) 
    readTagEnd("Object");
  return obj;
}

//virtual 
void 
AbstractObjectReader::defaultReadObject(IN(RClass) cls, IN(RObject) obj)
{
  // todo ### if SerializedObjectDescriptor is available use it

  ::acdk::lang::dmi::SysFields members = obj->getInternalFields(::acdk::lang::dmi::MiNonStatic | ::acdk::lang::dmi::MiIvDeclared, cls->objectClazzInfo());
  int membersize = members.size();
  DOUT("RO: " << cls->toString() << "membersize=" << membersize);
  for (int i = 0; i < membersize; i++) 
  {
    ::acdk::lang::dmi::SysField& f = members[i];
    if (f.fieldInfo->getMetaInfo()->isStatic() == true ||
        f.fieldInfo->isTransient() == true)
      continue;
    DOUTLEVEL();
    DOUT(RString(f.fieldInfo->name) << " type: " << f.type);
    switch (f.type) {
    case ::acdk::lang::dmi::SysField::FT_Void: // nothing
        break;
    case ::acdk::lang::dmi::SysField::FT_Bool: *f.cont.bval = readBooleanElement(); break;
    case ::acdk::lang::dmi::SysField::FT_Char : *f.cont.cval = readCharElement();  break;
    case ::acdk::lang::dmi::SysField::FT_UcChar : *f.cont.ucval = readUcCharElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Byte : *f.cont.cval = readCharElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Short : *f.cont.sval= readShortElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Int :  *f.cont.ival = readIntElement();  break;
    case ::acdk::lang::dmi::SysField::FT_JLong : *f.cont.jlval = readLongElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Float : *f.cont.fval = readFloatElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Double : *f.cont.dval = readDoubleElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Object : 
    {
      RObject fo = readObject2();
      if (f.fieldInfo->accessor != 0)
      {
        dmi::AcdkDmiClient dc;
        dmi::ScriptVar sv = inOf(fo);
        f.fieldInfo->accessor(obj, Nil, sv, dc, 0, 0, 0);
      }
      else
      {
        f.set(fo);
      }
      break;
    }
    }
  }
}

RClass 
AbstractObjectReader::readClassDescriptor(IN(RClass) cls)
{
  if (isNamed() == true)
    return readClassId();
  return cls;
}

void 
AbstractObjectReader::readObjectHierarchy(IN(RClass) cls, IN(RObject) obj)
{
  RClassArray supers = cls->getSuperClasses();
  for (int i = 1; i < supers->length(); ++i)
  {
    RClass cclass = supers[i];
    if (cclass->hasReadObject() == true) 
    {
      obj->readObject(this, cclass);
    }
    else
    {
      defaultReadObject(cclass, obj);
    }
  }
}

//virtual 
RObject 
AbstractObjectReader::_readObject(IN(::acdk::lang::RClass) theClass)
{
  RClass cls = theClass;
  int lref = -1;
  if (isReduced() == true) {
    lref = readObjectLocalId();
    if (lref > _lrefs.length())
    {
      THROW1(ObjectStreamException, "Inkonsitent ObjectStream (ObjectRefID is out of range)");
    }
    if (lref < _lrefs.length()) {
      DOUT("RRO: " <<  (_lrefs[lref] == Nil ? RString("<Nil>") : _lrefs[lref]->getClass()->toString()) << " lref=" << lref);

      return _lrefs[lref];
    }
  }
  if (isNamed() == false && cls == Nil)
      THROW1(ObjectStreamException, "Cannot read Object without given Class");
  
  cls = readClassDescriptor(cls);
   

  if (cls == Nil) {
    _lrefs->append(RObject(Nil));
    DOUT("RO: " << "<NilObject>" << " lref=" << lref);
    return Nil;
  }
  DOUT("RO: " << cls->toString() << " lref=" << lref);

  RObject obj = cls->newInstance();
  

  if (cls->hasReadResolve() == true)
    obj = obj->readResolve();

  
  if (instanceof(obj, String) == true) 
  {
    _lrefs->append(obj);
    return (RObject)readString();
  }
  if (instanceof(obj, byteArray) == true)
  {
    return &readOpaque();
  }
  
  if (cls->isArray() == true) 
  {
    int elcount = readIntElement();
    if (Class::getSingeltonClass((const dmi::ClazzInfo*)cls->objectClazzInfo()->userInfo)->isPrimitive() == true) {
      RObject o = _readBasicArray(cls, elcount);
      _lrefs->append(o);
      return o;
    }
    ObjectArrayBase* oar = (ObjectArrayBase*)obj.iptr();
    oar->resize(elcount);
    _lrefs->append(obj);
    for (int i = 0; i < elcount; i++) 
    {
      RObject co = readObject2();
      oar->setElement(i, co);
    }
    return obj;
  }
  _lrefs->append(obj);

  readObjectHierarchy(cls, obj);
 /*
  // ### TODO AbstractObjectReader allow read members not by field position, but by name

  //doen't, because reader doesn't expect meta info write_string(theClass->getName());
  ::acdk::lang::dmi::SysFields members = obj->getInternalFields(::acdk::lang::dmi::MiNonStatic);
  int membersize = members.size();
  DOUT("RO: " << cls->toString() << "membersize=" << membersize);
  for (int i = 0; i < membersize; i++) 
  {
    ::acdk::lang::dmi::SysField& f = members[i];
    if (f.fieldInfo->getMetaInfo()->isStatic() == true ||
        f.fieldInfo->isTransient() == true)
      continue;
    DOUTLEVEL();
    DOUT(RString(f.fieldInfo->name) << " type: " << f.type);
    switch (f.type) {
    case ::acdk::lang::dmi::SysField::FT_Void: // nothing
        break;
    case ::acdk::lang::dmi::SysField::FT_Bool: *f.cont.bval = readBooleanElement(); break;
    case ::acdk::lang::dmi::SysField::FT_Char : *f.cont.cval = readCharElement();  break;
    case ::acdk::lang::dmi::SysField::FT_UcChar : *f.cont.ucval = readUcCharElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Byte : *f.cont.cval = readCharElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Short : *f.cont.sval= readShortElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Int :  *f.cont.ival = readIntElement();  break;
    case ::acdk::lang::dmi::SysField::FT_JLong : *f.cont.jlval = readLongElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Float : *f.cont.fval = readFloatElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Double : *f.cont.dval = readDoubleElement();  break;
    case ::acdk::lang::dmi::SysField::FT_Object : 
      *f.set(readObject2());
      break;
    }
  }
  */
  return obj;
}

//foreign virtual 
RString 
AbstractObjectReader::readString()
{
  if (joinStrings() == false)
    return readStringImpl();
  int lref = readObjectLocalId();
  /*
  for (int i = 0; i < _stringCache->length(); ++i)
  {
      DOUT("LSREF: " << i << " " << _stringCache[i]);
  }*/
  if (lref < _stringCache->length())
  {
    //Integer key(lref);
    //RString val = (RString)_stringCache->get(&key);
    RString val = _stringCache[lref];
    DOUT("CS: " << val << " sref=" << lref);
    return val;
  }
  if (lref != _stringCache->length())
    THROW1(StreamCorruptedException, "Inkonsitent ObjectStream (StringRefID is out of range)");

   RString sret = readStringImpl();
   _stringCache->append(sret);
   sret = Nil;
   sret = _stringCache[_stringCache->length() - 1];
  return sret;
}
  
using acdk::lang::dmi::ScriptVar;

//virtual 
acdk::lang::dmi::ScriptVar 
AbstractObjectReader::readScriptVar(bool withTypeInfo, bool withFlags)
{
  if (withTypeInfo == false && isTagged() == true)
    THROW1(StreamCorruptedException, "Cannot read ScriptVar from untagged String");
  
  if (withTypeInfo == false)
    THROW1(StreamCorruptedException, "Untyped ScriptVar not yet supported");

  if (isTagged() == true)  
    readTagStart("ScriptVar");

  int type = readInt();
  int flags = 0;
  if (withFlags == true)
    flags = readInt();
  
  ScriptVar erg = _readScriptVar(type, flags);
  if (isTagged() == true)  
    readTagStart("ScriptVar");
  DOUT("SC: type=" << type << ": " << erg.toString());
  return erg;
}

ScriptVar
AbstractObjectReader ::_readScriptVar(int type, int flags)
{
  
  switch (type) {
  case ScriptVar::BoolType : return ScriptVar(readBoolean(), flags);
  case ScriptVar::CharType : return ScriptVar(readChar(), flags);
  case ScriptVar::ByteType : return ScriptVar(readChar(), flags);
  case ScriptVar::ShortType : return ScriptVar(readShort(), flags);
  case ScriptVar::IntType : return ScriptVar(readInt(), flags);
  case ScriptVar::LongType : return ScriptVar(readLong(), flags);
  case ScriptVar::FloatType : return ScriptVar(readFloat(), flags);
  case ScriptVar::DoubleType : return ScriptVar(readDouble(), flags);
  case ScriptVar::ObjectType : return ScriptVar(readObject2(), flags);
  
  case ScriptVar::UnknownType: 
  default :
  {
    ScriptVar sv;
    sv.flags = flags;
    return sv;
  }
  }
}


} // io
} // acdk

