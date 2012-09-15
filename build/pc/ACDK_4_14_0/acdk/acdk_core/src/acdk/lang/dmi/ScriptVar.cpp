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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/ScriptVar.cpp,v 1.75 2005/04/18 15:14:26 kommer Exp $



#include <acdk.h>
#include "../Boolean.h"
#include "../Character.h"
#include "../UnicodeCharacter.h"
#include "../Number.h"
#include "../Byte.h"
#include "../Short.h"
#include "../Integer.h"
#include "../Long.h"
#include "../Float.h"
#include "../Double.h"
#include "../Exception.h"
#include "../RuntimeException.h"
#include "../DmiTypeConversionException.h"
#include "../ArithmeticException.h"

#include "ScriptVarInl.h"
#include "DmiObject.h"

namespace acdk {
namespace lang {
namespace dmi {

using namespace ::acdk::lang;

RObject& ScriptVar::_getrobject() { return *reinterpret_cast<RObject*>(var.object_buf); }
const RObject& ScriptVar::_getrobject() const { return *reinterpret_cast<const RObject*>(var.object_buf); }

void
ScriptVar::_init(Object* obj)
{
  _setrobject(obj);
}

void
ScriptVar::_init(RObject* obj)
{
  ACDK_SCRIPTVAR_CLEARVAR()
  var.oref.objectRef = obj;
  var.oref.ownsObjectRef = false;
}

ScriptVar::ScriptVar(IN(RObject) obj, int fl, const ClazzInfo* ci)
: type(ObjectType)
, flags(fl)
, _clazzType(ci)
{
    _init(obj.impl());
}


ScriptVar::ScriptVar(RObject* c, int fl, const ClazzInfo* ci)
: type(ObjectRefType)
, flags(fl)
, _clazzType(ci)
{
  ACDK_SCRIPTVAR_CLEARVAR()
  var.oref.objectRef = c;
  var.oref.ownsObjectRef = false;
}

ScriptVar::ScriptVar(InterfaceBase* obj, int fl, const ClazzInfo* ci)
: type(ObjectType)
, flags(fl)
, _clazzType(ci)
{
  _setrobject(obj->_getObjectPtr());
}


ScriptVar::ScriptVar(const char* obj)
: type(ObjectType)
, flags(MiAiIn)
, _clazzType(0)
{
  _setrobject(new String(obj));
}

ScriptVar::ScriptVar(const ScriptVar& o)
: type(o.type)
, flags(o.flags)
, _clazzType(o._clazzType)
{
  _initScriptVar(o);
}

ScriptVar::ScriptVar(const ScriptVar& o, int fl, const ClazzInfo* ci)
: type(o.type)
, flags(fl)
, _clazzType(ci)
{
  if (_clazzType == 0)
    _clazzType = o._clazzType;
  _initScriptVar(o);
}

void
ScriptVar::_initScriptVar(const ScriptVar& o)
{
  switch(type) {
  case UnknownType: break;

  case BoolType : var.boolVal = o.var.boolVal; break;
  case CharType : var.charVal = o.var.charVal; break;
  case ByteType : var.byteVal = o.var.byteVal; break;
  case UcCharType: var.uccharVal = o.var.uccharVal; break;
  case ShortType : var.shortVal = o.var.shortVal; break;
  case IntType : var.intVal = o.var.intVal; break;
  case LongType : var.longVal = o.var.longVal; break;
  case FloatType : var.floatVal = o.var.floatVal; break;
  case DoubleType : var.doubleVal = o.var.doubleVal; break;
  case BoolRefType : var.boolRef = o.var.boolRef; break;
  case CharRefType : var.charRef = o.var.charRef; break;
  case UcCharRefType : var.uccharRef = o.var.uccharRef; break;
  case ByteRefType : var.byteRef = o.var.byteRef; break;
  case ShortRefType : var.shortRef = o.var.shortRef; break;
  case IntRefType : var.intRef = o.var.intRef; break;
  case LongRefType : var.longRef = o.var.longRef; break;
  case FloatRefType : var.floatRef = o.var.floatRef; break;
  case DoubleRefType : var.doubleRef = o.var.doubleRef; break;
  case ObjectType:
    _setrobject(o._getrobject());
    break;
  case ObjectRefType:
    if (o.var.oref.ownsObjectRef == true)
    {
      /// #### error
    }
    var.oref.objectRef  = o.var.oref.objectRef;
    var.oref.ownsObjectRef = false;
    break;
  }
  
}


ScriptVar::~ScriptVar()
{
  dispose();
}


ScriptVar 
ScriptVar::_castScriptVar(const ClazzInfo* ci, short castFlags) const
{
  if (ci->isBasicClazz() == true)
  {
    if (ci->isBoolClazz() == true)
      return getBoolVar(castFlags);
    if (ci->isCharClazz() == true)
      return getCharVar(castFlags);
    if (ci->isUcCharClazz() == true)
      return getUcCharVar(castFlags);
    if (ci->isByteClazz() == true)
      return getByteVar(castFlags);
    if (ci->isShortClazz() == true)
      return getShortVar(castFlags);
    if (ci->isIntClazz() == true)
      return getIntVar(castFlags);
    if (ci->isLongClazz() == true)
      return getLongVar(castFlags);
    if (ci->isFloatClazz() == true)
      return getFloatVar(castFlags);
    if (ci->isDoubleClazz() == true)
      return getDoubleVar(castFlags);
  }
  RObject obj =  getObjectVar(castFlags, ci);
  if (obj == Nil)
    return *this;
  
  RObject ret = obj->_cast(ci);
  
  if (ret != Nil)
    return ret;
  if (castFlags & SVCastUnwrapDmiObject && instanceof(obj, DmiObject) == true)
      return RDmiObject(obj)->_castScriptVar(ci, castFlags);
  

  if (castFlags & SVCastWrapDmiObject && ci == DmiObject::clazzInfo())
    return new DmiObject(*this);

  if (castFlags & SVCastEncodeString && ci == String::clazzInfo())
    return &obj->toString();
  
  badCast(ci, getObjectVar());
  return Nil;
}

void
ScriptVar::dispose()
{
  if (type == ObjectType)
  {
    _deleterobject();
  }
  else if (type == ObjectRefType && var.oref.ownsObjectRef == true)
  {
    delete var.oref.objectRef;
    var.oref.objectRef = 0;
  }
}

void
ScriptVar::reset()
{
  dispose();
  type = UnknownType;
  flags = MiAiIn;
  ACDK_SCRIPTVAR_CLEARVAR()
}


void
ScriptVar::get(RObject& b) const
{
  b = getObjectVar();
}

//static
const ClazzInfo*
ScriptVar::getClazzInfo(Type typ)
{
  switch(typ)
  {
  case ScriptVar::BoolType:
  case ScriptVar::BoolRefType:
    return ClazzInfo::getBoolClazz();
  case ScriptVar::CharType:
  case ScriptVar::CharRefType:
    return ClazzInfo::getCharClazz();
  case ScriptVar::UcCharType:
  case ScriptVar::UcCharRefType:
    return ClazzInfo::getUcCharClazz();
  case ScriptVar::ByteType:
  case ScriptVar::ByteRefType:
    return ClazzInfo::getByteClazz();
  case ScriptVar::ShortType:
  case ScriptVar::ShortRefType:
    return ClazzInfo::getShortClazz();
  case ScriptVar::IntType:
  case ScriptVar::IntRefType:
    return ClazzInfo::getIntClazz();
  case ScriptVar::LongType:
  case ScriptVar::LongRefType:
    return ClazzInfo::getLongClazz();
  case ScriptVar::FloatType:
  case ScriptVar::FloatRefType:
    return ClazzInfo::getFloatClazz();
  case ScriptVar::DoubleType:
  case ScriptVar::DoubleRefType:
    return ClazzInfo::getDoubleClazz();
  case ScriptVar::ObjectType:
  case ScriptVar::ObjectRefType:
      return Object::clazzInfo();
  case ScriptVar::UnknownType :
  default:
    return ClazzInfo::getUnknownBasicClazz();
  }
}

//static
ScriptVar
ScriptVar::getInitialized(const acdk::lang::dmi::ClazzInfo* ci)
{

  if (ci == ClazzInfo::getBoolClazz())
  {
    return ScriptVar(false, MiAiIn, ci);
  }
  if (ci == ClazzInfo::getCharClazz())
  {
    return ScriptVar(char(0), MiAiIn, ci);
  }
  if (ci == ClazzInfo::getUcCharClazz())
  {
    return ScriptVar(ucchar(0), MiAiIn, ci);
  }
  if (ci == ClazzInfo::getByteClazz())
  {
    return ScriptVar(byte(0), MiAiIn, ci);
  }
  if (ci == ClazzInfo::getShortClazz())
  {
    return ScriptVar(short(0), MiAiIn, ci);
  }
  if (ci == ClazzInfo::getIntClazz())
  {
    return ScriptVar(int(0), MiAiIn, ci);
  }
  if (ci == ClazzInfo::getLongClazz())
  {
    return ScriptVar(jlong(0), MiAiIn, ci);
  }
  if (ci == ClazzInfo::getFloatClazz())
  {
    return ScriptVar(float(0.0), MiAiIn, ci);
  }
  if (ci == ClazzInfo::getDoubleClazz())
  {
    return ScriptVar(double(0.0), MiAiIn, ci);
  }
  // otherwise
  {
    return ScriptVar(RObject(), MiAiIn, ci);
  }
}

const ClazzInfo*
ScriptVar::getValueClazzInfo() const
{
  switch(type)
  {
  case ScriptVar::BoolType:
  case ScriptVar::BoolRefType:
    return ClazzInfo::getBoolClazz();
  case ScriptVar::CharType:
  case ScriptVar::CharRefType:
    return ClazzInfo::getCharClazz();
  case ScriptVar::UcCharType:
  case ScriptVar::UcCharRefType:
    return ClazzInfo::getUcCharClazz();
  case ScriptVar::ByteType:
  case ScriptVar::ByteRefType:
    return ClazzInfo::getByteClazz();
  case ScriptVar::ShortType:
  case ScriptVar::ShortRefType:
    return ClazzInfo::getShortClazz();
  case ScriptVar::IntType:
  case ScriptVar::IntRefType:
    return ClazzInfo::getIntClazz();
  case ScriptVar::LongType:
  case ScriptVar::LongRefType:
    return ClazzInfo::getLongClazz();
  case ScriptVar::FloatType:
  case ScriptVar::FloatRefType:
    return ClazzInfo::getFloatClazz();
  case ScriptVar::DoubleType:
  case ScriptVar::DoubleRefType:
    return ClazzInfo::getDoubleClazz();
  case ScriptVar::ObjectType:
  case ScriptVar::ObjectRefType:
  {
    RObject obj = getObjectVar();
    if (obj == Nil)
      return Object::clazzInfo();
    return obj->getClazzInfo();
  }
  case ScriptVar::UnknownType :
  default:
    return ClazzInfo::getUnknownBasicClazz();
  }
}

void*
ScriptVar::getDataPtr()
{
  switch(type)
  {
  case ScriptVar::BoolType: return (void*)&var.boolVal;
  case ScriptVar::BoolRefType: return (void*)var.boolRef;
  case ScriptVar::CharType: return (void*)&var.charVal;
  case ScriptVar::CharRefType: return (void*)var.charRef;
  case ScriptVar::UcCharType: return (void*)&var.uccharVal;
  case ScriptVar::UcCharRefType: return (void*)var.uccharRef;
  case ScriptVar::ByteType: return (void*)&var.byteVal;
  case ScriptVar::ByteRefType: return (void*)var.byteRef;
  case ScriptVar::ShortType: return (void*)&var.shortVal;
  case ScriptVar::ShortRefType: return (void*)var.shortRef;
  case ScriptVar::IntType: return (void*)&var.intVal;
  case ScriptVar::IntRefType: return (void*)var.intRef;
  case ScriptVar::LongType: return (void*)&var.longVal;
  case ScriptVar::LongRefType: return (void*)var.longRef;
  case ScriptVar::FloatType: return (void*)&var.floatVal;
  case ScriptVar::FloatRefType: return (void*)var.floatRef;
  case ScriptVar::DoubleType: return (void*)&var.doubleVal;
  case ScriptVar::DoubleRefType: return (void*)var.doubleRef;
  case ScriptVar::ObjectType: return (void*)_getrobject()._ref_this();
  case ScriptVar::ObjectRefType: return (void*)var.oref.objectRef;
  case ScriptVar::UnknownType : return 0;
  default:
    return 0;
  }
}

int
ScriptVar::getTypeStorageSize() const
{
   switch(type)
  {
  case ScriptVar::BoolType:
  case ScriptVar::BoolRefType: return sizeof(bool);
  case ScriptVar::CharType:
  case ScriptVar::CharRefType: return sizeof(char);
  case ScriptVar::UcCharType:
  case ScriptVar::UcCharRefType: return sizeof(var.uccharVal);
  case ScriptVar::ByteType:
  case ScriptVar::ByteRefType: return sizeof(byte);
  case ScriptVar::ShortType:
  case ScriptVar::ShortRefType: return sizeof(short);
  case ScriptVar::IntType:
  case ScriptVar::IntRefType: return sizeof(int);
  case ScriptVar::LongType:
  case ScriptVar::LongRefType: return sizeof(jlong);
  case ScriptVar::FloatType:
  case ScriptVar::FloatRefType: return sizeof(float);
  case ScriptVar::DoubleType:
  case ScriptVar::DoubleRefType: return sizeof(double);
  default:
    return -1;
  }
}

void
ScriptVar::_throwWrongType(Type wanted) const
{
  const ClazzInfo* iscls = getClazzInfo();
  StringBuffer sb("ScriptVar: Cannot convert ");
  if (isReference(type) == true)
    sb << "reference of ";
  iscls->toTypeString(sb);
  sb << " to  type ";
  if (isReference(wanted) == true)
    sb << "reference of ";
  const ClazzInfo* wantcls = getClazzInfo(wanted);
  wantcls->toTypeString(sb);
  THROW1(DmiTypeConversionException, sb.toString());
}

//static
void
ScriptVar::_throwWrongType(const ClazzInfo* wanted, const ClazzInfo* existant)
{
  StringBuffer sb("ScriptVar: Cannot convert ");
  existant->toTypeString(sb);
  sb.append(" to ");
  wanted->toTypeString(sb);
  THROW1(DmiTypeConversionException, sb.toString());
}



void
ScriptVar::assign(Object* obj, short castFlags)
{
  if (type == ObjectRefType) 
  {
    *var.oref.objectRef = obj;
    return;
  }
  if (type == ObjectType && _getrobject().impl() == obj)
    return;
  reset();
  _setrobject(obj);
  type = ObjectType;
}



ScriptVar&
ScriptVar::operator=(const ScriptVar& o)
{
  int oldType = type;
  switch(o.type) {
  case BoolType : operator=(o.var.boolVal); break;
  case CharType : operator=(o.var.charVal); break;
  case UcCharType : operator=(o.var.uccharVal); break;
  case ByteType : operator=(o.var.byteVal); break;
  case ShortType : operator=(o.var.shortVal); break;
  case IntType : operator=(o.var.intVal); break;
  case LongType : operator=(o.var.longVal); break;
  case FloatType : operator=(o.var.floatVal); break;
  case DoubleType : operator=(o.var.doubleVal); break;
  case ObjectType: operator=(o._getrobject()); break;
  case BoolRefType : operator=(o.var.boolRef); break;
  case CharRefType : operator=(o.var.charRef); break;
  case UcCharRefType : operator=(o.var.uccharRef); break;
  case ByteRefType : operator=(o.var.byteRef); break;
  case ShortRefType : operator=(o.var.shortRef); break;
  case IntRefType : operator=(o.var.intRef); break;
  case LongRefType : operator=(o.var.longRef); break;
  case FloatRefType : operator=(o.var.floatRef); break;
  case DoubleRefType : operator=(o.var.doubleRef); break;
  case ObjectRefType: operator=(o.var.oref.objectRef); break;
  default:
    break; // #ex
  }
  flags = o.flags; //  must set here, because operator=() reset it
  // note, dont do that, because assign do not change
  // type of variable
  if (oldType == UnknownType && _clazzType == 0)
    _clazzType = o._clazzType;
  return *this;
}


void
ScriptVar::setOwnObjectReference(IN(RObject) obj)
{
  if (type == ObjectRefType)
  {
    *var.oref.objectRef = obj;
    return;
  }
  reset();
  type = ObjectRefType;
  var.oref.objectRef =  new RObject();
  *var.oref.objectRef = obj;
  var.oref.ownsObjectRef = true;
}

bool
ScriptVar::getBoolVar(short castFlags) const
{
  switch(type) {
  case UnknownType : _checkCastNum2Bool(castFlags); return false;
  case BoolType : return var.boolVal;
  case BoolRefType: return *var.boolRef;
  case CharType :
  case UcCharType:
  case ByteType :
  case ShortType :
  case IntType :
  case LongType : _checkCastNum2Bool(castFlags); return var.longVal != 0;
  case FloatType :
  case DoubleType : _checkCastNum2Bool(castFlags); return var.doubleVal != 0;
  
  case CharRefType : _checkCastNum2Bool(castFlags); return *var.charRef != 0;
  case UcCharRefType: _checkCastNum2Bool(castFlags); return *var.uccharRef != 0;
  case ByteRefType : _checkCastNum2Bool(castFlags); return *var.byteRef != 0;
  case ShortRefType : _checkCastNum2Bool(castFlags); return *var.shortRef != 0;
  case IntRefType : _checkCastNum2Bool(castFlags); return *var.intRef != 0;
  case LongRefType : _checkCastNum2Bool(castFlags); return *var.longRef != 0;
  case FloatRefType : _checkCastNum2Bool(castFlags); return *var.floatRef != 0;
  case DoubleRefType : _checkCastNum2Bool(castFlags); return *var.doubleRef != 0;
  case ObjectRefType:
  case ObjectType:
  {
    RObject tobj = _getrobject();
    if ((castFlags & SVCastObject2Bool) && tobj == Nil)
      return false;
    if (instanceof(tobj, Boolean) == true && (castFlags & SVCastAutobox))
    {
      return RBoolean(tobj)->booleanValue();
    }
    if (castFlags & SVCastDecodeString)
    {
      if (tobj == Nil)
        return false;
      RString sv = tobj->toString();
      if (sv == Nil)
        return false;
      if (sv->equalsIgnoreCase("true") == true || sv->equalsIgnoreCase("on") == true)
        return true;
      if (sv->equalsIgnoreCase("false") == true || sv->equalsIgnoreCase("off") == true)
        return false;
      if (castFlags & SVCastNum2Bool)
      {
        RNumber num = Number::decodeToNumber(sv, true);
        if (num != Nil)
        {
          return num->doubleValue() != 0.0;
        }
      }
    }
    if (castFlags & SVCastObject2Bool)
      return tobj != Nil;
    _throwWrongType(BoolType);
  }
  }
  if (castFlags & SVCastObject2Bool)
    return false;
  _throwWrongType(BoolType);
  return 0;
}

bool& 
ScriptVar::getBoolRef()
{
  if (type == UnknownType)
    *this = ::acdk::lang::inOf(bool(false));
  if (type == BoolRefType) return *var.boolRef;
  if (type == BoolType) return var.boolVal;
  _throwWrongType(BoolRefType);
  return *var.boolRef; // never reached
}

template <class ValueType>
void _throwOverflow(ScriptVar::Type requestedType, ValueType value)
{
  StringBuffer sb;
  sb << "the value " << value << " does not fit into ";
  const ClazzInfo* wantcls = ScriptVar::getClazzInfo(requestedType);
  wantcls->toTypeString(sb);
  THROW1(ArithmeticOverflowException, sb.toString());
}

template <class NumberType, typename ValueType> 
void _checkOverflowT(short castFlags, ScriptVar::Type requestedType, ValueType value) 
{
  if ((castFlags & SVCastCheckOvervflow) == 0)
    return;
  if (NumberType::MAX_VALUE < value || NumberType::MIN_VALUE > value)
    _throwOverflow(requestedType, value);
}

char
ScriptVar::getCharVar(short castFlags) const
{
  switch(type) {
  case UnknownType :
  case BoolType :
  case BoolRefType:
    _throwWrongType(CharType);
    return 0;
  case UcCharType : 
  case UcCharRefType : 
    _checkOverflowT<Character, ucchar> (castFlags, CharType, _getUcChar());
    return _getUcChar();
  case CharType : 
    return var.charVal;
  case CharRefType : 
    return *var.charRef;
  case ByteType : 
  case ByteRefType : 
    _checkCastChar2Int(castFlags, CharType); 
    return (char)_getByte();
  case ShortType : 
  case ShortRefType : 
    _checkCastChar2Int(castFlags, CharType); 
    _checkOverflowT<Character, short> (castFlags, CharType, _getShort());
    return (char)_getShort();
  case IntType:
  case IntRefType : 
    _checkCastChar2Int(castFlags, CharType); 
    _checkOverflowT<Character, int> (castFlags, CharType, _getInt());
    return (char)_getInt();
  case LongType : 
  case LongRefType : 
    _checkCastChar2Int(castFlags, CharType); 
    _checkOverflowT<Character, jlong> (castFlags, CharType, _getLong());
    return (char)_getLong();
  case FloatType : 
  case FloatRefType : 
    _checkCastChar2Int(castFlags, CharType); 
    _checkCastInt2Float(castFlags, CharType); 
    _checkOverflowT<Character, float> (castFlags, CharType, _getFloat());
    return (char)_getFloat();
  case DoubleType : 
  case DoubleRefType : 
    _checkCastChar2Int(castFlags, CharType); 
    _checkCastInt2Float(castFlags, CharType); 
    _checkOverflowT<Character, float> (castFlags, CharType, _getDouble());
    return (char)_getDouble();
  case ObjectType:
  case ObjectRefType:
  {
    RObject obj = _getObject();
    if (castFlags & SVCastUnwrapDmiObject && instanceof(obj, DmiObject) == true)
      return RDmiObject(obj)->getCharVar();
    
    _checkCastAutoboxing(castFlags, CharType);

    if (instanceof(obj, Character) == true)
      return RCharacter(obj)->charValue();
    if (instanceof(obj, UnicodeCharacter) == true)
    {
      ucchar ch = RUnicodeCharacter(obj)->charValue();
      _checkOverflowT<Character, ucchar>(castFlags, CharType, ch);
      return (char)ch;
    }
    
    if (instanceof(obj, Number) == true)
    {
      _checkCastChar2Int(castFlags, CharType);
      jlong l = getLongVar(castFlags);
      _checkOverflowT<Character, jlong>(castFlags, CharType, l);
      return (char)l;
    }
    if (castFlags & SVCastDecodeString)
    {
      if (obj == Nil)
        return 0;
      return acdk::lang::inOf(&Number::decodeToNumber(obj->toString())).getCharVar(castFlags);
    }
    _throwWrongType(CharType);
  }
  default :
    _throwWrongType(CharType);
    return 0;
  }
}

char& 
ScriptVar::getCharRef()
{
  if (type == UnknownType)
    *this = ::acdk::lang::inOf(char(0));
  if (type == CharRefType) return *var.charRef;
  if (type == CharType) return var.charVal;
  _throwWrongType(CharRefType);
  return *var.charRef; // never reached
}

ucchar
ScriptVar::getUcCharVar(short castFlags) const
{
  switch(type) {
  case UcCharType : return var.uccharVal;
  case UcCharRefType : return *var.uccharRef;

  case UnknownType :
  case BoolType :
  case BoolRefType:
    _throwWrongType(UcCharType);

  case CharType : return var.charVal;
  case CharRefType : return *var.charRef;
  case ByteType : 
  case ByteRefType :
    _checkCastChar2Int(castFlags, UcCharType);
    return (ucchar)_getByte();
  case ShortType :
  case ShortRefType :
    _checkCastChar2Int(castFlags, UcCharType);
    return (ucchar)_getShort();
  case IntType :
  case IntRefType :
    _checkCastChar2Int(castFlags, UcCharType);
    _checkOverflowT<UnicodeCharacter, int> (castFlags, UcCharType, _getInt());
    return (ucchar)_getInt();
  case LongType :
  case LongRefType :
    _checkCastChar2Int(castFlags, UcCharType);
    _checkOverflowT<UnicodeCharacter, jlong> (castFlags, UcCharType, _getLong());
    return (ucchar)_getLong();
  case FloatType :
  case FloatRefType :
    _checkCastChar2Int(castFlags, UcCharType);
    _checkCastInt2Float(castFlags, UcCharType); 
    _checkOverflowT<UnicodeCharacter, float> (castFlags, UcCharType, _getFloat());
    return (ucchar)_getFloat();
  case DoubleType :
  case DoubleRefType :
    _checkCastChar2Int(castFlags, UcCharType);
    _checkCastInt2Float(castFlags, UcCharType); 
    _checkOverflowT<UnicodeCharacter, double> (castFlags, UcCharType, _getDouble());
    return (ucchar)_getDouble();
  case ObjectType:
  case ObjectRefType:
  {
    RObject obj = _getObject();
    if (castFlags & SVCastUnwrapDmiObject && instanceof(obj, DmiObject) == true)
      return RDmiObject(obj)->getUcCharVar();
    _checkCastAutoboxing(castFlags, CharType);

    if (instanceof(obj, Character) == true)
      return RCharacter(obj)->charValue();
    if (instanceof(obj, UnicodeCharacter) == true)
      return RUnicodeCharacter(obj)->charValue();
    if (instanceof(obj, Number) == true)
    {
      _checkCastChar2Int(castFlags, UcCharType);
      jlong l = getLongVar(castFlags);
      _checkOverflowT<UnicodeCharacter, jlong>(castFlags, UcCharType, l);
      return (ucchar)l;
    }
    if (castFlags & SVCastDecodeString)
    {
      if (obj == Nil)
        return 0;
      return acdk::lang::inOf(&Number::decodeToNumber(obj->toString()) ).getUcCharVar(castFlags);
    }
    _throwWrongType(UcCharType);
  }
  default :
    _throwWrongType(UcCharType);
    return 0;
  }
}

ucchar& 
ScriptVar::getUcCharRef()
{
   if (type == UnknownType)
    *this = ::acdk::lang::inOf(ucchar(0));
  if (type == UcCharRefType) return *var.uccharRef;
  if (type == UcCharType) return var.uccharVal;
  _throwWrongType(UcCharRefType);
  return *var.uccharRef; // never reached
}

byte
ScriptVar::getByteVar(short castFlags) const
{
  switch(type) {
  case UnknownType :
    _throwWrongType(ByteType);
  case BoolRefType:
  case BoolType :
    _checkCastBool2Num(castFlags, ByteType);
    return _getBool() == true ? 1 : 0;
  case CharType : 
  case CharRefType : 
    _checkCastChar2Int(castFlags, ByteType);
    return (byte)_getChar();
  case UcCharType : 
  case UcCharRefType :
    _checkCastChar2Int(castFlags, ByteType);
    return (byte)_getUcChar();
  case ByteType : 
    return (byte)var.byteVal;
  case ByteRefType : 
    return (byte)*var.byteRef;

  case ShortType : 
  case ShortRefType : 
    _checkOverflowT<Byte, short> (castFlags, ByteType, _getShort());
    return (byte)_getShort();
  case IntType : 
  case IntRefType : 
    _checkOverflowT<Byte, int> (castFlags, ByteType, _getInt());
    return (byte)_getInt();
  case LongType : 
  case LongRefType :
    _checkOverflowT<Byte, jlong> (castFlags, ByteType, _getLong());
    return (byte)_getLong();
  case FloatType : 
  case FloatRefType : 
    _checkCastInt2Float(castFlags, ByteType);
    _checkOverflowT<Byte, float>(castFlags, ByteType, _getFloat());
    return (byte)_getFloat();
  case DoubleType : 
  case DoubleRefType : 
    _checkCastInt2Float(castFlags, ByteType);
    _checkOverflowT<Byte, double>(castFlags, ByteType, _getDouble());
    return (byte)_getDouble();
  case ObjectRefType:
  case ObjectType:
  {
    RObject obj = _getObject();
    if (castFlags & SVCastUnwrapDmiObject && instanceof(obj, DmiObject) == true)
      return RDmiObject(obj)->getByteVar();
   
    _checkCastAutoboxing(castFlags, ByteType);

    if (instanceof(obj, Number) == true)
    {
      jlong l = getLongVar(castFlags);
      _checkOverflowT<Byte, jlong>(castFlags, ByteType, l);
      return (byte)l;
    }
    if (castFlags & SVCastDecodeString)
    {
      if (obj == Nil)
        return 0;
      return acdk::lang::inOf(&Number::decodeToNumber(obj->toString()) ).getShortVar(castFlags);
    }
    _throwWrongType(ByteType);
    return 0;
  }
  default :
    _throwWrongType(ByteType);
    return 0;
    // oops
  }
}

byte& 
ScriptVar::getByteRef()
{
  if (type == UnknownType)
    *this = ::acdk::lang::inOf(byte(0));
  if (type == ByteRefType) return *var.byteRef;
  if (type == ByteType) return var.byteVal;
  _throwWrongType(ByteRefType);
  return *var.byteRef; // never reached
}

short
ScriptVar::getShortVar(short castFlags) const
{
  switch(type) {
  case UnknownType :
    _throwWrongType(ShortType);
    return 0;
  case BoolRefType:
  case BoolType :
    _checkCastBool2Num(castFlags, ShortType);
    return _getBool() == true ? 1 : 0;
  case CharType : 
  case CharRefType :
     _checkCastChar2Int(castFlags, ShortType);
    return (short)_getChar();
  case UcCharType :
  case UcCharRefType :
     _checkCastChar2Int(castFlags, ShortType);
    return (short)_getUcChar();
  case ByteType : return (short)var.byteVal;
  case ByteRefType : return (short)*var.byteRef;
  case ShortType : return (short)var.shortVal;
  case ShortRefType : return (short)*var.shortRef;
  case IntType : 
  case IntRefType : 
    _checkOverflowT<Short, int>(castFlags, ShortType, _getInt());
    return (short)_getInt();
  case LongType : 
  case LongRefType : 
    _checkOverflowT<Short, jlong>(castFlags, ShortType, _getLong());
    return (short)_getLong();
  case FloatType : 
  case FloatRefType : 
    _checkCastInt2Float(castFlags, ShortType);
    _checkOverflowT<Short, float>(castFlags, ShortType, _getFloat());
    return (short)_getFloat();
  case DoubleType : 
  case DoubleRefType : 
    _checkCastInt2Float(castFlags, ShortType);
    _checkOverflowT<Short, double>(castFlags, ShortType, _getDouble());
    return (short)_getDouble();
  case ObjectRefType:
  case ObjectType:
  {
    RObject obj = _getObject();
    if (castFlags & SVCastUnwrapDmiObject && instanceof(obj, DmiObject) == true)
      return RDmiObject(obj)->getShortVar();
    _checkCastAutoboxing(castFlags, ShortType);
    if (instanceof(obj, Number) == true)
    {
      jlong l = getLongVar(castFlags);
      _checkOverflowT<Short, jlong>(castFlags, ShortType, l);
      return (short)l;
    }
    if (castFlags & SVCastDecodeString)
    {
      if (obj == Nil)
        return 0;
      return acdk::lang::inOf(&Number::decodeToNumber(obj->toString()) ).getShortVar(castFlags);
    }
    _throwWrongType(ShortType);
  }
   default :
    _throwWrongType(ShortType);
    return 0;
    // oops
  }
}

short& 
ScriptVar::getShortRef()
{
  if (type == UnknownType)
    *this = ::acdk::lang::inOf(short(0));
  if (type == ShortRefType) return *var.shortRef;
  if (type == ShortType) return var.shortVal;
  _throwWrongType(ShortRefType);
  return *var.shortRef; // never reached
}

int
ScriptVar::getIntVar(short castFlags) const
{
  switch(type) {
  case UnknownType :
     _throwWrongType(IntType);
    return 0;
  case BoolRefType:
  case BoolType :
    _checkCastBool2Num(castFlags, IntType);
    return _getBool() == true ? 1 : 0;
  case CharType : 
  case CharRefType : 
    _checkCastChar2Int(castFlags, IntType);
    return (int)_getChar();
  case UcCharType : 
  case UcCharRefType : 
    _checkCastChar2Int(castFlags, IntType);
    return (int)_getUcChar();
  case ByteType : return (int)var.byteVal;
  case ByteRefType : return (int)*var.byteRef;
  case ShortType : return (int)var.shortVal;
  case ShortRefType : return (int)*var.shortRef;
  case IntType : return (int)var.intVal;
  case IntRefType : return (int)*var.intRef;
  case LongType : 
  case LongRefType : 
    _checkOverflowT<Integer, jlong>(castFlags, IntType, _getLong());
    return (int)_getLong();
  case FloatType : 
  case FloatRefType : 
    _checkCastInt2Float(castFlags, IntType);
    return (int)_getFloat();
  case DoubleType : 
  case DoubleRefType : 
    _checkCastInt2Float(castFlags, IntType);
    _checkOverflowT<Integer, double>(castFlags, IntType, _getDouble());
    return (int)_getDouble();
  case ObjectRefType:
  case ObjectType:
  {
    RObject obj = _getObject();
    if (castFlags & SVCastUnwrapDmiObject && instanceof(obj, DmiObject) == true)
      return RDmiObject(obj)->getIntVar();
    _checkCastAutoboxing(castFlags, IntType);
    if (instanceof(obj, Number) == true)
    {
      jlong l = getLongVar(castFlags);
      _checkOverflowT<Integer, jlong>(castFlags, IntType, l);
      return (int)l;
    }
    if (obj != Nil && castFlags & SVCastString2EnumInt && instanceof(obj, String) == true)
    {
      RString s = (RString)obj;
      RString clsname, ns;
      ClazzInfo::splitClassAndNs(s, clsname, ns);
      const ClazzEnumValueInfo* evi = ClazzEnumInfo::findEnumValue(clsname, ns);
      if (evi != 0)
        return evi->value;
        
    }
    if (castFlags & SVCastDecodeString)
    {
      if (obj == Nil)
        return 0;
      return acdk::lang::inOf(&Number::decodeToNumber(obj->toString()) ).getIntVar(castFlags);
    }
    _throwWrongType(IntType);
  }
   default :
    _throwWrongType(IntType);
    return 0;
    // oops
  }
}

int& 
ScriptVar::getIntRef()
{
  if (type == UnknownType)
    *this = ::acdk::lang::inOf(int(0));
  if (type == IntRefType) return *var.intRef;
  if (type == IntType) return var.intVal;
  _throwWrongType(IntRefType);
  return *var.intRef; // never reached
}

jlong
ScriptVar::getLongVar(short castFlags) const
{
  switch(type) {
  case UnknownType :
    _throwWrongType(LongType);
    return 0;
  case BoolRefType:
  case BoolType :
    _checkCastBool2Num(castFlags, IntType);
    return _getBool() == true ? 1 : 0;
  case CharType : 
  case CharRefType : 
    _checkCastChar2Int(castFlags, LongType);
    return (jlong)_getChar();
  case UcCharType : 
  case UcCharRefType : 
    _checkCastChar2Int(castFlags, LongType);
    return (jlong)_getUcChar();
  case ByteType : return (jlong)var.byteVal;
  case ByteRefType : return (jlong)*var.byteRef;
  case ShortType : return (jlong)var.shortVal;
  case ShortRefType : return (jlong)*var.shortRef;
  case IntType : return (jlong)var.intVal;
  case IntRefType : return (jlong)*var.intRef;
  case LongType : return (jlong)var.longVal;
  case LongRefType : return (jlong)*var.longRef;

  case FloatType : _checkCastInt2Float(castFlags, LongType); return (jlong)var.floatVal;
  case FloatRefType : _checkCastInt2Float(castFlags, LongType); return (jlong)*var.floatRef;
  case DoubleType : _checkCastInt2Float(castFlags, LongType); return (jlong)var.doubleVal;
  case DoubleRefType : _checkCastInt2Float(castFlags, LongType); return (jlong)*var.doubleRef;
  case ObjectRefType:
  case ObjectType:
  {
    RObject obj = _getObject();
    if (castFlags & SVCastUnwrapDmiObject && instanceof(obj, DmiObject) == true)
      return RDmiObject(obj)->getLongVar();
    _checkCastAutoboxing(castFlags, LongType);
    if (instanceof(obj, Number) == true)
    {
      if (instanceof(obj, Double) || instanceof(obj, Float))
      {
        _checkCastInt2Float(castFlags, LongType);
        double d = RNumber(obj)->doubleValue();
        return (jlong)d;
      }
      else
      {
        return RNumber(obj)->longValue();
      }      
    }
    if (castFlags & SVCastDecodeString)
    {
      if (obj == Nil)
        return 0;
      return acdk::lang::inOf(&Number::decodeToNumber(obj->toString())).getLongVar(castFlags);
    }
    _throwWrongType(LongType);
  }
   default :
    _throwWrongType(LongType);
    return 0;
    // oops
  }
}

jlong& 
ScriptVar::getLongRef()
{
   if (type == UnknownType)
    *this = ::acdk::lang::inOf(jlong(0));
  if (type == LongRefType) return *var.longRef;
  if (type == LongType) return var.longVal;
    _throwWrongType(LongRefType);
  return *var.longRef; // never reached
}

float
ScriptVar::getFloatVar(short castFlags) const
{
  switch(type) {
  case UnknownType :
    _throwWrongType(FloatType);
    return 0;
  case BoolRefType:
  case BoolType :
    _checkCastBool2Num(castFlags, IntType);
    return _getBool() == true ? 1 : 0;
   
  case CharType : 
  case CharRefType : 
    _checkFloat2Char(castFlags, FloatType);
    return (float)_getChar();
  case UcCharType : 
  case UcCharRefType : 
    _checkFloat2Char(castFlags, FloatType);
    return (float)_getUcChar();
  case ByteType : _checkCastInt2Float(castFlags, FloatType); return (float)var.byteVal;
  case ByteRefType : _checkCastInt2Float(castFlags, FloatType); return (float)*var.byteRef;
  case ShortType : _checkCastInt2Float(castFlags, FloatType); return (float)var.shortVal;
  case ShortRefType : _checkCastInt2Float(castFlags, FloatType); return (float)*var.shortRef;
  case IntType : _checkCastInt2Float(castFlags, FloatType); return (float)var.intVal;
  case IntRefType : _checkCastInt2Float(castFlags, FloatType); return (float)*var.intRef;
  case LongType : _checkCastInt2Float(castFlags, FloatType); return (float)var.longVal;
  case LongRefType : _checkCastInt2Float(castFlags, FloatType); return (float)*var.longRef;
  case FloatType : return (float)var.floatVal;
  case FloatRefType : return (float)*var.floatRef;
  case DoubleType : 
  case DoubleRefType : 
    _checkOverflowT<Float, double>(castFlags, FloatType, _getDouble());
    return (float)_getDouble();
  case ObjectRefType:
  case ObjectType:
  {
    RObject obj = _getObject();
    if (castFlags & SVCastUnwrapDmiObject && instanceof(obj, DmiObject) == true)
      return RDmiObject(obj)->getFloatVar();
    _checkCastAutoboxing(castFlags, FloatType);
    if (instanceof(obj, Number) == true)
    {
      if (instanceof(obj, Float) == true || instanceof(obj, Double))
      {
        double d = RNumber(obj)->doubleValue();
        _checkOverflowT<Float, double>(castFlags, FloatType, d);
        return (float)d;
      }
      else
      {
        _checkCastInt2Float(castFlags, FloatType);
        return RNumber(obj)->floatValue();
      }
    }
    if (castFlags & SVCastDecodeString)
    {
      if (obj == Nil)
        return 0;
      return acdk::lang::inOf(&Number::decodeToNumber(obj->toString())).getFloatVar(castFlags);
    }
    _throwWrongType(FloatType);
  }
   default :
    _throwWrongType(FloatType);
    return 0;
    // oops
  }
}
float& 
ScriptVar::getFloatRef()
{
  if (type == UnknownType)
    *this = ::acdk::lang::inOf(float(0.0));
  if (type == FloatRefType) return *var.floatRef;
  if (type == FloatType) return var.floatVal;
  _throwWrongType(FloatRefType);
  return *var.floatRef; // never reached
}

double
ScriptVar::getDoubleVar(short castFlags) const
{
  switch(type) {
  case UnknownType :
    _throwWrongType(DoubleType);
    return 0;
  case BoolRefType:
  case BoolType :
    _checkCastBool2Num(castFlags, IntType);
    return _getBool() == true ? 1 : 0;
    
  case CharType : _checkFloat2Char(castFlags, DoubleType); return (double)var.charVal;
  case CharRefType : _checkFloat2Char(castFlags, DoubleType); return (double)*var.charRef;
  case UcCharType : _checkFloat2Char(castFlags, DoubleType); return (double)var.uccharVal;
  case UcCharRefType : _checkFloat2Char(castFlags, DoubleType); return (double)*var.uccharRef;
  case ByteType : _checkCastInt2Float(castFlags, DoubleType); return (double)var.byteVal;
  case ByteRefType : _checkCastInt2Float(castFlags, DoubleType); return (double)*var.byteRef;
  case ShortType : _checkCastInt2Float(castFlags, DoubleType); return (double)var.shortVal;
  case ShortRefType : _checkCastInt2Float(castFlags, DoubleType); return (double)*var.shortRef;
  case IntType : _checkCastInt2Float(castFlags, DoubleType); return (double)var.intVal;
  case IntRefType : _checkCastInt2Float(castFlags, DoubleType); return (double)*var.intRef;
  case LongType : _checkCastInt2Float(castFlags, DoubleType); return (double)var.longVal;
  case LongRefType : _checkCastInt2Float(castFlags, DoubleType); return (double)*var.longRef;
  case FloatType : return (double)var.floatVal;
  case FloatRefType : return (double)*var.floatRef;
  case DoubleType : return (double)var.doubleVal;
  case DoubleRefType : return (double)*var.doubleRef;
  case ObjectRefType:
  case ObjectType:
  {
    RObject obj = _getObject();
    if (castFlags & SVCastUnwrapDmiObject && instanceof(obj, DmiObject) == true)
      return RDmiObject(obj)->getDoubleVar();
    _checkCastAutoboxing(castFlags, DoubleType);
 
    if (instanceof(obj, Number) == true)
       return RNumber(obj)->doubleValue();
    
    if (castFlags & SVCastDecodeString)
    {
      if (obj == Nil)
        return 0;
      return acdk::lang::inOf(&Number::decodeToNumber(obj->toString())).getDoubleVar(castFlags);
    }

    _throwWrongType(DoubleType);
  }
   default :
    _throwWrongType(DoubleType);
    return 0;
    // oops
  }
}

double& 
ScriptVar::getDoubleRef()
{
  if (type == UnknownType)
    *this = ::acdk::lang::inOf(double(0.0));
  if (type == DoubleRefType) return *var.doubleRef;
  if (type == DoubleType) return var.doubleVal;
  _throwWrongType(DoubleRefType);
  return *var.doubleRef; // never reached
}

RObject
ScriptVar::getObjectVar(short castFlags, const ClazzInfo* ci) const
{
  if (ci == String::clazzInfo() && castFlags & SVCastEncodeString)
  {
    return (RObject)getStringVar(castFlags);
  }
  if (castFlags & SVCastWrapDmiObject && ci == DmiObject::clazzInfo())
  {
    if (getValueClazzInfo() != DmiObject::clazzInfo())
      return new DmiObject(*this);
  }
  
  if (ci != 0 && Number::clazzInfo()->assignableFrom(ci) == true)
  {
    const ClazzInfo* valci = getValueClazzInfo();
    if (isObjectType() == true &&  valci != 0 && ci->assignableFrom(valci) == true)
    {
      if (type == ObjectType)
        return _getrobject();
      if (type == ObjectRefType)
        return *var.oref.objectRef;
    }

    if (ci == Byte::clazzInfo())
      return new Byte(getByteVar(castFlags));
    if (ci == Short::clazzInfo())
      return new Short(getShortVar(castFlags));
    if (ci == Integer::clazzInfo())
      return new Integer(getIntVar(castFlags));
    if (ci == Long::clazzInfo())
      return new Long(getLongVar(castFlags));
    if (ci == Float::clazzInfo())
      return new Float(getFloatVar(castFlags));
    if (ci == Double::clazzInfo())
      return new Double(getDoubleVar(castFlags));
    if (ci == Boolean::clazzInfo())
      return new Boolean(getBoolVar(castFlags));
    if (ci == Character::clazzInfo())
      return new Character(getCharVar(castFlags));
    if (ci == UnicodeCharacter::clazzInfo())
      return new UnicodeCharacter(getUcCharVar(castFlags));
    if (ci == Number::clazzInfo())
      return getObjectVar(castFlags);
  }
  
  if (castFlags & SVCastAutobox && (ci == 0 || isObjectType()))
  {
    switch(type) {
    case UnknownType : return Nil;
    case BoolType : return new Boolean(var.boolVal);
    case BoolRefType : return new Boolean(*var.boolRef);
    case CharType : return new Character(var.charVal);
    case CharRefType : return new Character(*var.charRef);
    case UcCharType : return new UnicodeCharacter(var.uccharVal);
    case UcCharRefType : return new UnicodeCharacter(*var.uccharRef);
    case ByteType : return new Byte(var.byteVal);
    case ByteRefType : return new Byte(*var.byteRef);
    case ShortType : return new Short(var.shortVal);
    case ShortRefType : return new Short(*var.shortRef);
    case IntType : return new Integer(var.intVal);
    case IntRefType : return new Integer(*var.intRef);
    case LongType : return new Long(var.longVal);
    case LongRefType : return new Long(*var.longRef);
    case FloatType : return new Float(var.floatVal);
    case FloatRefType : return new Float(*var.floatRef);
    case DoubleType : return new Double(var.doubleVal);
    case DoubleRefType : return new Double(*var.doubleRef);
    case ObjectType: 
      return _getrobject();
    case ObjectRefType: 
      return *var.oref.objectRef;
    default: return Nil;
    }
  }
  if (isObjectType() == true)
  {
    RObject ret = _getObject();
    if (ci == 0)
      return ret;
    //### ???return ret->_castTo(ci);
    return ret;
  }
  _throwWrongType(ObjectType);
  return Nil;
}

RObject& 
ScriptVar::getObjectRef()
{
  if (type == UnknownType)
    *this = ::acdk::lang::inOf(RObject());
  if (type == ObjectRefType) return *var.oref.objectRef;
  if (type == ObjectType) return _getrobject();
    _throwWrongType(ObjectRefType);
  return *var.oref.objectRef; // never reached
}

bool
ScriptVar::isString(short castFlags)  const
{
  if (castFlags & SVCastEncodeString)
    return true;
  if (isStringType() == true)
    return true;
  return false;
}

RString
ScriptVar::getStringVar(short castFlags) const
{
  if (castFlags & SVCastEncodeString)
  {
    RObject o = getObjectVar(castFlags | SVCastAutobox);
    if (o == Nil)
      return "Nil";
    return o->toString();
  }
  return (RString)getObjectVar(castFlags, String::clazzInfo());
}

Object*
ScriptVar::operator->()
{
  if (type == ObjectType)
  {
    if (_getrobject() == Nil)
      ObjectBase::_throwNullPointerException();
    return &_getrobject();
  } else if (type == ObjectRefType) {
    if (*var.oref.objectRef == Nil)
      ObjectBase::_throwNullPointerException();
    return var.oref.objectRef->impl();
  }
  THROW1(DmiTypeConversionException, "Only Object instances can have ->");
  return 0; // never reached
}

RString
ScriptVar::toString() const
{
  RString obj = getStringVar(SVCastStdFlags | SVCastWrapDmiObject | SVCastEncodeString);
  if (obj == Nil)
    return "Nil";
  return obj->toString();
}

RString
ScriptVar::toCode() const
{
  switch (type) {
  case UnknownType : return "<undefined>";
  case BoolRefType:
  case BoolType : return Boolean(getBoolVar()).toString();
  case CharRefType:
  case CharType : return "\'" + Character(getCharVar()).toString() + "\'";
  case UcCharRefType:
  case UcCharType : return "\'" + UnicodeCharacter(getUcCharVar()).toString() + "\'";
  case ByteRefType :
  case ByteType : return Byte(getByteVar()).toString();
  case ShortRefType :
  case ShortType : return Short(getShortVar()).toString();
  case IntRefType :
  case IntType : return Integer::toString(getIntVar());
  case LongRefType :
  case LongType : return Long(getLongVar()).toString();
  case FloatRefType :
  case FloatType : return Float(getFloatVar()).toString();
  case DoubleRefType :
  case DoubleType : return Double(getDoubleVar()).toString();
  case ObjectRefType :
  case ObjectType :
  {
    RObject o = _getObject();
    if (o == Nil)
      return "Nil";
    if (instanceof(o, String) == true)
      return "\"" + o->toString() + "\"";
    return o->toString();
  }
  default : return "<undefined>";
  }
}


bool 
ScriptVar::isCharacter(short castFlags) const 
{
  if (isCharacterType() == true)
    return true;
  if (castFlags & SVCastSVCastChar2Int && isIntegerType() == true)
    return true;
  if (isObjectType() == true)
  {
    RObject obj = getObjectVar();
    if (obj == Nil)
      return false;
    if (castFlags & SVCastAutobox)
    {
      if (instanceof(obj, Character) == true ||
          instanceof(obj, UnicodeCharacter) == true
        )
        return true;
    }
    if (castFlags & SVCastUnwrapDmiObject)
    {
      if (instanceof(obj, DmiObject) == true)
        return RDmiObject(obj)->isCharacter(castFlags);

    }
    if (castFlags & SVCastSVCastChar2Int)
      return isInteger(castFlags & ~SVCastSVCastChar2Int);
  }
  return false;
}


bool
ScriptVar::isInteger(short castFlags) const
{
  if (isIntegerType() == true)
    return true;
  if (castFlags & SVCastInt2Float && isFloatingType())
    return true;
  if (castFlags & SVCastNum2Bool && isBoolType())
    return true;
  if (castFlags & SVCastSVCastChar2Int && isCharacterType())
    return true;
  if (isObjectType())
  {
    RObject obj = _getObject();
    if (obj == Nil)
      return false;
    if (castFlags & SVCastAutobox)
    {
      if (instanceof(obj, Byte) || instanceof(obj, Short) || instanceof(obj, Integer) || instanceof(obj, Long))
       return true;
      if (castFlags & SVCastInt2Float &&
        (instanceof(obj, Float) || instanceof(obj, Double)))
        return true;
      if (castFlags & SVCastSVCastChar2Int && 
        (instanceof(obj, Character) || instanceof(obj, UnicodeCharacter)))
        return true;
    }
    if (castFlags & SVCastUnwrapDmiObject  && instanceof(obj, DmiObject))
    {
      return RDmiObject(obj)->isInteger(castFlags);
    }
    if (castFlags & SVCastDecodeString && instanceof(obj, String))
    {
      RString sval = getStringVar();
      bool tryOnly = true;
      RNumber num = Number::decodeToNumber(sval, true);
      if (num == Nil)
        return false;
      if ((castFlags & SVCastInt2Float) == false &&
        (instanceof(num, Float) || instanceof(num, Double)))
        return false;
      return true;
    }
  }
  return false;
}


bool
ScriptVar::isFloating(short castFlags) const
{
  if (isFloatingType() == true)
    return true;
  if (castFlags & SVCastInt2Float)
  {
    if (isInteger(castFlags) == true)
      return true;
  }
  if (isObjectType() == true)
  {
    RObject obj = _getObject();
    if (obj == Nil)
      return false;
    if (castFlags & SVCastAutobox && (instanceof(obj, Float) || instanceof(obj, Double)))
      return true;

    if (castFlags & SVCastUnwrapDmiObject  && instanceof(obj, DmiObject))
    {
      return RDmiObject(obj)->isFloating(castFlags);
    }

  }
  return false;
}

bool
ScriptVar::isBoolean(short castFlags) const
{
  if (isBoolType() == true)
    return true;
  if (castFlags & SVCastNum2Bool && isNumberType() == true)
    return true;

  if (isObjectType())
  {
    if (castFlags & SVCastObject2Bool)
      return true;

    RObject obj = _getObject();
    if (castFlags & SVCastAutobox &&  instanceof(obj, Boolean) == true)
      return true;
    if (castFlags & SVCastUnwrapDmiObject && instanceof(obj, DmiObject) == true)
      return RDmiObject(obj)->isCharacter(castFlags);
    if (castFlags & SVCastDecodeString && instanceof(obj, String) == true)
    {
      RString s = (RString)s;
      if (s->equals("t") || s->equals("true") || s->equals("false"))
        return true;
      if (castFlags & SVCastNum2Bool)
        return isInteger(castFlags & ~SVCastNum2Bool);
    }

  }
  return false;
}


ScriptVar&
ScriptVar::operator=(bool c)
{
  switch (type)
  {
  case BoolType:
    var.boolVal = c;
    break;
  case BoolRefType:
    *var.boolRef = c;
    break;
  case CharType:
  case ByteType:
  case ShortType:
  case IntType:
  case LongType:
  case FloatType:
  case DoubleType:
  case ObjectType:
  case UnknownType:
    reset();
    type = BoolType;
    var.boolVal = c;
    break;
  default:
    _throwWrongType(BoolType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(bool* c)
{
  switch (type)
  {
  case BoolRefType:
    *var.boolRef = *c;
    break;
  case BoolType:
    var.boolVal = *c;
    break;
  case UnknownType:
    reset();
    type = BoolRefType;
    var.boolRef = c;
    break;
  default:
    _throwWrongType(BoolType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(char c)
{
  switch (type)
  {
  case CharType: var.charVal = c; break;
  case CharRefType: *var.charRef = c; break;
  case UcCharRefType: *var.uccharRef = c; break;
  case ByteType: var.byteVal = c; break;
  case ByteRefType: *var.byteRef = c; break;
  case ShortType: var.shortVal = c; break;
  case ShortRefType: *var.shortRef = c; break;
  case IntType: var.intVal = c; break;
  case IntRefType: *var.intRef = c; break;
  case LongType: var.longVal = c; break;
  case LongRefType: *var.longRef = c; break;
  case FloatType: var.longVal = c; break;
  case FloatRefType: *var.longRef = c; break;
  case DoubleType: var.doubleVal = c; break;
  case DoubleRefType: *var.doubleRef = c; break;
  case BoolType:
  case ObjectType:
  case UnknownType:
  case UcCharType: 
    type = CharType;
    var.charVal = c;
    break;
  default:
    _throwWrongType(CharType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(char* c)
{
  switch (type)
  {
  case CharType: var.charVal = *c; break;
  case CharRefType: *var.charRef = *c; break;
  case UcCharRefType: *var.uccharRef = *c; break;
  case ByteType: var.byteVal = *c; break;
  case ByteRefType: *var.byteRef = *c; break;
  case ShortType: var.shortVal = *c; break;
  case ShortRefType: *var.shortRef = *c; break;
  case IntType: var.intVal = *c; break;
  case IntRefType: *var.intRef = *c; break;
  case LongType: var.longVal = *c; break;
  case LongRefType: *var.longRef = *c; break;
  case FloatType: var.longVal = *c; break;
  case FloatRefType: *var.longRef = *c; break;
  case DoubleType: var.doubleVal = *c; break;
  case DoubleRefType: *var.doubleRef = *c; break;
  case UnknownType:
    type = CharRefType;
    var.charRef = c;
    break;
  default:
    _throwWrongType(CharRefType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(const char* c)
{
  return operator=(ScriptVar(c));
}

/// #### @todo check rules of assignments (especially UcChar)
ScriptVar&
ScriptVar::operator=(byte c)
{
  switch (type)
  {
  case CharType: var.charVal = c; break;
  case CharRefType: *var.charRef = c; break;
  case UcCharRefType: *var.uccharRef = c; break;
  case UcCharType: var.uccharVal = c; break;
  case ByteType: var.byteVal = c; break;
  case ByteRefType: *var.byteRef = c; break;
  case ShortType: var.shortVal = c; break;
  case ShortRefType: *var.shortRef = c; break;
  case IntType: var.intVal = c; break;
  case IntRefType: *var.intRef = c; break;
  case LongType: var.longVal = c; break;
  case LongRefType: *var.longRef = c; break;
  case FloatType: var.longVal = c; break;
  case FloatRefType: *var.longRef = c; break;
  case DoubleType: var.doubleVal = c; break;
  case DoubleRefType: *var.doubleRef = c; break;
  case BoolType:
  case ObjectType:
  case UnknownType:
    type = ByteType;
    var.byteVal = c;
    break;
  default:
    _throwWrongType(ByteType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(ucchar* c)
{
  switch (type)
  {
  case CharType: var.charVal = *c; break;
  case CharRefType: *var.charRef = *c; break;
  case UcCharType: var.uccharVal = *c; break;
  case UcCharRefType: *var.uccharRef = *c; break;
  case ByteType: var.byteVal = *c; break;
  case ByteRefType: *var.byteRef = *c; break;
  case ShortType: var.shortVal = *c; break;
  case ShortRefType: *var.shortRef = *c; break;
  case IntType: var.intVal = *c; break;
  case IntRefType: *var.intRef = *c; break;
  case LongType: var.longVal = *c; break;
  case LongRefType: *var.longRef = *c; break;
  case FloatType: var.longVal = *c; break;
  case FloatRefType: *var.longRef = *c; break;
  case DoubleType: var.doubleVal = *c; break;
  case DoubleRefType: *var.doubleRef = *c; break;
  case UnknownType:
    type = UcCharRefType;
    var.uccharRef = c;
    break;
  default:
    _throwWrongType(UcCharRefType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(byte* c)
{
  switch (type)
  {
  case CharType: var.charVal = *c; break;
  case CharRefType: *var.charRef = *c; break;
  case ByteType: var.byteVal = *c; break;
  case ByteRefType: *var.byteRef = *c; break;
  case ShortType: var.shortVal = *c; break;
  case ShortRefType: *var.shortRef = *c; break;
  case IntType: var.intVal = *c; break;
  case IntRefType: *var.intRef = *c; break;
  case LongType: var.longVal = *c; break;
  case LongRefType: *var.longRef = *c; break;
  case FloatType: var.longVal = *c; break;
  case FloatRefType: *var.longRef = *c; break;
  case DoubleType: var.doubleVal = *c; break;
  case DoubleRefType: *var.doubleRef = *c; break;
  case UnknownType:
    type = ByteRefType;
    var.byteRef = c;
    break;
  default:
    _throwWrongType(ByteRefType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(short c)
{
  switch (type)
  {

  case CharRefType: *var.charRef = c; break;
  case UcCharRefType: *var.uccharRef = c; break;
  case ByteRefType: *var.byteRef = c; break;
  case ShortType: var.shortVal = c; break;
  case ShortRefType: *var.shortRef = c; break;
  case IntType: var.intVal = c; break;
  case IntRefType: *var.intRef = c; break;
  case LongType: var.longVal = c; break;
  case LongRefType: *var.longRef = c; break;
  case FloatType: var.longVal = c; break;
  case FloatRefType: *var.longRef = c; break;
  case DoubleType: var.doubleVal = c; break;
  case DoubleRefType: *var.doubleRef = c; break;
  case UcCharType:
  case BoolType:
  case ObjectType:
  case CharType:
  case ByteType:
  case UnknownType:
    type = ShortType;
    var.shortVal = c;
    break;
  default:
    _throwWrongType(ShortType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(short* c)
{
  switch (type)
  {
  case CharRefType: *var.charRef = *c; break;
  case ByteRefType: *var.byteRef = *c; break;
  case ShortType: var.shortVal = *c; break;
  case ShortRefType: *var.shortRef = *c; break;
  case IntType: var.intVal = *c; break;
  case IntRefType: *var.intRef = *c; break;
  case LongType: var.longVal = *c; break;
  case LongRefType: *var.longRef = *c; break;
  case FloatType: var.longVal = *c; break;
  case FloatRefType: *var.longRef = *c; break;
  case DoubleType: var.doubleVal = *c; break;
  case DoubleRefType: *var.doubleRef = *c; break;
  case ByteType:
  case UcCharType:
  case CharType:
    type = ShortType;
    var.shortVal = *c;
    break;
  case UnknownType:
    type = ShortRefType;
    var.shortRef = c;
    break;
  default:
    _throwWrongType(ShortRefType);
    break;
  }
  return *this;
}

ScriptVar& 
ScriptVar::operator=(ucchar c)
{
  switch (type)
  {
  /*
  case CharRefType: *var.charRef = c; break;
  case ByteRefType: *var.byteRef = c; break;
  */
  case UcCharRefType: *var.uccharRef = c; break;
  case UcCharType: var.uccharVal = c; break;
  case ShortRefType: *var.shortRef = c; break;
  case IntType: var.intVal = c; break;
  case IntRefType: *var.intRef = c; break;
  case LongType: var.longVal = c; break;
  case LongRefType: *var.longRef = c; break;
  case FloatType: var.longVal = c; break;
  case FloatRefType: *var.longRef = c; break;
  case DoubleType: var.doubleVal = c; break;
  case DoubleRefType: *var.doubleRef = c; break;
  case CharType:
  case ByteType:
  case ShortType:
  case BoolType:
  case ObjectType:
  case UnknownType:
    type = UcCharType;
    var.uccharVal = c;
    break;
  default:
    _throwWrongType(IntType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(int c)
{
  switch (type)
  {
  case CharRefType: *var.charRef = c; break;
  case UcCharRefType: *var.uccharRef = c; break;
  case ByteRefType: *var.byteRef = c; break;
  case ShortRefType: *var.shortRef = c; break;
  case IntType: var.intVal = c; break;
  case IntRefType: *var.intRef = c; break;
  case LongType: var.longVal = c; break;
  case LongRefType: *var.longRef = c; break;
  case FloatType: var.longVal = c; break;
  case FloatRefType: *var.longRef = c; break;
  case DoubleType: var.doubleVal = c; break;
  case DoubleRefType: *var.doubleRef = c; break;
  case CharType:
  case UcCharType:
  case ByteType:
  case ShortType:
  case BoolType:
  case ObjectType:
  case UnknownType:
    type = IntType;
    var.intVal = c;
    break;
  default:
    _throwWrongType(IntType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(int* c)
{
  switch (type)
  {
  case CharRefType: *var.charRef = *c; break;
  case UcCharRefType: *var.uccharRef = *c; break;
  case ByteRefType: *var.byteRef = *c; break;
  case ShortRefType: *var.shortRef = *c; break;
  case IntType: var.intVal = *c; break;
  case IntRefType: *var.intRef = *c; break;
  case LongType: var.longVal = *c; break;
  case LongRefType: *var.longRef = *c; break;
  case FloatType: var.longVal = *c; break;
  case FloatRefType: *var.longRef = *c; break;
  case DoubleType: var.doubleVal = *c; break;
  case DoubleRefType: *var.doubleRef = *c; break;
  case ByteType:
  case CharType:
  case UcCharType:
  case ShortType:
    type = IntType;
    var.intVal = *c;
    break;
  case UnknownType:
    type = IntRefType;
    var.intRef = c;
    break;
  default:
    _throwWrongType(IntRefType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(jlong c)
{
  switch (type)
  {
  case CharRefType: *var.charRef = c; break;
  case UcCharRefType: *var.uccharRef = c; break;
  case ByteRefType: *var.byteRef = c; break;
  case ShortRefType: *var.shortRef = c; break;
  case IntRefType: *var.intRef = c; break;
  case LongType: var.longVal = c; break;
  case LongRefType: *var.longRef = c; break;
  case FloatType: var.longVal = c; break;
  case FloatRefType: *var.longRef = c; break;
  case DoubleType: var.doubleVal = c; break;
  case DoubleRefType: *var.doubleRef = c; break;
  case CharType:
  case UcCharType:
  case ByteType:
  case ShortType:
  case IntType:
  case BoolType:
  case ObjectType:
  case UnknownType:
    type = LongType;
    var.longVal = c;
    break;
  default:
    _throwWrongType(LongType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(jlong* c)
{
  switch (type)
  {
  case CharRefType: *var.charRef = *c; break;
  case UcCharRefType: *var.uccharRef = *c; break;
  case ByteRefType: *var.byteRef = *c; break;
  case ShortRefType: *var.shortRef = *c; break;
  case IntRefType: *var.intRef = *c; break;
  case LongType: var.longVal = *c; break;
  case LongRefType: *var.longRef = *c; break;
  case FloatType: var.longVal = *c; break;
  case FloatRefType: *var.longRef = *c; break;
  case DoubleType: var.doubleVal = *c; break;
  case DoubleRefType: *var.doubleRef = *c; break;

  case CharType:
  case UcCharType:
  case ByteType:
  case ShortType:
  case IntType:
    type = LongType;
    var.longVal = *c;
    break;
  case UnknownType:
    type = LongRefType;
    var.longRef = c;
    break;
  default:
    _throwWrongType(LongRefType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(float c)
{
  switch (type)
  {
  case CharRefType: *var.charRef = (char)c; break;
  case UcCharRefType: *var.uccharRef = (ucchar)c; break;
  case ByteRefType: *var.byteRef = (byte)c; break;
  case ShortRefType: *var.shortRef = (short)c; break;
  case IntRefType: *var.intRef = (int)c; break;
  case LongRefType: *var.longRef = (jlong)c; break;
  case FloatType: var.floatVal = c; break;
  case FloatRefType: *var.floatRef = c; break;
  case DoubleType: var.doubleVal = c; break;
  case DoubleRefType: *var.doubleRef = c; break;
  case CharType:
  case UcCharType:
  case ByteType:
  case ShortType:
  case IntType:
  case LongType:
  case BoolType:
  case ObjectType:
  case UnknownType:
    type = FloatType;
    var.floatVal = c;
    break;
  default:
    _throwWrongType(FloatType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(float* c)
{
  switch (type)
  {
  case CharRefType: *var.charRef = (char)*c; break;
  case UcCharRefType: *var.uccharRef = (ucchar)*c; break;
  case ByteRefType: *var.byteRef = (byte)*c; break;
  case ShortRefType: *var.shortRef = (short)*c; break;
  case IntRefType: *var.intRef = (int)*c; break;
  case LongRefType: *var.longRef = (jlong)*c; break;
  case FloatType: var.longVal = (jlong)*c; break;
  case FloatRefType: *var.longRef = (jlong)*c; break;
  case DoubleType: var.doubleVal = (double)*c; break;
  case DoubleRefType: *var.doubleRef = (double)*c; break;
  case CharType:
  case UcCharType:
  case ByteType:
  case ShortType:
  case IntType:
  case LongType:
    type = FloatType;
    var.floatVal = *c;
    break;
  case UnknownType:
    type = FloatRefType;
    var.floatRef = c;
    break;
  default:
    _throwWrongType(FloatRefType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(double c)
{
  switch (type)
  {
  case CharRefType: *var.charRef = (char)c; break;
  case UcCharRefType: *var.uccharRef = (ucchar)c; break;
  case ByteRefType: *var.byteRef = (byte)c; break;
  case ShortRefType: *var.shortRef = (short)c; break;
  case IntRefType: *var.intRef = (int)c; break;
  case LongRefType: *var.longRef = (jlong)c; break;
  case FloatRefType: *var.longRef = (jlong)c; break;
  case DoubleType: var.doubleVal = (double)c; break;
  case DoubleRefType: *var.doubleRef = (double)c; break;
  case ByteType:
  case CharType:
  case UcCharType:
  case ShortType:
  case IntType:
  case LongType:
  case FloatType:
  case BoolType:
  case ObjectType:
  case UnknownType:
    type = DoubleType;
    var.doubleVal = c;
    break;
  default:
    _throwWrongType(DoubleType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(double* c)
{
  switch (type)
  {
  case CharRefType: *var.charRef = (char)*c; break;
  case UcCharRefType: *var.uccharRef = (ucchar)*c; break;
  case ByteRefType: *var.byteRef = (byte)*c; break;
  case ShortRefType: *var.shortRef = (short)*c; break;
  case IntRefType: *var.intRef = (int)*c; break;
  case LongRefType: *var.longRef = (jlong)*c; break;
  case FloatRefType: *var.longRef = (jlong)*c; break;
  case DoubleType: var.doubleVal = *c; break;
  case DoubleRefType: *var.doubleRef = *c; break;
  case CharType:
  case UcCharType:
  case ByteType:
  case ShortType:
  case IntType:
  case LongType:
  case FloatType:
    type = DoubleType;
    var.doubleVal = *c;
    break;
  case UnknownType:
    type = DoubleRefType;
    var.doubleRef = c;
    break;
  default:
    _throwWrongType(DoubleRefType);
    break;
  }
  return *this;
}

ScriptVar&
ScriptVar::operator=(const RObject& obj)
{
  assign(obj.impl());
  return *this;
}

ScriptVar&
ScriptVar::operator=(Object* c)
{
  assign(c);
  return *this;
}

ScriptVar&
ScriptVar::operator=(RObject* c)
{
  reset();
  type = ObjectRefType;
  var.oref.objectRef = c;
  var.oref.ownsObjectRef = false;
  return *this;
}



RString
ScriptVar::getTypeInfo() const
{
  switch(type){
  case UnknownType:
    return new String("UnknownType");
  case BoolType:
  case BoolRefType:
    return new String("bool");
  case CharRefType:
  case CharType:
    return new String("char");
  case UcCharRefType:
  case UcCharType:
    return new String("ucchar");
  case ByteRefType:
  case ByteType:
    return new String("byte");
  case ShortRefType:
  case ShortType:
    return new String("short");
  case IntRefType:
  case IntType:
    return new String("int");
  case LongRefType:
  case LongType:
    return new String("jlong");
  case FloatRefType:
  case FloatType:
    return new String("float");
  case DoubleRefType:
  case DoubleType:
    return new String("double");
  case ObjectRefType:
  case ObjectType: {
    RObject tobj = const_cast<ScriptVar*>(this)->getObjectVar();
    if (tobj == Nil)
      return new String("RObject");
    StringBuffer sb;
    tobj->getClass()->objectClazzInfo()->toTypeString(sb, true);
    return sb.toString();
  }
  default:
    return new String("UndefinedType");
  }
}

//static
RString
ScriptVar::getAsCodeArguments(ScriptVarArray& args)
{
  StringBuffer ret(256);
  ret.append("(");
  for (int i = 0; i < args.size(); i++) {
    if (i > 0)
      ret.append(", ");
    ret.append(args[i].getTypeInfo());
  }
  ret.append(")");
  return ret.toString();
}

ScriptVar
ScriptVar::inOf() const
{
  if (ScriptVar::isReference(type) == false)
  {
    ScriptVar ret = *this;
    ret.flags = MiAiIn;
    return ret;
  }
  switch (type)
  {
  case UnknownType: return *this;
  case ScriptVar::BoolRefType: return acdk::lang::inOf(*var.boolRef);
  case ScriptVar::BoolType : return acdk::lang::inOf(var.boolVal);
  case ScriptVar::CharRefType: return acdk::lang::inOf(*var.charRef);
  case ScriptVar::CharType : return acdk::lang::inOf(var.charVal);
  case UcCharRefType: return acdk::lang::inOf(*var.uccharRef);
  case UcCharType:  return acdk::lang::inOf(var.uccharVal);
  case ScriptVar::ByteType: return acdk::lang::inOf(var.byteVal);
  case ScriptVar::ByteRefType: return acdk::lang::inOf(*var.byteRef);
  case ScriptVar::ShortType: return acdk::lang::inOf(var.shortVal);
  case ScriptVar::ShortRefType: return acdk::lang::inOf(*var.shortRef);
  case ScriptVar::IntType: return acdk::lang::inOf(var.intVal);
  case ScriptVar::IntRefType: return acdk::lang::inOf(*var.intRef);
  case ScriptVar::LongType: return acdk::lang::inOf(var.longVal);
  case ScriptVar::LongRefType: return acdk::lang::inOf(*var.longRef);
  case ScriptVar::FloatType: return acdk::lang::inOf(var.floatVal);
  case ScriptVar::FloatRefType: return acdk::lang::inOf(*var.floatRef);
  case ScriptVar::DoubleType: return acdk::lang::inOf(var.doubleVal);
  case ScriptVar::DoubleRefType: return acdk::lang::inOf(*var.doubleRef);
  case ScriptVar::ObjectType: return acdk::lang::inOf(_getrobject());
  case ScriptVar::ObjectRefType: return acdk::lang::inOf(*var.oref.objectRef);
  }
  return *this;
}



ScriptVar
ScriptVar::outOf()
{
  if (ScriptVar::isReference(type))
  {
    ScriptVar ret = *this;
    ret.flags = MiAiOut;
    return ret;
  }
  switch (type)
  {
  case ScriptVar::BoolRefType:
  case ScriptVar::BoolType : return acdk::lang::outOf(getBoolRef());
  case ScriptVar::CharRefType:
  case ScriptVar::CharType : return acdk::lang::outOf(getCharRef());
  case UcCharRefType:
  case UcCharType:  return acdk::lang::outOf(getUcCharRef());
  case ScriptVar::ByteType:
  case ScriptVar::ByteRefType: return acdk::lang::outOf(getByteRef());
  case ScriptVar::ShortType:
  case ScriptVar::ShortRefType: return acdk::lang::outOf(getShortRef());
  case ScriptVar::IntType:
  case ScriptVar::IntRefType: return acdk::lang::outOf(getIntRef());
  case ScriptVar::LongType:
  case ScriptVar::LongRefType: return acdk::lang::outOf(getLongRef());
  case ScriptVar::FloatType:
  case ScriptVar::FloatRefType: return acdk::lang::outOf(getFloatRef());
  case ScriptVar::DoubleType:
  case ScriptVar::DoubleRefType: return acdk::lang::outOf(getDoubleRef());
  case ScriptVar::ObjectType:
  case ScriptVar::ObjectRefType: return acdk::lang::outOf(getObjectRef());
  default: break;
  }
  ScriptVar ret = *this;
  ret.flags = MiAiOut;
  return ret;
}

ScriptVar
ScriptVar::inoutOf()
{
  if (ScriptVar::isReference(type))
  {
    ScriptVar ret = *this;
    ret.flags = MiAiIn | MiAiOut;
    return ret;
  }
  switch (type)
  {
   case UnknownType: return *this;
  case ScriptVar::BoolRefType:
  case ScriptVar::BoolType : return acdk::lang::inoutOf(getBoolRef());
  case ScriptVar::CharRefType:
  case ScriptVar::CharType : return acdk::lang::inoutOf(getCharRef());
  case UcCharRefType:
  case UcCharType:  return acdk::lang::inoutOf(getUcCharRef());
  case ScriptVar::ByteType:
  case ScriptVar::ByteRefType: return acdk::lang::inoutOf(getByteRef());
  case ScriptVar::ShortType:
  case ScriptVar::ShortRefType: return acdk::lang::inoutOf(getShortRef());
  case ScriptVar::IntType:
  case ScriptVar::IntRefType: return acdk::lang::inoutOf(getIntRef());
  case ScriptVar::LongType:
  case ScriptVar::LongRefType: return acdk::lang::inoutOf(getLongRef());
  case ScriptVar::FloatType:
  case ScriptVar::FloatRefType: return acdk::lang::inoutOf(getFloatRef());
  case ScriptVar::DoubleType:
  case ScriptVar::DoubleRefType: return acdk::lang::inoutOf(getDoubleRef());
  case ScriptVar::ObjectType:
  case ScriptVar::ObjectRefType: return acdk::lang::inoutOf(getObjectRef());
  }
  ScriptVar ret = *this;
  ret.flags = MiAiIn | MiAiOut;
  return ret;
}

} // dmi
} // lang
} // acdk

