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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/DmiObject.h,v 1.44 2005/05/02 23:09:15 kommer Exp $

#ifndef acdk_lang_dmi_DmiObject_h
#define acdk_lang_dmi_DmiObject_h


#include <acdk.h>
#include <acdk/io/ObjectWriter.h>
#include <acdk/io/ObjectReader.h>
#include <acdk/lang/ClassCastException.h>

namespace acdk {
namespace lang {
namespace dmi {

class DmiObject;
typedef ::RefHolder<DmiObject> RDmiObject;

class DmiObjectArray;
typedef ::RObjectArrayImpl<RDmiObject> RDmiObjectArray;

/**
  typed held by a DmiObject
*/
enum VarType
{
  UnknownVT = ScriptVar::UnknownType,
  BoolVT  = ScriptVar::BoolType,
  CharVT = ScriptVar::CharType,
  UcCharVT = ScriptVar::UcCharType,
  ShortVT = ScriptVar::ShortType,
  IntVT = ScriptVar::IntType,
  LongVT = ScriptVar::LongType,
  FloatVT = ScriptVar::FloatType,
  DoubleVT = ScriptVar::DoubleType,
  ObjectVT = ScriptVar::ObjectType
};

ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, VarType);

/**
  Represents an any type which can hold any other ACDK type.
  Wrapper to a ScriptVar
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiCiWeakBind))
ACDK_CLASSATTRIBUTE(acdk.tools.mc.McConfigAttribute(acdk.tools.mc.McConfOwnCollectableFields))
class ACDK_CORE_PUBLIC DmiObject
: extends ::acdk::lang::Object
, foreign public ScriptVar
{
  ACDK_WITH_METAINFO(DmiObject)
private:
 
public:
  foreign ::acdk::lang::Object* _cast(const ::acdk::lang::dmi::ClazzInfo* ci);
  /**   creates 'void' type */
  DmiObject();
  /** create unitinialized type with gieven clazz */
  foreign DmiObject(const ClazzInfo* clazz);
  DmiObject(IN(RDmiObject) obj, int flags = 0) : ScriptVar(*obj, flags) { }
  foreign DmiObject(IN(RDmiObject) obj, int flags, const ClazzInfo* clazz) : ScriptVar(*obj, flags, clazz) { }
  foreign DmiObject(const ScriptVar& sv) : ScriptVar(sv) { }
  foreign DmiObject& operator=(const ScriptVar& sv) { *((ScriptVar*)this) = sv; return *this; }

  DmiObject(IN(bool) c, int fl = MiAiIn) : ScriptVar(c, fl) { }
  DmiObject(OUT(bool) c, int fl =  MiAiOut) : ScriptVar(&c, fl) { }
  foreign DmiObject(bool* c, int fl/* =  MiAiOut*/) : ScriptVar(c, fl) { }
  DmiObject(IN(char) c, int fl = MiAiIn) : ScriptVar(c, fl) { }
  DmiObject(OUT(char) c, int fl =  MiAiOut) : ScriptVar(&c, fl) { }
  foreign DmiObject(char* c, int fl/* =  MiAiOut*/) : ScriptVar(c, fl) { }
  DmiObject(IN(ucchar) c, int fl = MiAiIn) : ScriptVar(c, fl) { }
  DmiObject(OUT(ucchar) c, int fl =  MiAiOut) : ScriptVar(&c, fl) { }
  foreign DmiObject(ucchar* c, int fl/* =  MiAiOut*/) : ScriptVar(c, fl) { }
  DmiObject(IN(byte) c, int fl = MiAiIn) : ScriptVar(c, fl) { }
  DmiObject(OUT(byte) c, int fl =  MiAiOut) : ScriptVar(&c, fl) { }
  foreign DmiObject(byte* c, int fl/* =  MiAiOut*/) : ScriptVar(c, fl) { }
  DmiObject(IN(short) c, int fl = MiAiIn) : ScriptVar(c, fl) { }
  DmiObject(OUT(short) c, int fl =  MiAiOut) : ScriptVar(&c, fl) { }
  foreign DmiObject(short* c, int fl/* =  MiAiOut*/) : ScriptVar(c, fl) { }
  DmiObject(IN(int) c, int fl = MiAiIn) : ScriptVar(c, fl) { }
  DmiObject(OUT(int) c, int fl =  MiAiOut) : ScriptVar(&c, fl) { }
  foreign DmiObject(int* c, int fl/* =  MiAiOut*/) : ScriptVar(c, fl) { }
  DmiObject(IN(jlong) c, int fl = MiAiIn) : ScriptVar(c, fl) { }
  DmiObject(OUT(jlong) c, int fl =  MiAiOut) : ScriptVar(&c, fl) { }
  foreign DmiObject(jlong* c, int fl/* =  MiAiOut*/) : ScriptVar(c, fl) { }
  DmiObject(IN(float) c, int fl = MiAiIn) : ScriptVar(c, fl) { }
  DmiObject(OUT(float) c, int fl =  MiAiOut) : ScriptVar(&c, fl) { }
  foreign DmiObject(float* c, int fl/* =  MiAiOut*/) : ScriptVar(c, fl) { }
  DmiObject(IN(double) c, int fl = MiAiIn) : ScriptVar(c, fl) { }
  DmiObject(OUT(double) c, int fl =  MiAiOut) : ScriptVar(&c, fl) { }
  foreign DmiObject(double* c, int fl/* =  MiAiOut*/) : ScriptVar(c, fl) { }
  foreign DmiObject(NilRef nil, int fl = MiAiIn) : ScriptVar(RObject(nil), fl) { }
  DmiObject(IN(RObject) c, int fl = MiAiIn) : ScriptVar(c, fl) { }
  foreign DmiObject(IN(RObject) c, int fl, const ClazzInfo* clazz) : ScriptVar(c, fl, clazz) { }
  DmiObject(OUT(RObject) c, int fl =  MiAiOut) : ScriptVar(&c, fl) { }
  foreign DmiObject(OUT(RObject) c, int fl, const ClazzInfo* clazz) : ScriptVar(&c, fl, clazz) { }
  foreign DmiObject(RObject* c, int fl =  MiAiOut) : ScriptVar(c, fl) { }
  foreign DmiObject(Object* obj, int fl = MiAiIn) : ScriptVar(obj, fl) { }
  foreign DmiObject(InterfaceBase* obj, int fl = MiAiIn) : ScriptVar(obj, fl) { }
  VarType getVarType() { return (VarType)type; }
  foreign const ClazzInfo* getClazzType() { return _clazzType; }
  foreign void setClazzType(const ClazzInfo* ci) { _clazzType = ci; }
  /**
    returns a combination of acdk::lang::dmi::MethodArgInfoExtFlags
  */
  int getFlags() { return flags; }

  bool getBoolVar(short castFlags = SVCastStdFlags) { return ScriptVar::getBoolVar(castFlags); }
  char getCharVar(short castFlags = SVCastStdFlags) { return ScriptVar::getCharVar(castFlags); }
  byte getByteVar(short castFlags = SVCastStdFlags) { return ScriptVar::getByteVar(castFlags); }
  short getShortVar(short castFlags = SVCastStdFlags) { return ScriptVar::getShortVar(castFlags); }
  int getIntVar(short castFlags = SVCastStdFlags) { return ScriptVar::getIntVar(castFlags); }
  jlong getLongVar(short castFlags = SVCastStdFlags) { return ScriptVar::getLongVar(castFlags); }
  float getFloatVar(short castFlags = SVCastStdFlags) { return ScriptVar::getFloatVar(castFlags); }
  double getDoubleVar(short castFlags = SVCastStdFlags) { return ScriptVar::getDoubleVar(castFlags); }
  RObject getObjectVar(short castFlags = SVCastStdFlags) { return ScriptVar::getObjectVar(castFlags); }
  RString getStringVar(short castFlags = SVCastStdFlags) { return (RString)ScriptVar::getObjectVar(castFlags); }
  

  /** contains true if the hold value has type bool */
  bool isBoolType() const { return type == BoolType || type == BoolRefType; }
  /** contains true if the hold value has type char */
  bool isCharType() const { return type == CharType || type == CharRefType; }
  /** contains true if the hold value has type ucchar */
  bool isUcCharType() const { return type == UcCharType || type == UcCharRefType; }
  /** contains true if the hold value has type byte */
  bool isByteType() const { return type == ByteType || type == ByteRefType; }
  /** contains true if the hold value has type short */
  bool isShortType() const { return type == ShortType || type == ShortRefType; }
  /** contains true if the hold value has type int */
  bool isIntType() const { return type == IntType || type == IntRefType; }
  /** contains true if the hold value has type jlong */
  bool isLongType() const { return type == LongType || type == LongRefType; }
  /** contains true if the hold value has type float */
  bool isFloatType() const { return type == FloatType || type == FloatRefType; }
  /** contains true if the hold value has type double */
  bool isDoubleType() const { return type == DoubleType || type == ObjectRefType; }
  /** contains true if the hold value has type Object */
  bool isObjectType() const { return type == ObjectType || type == ObjectRefType; }
  /** return true if hold type is char or ucchar */
  bool isCharacterType() { return ScriptVar::isCharacterType();  }
   /**
    return true if the hold type is byte, short, int or long
  */
  bool isIntegerType() { return ScriptVar::isIntegerType();  }
  /** return true if hold type is float or double */
  bool isFloatingType() { return ScriptVar::isFloatingType();  }
  /** return true if hold type is byte, short, int, long, float or double */
  bool isNumberType() { return ScriptVar::isNumberType();  }
   /**
    return true if the hold type is a String instance
  */
  bool isStringType() { return ScriptVar::isStringType(); }
  /**
    return true if containing value can be casted to a boolean type
  */
  bool isBoolean(short castFlags = SVCastStdFlags) { return ScriptVar::isBoolean(castFlags); }
  /**
    return true if containing value can be casted to a character type
  */
  bool isNumber(short castFlags = SVCastStdFlags) { return ScriptVar::isNumber(castFlags); }
   /**
    return true if containing value can be casted to a floating type
  */
  bool isFloating(short castFlags = SVCastStdFlags) { return ScriptVar::isFloating(castFlags); }
  /**
    return true if containing can be casted to a integer type
  */
  bool isInteger(short castFlags = SVCastStdFlags) { return ScriptVar::isInteger(castFlags); }
  /**
    return true if the containing type can be casted to a string
  */
  bool isString(short castFlags = SVCastStdFlags) { return ScriptVar::isString(castFlags); }
  /**
    return true if containing type can be casted to a Object
  */
  bool isObject(short castFlags = SVCastStdFlags) { return ScriptVar::isObject(castFlags); }

  //void getBoolRef(OUT(bool) b) { b = ScriptVar::getBoolRef(); }
  //OUT(bool) getBoolRef() return ScriptVar::getBoolRef(); }

  RString getTypeInfo() { return ScriptVar::getTypeInfo(); }
  RString toString() { return ScriptVar::toString(); }
  RString toCode() { return ScriptVar::toCode(); }

  bool equals(IN(RDmiObject) other)
  {
    if (other == Nil)
      return false;
    return ScriptVar::equal(*other);
  }
  bool equals(IN(RObject) other)
  {
    if (instanceof(other, DmiObject) == false)
      return false;
    return equals(RDmiObject(other));
  }
 /* old
  RDmiObject assign(IN(RDmiObject) other) { _ensureAssignable(((ScriptVar*)other)->getClazzInfo()); *((ScriptVar*)this) = (ScriptVar&)*other; return this; }
  foreign RDmiObject assign(const ScriptVar& other) { _ensureAssignable(other.getClazzInfo()); *((ScriptVar*)this) = other; return this; }
  RDmiObject assign(IN(bool) c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(c)); *((ScriptVar*)this) = c; return this; }
  foreign RDmiObject assign(bool* c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(*c)); *((ScriptVar*)this) = *c; return this; }
  RDmiObject assign(IN(char) c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(c)); *((ScriptVar*)this) = c; return this; }
  foreign RDmiObject assign(char* c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(*c)); *((ScriptVar*)this) = c; return this; }
  RDmiObject assign(IN(ucchar) c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(c)); *((ScriptVar*)this) = c; return this; }
  foreign RDmiObject assign(ucchar* c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(*c)); *((ScriptVar*)this) = c; return this; }
  RDmiObject assign(IN(byte) c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(c)); *((ScriptVar*)this) = c; return this; }
  foreign RDmiObject assign(byte* c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(*c)); *((ScriptVar*)this) = c; return this; }
  RDmiObject assign(IN(short) c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(c)); *((ScriptVar*)this) = c; return this; }
  foreign RDmiObject assign(short* c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(*c)); *((ScriptVar*)this) = c; return this; }
  RDmiObject assign(IN(int) c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(c)); *((ScriptVar*)this) = c; return this; }
  foreign RDmiObject assign(int* c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(*c)); *((ScriptVar*)this) = c; return this; }
  RDmiObject assign(IN(jlong) c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(c)); *((ScriptVar*)this) = c; return this; }
  foreign RDmiObject assign(jlong* c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(*c)); *((ScriptVar*)this) = c; return this; }

  RDmiObject assign(IN(float) c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(c)); *((ScriptVar*)this) = c; return this; }
  foreign RDmiObject assign(float* c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(*c)); *((ScriptVar*)this) = c; return this; }
  RDmiObject assign(IN(double) c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(c)); *((ScriptVar*)this) = c; return this; }
  foreign RDmiObject assign(double* c) { _ensureAssignable(ClazzInfo::getBasicTypeClazz(*c)); *((ScriptVar*)this) = c; return this; }
  RDmiObject assign(IN(RObject) c) { _ensureAssignable(c); *((ScriptVar*)this) = c; return this;  }
  foreign RDmiObject assign(RObject* c) { _ensureAssignable(*c); *((ScriptVar*)this) = c; return this; }
  */
  RDmiObject assign(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { _assign((ScriptVar&)*other, castFlags); return this; }
  foreign RDmiObject assign(const ScriptVar& other, short castFlags = SVCastStdFlags) { _assign(other, castFlags); return this; }
  RDmiObject assign(IN(bool) c, short castFlags = SVCastStdFlags) { _assign(c, castFlags); return this; }
  foreign RDmiObject assign(bool* c, short castFlags = SVCastStdFlags) { _assign(*c, castFlags); return this; }
  RDmiObject assign(IN(char) c, short castFlags = SVCastStdFlags) { _assign(::acdk::lang::inOf(c), castFlags); return this; }
  foreign RDmiObject assign(char* c, short castFlags = SVCastStdFlags) { _assign(::acdk::lang::inOf(c), castFlags); return this; }
  RDmiObject assign(IN(ucchar) c, short castFlags = SVCastStdFlags) { _assign(c, castFlags); return this; }
  foreign RDmiObject assign(ucchar* c, short castFlags = SVCastStdFlags) { _assign(::acdk::lang::inOf(c), castFlags); return this; }
  RDmiObject assign(IN(byte) c, short castFlags = SVCastStdFlags) { _assign(c, castFlags); return this; }
  foreign RDmiObject assign(byte* c, short castFlags = SVCastStdFlags) { _assign(::acdk::lang::inOf(c), castFlags); return this; }
  RDmiObject assign(IN(short) c, short castFlags = SVCastStdFlags) { _assign(c, castFlags); return this; }
  foreign RDmiObject assign(short* c, short castFlags = SVCastStdFlags) { _assign(::acdk::lang::inOf(c), castFlags); return this; }
  RDmiObject assign(IN(int) c, short castFlags = SVCastStdFlags) { _assign(c, castFlags); return this; }
  foreign RDmiObject assign(int* c, short castFlags = SVCastStdFlags) { _assign(::acdk::lang::inOf(c), castFlags); return this; }
  RDmiObject assign(IN(jlong) c, short castFlags = SVCastStdFlags) { _assign(c, castFlags); return this; }
  foreign RDmiObject assign(jlong* c, short castFlags = SVCastStdFlags) { _assign(::acdk::lang::inOf(c), castFlags); return this; }

  RDmiObject assign(IN(float) c, short castFlags = SVCastStdFlags) { _assign(c, castFlags); return this; }
  foreign RDmiObject assign(float* c, short castFlags = SVCastStdFlags) { _assign(::acdk::lang::inOf(c), castFlags); return this; }
  RDmiObject assign(IN(double) c, short castFlags = SVCastStdFlags) { _assign(c, castFlags); return this; }
  foreign RDmiObject assign(double* c, short castFlags = SVCastStdFlags) { _assign(::acdk::lang::inOf(c), castFlags); return this; }
  RDmiObject assign(IN(RObject) c, short castFlags = SVCastStdFlags) { _ensureAssignable(c, castFlags); *((ScriptVar*)this) = c; return this;  }
  foreign RDmiObject assign(RObject* c, short castFlags = SVCastStdFlags) { _ensureAssignable(*c, castFlags); *((ScriptVar*)this) = c; return this; }

  RDmiObject addition(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::addition(*other, castFlags)); }
  RDmiObject addition(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::addition(ScriptVar(other), castFlags)); }
  RDmiObject addition(IN(double) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::addition(ScriptVar(other), castFlags)); }
  RDmiObject operator+(IN(RDmiObject) other) { return addition(other); }
  RDmiObject operator+(IN(RString) other) { return new DmiObject(ScriptVar::addition(ScriptVar(&other))); }
  RDmiObject operator+(IN(RObject) other) { return new DmiObject(ScriptVar::addition(ScriptVar(&other))); }
  RDmiObject operator+(IN(jlong) other) { return addition(other); }
  RDmiObject operator+(IN(double) other) { return addition(other); }

  RDmiObject operator+() { return this; }

  RDmiObject subtraction(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::subtraction(*other, castFlags)); }
  RDmiObject subtraction(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::subtraction(ScriptVar(other), castFlags)); }
  RDmiObject subtraction(IN(double) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::subtraction(ScriptVar(other), castFlags)); }
  RDmiObject operator-(IN(RDmiObject) other) { return subtraction(other); }
  RDmiObject operator-(IN(RObject) other) { return new DmiObject(ScriptVar::subtraction(ScriptVar(other))); }
  RDmiObject operator-(IN(jlong) other) { return subtraction(other); }
  RDmiObject operator-(IN(double) other) { return subtraction(other); }

  RDmiObject operator-() { return new DmiObject(negation()); }

  RDmiObject multiply(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::multiply(*other, castFlags)); }
  RDmiObject multiply(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::multiply(ScriptVar(other), castFlags)); }
  RDmiObject multiply(IN(double) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::multiply(ScriptVar(other), castFlags)); }
  RDmiObject operator*(IN(RDmiObject) other) { return multiply(other); }
  RDmiObject operator*(IN(jlong) other) { return multiply(other); }
  RDmiObject operator*(IN(double) other) { return multiply(other); }
  RDmiObject operator*(IN(RObject) other) { return new DmiObject(ScriptVar::multiply(ScriptVar(other))); }

  RDmiObject divide(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::divide(*other, castFlags)); }
  RDmiObject divide(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::divide(ScriptVar(other), castFlags)); }
  RDmiObject divide(IN(double) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::divide(ScriptVar(other), castFlags)); }
  RDmiObject operator/(IN(RDmiObject) other) { return divide(other); }
  RDmiObject operator/(IN(jlong) other) { return divide(other); }
  RDmiObject operator/(IN(double) other) { return divide(other); }
  RDmiObject operator/(IN(RObject) other) { return new DmiObject(ScriptVar::divide(ScriptVar(other))); }

  RDmiObject modulo(IN(RDmiObject) other, short castFlags = SVCastStdFlags)  { return new DmiObject(ScriptVar::modulo(*other, castFlags)); }
  RDmiObject modulo(IN(jlong) other, short castFlags = SVCastStdFlags)  { return new DmiObject(ScriptVar::modulo(ScriptVar(other), castFlags)); }
  RDmiObject operator%(IN(RDmiObject) other)  { return modulo(other); }
  RDmiObject operator%(IN(jlong) other)  { return modulo(other); }
  RDmiObject operator%(IN(RObject) other)  { return new DmiObject(ScriptVar::modulo(ScriptVar(other))); }

  RDmiObject equal(IN(RDmiObject) other, short castFlags = SVCastStdFlags)  { return new DmiObject(ScriptVar::equal(*other, castFlags)); }
  RDmiObject equal(IN(jlong) other, short castFlags = SVCastStdFlags)  { return new DmiObject(ScriptVar::equal(ScriptVar(other), castFlags)); }
  RDmiObject equal(IN(double) other, short castFlags = SVCastStdFlags)  { return new DmiObject(ScriptVar::equal(ScriptVar(other), castFlags)); }
  RDmiObject equal(IN(bool) other, short castFlags = SVCastStdFlags)  { return new DmiObject(ScriptVar::equal(ScriptVar(other), castFlags)); }
  RDmiObject equal(IN(char) other, short castFlags = SVCastStdFlags)  { return new DmiObject(ScriptVar::equal(ScriptVar(other), castFlags)); }
  RDmiObject equal(IN(uc2char) other, short castFlags = SVCastStdFlags)  { return new DmiObject(ScriptVar::equal(ScriptVar(other), castFlags)); }
  RDmiObject equal(IN(RObject) other, short castFlags = SVCastStdFlags)  { return new DmiObject(ScriptVar::equal(ScriptVar(other), castFlags)); }

  RDmiObject operator==(IN(RDmiObject) other) { return equal(other); }
  RDmiObject operator==(IN(jlong) other) { return equal(other); }
  RDmiObject operator==(IN(double) other) { return equal(other); }
  RDmiObject operator==(IN(bool) other) { return equal(other); }
  RDmiObject operator==(IN(char) other) { return equal(other); }
  RDmiObject operator==(IN(uc2char) other) { return equal(other); }
  RDmiObject operator==(IN(RObject) other) { return equal(other); }

  RDmiObject not_equal(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::not_equal(*other, castFlags)); }
  RDmiObject not_equal(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::not_equal(ScriptVar(other), castFlags)); }
  RDmiObject not_equal(IN(double) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::not_equal(ScriptVar(other), castFlags)); }
  RDmiObject not_equal(IN(bool) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::not_equal(ScriptVar(other), castFlags)); }
  RDmiObject not_equal(IN(char) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::not_equal(ScriptVar(other), castFlags)); }
  RDmiObject not_equal(IN(uc2char) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::not_equal(ScriptVar(other), castFlags)); }
  RDmiObject operator!=(IN(RDmiObject) other) { return not_equal(other); }
  RDmiObject operator!=(IN(jlong) other) { return not_equal(other); }
  RDmiObject operator!=(IN(double) other) { return not_equal(other); }
  RDmiObject operator!=(IN(bool) other) { return not_equal(other); }
  RDmiObject operator!=(IN(char) other) { return not_equal(other); }
  RDmiObject operator!=(IN(uc2char) other) { return not_equal(other); }
  RDmiObject operator!=(IN(RObject) other) { return new DmiObject(ScriptVar::not_equal(ScriptVar(other))); }

  RDmiObject greater_than(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::greater_than(*other, castFlags)); }
  RDmiObject greater_than(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::greater_than(ScriptVar(other), castFlags)); }
  RDmiObject greater_than(IN(double) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::greater_than(ScriptVar(other), castFlags)); }
  RDmiObject operator>(IN(RDmiObject) other) { return greater_than(other); }
  RDmiObject operator>(IN(jlong) other) { return greater_than(other); }
  RDmiObject operator>(IN(double) other) { return greater_than(other); }
  RDmiObject operator>(IN(RObject) other) { return new DmiObject(ScriptVar::greater_than(ScriptVar(other))); }

  RDmiObject less_than(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::less_than(*other, castFlags)); }
  RDmiObject less_than(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::less_than(ScriptVar(other), castFlags)); }
  RDmiObject less_than(IN(double) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::less_than(ScriptVar(other), castFlags)); }
  RDmiObject operator<(IN(RDmiObject) other) { return less_than(other); }
  RDmiObject operator<(IN(jlong) other) { return less_than(other); }
  RDmiObject operator<(IN(double) other) { return less_than(other); }
  RDmiObject operator<(IN(RObject) other) { return new DmiObject(ScriptVar::less_than(ScriptVar(other))); }

  RDmiObject greater_or_equal(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::greater_or_equal(*other, castFlags)); }
  RDmiObject greater_or_equal(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::greater_or_equal(ScriptVar(other), castFlags)); }
  RDmiObject greater_or_equal(IN(double) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::greater_or_equal(ScriptVar(other), castFlags)); }
  RDmiObject operator>=(IN(RDmiObject) other) { return greater_or_equal(other); }
  RDmiObject operator>=(IN(jlong) other) { return greater_or_equal(other); }
  RDmiObject operator>=(IN(double) other) { return greater_or_equal(other); }
  RDmiObject operator>=(IN(RObject) other) { return new DmiObject(ScriptVar::greater_or_equal(ScriptVar(other))); }

  RDmiObject less_or_equal(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::less_or_equal(*other, castFlags)); }
  RDmiObject less_or_equal(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::less_or_equal(ScriptVar(other), castFlags)); }
  RDmiObject less_or_equal(IN(double) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::less_or_equal(ScriptVar(other), castFlags)); }
  RDmiObject operator<=(IN(RDmiObject) other) { return less_or_equal(other); }
  RDmiObject operator<=(IN(jlong) other) { return less_or_equal(other); }
  RDmiObject operator<=(IN(double) other) { return less_or_equal(other); }
  RDmiObject operator<=(IN(RObject) other) { return new DmiObject(ScriptVar::less_or_equal(ScriptVar(other))); }


  bool isTrue(short castFlags = SVCastStdFlags)  { return ScriptVar::isTrue(castFlags); }

  RDmiObject logical_and(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::logical_and(*other, castFlags)); }
  RDmiObject logical_and(IN(bool) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::logical_and(ScriptVar(other), castFlags)); }
  RDmiObject operator&&(IN(RDmiObject) other) { return logical_and(other); }
  RDmiObject operator&&(IN(bool) other) { return logical_and(other); }
  RDmiObject operator&&(IN(RObject) other) { return new DmiObject(ScriptVar::logical_and(ScriptVar(other))); }

  RDmiObject logical_not(short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::logical_not(castFlags)); }
  RDmiObject operator!() { return logical_not(); }

  RDmiObject logical_or(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::logical_or(*other, castFlags)); }
  RDmiObject logical_or(IN(bool) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::logical_or(ScriptVar(other), castFlags)); }
  RDmiObject operator||(IN(RDmiObject) other) { return logical_or(other); }
  RDmiObject operator||(IN(bool) other) { return logical_or(other); }
  RDmiObject operator||(IN(RObject) other) { return new DmiObject(ScriptVar::logical_or(ScriptVar(other))); }

  RDmiObject logical_xor(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::logical_xor(*other, castFlags)); }
  RDmiObject logical_xor(IN(bool) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::logical_xor(ScriptVar(other), castFlags)); }
  RDmiObject logical_xor(IN(RObject) other) { return new DmiObject(ScriptVar::logical_xor(ScriptVar(other))); }


  RDmiObject binary_and(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_and(*other, castFlags)); }
  RDmiObject binary_and(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_and(other, castFlags)); }
  
  RDmiObject operator&(IN(RDmiObject) other) { return binary_and(other); }
  RDmiObject operator&(IN(jlong) other) { return binary_and(other); }
  RDmiObject operator&(IN(RObject) other) { return new DmiObject(ScriptVar::binary_and(ScriptVar(other))); }

  RDmiObject binary_or(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_or(*other, castFlags)); }
  RDmiObject binary_or(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_or(ScriptVar(other), castFlags)); }
  RDmiObject operator|(IN(RDmiObject) other) { return binary_or(other); }
  RDmiObject operator|(IN(jlong) other) { return binary_or(other); }
  RDmiObject operator|(IN(RObject) other) { return new DmiObject(ScriptVar::binary_or(ScriptVar(other))); }

  RDmiObject binary_xor(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_xor(*other, castFlags)); }
  RDmiObject binary_xor(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_xor(other, castFlags)); }
  RDmiObject operator^(IN(RDmiObject) other) { return binary_xor(other); }
  RDmiObject operator^(IN(jlong) other) { return binary_xor(other); }
  RDmiObject operator^(IN(RObject) other) { return new DmiObject(ScriptVar::binary_xor(ScriptVar(other))); }

  RDmiObject binary_leftshift(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_leftshift(*other, castFlags)); }
  RDmiObject binary_leftshift(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_leftshift(other, castFlags)); }
  RDmiObject operator<<(IN(RDmiObject) other) { return binary_leftshift(other); }
  RDmiObject operator<<(IN(jlong) other) { return binary_leftshift(other); }
  RDmiObject operator<<(IN(RObject) other) { return new DmiObject(ScriptVar::binary_leftshift(ScriptVar(other))); }

  RDmiObject binary_rightshift(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_leftshift(*other, castFlags)); }
  RDmiObject binary_rightshift(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_leftshift(other, castFlags)); }
  RDmiObject operator>>(IN(RDmiObject) other) { return binary_rightshift(other); }
  RDmiObject operator>>(IN(jlong) other) { return binary_rightshift(other); }
  RDmiObject operator>>(IN(RObject) other) { return new DmiObject(ScriptVar::binary_rightshift(ScriptVar(other))); }

  RDmiObject binary_rightshift_unsigned(IN(RDmiObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_rightshift_unsigned(*other, castFlags)); }
  RDmiObject binary_rightshift_unsigned(IN(jlong) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_rightshift_unsigned(other, castFlags)); }
  RDmiObject binary_rightshift_unsigned(IN(RObject) other, short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_rightshift_unsigned(ScriptVar(other), castFlags)); }

  RDmiObject binary_not(short castFlags = SVCastStdFlags) { return new DmiObject(ScriptVar::binary_not()); }
  RDmiObject operator~() { return binary_not(); }

  RDmiObject increment();
  RDmiObject operator++() { return increment(); }

  RDmiObject increment(int);
  RDmiObject operator++(int x) { return increment(x); }

  RDmiObject decrement(short castFlags = SVCastStdFlags);
  RDmiObject operator--() { return decrement(); }
  RDmiObject decrement(int);
  RDmiObject operator--(int x) { return decrement(x); }
  /**
    must not be foreign, because AbstractObjectReader/Writer search for this
  */
  void writeObject(IN(::acdk::io::RObjectWriter) out, IN(RClass) cls)
  {
    if (cls == GetClass())
      out->writeScriptVar(*this, true, true);
  }
  /**
    must not be foreign
  */
  void readObject(IN(::acdk::io::RObjectReader) in, IN(RClass) cls)
  {
    if (cls == GetClass())
      *((ScriptVar*)this) = in->readScriptVar(true, true);
  }
  /**
    DmiObject forward function calls to underlying objects
  */
  foreign
  const acdk::lang::dmi::ClazzMethodInfo*
  standardDispatch(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret,
                                acdk::lang::dmi::ScriptVarArray& args,
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf/* = 0*/);
protected:
  foreign inline void _ensureAssignable(const ClazzInfo* ci, short castFlags)
  {
    const ClazzInfo* thisClazzInfo = ScriptVar::getClazzInfo();
    if (thisClazzInfo == 0)
      return;
    if (_clazzType->assignableFrom(thisClazzInfo) == true)
      return;
    THROW2(ClassCastException, ci, thisClazzInfo);
  }
  foreign inline void _ensureAssignable(IN(RObject) obj, short castFlags)
  {
    const ClazzInfo* thisClazzInfo = ScriptVar::getClazzInfo();
    
    if (thisClazzInfo == 0)
      return;
    if (obj == Nil)
    {
      if (thisClazzInfo->isBasicClazz() == true)
        THROW1(ClassCastException, "Cannot assign Nil to basic type");
    }
    else
    {
      if (thisClazzInfo->assignableFrom(obj->getClazzInfo()) == true)
        return;
      THROW2(ClassCastException, obj->getClazzInfo(), thisClazzInfo);
    }
  }
  foreign void _assign(const ScriptVar& other, short castFlags)
  {
    // if DmiObject is 'any' type allow assignment
    if (ScriptVar::_clazzType  == 0)
    {
       *((ScriptVar*)this) = other;
    }
    else
    {
      // else cast the type
      const ClazzInfo* thisClazzInfo = ScriptVar::getClazzInfo();
      *((ScriptVar*)this) = other._castScriptVar(thisClazzInfo, castFlags);
    }
  }
};


} // namespace dmi
} // namespace lang
} // namespace acdk

#include "DmiObjectArray.h"

#endif // acdk_lang_dmi_DmiObject_h

