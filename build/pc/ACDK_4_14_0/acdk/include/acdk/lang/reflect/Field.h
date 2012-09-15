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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Field.h,v 1.18 2005/02/11 10:14:54 kommer Exp $

#ifndef acdk_lang_reflect_Field_h
#define acdk_lang_reflect_Field_h

namespace acdk {
namespace lang {
namespace reflect {


ACDK_DECL_CLASS(Field);

/**
  represent a field member of a class 
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CORE_PUBLIC Field
: extends AccessibleObject
, implements acdk::lang::dmi::MetaObject
{
  ACDK_WITH_METAINFO(Field)
private:
  foreign const acdk::lang::dmi::ClazzInfo* _clazz;
  foreign const acdk::lang::dmi::ClazzFieldInfo* _field;
public:
  /**
    Internal constructor
    @param clazz must not be 0
    @param field must not be 0
  */
  foreign Field(const acdk::lang::dmi::ClazzInfo* clazz, const acdk::lang::dmi::ClazzFieldInfo* field);
  
  /// implemented interface for MetaObject
  foreign virtual dmi::MetaInfo* getMetaInfo() { return (dmi::MetaInfo*)_field; }
  virtual RString toTypeString(int format = acdk::lang::dmi::TpFtFormatStandard) { return _field == 0 ? String::emptyString() : _field->toTypeString(format); }

  virtual bool equals(IN(RObject) obj);
  virtual RObject get(IN(RObject) obj, int accessFlags = dmi::MiPublic);
  virtual bool getBoolean(IN(RObject) obj, int accessFlags = dmi::MiPublic);
  virtual byte getByte(IN(RObject) obj, int accessFlags = dmi::MiPublic);
  virtual char getChar(IN(RObject) obj, int accessFlags = dmi::MiPublic);
  virtual ucchar getUcChar(IN(RObject) obj, int accessFlags = dmi::MiPublic);
  virtual RClass getDeclaringClass();
  virtual double getDouble(IN(RObject) obj, int accessFlags = dmi::MiPublic);
  virtual float getFloat(IN(RObject) obj, int accessFlags = dmi::MiPublic);
  virtual int getInt(IN(RObject) obj, int accessFlags = dmi::MiPublic);
  virtual jlong getLong(IN(RObject) obj, int accessFlags = dmi::MiPublic);
  virtual int getModifiers();
  virtual RString getName();
  virtual short getShort(IN(RObject) obj, int accessFlags = dmi::MiPublic);
  virtual RClass getType();
  virtual int hashCode();
  virtual void set(IN(RObject) obj, IN(RObject) value, int accessFlags = dmi::MiPublic);
  virtual void setBoolean(IN(RObject) obj, bool v, int accessFlags = dmi::MiPublic);
  virtual void setByte(IN(RObject) obj, byte b, int accessFlags = dmi::MiPublic);
  virtual void setChar(IN(RObject) obj, char c, int accessFlags = dmi::MiPublic);
  virtual void setUcChar(IN(RObject) obj, ucchar c, int accessFlags = dmi::MiPublic);
  virtual void setDouble(IN(RObject) obj, double d, int accessFlags = dmi::MiPublic);
  virtual void setFloat(IN(RObject) obj, float f, int accessFlags = dmi::MiPublic);
  virtual void setInt(IN(RObject) obj, int i, int accessFlags = dmi::MiPublic);
  virtual void setLong(IN(RObject) obj, jlong l, int accessFlags = dmi::MiPublic);
  virtual void setShort(IN(RObject) obj, short s, int accessFlags = dmi::MiPublic);
  foreign virtual RString toString();
  /** 
    ACDK defined, return wrapped Object with current field implementation 
  */
  RObject get(int accessFlags = dmi::MiPublic);
  foreign const acdk::lang::dmi::ClazzFieldInfo* clazzField() { return _field; }
  foreign const acdk::lang::dmi::ClazzInfo* clazz() { return _clazz; }
private:
  bool _isStatic();

};

} // reflect
} // lang
} // acdk

#endif //acdk_lang_reflect_Field_h

