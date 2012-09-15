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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/SerializedObjectDescriptor.h,v 1.3 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_SerializedObjectDescriptor_h
#define acdk_io_SerializedObjectDescriptor_h

#include <acdk/io/DataReader.h>


namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(SerializedFieldDescriptor);

/**
  used to describe a serialized class.
  @see gw_ref[acdk_hb_mi_serialization].
*/
class ACDK_CORE_PUBLIC SerializedFieldDescriptor
: extends acdk::lang::Object
, implements acdk::io::Serializable
, implements acdk::lang::Comparable
{
  ACDK_WITH_METAINFO(SerializedFieldDescriptor)
protected:
  RString name;
  transient RClass typeClass;
  RString type;
  bool unshared;
  
public:
  SerializedFieldDescriptor(IN(RString) nam, IN(RClass) cls, bool unshar = false)
    : name(nam)
    , typeClass(cls)
    , type(cls->getName())
    , unshared(unshar)
  {
  }
  virtual int compareTo(IN(RObject) obj)
  {
    return compareTo((RSerializedFieldDescriptor)obj);
  }
  virtual int compareTo(IN(RSerializedFieldDescriptor) other)
  {
    return getName()->compareTo(other->getName());
  }
  virtual RString getName() { return name; }
          
  virtual RClass getType() { return typeClass; }
  virtual char getTypeCode() { return getTypeString()->charAt(0); }
  virtual RString getTypeString() { return getType()->getName(); }
  
  virtual bool isPrimitive() { return typeClass->isPrimitive(); }
  virtual bool isUnshared() { return unshared; }
  RString toString() { return getType()->toString() + " " + getName(); }


};


ACDK_DECL_CLASS(SerializedObjectDescriptor);

/**
  Similar to ObjectStreamClass in Java.
  API: ACDK<br/>
  @see gw_ref[acdk_hb_mi_serialization].

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/04/09 19:26:45 $
*/
class ACDK_CORE_PUBLIC SerializedObjectDescriptor
: extends acdk::lang::Object
, implements Serializable
{
  ACDK_WITH_METAINFO(SerializedObjectDescriptor)
protected:
  RString name;
  transient RClass _class;
  RSerializedFieldDescriptorArray fields;
  jlong _serializedVersion;
  SerializedObjectDescriptor(IN(RClass) cls);
public:

  static RSerializedObjectDescriptor lookup(IN(RClass) cl);

  virtual RString getName() { return name; }
  virtual RClass forClass() { return _class; }
  virtual RSerializedFieldDescriptorArray getFields() { return fields; }
  virtual jlong getSerialVersionUID() { return _serializedVersion; }
	virtual RString toString();

};

} // io
} // acdk

#endif //acdk_io_SerializedObjectDescriptor_h

