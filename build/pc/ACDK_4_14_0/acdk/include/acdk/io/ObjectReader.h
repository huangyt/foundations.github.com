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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ObjectReader.h,v 1.20 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_ObjectReader_h
#define acdk_io_ObjectReader_h

#include <acdk/io/DataReader.h>
#include "SerializedObjectDescriptor.h"

namespace acdk {
namespace io {

using namespace acdk::lang;


ACDK_DECL_INTERFACE(ObjectWriter);


/**
  Interface for reading basic and object types.
  API: ACDK<br/>
  See also: gw_ref[acdk_hb_mi_serialization].

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.20 $
  @date $Date: 2005/04/09 19:26:45 $
*/
ACDK_INTERFACE class ACDK_CORE_PUBLIC ObjectReader
: implements DataReader
{
  ACDK_WITH_METAINFO(ObjectReader)
public:
   // from Reader
  overwrite jlong seek(SeekPos seekrel, jlong seekpos) = 0;
  overwrite void reset() = 0;
  overwrite jlong skip(jlong n) = 0;

  // from FilterReader
  overwrite void setIn(IN(RReader) reader) = 0;
  overwrite RStorage getStorage() = 0;
  overwrite RReader getStorageReader() = 0;

  overwrite bool readBoolean() = 0;
  overwrite char readChar() = 0;
  overwrite uc2char readUcChar() = 0;
  overwrite double readDouble() = 0;
  overwrite float readFloat() = 0;
  overwrite int readInt() = 0;
  overwrite jlong readLong() = 0;
  overwrite short readShort() = 0;
  overwrite RString readString() = 0;

  /**
    in this Version the stream itself has to store meta information
    about the class of the object.
  */
  virtual RObject readObject() = 0;
  /**
    In this version, the stream may not store meta information
    about the classes
  */
  virtual RObject readObject(IN(::acdk::lang::RClass) cls) = 0;
  /**
    @param cls may be Nul
    @return if the readClassDescriptor just return param cls
  */
  virtual RClass readClassDescriptor(IN(RClass) cls) = 0;
  /**
    default method for reading objects data
    only fields of current class are read.
  */
  virtual void defaultReadObject(IN(RClass) cls, IN(RObject) obj) = 0;
  /**
    @param withTypeInfo the the is encoded in the stream
    @param withFlags  writes also the flags
  */
  foreign virtual acdk::lang::dmi::ScriptVar readScriptVar(bool withTypeInfo = true, bool withFlags = true) { return acdk::lang::dmi::ScriptVar(); }
};


typedef ObjectReader ObjectInput;
typedef RObjectReader RObjectInput;

#ifndef DOXYGENONLY

/**
  cannot implement in ObjectArrayImpl itself cause forward decl
  @internal
*/
template <class T>
inline void readObjectArray(ObjectArrayImpl<T>* This, ::acdk::io::RObjectReader in)
{
  int size = in->readInt();
  This->resize(size);
  for (int i = 0; i < size; i++)
    This->set(i, (T)in->readObject(::acdk::lang::Class::getSingeltonClass(T::clazzInfo())));
}

/**
  cannot implement in BasicArrayImpl itself cause forward decl
  @internal
*/
template <class T>
inline void readBasicArray(BasicArray<T>* This, IN(::acdk::io::RObjectReader) in)
{
  int size = in->readInt();
  This->resize(size);
  if (typeid(T) == typeid(bool)) {
    for (int i = 0; i < size; i++)
      This->set(i, in->readBoolean());
  } else if (typeid(T) == typeid(char)) {
    for (int i = 0; i < size; i++)
      This->set(i, in->readChar());
  } else if  (typeid(T) == typeid(byte)) {
    for (int i = 0; i < size; i++)
      This->set(i, (byte)in->readChar());
  } else if (typeid(T) == typeid(short)) {
    for (int i = 0; i < size; i++)
      This->set(i, in->readShort());
  } else if (typeid(T) == typeid(int)) {
    for (int i = 0; i < size; i++)
      This->set(i, in->readInt());
  } else if (typeid(T) == typeid(jlong)) {
    for (int i = 0; i < size; i++)
      This->set(i, in->readLong());
#if !defined(__GNUC__) // avoid waring, will be implemented via template specialization
  } else if (typeid(T) == typeid(float)) {
    for (int i = 0; i < size; i++)
      This->set(i, in->readFloat());
  } else if (typeid(T) == typeid(double)) {
    for (int i = 0; i < size; i++)
      This->set(i, in->readDouble());
#endif
  }
}

/// @internal
template <>
inline void readBasicArray(BasicArray<char>* This, IN(::acdk::io::RObjectReader) in)
{
  int size = in->readInt();
  This->resize(size);
  for (int i = 0; i < size; i++)
    This->set(i, in->readChar());
}

/// @internal
template <>
inline void readBasicArray(BasicArray<uc2char>* This, IN(::acdk::io::RObjectReader) in)
{
  int size = in->readInt();
  This->resize(size);
  for (int i = 0; i < size; i++)
    This->set(i, in->readShort());
}

/// @internal
template <>
inline void readBasicArray(BasicArray<bool>* This, IN(::acdk::io::RObjectReader) in)
{
  int size = in->readInt();
  This->resize(size);
  for (int i = 0; i < size; i++)
    This->set(i, in->readBoolean());
}

/// @internal
template <>
inline void readBasicArray<byte>(BasicArray<byte>* This, IN(::acdk::io::RObjectReader) in)
{
  int size = in->readInt();
  This->resize(size);
  for (int i = 0; i < size; i++)
    This->set(i, (byte)in->readChar());
}

/// @internal
template <>
inline void readBasicArray(BasicArray<short>* This, IN(::acdk::io::RObjectReader) in)
{
  int size = in->readInt();
  This->resize(size);
  for (int i = 0; i < size; i++)
    This->set(i, in->readShort());
}

/// @internal
template <>
inline void readBasicArray(BasicArray<int>* This, IN(::acdk::io::RObjectReader) in)
{
  int size = in->readInt();
  This->resize(size);
  for (int i = 0; i < size; i++)
    This->set(i, in->readInt());
}

/// @internal
template <>
inline void readBasicArray(BasicArray<jlong>* This, IN(::acdk::io::RObjectReader) in)
{
#ifndef __BORLANDC__
  int size = in->readInt();
  This->resize(size);
  for (int i = 0; i < size; i++)
  {
    This->set(i, in->readLong());
  }
#endif //__BORLANDC__
}

/// @internal
template <>
inline void readBasicArray(BasicArray<float>* This, IN(::acdk::io::RObjectReader) in)
{
  int size = in->readInt();
  This->resize(size);
  for (int i = 0; i < size; i++)
    This->set(i, in->readFloat());
}

/// @internal
template <>
inline void readBasicArray(BasicArray<double>* This, IN(::acdk::io::RObjectReader) in)
{
  int size = in->readInt();
  This->resize(size);
  for (int i = 0; i < size; i++)
    This->set(i, in->readDouble());
}

#endif //#ifndef DOXYGENONLY

} // io
} // acdk

#endif //acdk_io_ObjectReader_h

