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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/JavaObjectReader.h,v 1.7 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_serialization_JavaObjectReader_h
#define acdk_java_serialization_JavaObjectReader_h

#include <acdk.h>
#include <acdk/io/BinaryObjectReader.h>

#include "Config.h"
#include "ClassDescription.h"

namespace acdk {
namespace java {
namespace serialization {


ACDK_DECL_CLASS(JavaObjectReadWriteCache);

enum 
{
  BaseWireHandle  = 0x7E0000
};

class JavaObjectReadWriteCache
: extends ::acdk::lang::Object
{
  RObjectArray _objects;
public:
  JavaObjectReadWriteCache()
  : Object()
  , _objects(new ObjectArray(0))
  {
  }
  int newObject(IN(RObject) obj)
  {
    _objects->append(obj);
    return _objects->length() - 1 + BaseWireHandle;
  }
  RObject get(int idx)
  {
    return _objects[idx - BaseWireHandle];
  }
  /**
    returns -1 if obj not found
  */
  int findObject(IN(RObject) obj)
  {
    for (int i = 0; i < _objects->length(); ++i)
    {
      if (_objects[i] == obj)
        return i;
    }
    return -1;
  }
  RObjectArray objects() { return _objects; }
};

ACDK_DECL_CLASS(JavaObjectReader);

class ACDK_JAVA_SERIALIZATION_PUBLIC JavaObjectReader
: extends ::acdk::io::BinaryObjectReader
{
protected:
  RJavaObjectReadWriteCache _prevObjects;
public:
  JavaObjectReader(IN(::acdk::io::RReader) in);
  foreign virtual RString readString();
  foreign virtual RObject readObject();
  
  /**
    creates an Object corresponding to 
    the javaclassname
  */
  RObject createObject(IN(RString) javaclassname, const ClassTypeMapping* ctm);
  /** 
    reads only ASCII subset of UTF 
  */
  RString readUtf();
  RString readLongUtf();
  RbyteArray readBlock();
  RClassDescription readClassDesc();
  RObject readObject2(int tp);
  int registerNewObject(IN(RObject) obj)
  {
    return _prevObjects->newObject(obj);
  }
protected:
  void _readStreamHeader();
};

} // namespace serialization
} // namespace java 
} // namespace acdk 

#endif //acdk_java_serialization_JavaObjectReader_h
