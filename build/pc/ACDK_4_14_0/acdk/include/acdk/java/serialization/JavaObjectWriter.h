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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/JavaObjectWriter.h,v 1.9 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_serialization_JavaObjectWriter_h
#define acdk_java_serialization_JavaObjectWriter_h

#include <acdk.h>
#include <acdk/io/BinaryObjectWriter.h>

#include "Config.h"
#include "JavaObjectReader.h"

namespace acdk {
namespace java {
namespace serialization {

class ACDK_JAVA_SERIALIZATION_PUBLIC JavaObjectWriter
: extends ::acdk::io::BinaryObjectWriter
{
  RJavaObjectReadWriteCache _prevObjects;
public:
  JavaObjectWriter(IN(::acdk::io::RWriter) out);

  void writeObject(IN(RObject) obj);
  void writeString(IN(RString) str);
  /**
    Write with TC_STRING or TC_LONGSTRING signature
  */
  void writeUtf(IN(RString) str);
  /**
    Writes without signature
  */
  void writeShortUtf(IN(RString) str);
  void writeBlock(IN(RbyteArray) block);
  void writeClassDesc(IN(RClassDescription) cdesc);

  int registerNewObject(IN(RObject) obj)
  {
    return _prevObjects->newObject(obj);
  }
  /**
    return -1 if Object not found
  */
  int findPrevObject(IN(RObject) obj)
  {
    return _prevObjects->findObject(obj);
  }
  RClassDescription getClassDescr(const ClassTypeMapping* ctm, IN(RClass) cls);
protected:
  void _writeStreamHeader();
};

} // namespace serialization
} // namespace java 
} // namespace acdk 

#endif //acdk_java_serialization_JavaObjectWriter_h
