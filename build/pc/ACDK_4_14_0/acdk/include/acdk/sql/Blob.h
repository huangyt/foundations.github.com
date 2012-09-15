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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/Blob.h,v 1.1 2005/04/05 23:21:01 kommer Exp $

#ifndef acdk_sql_Blob_h
#define acdk_sql_Blob_h

#include "sql.h"
#include "SQLException.h"

#include <acdk/lang/ByteBuffer.h>
#include <acdk/lang/Integer.h>


namespace acdk {
namespace sql {


ACDK_DECL_INTERFACE(Blob);

/**
  similar to corresponding JDBC class
*/
class ACDK_SQL_PUBLIC Blob
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Blob)
public:
  virtual jlong length() THROWS1(RException) = 0;
  virtual void truncate(jlong length) THROWS1(RException) = 0;
  virtual acdk::io::RReader getReader() THROWS1(RException) = 0;
  virtual acdk::io::RWriter getWriter() THROWS1(RException) = 0;
  /**
    return the readed bytes
  */
  virtual RReadByteBuffer getReadByteBuffer() THROWS1(RException) = 0;
  /** 
    copy byte into the Blob
  */
  virtual void setByteBuffer(IN(RReadByteBuffer) buffer) THROWS1(RException) = 0;
};

ACDK_DECL_CLASS(StandardMemBlob);

/**
  implement Blob with ByteBuffer
*/
class ACDK_SQL_PUBLIC StandardMemBlob
: extends acdk::lang::Object
, implements Blob
{
  ACDK_WITH_METAINFO(StandardMemBlob)
protected:
  acdk::lang::RFlexByteBuffer _coreBuffer;
public:
  StandardMemBlob(IN(acdk::lang::RFlexByteBuffer) buffer)
    : _coreBuffer(buffer)
  {}
  StandardMemBlob()
  : _coreBuffer(new CoreByteBuffer(0))
  {}
  virtual jlong length() THROWS1(RException) { return _coreBuffer->length(); }
  virtual void truncate(jlong length) THROWS1(RException) 
  {
    if (length > Integer::MAX_VALUE)
      THROW1(SQLException, SBSTR("StandardMemBlob supports only up to " << Integer::MAX_VALUE << " bytes"));
    _coreBuffer->resize(int(length));
  }
  virtual acdk::io::RReader getReader() THROWS1(RException)
  {
    return Buffers::getReader(&_coreBuffer);
  }

  virtual acdk::io::RWriter getWriter() THROWS1(RException)
  {
    return Buffers::getAppendWriter(&_coreBuffer);
  }

  /**
    return the readed bytes
  */
  virtual RReadByteBuffer getReadByteBuffer() THROWS1(RException) { return &_coreBuffer; }
  /** 
    copy byte into the Blob
  */
  virtual void setByteBuffer(IN(RReadByteBuffer) buffer) THROWS1(RException) 
  {
    _coreBuffer->resize(buffer->length());
    Buffers::copyBuffer(buffer, &_coreBuffer);
  }
};


} // sql
} // acdk

#endif //acdk_sql_Blob_h

