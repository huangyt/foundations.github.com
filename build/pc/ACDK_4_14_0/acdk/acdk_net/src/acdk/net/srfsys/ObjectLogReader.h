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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srfsys/ObjectLogReader.h,v 1.13 2005/03/14 17:27:28 kommer Exp $


#ifndef acdk_net_srsync_ObjectLogReader_h
#define acdk_net_srsync_ObjectLogReader_h

#include <acdk.h>
#include "Config.h"
#include <acdk/io/ObjectReader.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace net {
namespace srfsys {



/**
  ### move this class to acdk::io
*/
class ACDK_NET_SRFSYS_PUBLIC FilterObjectReader
: extends ::acdk::lang::Object
, implements ::acdk::io::ObjectReader
{
protected:
  ::acdk::io::RObjectReader _in;
public:
  FilterObjectReader(IN(::acdk::io::RObjectReader) in)
  : Object()
  , _in(in)
  {
  }

  // Reader
  virtual int available() { return _in->available(); }
  virtual void close() { _in->close(); }
  virtual jlong seek(SeekPos seekrel, jlong seekpos)  { return _in->seek(seekrel, seekpos); }
  virtual jlong skip(jlong n) { return _in->skip(n); }
  virtual int read() { return _in->read(); }
  virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1) { return _in->read(buffer, offset, len); }
  virtual int read(byte* buffer, int offset, int len) { return _in->read(buffer, offset, len); }
  virtual void mark(int readAheadLimit) { _in->mark(readAheadLimit); }
  virtual bool markSupported() { return _in->markSupported(); }
  virtual void reset() { _in->reset(); }
  

    // FilterReader
  virtual void setIn(IN(RReader) reader)
  {
    _in = (::acdk::io::RObjectReader) reader;
  }
  virtual RStorage getStorage() 
  {
    return _in->getStorage();
  }
  virtual RReader getStorageReader() { return _in->getStorageReader(); }
  // DataReader
  virtual bool readBoolean() { return _in->readBoolean(); }
  virtual char readChar() { return _in->readChar(); }
  virtual uc2char readUcChar() { return _in->readUcChar(); }
  virtual double readDouble()  { return _in->readDouble(); }
  virtual float readFloat()  { return _in->readFloat(); }
  virtual int readInt()  { return _in->readInt(); }
  virtual jlong readLong()  { return _in->readLong(); }
  virtual short readShort()  { return _in->readShort(); }
  virtual RString readString()  { return _in->readString(); }
  // ObjectReader
  virtual RObject readObject()  { return _in->readObject();  }
  virtual RObject readObject(IN(::acdk::lang::RClass) cls) { return _in->readObject(cls); }

  virtual RClass readClassDescriptor(IN(RClass) cls)  { return _in->readClassDescriptor(cls); }
  virtual void defaultReadObject(IN(RClass) cls, IN(RObject) obj) { defaultReadObject(cls, obj); }

  foreign virtual acdk::lang::dmi::ScriptVar readScriptVar(bool withTypeInfo = true, bool withFlags = true) 
  {
    return acdk::lang::dmi::ScriptVar();
  }
};


/**
  Little helper for exception safeness.
*/
struct BoolResetHelper
{
  bool& _b;
  bool _resetVal;
  BoolResetHelper(bool& b, bool initval = true, bool resetval = false)
    : _b(b)
    , _resetVal(resetval)
  {
    _b = initval;
  }
  ~BoolResetHelper()
  {
    _b = _resetVal;
  }
};

ACDK_DECL_CLASS(ObjectLogReader);

class ACDK_NET_SRFSYS_PUBLIC ObjectLogReader
: extends FilterObjectReader
{
  
  ::acdk::util::logging::RLogger _logger;
  bool _rec;
public:
  ObjectLogReader(IN(::acdk::io::RObjectReader) in, IN(::acdk::util::logging::RLogger) logger)
  : FilterObjectReader(in)
  , _logger(logger)
  , _rec(false)
  {
  }
  virtual RObject readObject()  
  { 
    if (_rec == true) 
      return _in->readObject();  
    BoolResetHelper _helper(_rec);
    
    RObject obj = _in->readObject();  
    print(obj);
    return obj;
  }
  virtual RObject readObject(IN(::acdk::lang::RClass) cls) 
  { 
    if (_rec == true) 
      return _in->readObject(cls);
    BoolResetHelper _helper(_rec);
    RObject obj = _in->readObject(cls);  
    print(obj);
    return obj;
  }
  void print(IN(RObject) obj)
  {
    if (_logger == Nil)
      return;
    _logger->log(::acdk::util::logging::Info, "", "Readed Message", 
                                              LOG_NPS(LOG_NPV("Device", &getStorage()->getDeviceName()) <<
                                              LOG_NPV("Message", &obj)), __FILE__, __LINE__);
  }
};

} // namespace srfsys
} // namespace net
} // namespace acdk 


#endif //acdk_net_srsync_ObjectLogReader_h
