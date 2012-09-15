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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srfsys/ObjectLogWriter.h,v 1.13 2005/03/14 17:27:28 kommer Exp $


#ifndef acdk_net_srsync_ObjectLogWriter_h
#define acdk_net_srsync_ObjectLogWriter_h

#include <acdk.h>
#include "Config.h"
#include <acdk/io/ObjectWriter.h>
#include "ObjectLogReader.h"

namespace acdk {
namespace net {
namespace srfsys {



/**
  ### move this class to acdk::io
*/
class ACDK_NET_SRFSYS_PUBLIC FilterObjectWriter
: extends ::acdk::lang::Object
, implements ::acdk::io::ObjectWriter
{
protected:
  ::acdk::io::RObjectWriter _out;
public:
  FilterObjectWriter(IN(::acdk::io::RObjectWriter) out)
  : Object()
  , _out(out)
  {
  }

  // Writer
  virtual void flush() { _out->flush(); }
  virtual void close() { _out->close(); }
  virtual void write(const byte* cstr, int offset, int len) { _out->write(cstr, offset, len); }
  virtual void write(byte c) { _out->write(&c, 0, 1); }
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1) { _out->write(ch, offset, len); }
  
  
  // FilterWriter
  virtual void setOut(IN(RWriter) writer)
  {
    _out = (::acdk::io::RObjectWriter)writer;
  }
  virtual RStorage getStorage()  { return _out->getStorage(); }
  virtual RWriter getStorageWriter() { return _out->getStorageWriter(); }

  // DataWriter
  virtual void writeBoolean(bool b) { _out->writeBoolean(b); }
  virtual void writeChar(char b)  { _out->writeChar(b); }
  virtual void writeUcChar(uc2char b)  { _out->writeUcChar(b); }
  virtual void writeShort(short b) { _out->writeShort(b); }
  virtual void writeInt(int b) { _out->writeInt(b); }
  virtual void writeLong(jlong b) { _out->writeLong(b); }
  virtual void writeFloat(float b) { _out->writeFloat(b); }
  virtual void writeDouble(double b) { _out->writeDouble(b); }

  
  virtual void writeString(IN(RString) str) { _out->writeString(str); }
  virtual void writeOpaque(IN(RbyteArray) array) { _out->writeOpaque(array); }
  // ObjectWriter
  virtual void writeObject(IN(RObject) obj)  { _out->writeObject(obj); }
  virtual void writeObject(IN(RClass) cls, IN(RObject) obj)  { _out->writeObject(cls, obj); }
  virtual void defaultWriteObject(IN(RClass) cls, IN(RObject) obj) { _out->defaultWriteObject(cls, obj); }
  virtual void writeUnshared(IN(RClass) cls, IN(RObject) obj) { _out->writeUnshared(cls, obj); }
  virtual void writeClassDescriptor(IN(RClass) cls, IN(RObject) obj) { _out->writeClassDescriptor(cls, obj); }
  foreign virtual void writeScriptVar(acdk::lang::dmi::ScriptVar& sv, bool withTypeInfo = true, bool withFlags = true)
  {
    // nothing
  }
};


ACDK_DECL_CLASS(ObjectLogWriter);

class ACDK_NET_SRFSYS_PUBLIC ObjectLogWriter
: extends FilterObjectWriter
{
  ::acdk::util::logging::RLogger _logger;
  bool _rec;
public:
  ObjectLogWriter(IN(::acdk::io::RObjectWriter) out, IN(::acdk::util::logging::RLogger) logger)
  : FilterObjectWriter(out)
  , _logger(logger)
  , _rec(false)
  {
  }
  virtual void writeObject(IN(RObject) obj)  
  { 
    if (_rec == false) 
      print(obj);
    BoolResetHelper _helper(_rec);
    FilterObjectWriter::writeObject(obj);
  }
  virtual void writeObject(IN(RClass) cls, IN(RObject) obj)  
  { 
    if (_rec == false) 
      print(obj);
    BoolResetHelper _helper(_rec);
    FilterObjectWriter::writeObject(cls, obj);
  }
  void print(IN(RObject) obj)
  {
    if (_logger == Nil)
      return;
    _logger->log(::acdk::util::logging::Info, "", "Wrote Message", LOG_NPS(LOG_NPV("Device", &getStorage()->getDeviceName()) <<
                                                               LOG_NPV("Message", &obj)), __FILE__, __LINE__);
  }
};

} // namespace srfsys
} // namespace net
} // namespace acdk 


#endif //acdk_net_srsync_ObjectLogWriter_h
