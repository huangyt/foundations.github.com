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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractCharFilterWriter.h,v 1.9 2005/03/01 10:12:47 kommer Exp $

#ifndef acdk_io_AbstractCharFilterWriter_h
#define acdk_io_AbstractCharFilterWriter_h

#include "AbstractCharWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(AbstractCharFilterWriter);

/** 
  Abstract filter for CharWriter

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/03/01 10:12:47 $
*/
class ACDK_CORE_PUBLIC AbstractCharFilterWriter
: extends AbstractCharWriter
{
  ACDK_WITH_METAINFO(AbstractCharFilterWriter)
protected:
  RCharWriter _out;
public :
  AbstractCharFilterWriter(IN(RCharWriter) out, IN(RObject) iolock = Nil) 
  : AbstractCharWriter(iolock)
  , _out(out)
  {
  }
  
  virtual void writeChar(char c) { _out->writeChar(c); }
  virtual void writeChar(ucchar c) { _out->writeChar(c); }
  virtual void writeString(const char* cstr) { _out->writeString(cstr); }
  virtual void writeString(const ucchar* cstr) { _out->writeString(cstr); }
  virtual void writeString(IN(RString) str) { _out->writeString(str); }
  virtual void flush() { _out->flush(); }
  virtual void close() { _out->close(); }

  virtual RWriter getWriter(IN(acdk::locale::RDecoder) decoder = Nil)  { return _out->getWriter(decoder); }
  RCharWriter getOut() { return _out; }
  void setOut(IN(RCharWriter) out) { _out = out; }
  
};

} // io
} // acdk

#endif //acdk_io_AbstractCharFilterWriter_h

