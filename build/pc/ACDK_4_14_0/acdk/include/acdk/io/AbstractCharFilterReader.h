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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractCharFilterReader.h,v 1.8 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_AbstractCharFilterReader_h
#define acdk_io_AbstractCharFilterReader_h

#include "AbstractCharReader.h"

namespace acdk {
namespace io {



ACDK_DECL_CLASS(AbstractCharFilterReader); 

/** 
  A filter interface for other CharReader.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/04/09 19:26:44 $
*/
class ACDK_CORE_PUBLIC AbstractCharFilterReader 
: extends AbstractCharReader
{
  ACDK_WITH_METAINFO(AbstractCharFilterReader)
protected:
  RCharReader _in;
public :
  AbstractCharFilterReader(IN(RCharReader) in, IN(RObject) iolock = Nil) 
  : AbstractCharReader(iolock)
  , _in(in)
  {
  }
  //foreign virtual acdk::locale::RDecoder getDecoder() { return _in->getDecoder(); }
  //foreign virtual void setDecoder(IN(acdk::locale::RDecoder) decoder) { _in->setDecoder(decoder); }
  virtual int readChar() { return _in->readChar(); }
  virtual RString readString() { return _in->readString(); }
  virtual void close() { _in->close(); }
  RCharReader getIn() { return _in; }
  virtual RReader getReader(IN(acdk::locale::REncoder) encoder = Nil) { return _in->getReader(encoder); }
  
};


} // namespace io 
} // namespace acdk 

#endif //acdk_io_AbstractCharFilterReader_h


