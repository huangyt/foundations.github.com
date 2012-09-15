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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/PipedWriter.cpp,v 1.10 2005/03/08 12:45:36 kommer Exp $





#include <acdk.h>
#include "PipedWriter.h"
#include "PipedReader.h"

#include "IOException.h"
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace io {

PipedWriter::PipedWriter()
: AbstractStorageWriter(),
  _out(),
  _connected(false),
  _closed(false)
{
}

PipedWriter::PipedWriter(IN(RPipedReader) out)
: AbstractStorageWriter(),
  _out(),
  _connected(false),
  _closed(false)
{
  connect(out);
}

//virtual 
void 
PipedWriter::close()
{
  SYNCHRONIZETHIS();
  _closed = true;
  _out->close();
  notifyAll();
  // to avoid cyclic reference
  _out = Nil;
}

//virtual 
void 
PipedWriter::connect(IN(RPipedReader) out)
{
  if (out == _out)
    return;
  if (_connected == true)
    THROW1(IOException, "Already connected");
  if (_closed == true)
    THROW1(IOException, "Stream is closed and cannot be reopened");

  
  {  SYNCHRONIZETHIS();
    _out = out;
    _connected = true;
    _out->_in = this;
    _out->connect(this);
  } // synchronized

}

//virtual 
void 
PipedWriter::flush()
{

}

//virtual 
void 
PipedWriter::write(byte c)
{
  _out->pipewrite(c);
}

//virtual 
void 
PipedWriter::write(const byte* cstr, int offset, int len)
{
  _out->pipewrite(cstr, offset, len);
}

//virtual 
RString 
PipedWriter::getDeviceName()  
{ 
  THROW1(UnsupportedOperationException, "Not Implemented Yet");
  return "[PipedWriter]"; 
} 

//virtual 
bool 
PipedWriter::isWriteable()  
{ 
  THROW1(UnsupportedOperationException, "Not Implemented Yet");
  return false; 
} 

//virtual 
bool 
PipedWriter::isReadable()  
{ 
  THROW1(UnsupportedOperationException, "Not Implemented Yet");
  return false; 
}

} // io
} // acdk

