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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/OrbOutputStream.cpp,v 1.8 2005/02/05 10:45:39 kommer Exp $


#include "OrbOutputStream.h"
#include <org/omg/CORBA/OrbExceptions.h>
#include <acdk/util/logging/Log.h>
#include <acdk/text/Format.h>

namespace acdkx {
namespace orb {

//static
void 
OrbOutputStream::setMessageSize(byte* buffer, int size)
{
  org::omg::CORBA::GIOP::MessageHeader* msgheader = reinterpret_cast<org::omg::CORBA::GIOP::MessageHeader*>(buffer);
  CDRSWAP4(size);
  msgheader->message_size = size;
  /*
  byte* buffptr = buffer + 8; // the position of size
  int* iptr = (int*)buffptr;
  *iptr = size;
  */
}

void 
OrbOutputStream::sendMessage()
{
  ::acdk::io::RMemWriter memw(getStorageWriter());
  RbyteArray buffer = memw->getBuffer();
  setMessageSize(buffer->data(), buffer->length() - sizeof(::org::omg::CORBA::GIOP::MessageHeader));
  {
    SYNCOBJECT(_realOut);
    ACDK_LOG(Debug, "GIOP Message written: \n" + acdk::text::Format::dumpbin(buffer, 16) + "\n");
    _realOut->write(buffer);
  }
}

} // namespace orb 
} // namespace acdkx 



