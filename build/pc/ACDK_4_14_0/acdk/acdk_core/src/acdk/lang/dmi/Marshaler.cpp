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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/Marshaler.cpp,v 1.7 2005/02/05 10:44:58 kommer Exp $


#include <acdk.h>
#include "Marshaler.h"
#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>
#include <acdk/io/BinaryObjectWriter.h>
#include <acdk/io/BinaryObjectReader.h>

namespace acdk {
namespace lang {
namespace dmi {

//virtual 
void 
StandardMarshaler::marshal(IN(RClass) cls, IN(RObject) obj, byteArray& outstore, MarshalMode mode/* = Unknown*/)
{
  if (mode == SendReceiveClient || mode == SendReceiveServer) {
    _obj = obj;
    return;
  }
  ::acdk::io::MemWriter car(&outstore);
  ::acdk::io::BinaryObjectWriter bow((::acdk::io::RWriter)&car);
  bow.writeObject(obj);
  //RbyteArray erg = car.getBuffer();
  //outstore.resize(erg.length());
  //System::arraycopy(erg, 0, &outstore, 0, erg.length());
  //System::out->println("marshal: " + outstore.toString());
}

//virtual 
RObject 
StandardMarshaler::unmarshal(IN(RClass) cls, byteArray& instore, MarshalMode mode/* = Unknown*/, IN(RObject) cachedObject/* = Nil*/)
{
  if (mode == SendReceiveClient || mode == SendReceiveServer) 
    return _obj;
  ::acdk::io::MemReader cir((RbyteArray)&instore);
  ::acdk::io::BinaryObjectReader bor((::acdk::io::RReader)&cir);
  //System::out->println("unmarshal");
  return bor.readObject();
}

StandardMarshaler StandardMarshaler::gInstance;


} // namespace dmi
} // namespace lang
} // namespace acdk




