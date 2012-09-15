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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/GIOP/GIOP.cpp,v 1.7 2005/02/05 10:45:41 kommer Exp $


#include <org/omg/CORBA/GIOP/GIOP.h>

namespace org {
namespace omg {
namespace CORBA {
namespace GIOP {

void 
MessageHeader::write(::org::omg::CORBA::portable::OutputStream& out)
{
  out.write_char_array(magic, 0, 4);
  version.write(out);
  out.write_octet(flag);

  out.write_octet(message_type);
  out.write_long(message_size); // should be ajusted later
}

void 
MessageHeader::read(::org::omg::CORBA::portable::InputStream& in)
{
  in.read_char_array(magic, 0, 4); 
  flag = 0;
  if (magic[0] != 'G' || magic[1] != 'I' || magic[2] != 'O' || magic[3] != 'P')
    THROW3(COMM_FAILURE, "broken GIOPMessage (not magic)", 0, COMPLETED_NO);
  
  version.major = in.read_octet();
  version.minor = in.read_octet();
  if (version.major != 1)
    THROW3(COMM_FAILURE, "unsupported GIOP version", 0, COMPLETED_NO);

  if (version.major > 2)
    THROW3(COMM_FAILURE, "unsupported GIOP version", 0, COMPLETED_NO);

  if (version.minor == 0) 
  {
    if (in.read_boolean() == true)
      flag |= 1;
  } 
  else
  {
    flag = in.read_octet();
  }
  org::omg::CORBA::portable::Endian endian = org::omg::CORBA::portable::BigEndian;
  if (flag & 1)
    endian = org::omg::CORBA::portable::LittleEndian;

  in.setEndian(endian);

  message_type = in.read_octet();
  message_size = in.read_long();
}

} // namespace GIOP
} // namespace CORBA
} // namespace omg
} // namespace org
