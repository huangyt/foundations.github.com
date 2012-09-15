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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/IIOP/IIOP.h,v 1.7 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_GIOP_IIOP_h
#define org_omg_CORBA_GIOP_GIOP_h


namespace org {
namespace omg {
namespace CORBA {
  
/**
  Internet Inter Orb Protocoll
*/
namespace IIOP
{

class Version
{
public:
    octet major;
    octet minor;
    void read(::org::omg::CORBA::portable::InputStream& in)
    {
      major = in.read_octet();
      minor = in.read_octet();
    }
    void write(::org::omg::CORBA::portable::OutputStream& out)
    {
      out.write_octet(major);
      out.write_octet(minor);
    }
};

class ProfileBody
{
public:
  Version iiop_version;
  RString host;
  unsigned short port;
  sequence<octet> object_key;
  sequence<IOP::TaggedComponent> components;

  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    iiop_version.read(in);
    if (iiop_version.major != 1 || iiop_version.minor > 2)
      THROW1(Exception, RString("ProfileBody::read(). unexpexted version: ") + (int)iiop_version.major + RString(".") + (int)iiop_version.minor);
    
    host = in.read_string();
    port = in.read_short();
    IOP::read_octet_sequence(in, object_key);
    try {
      if (iiop_version.minor > 0) 
        IOP::read_t_sequence(in, components);
    } catch (::acdk::io::REOFException ) {
      // ignore this sitation
    }
    
  }
  void write(::org::omg::CORBA::portable::OutputStream& out)
  {
    iiop_version.write(out);
    out.write_string(host);
    out.write_short(port);
    IOP::write_octet_sequence(out, object_key);
    if (iiop_version.minor > 0)
       IOP::write_t_sequence(out, components);
  }
};

} // namespace IIOP
} // namespace CORBA
} // namespace omg
} // namespace org

#endif //org_omg_CORBA_GIOP_GIOP_h

