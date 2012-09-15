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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/IOP/IOP.h,v 1.8 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_GIOP_IOP_h
#define org_omg_CORBA_GIOP_IOP_h

#include <org/omg/CORBA/CORBA.h>
#include <org/omg/CORBA/portable/InputStream.h>
#include <org/omg/CORBA/portable/OutputStream.h>

namespace org {
namespace omg {
namespace CORBA {
  /** 
  Inter Orb Protokoll
  */
namespace IOP {

typedef unsigned long  ProfileId;
const ProfileId	 TAG_INTERNET_IOP = 0;
const ProfileId	 TAG_MULTIPLE_COMPONENTS = 1;


inline void read_octet_sequence(::org::omg::CORBA::portable::InputStream& in, sequence<octet>& data)
{
  int size = in.read_long();
  for (int i = 0; i < size; i++)
    data.push_back(in.read_octet());
}

inline void write_octet_sequence(::org::omg::CORBA::portable::OutputStream& out, sequence<octet>& data)
{
  int size = data.size();
  out.write_long(size);
  for (int i = 0; i < size; i++)
    out.write_octet(data[i]);
}


/** T must provide read(::org::omg::CORBA::portable::InputStream& in) method */
template <class T> 
void 
read_t_sequence(::org::omg::CORBA::portable::InputStream& in, sequence<T>& data)
{
  int size = in.read_long();
  data.ensureSize(size);
  for (int i = 0; i < size; i++) {
    T t; t.read(in);
    data[i] = t;
  }
}

/** T must provide write(::org::omg::CORBA::portable::OutputStream& out) method */
template <class T> 
void 
write_t_sequence(::org::omg::CORBA::portable::OutputStream& out, sequence<T>& data)
{
  int size = data.size();
  out.write_long(size);
  for (int i = 0; i < size; i++) {
    data[i].write(out);
  }
}

class TaggedProfile 
{
public:
  ProfileId  tag;
	sequence<octet> profile_data;
  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    tag = in.read_long();
    read_octet_sequence(in, profile_data);
  }
  void write(::org::omg::CORBA::portable::OutputStream& out)
  {
    out.write_long(tag);
    write_octet_sequence(out, profile_data);
  }
};

class IOR 
{
public:
  RString	type_id;
	sequence <TaggedProfile> profiles;
  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    type_id = in.read_string();
    read_t_sequence(in, profiles);
  }
  void write(::org::omg::CORBA::portable::OutputStream& out)
  {
    out.write_string(type_id);
    write_t_sequence(out, profiles);
  }
};

typedef unsigned long ComponentId;

const ComponentId TAG_ORB_TYPE = 0;
const ComponentId TAG_CODE_SETS= 1;
const ComponentId TAG_POLICIES= 2;
const ComponentId TAG_ALTERNATIVE_IIOP_ADDRESS = 3;
const ComponentId TAG_ASSOCIATION_OPTIONS = 13;
const ComponentId TAG_SEC_NAME = 14;
// ... others

class TaggedComponent 
{
public:
  ComponentId  tag;
	sequence <octet> component_data;
  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    tag = in.read_long();
    read_octet_sequence(in, component_data);
  }
  void write(::org::omg::CORBA::portable::OutputStream& out)
  {
    out.write_long(tag );
    write_octet_sequence(out, component_data);
  }
};

typedef sequence <TaggedComponent> MultipleComponentProfile;

typedef unsigned long ServiceId;

class ServiceContext 
{
public:
  ServiceId context_id;
  sequence <octet> context_data;
  void read(::org::omg::CORBA::portable::InputStream& in)
  {
    context_id = in.read_long();
    read_octet_sequence(in, context_data);
  }
  void write(::org::omg::CORBA::portable::OutputStream& out)
  {
    out.write_long(context_id);
    write_octet_sequence(out, context_data);
  }
};
typedef sequence <ServiceContext> ServiceContextList;

const ServiceId	TransactionService = 0;
const ServiceId	CodeSets = 1;
const ServiceId	ChainBypassCheck = 2;
const ServiceId	ChainBypassInfo = 3;
const ServiceId	LogicalThreadId = 4;
const ServiceId	BI_DIR_IIOP = 5;
const ServiceId	SendingContextRunTime = 6;
const ServiceId	INVOCATION_POLICIES = 7;
const ServiceId	FORWARDED_IDENTITY = 8;
const ServiceId	UnknownExceptionInfo = 9;

} // namespace IOP
} // namespace CORBA
} // namespace omg
} // namespace org

#endif //org_omg_CORBA_GIOP_IOP_h
