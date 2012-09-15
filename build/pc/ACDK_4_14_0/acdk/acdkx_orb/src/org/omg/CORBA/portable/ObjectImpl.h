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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/portable/ObjectImpl.h,v 1.8 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_portable_ObjectImpl_h
#define org_omg_CORBA_portable_ObjectImpl_h

#include "../CORBA.h"
#include "../ORB.h"
#include "../OrbExceptions.h"

#include "Delegate.h"

namespace org {
namespace omg {
namespace CORBA {
namespace portable {

USING_CLASS(::org::omg::CORBA::, BAD_OPERATION);

ACDK_DECL_CLASS(ObjectImpl);

class ACDKX_ORB_PUBLIC ObjectImpl
: extends ::acdk::lang::Object,
  implements ::org::omg::CORBA::Object
{
  ACDK_WITH_METAINFO(ObjectImpl)
private:
  ::org::omg::CORBA::RORB _orb;
  transient RDelegate _delegate;
public:

  ObjectImpl();
  /** acdkx::orb implementation */
  
  virtual ~ObjectImpl();
  virtual RDelegate _get_delegate();
  virtual void _set_delegate(IN(RDelegate) del) { _delegate = del; }

   //        InterfaceDef get_interface ();
  virtual bool is_nil() { return _get_delegate()->is_nil(); }
  virtual ::org::omg::CORBA::RObject duplicate() { return _get_delegate()->duplicate(this); }
  virtual void release() { _get_delegate()->release(this); }
  virtual bool is_a(IN(RString) logical_type_id) { return _get_delegate()->is_a(this, logical_type_id); }
  virtual bool non_existent() { return _get_delegate()->non_existent(this); }
  virtual bool is_equivalent(IN(::org::omg::CORBA::RObject) other_object) { return _get_delegate()->is_equivalent(this, other_object); }
  virtual int hash(int maximum)  { return _get_delegate()->hash(this, maximum); }

  /** called by a stub to obtain an OutputStream for marshaling arguments. */
  ROutputStream _request(IN(RString) operation, bool responseExpected) 
  { 
    return _get_delegate()->request(this, operation, responseExpected); 
  }
  /** called by a stup */
  RInputStream _invoke(IN(ROutputStream) output) THROWS2(RApplicationException, RRemarshalException)
  {
    return _get_delegate()->orb_invoke(this, output); 
  }
  
  virtual void _releaseReply(IN(::org::omg::CORBA::portable::RInputStream) input) 
  {
    _get_delegate()->releaseReply(this, input);
  }
  virtual bool is_local();

          
   //        void create_request (
   //                             in Context ctx
   //                             in Identifier operation,
   //                             in NVList arg_list,
   //                             inout NamedValue result,
   //                             out Request request,
   //                             in Flags req_flag
   //                             );
   //        Policy get_policy (
   //                           in PolicyType policy_type
   //                           );
   //        DomainManagersList get_domain_managers ();
   //        Object set_policy_overrides(
   //                                    in PolicyList policies,
   //                                    in SetOverrideType set_add
   //                                    );
};

} // namespace portable
} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_portable_ObjectImpl_h
