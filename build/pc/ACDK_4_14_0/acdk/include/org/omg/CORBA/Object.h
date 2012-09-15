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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/Object.h,v 1.8 2005/02/05 10:45:40 kommer Exp $

#ifndef org_omg_CORBA_Object_h
#define org_omg_CORBA_Object_h

#include "CORBA.h"


namespace org {
namespace omg {
namespace CORBA {

ACDK_DECL_INTERFACE(Object);

class ACDKX_ORB_PUBLIC Object
      ACDK_INTERFACEBASE
      
{
  ACDK_WITH_METAINFO(Object)
  //ACDK_ORB_INTERFACE(Object)
public:
   //        InterfaceDef get_interface ();
   virtual bool is_nil() = 0;
   virtual ::org::omg::CORBA::RObject duplicate() = 0;
   virtual void release() = 0;
   virtual bool is_a (IN(RString) logical_type_id) = 0;
   virtual bool non_existent() = 0;
   virtual bool is_equivalent(IN(::org::omg::CORBA::RObject) other_object) = 0;
   virtual int hash(int maximum) = 0;
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

} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_Object_h
