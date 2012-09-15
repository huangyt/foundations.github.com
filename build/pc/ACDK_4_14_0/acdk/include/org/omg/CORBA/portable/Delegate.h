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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/portable/Delegate.h,v 1.12 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_portable_Delegate_h
#define org_omg_CORBA_portable_Delegate_h

#include <acdk.h>
#include <acdk/lang/System.h>

#include "../CORBA.h"
#include "../ORB.h"
#include "OutputStream.h"
#include "InputStream.h"

#include "ApplicationException.h"
#include "RemarshalException.h"


namespace org {
namespace omg {
namespace CORBA {
namespace portable {

USING_CLASS(::org::omg::CORBA::, NO_IMPLEMENT);

ACDK_DECL_INTERFACE(Delegate);

class ACDKX_ORB_PUBLIC Delegate
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Delegate)
public:
    //::org::omg::CORBA::RInterfaceDef get_interface(::org::omg::CORBA::RObject self) = 0;
    
    virtual ::org::omg::CORBA::RObject get_interface_def(IN(::org::omg::CORBA::RObject) self) 
    {
      THROW0(NO_IMPLEMENT);
      return Nil;
    }
    virtual bool is_nil() = 0;
    virtual ::org::omg::CORBA::RObject duplicate(IN(::org::omg::CORBA::RObject) self) = 0;

    virtual void release(IN(::org::omg::CORBA::RObject) self) = 0;

    virtual bool is_a(IN(::org::omg::CORBA::RObject) self, IN(RString) repository_id) = 0;

    virtual bool non_existent(IN(::org::omg::CORBA::RObject) self) = 0;

    virtual bool is_equivalent(IN(::org::omg::CORBA::RObject) self, IN(::org::omg::CORBA::RObject) rhs) = 0;

    virtual int hash(IN(::org::omg::CORBA::RObject) self, int max) = 0;
    /*
    virtual ::org::omg::CORBA::Request create_request(::org::omg::CORBA::RObject self, 
                                                      ::org::omg::CORBA::RContext ctx,
                                                      RString operation,
                                                      ::org::omg::CORBA::RNVList arg_list,
                                                      ::org::omg::CORBA::RNamedValue result) = 0;

    virtual ::org::omg::CORBA::Request create_request(
                ::org::omg::CORBA::RObject self,
                ::org::omg::CORBA::RContext ctx,
                RString operation,
                ::org::omg::CORBA::NVList arg_list,
                ::org::omg::CORBA::RNamedValue result,
                ::org::omg::CORBA::ExceptionList exclist,
                ::org::omg::CORBA::RContextList ctxlist) = 0;
   
    virtual ::org::omg::CORBA::Request request(
                ::org::omg::CORBA::RObject self,
                RString operation) = 0;
   */
    virtual ::org::omg::CORBA::portable::ROutputStream request(IN(::org::omg::CORBA::RObject) self,
                                                              IN(RString) operation,
                                                              bool responseExpected) 
    {
        THROW0(NO_IMPLEMENT);
        return Nil;
    }

    virtual ::org::omg::CORBA::portable::RInputStream orb_invoke(IN(::org::omg::CORBA::RObject) self,
                                                            IN(::org::omg::CORBA::portable::ROutputStream) os) 
                                                              THROWS2(RApplicationException, RRemarshalException) 
    {
        THROW0(NO_IMPLEMENT);
        return Nil;
    }

    virtual void releaseReply(IN(::org::omg::CORBA::RObject) self, IN(::org::omg::CORBA::portable::RInputStream) is) 
    {
        THROW0(NO_IMPLEMENT);
    }
/*
    virtual ::org::omg::CORBA::Policy get_policy(::org::omg::CORBA::RObject self, int policy_type) 
    {
        THROW0(NO_IMPLEMENT);
    }

    virtual ::org::omg::CORBA::DomainManager[] get_domain_managers(::org::omg::CORBA::RObject self) 
    {
        THROW0(NO_IMPLEMENT);
    }


    virtual ::org::omg::CORBA::RObject set_policy_override(::org::omg::CORBA::RObject self,
                                                           ::org::omg::CORBA::Policy[] policies,
                                                            ::org::omg::CORBA::SetOverrideType set_add) 
    {
        THROW0(NO_IMPLEMENT);
    }
*/
    virtual ::org::omg::CORBA::RORB orb(IN(::org::omg::CORBA::RObject) self) 
    {
        THROW0(NO_IMPLEMENT);
        return Nil;
    }


    virtual bool is_local(::org::omg::CORBA::RObject self) 
    {
        return false;
    }
/*
    virtual RServantObject servant_preinvoke(::org::omg::CORBA::RObject self, RString operation, RClass expectedType) 
    {
        return Nil;
    }

    virtual void servant_postinvoke(::org::omg::CORBA::RObject self,
                RServantObject servant) {
    }
*/

    virtual RString toString(IN(::org::omg::CORBA::RObject) self) = 0;
    /*{
      return RObject(self)->getClass()->getName() + ":" + toString();
    }*/

    virtual int hashCode(IN(::org::omg::CORBA::RObject) self) 
    {
      return ::acdk::lang::System::identityHashCode((::acdk::lang::RObject)self);
    }

    virtual bool equals(IN(::org::omg::CORBA::RObject) self, IN(acdk::lang::RObject) obj) 
    {
        return ((::acdk::lang::RObject)self == obj);
    }
};

} // namespace portable
} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_portable_Delegate_h
