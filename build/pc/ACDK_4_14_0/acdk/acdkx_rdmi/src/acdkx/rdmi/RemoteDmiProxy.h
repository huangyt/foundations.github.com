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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/RemoteDmiProxy.h,v 1.5 2005/04/13 16:53:46 kommer Exp $

#ifndef acdkx_rdmi_RemoteDmiProxy_h
#define acdkx_rdmi_RemoteDmiProxy_h

#include "rdmi.h"
#include "RemoteDmiServer.h"

namespace acdkx {
namespace rdmi {

ACDK_DECL_CLASS(RemoteDmiProxy);


ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiCiWeakBind))
class ACDKX_RDMI_LIB_PUBLIC RemoteDmiProxy
: extends acdk::lang::Object
{
public:
  RRemoteDmiServer _server;
  RRemoteArg _remoteId;
  RConnection _connection;
  const acdk::lang::dmi::ClazzInfo* _interfaceClazzInfo;
  RemoteDmiProxy(IN(RRemoteDmiServer) server, IN(RRemoteArg) remoteId, IN(RConnection) con);
  ~RemoteDmiProxy();
  /**
    TODO ### has to be into StdDispath!
    @param className server class
    @param interfaceName class of type to transfer. May be Nil
    @param flags acdk::lang::dmi::MiAiByval or acdk::lang::dmi::MiAiByref
           if flags = 0 selection will be done by type
    @param memberName name of static member
  */
  acdk::lang::dmi::ScriptVar peekAs(IN(RString) memberName, IN(RString) interfaceName = Nil, int flags = 0);
  
  virtual bool  isDmiOverLoaded(const acdk::lang::dmi::ClazzInfo* ci, IN(RString) funcname, const acdk::lang::dmi::ClazzMethodInfo* mi, acdk::lang::dmi::ClazzMethodArgInfo**const args);
  const acdk::lang::dmi::ClazzMethodInfo* 
  standardDispatch(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                                acdk::lang::dmi::ScriptVarArray& args, 
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf);
  
};


} // namespace rdmi 
} // namespace acdkx


#endif // acdkx_rdmi_RemoteDmiProxy_h
