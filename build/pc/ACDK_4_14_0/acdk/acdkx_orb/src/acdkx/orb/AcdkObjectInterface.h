// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, GermRDmiObject.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// RDmiObject commercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/AcdkObjectInterface.h,v 1.7 2005/04/13 16:54:07 kommer Exp $

#ifndef acdkx_orb_AcdkObjectInterface_h
#define acdkx_orb_AcdkObjectInterface_h

#include <acdk.h>
#include <acdkx/orb/orb.h>
#include <acdkx/orb/ServerDelegate.h>
#include <acdk/lang/dmi/DmiObject.h>

namespace acdkx {
namespace orb {


USING_CLASS(::acdk::lang::dmi::, DmiObject);

ACDK_DECL_INTERFACE(AcdkObjectInterface);

/**
  dynamic interface to orb objects
*/
class ACDKX_ORB_PUBLIC AcdkObjectInterface
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(AcdkObjectInterface)
public: 
  virtual ::acdk::lang::RObject get_cor_factory(IN(RString) classname) = 0;
  virtual ::acdk::lang::RObject dyn_new(IN(RString) classname, IN(RString) constructor, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp) = 0;
  virtual RDmiObject dyn_invoke(IN(RString) methodname, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp) = 0;
  virtual RDmiObject dyn_invoke_static(IN(RString) classname, IN(RString) methodname, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp) = 0;
  virtual RDmiObject dyn_peek(IN(RString) membername) = 0;
  virtual RDmiObject dyn_peek_static(IN(RString) classname, IN(RString) membername) = 0;
  virtual void dyn_poke(IN(RString) membername, IN(RDmiObject) value) = 0;
  virtual void dyn_poke_static(IN(RString) classname, IN(RString) membername, IN(RDmiObject) value) = 0;
};

} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_AcdkObjectInterface_h
