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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/portable/RemarshalException.h,v 1.7 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_portable_RemarshalException_h
#define org_omg_CORBA_portable_RemarshalException_h

#include <acdk.h>

#include "../CORBA.h"
#include "../ORB.h"


namespace org {
namespace omg {
namespace CORBA {
namespace portable {


ACDK_DECL_THROWABLE(RemarshalException, Exception);

class ACDKX_ORB_PUBLIC RemarshalException
: extends ::acdk::lang::Exception
{
  ACDK_WITH_METAINFO(RemarshalException)
private:
  /** not in CORBA standard */
  ::acdk::lang::RObject __newObjectKey;
  bool _replaceObjectKey;
public:
  
  RemarshalException() : Exception(), __newObjectKey(Nil), _replaceObjectKey(false) { }
  RemarshalException(IN(RString) what) : Exception(what), __newObjectKey(Nil), _replaceObjectKey(false) { }

  RemarshalException(IN(::acdk::lang::RObject) objkey, bool replace) : Exception(), __newObjectKey(objkey), _replaceObjectKey(replace) { }
  ::acdk::lang::RObject objectKey() { return __newObjectKey; }
  bool replaceObjectKey() { return _replaceObjectKey; }
};

} // namespace portable
} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_portable_RemarshalException_h
