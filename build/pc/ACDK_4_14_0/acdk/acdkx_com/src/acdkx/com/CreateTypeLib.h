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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/CreateTypeLib.h,v 1.9 2005/02/05 10:45:38 kommer Exp $


#ifndef acdkx_com_CreateTypeLib_h
#define acdkx_com_CreateTypeLib_h

#include "IUnknown.h"
#include "CreateTypeInfo.h"

ACDK_COMIFACE(ICreateTypeLib2);

ACDK_COMIFACE(ICreateTypeLib);


namespace acdkx {
namespace com {

ACDK_DECL_CLASS(CreateTypeLib);

foreign class ACDKX_COM_PUBLIC CreateTypeLib 
: extends ::acdk::lang::Object
{
  ::RICreateTypeLib2 _iface;
public:
  enum TypeKind 
  {
    Enum = 0,
    Record,
    Module,
    Interface,
    Dispatch,
    CoClass,
    Alias,
    Union,
    Max
  };
  CreateTypeLib(IN(RString) name);
  ~CreateTypeLib();
  void setName(IN(RString) str) 
  { 
    RString wstr = str->convert(CCUcs2);
    _iface->SetName(S2OLE(wstr)); 
  }
  void setDocString(IN(RString) str) 
  { 
    RString wstr = str->convert(CCUcs2);
    _iface->SetDocString(S2OLE(wstr)); 
  }
  void commit();
  RCreateTypeInfo createTypeInfo(TypeKind kind, IN(RString) name);
  ::RICreateTypeLib2 operator->() { return _iface; }
  ::RICreateTypeLib2 iface() { return _iface; }
};

} // namespace com 
} // namespace acdkx 



#endif //acdkx_com_CreateTypeLib_h

