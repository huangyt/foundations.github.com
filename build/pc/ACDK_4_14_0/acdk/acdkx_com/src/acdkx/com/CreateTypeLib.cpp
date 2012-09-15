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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/CreateTypeLib.cpp,v 1.9 2005/02/05 10:45:38 kommer Exp $


#include "CreateTypeLib.h"
#include "CoException.h"
#include "CreateTypeInfo.h"
#include <acdk/io/File.h>

namespace acdkx {
namespace com {

CreateTypeLib::CreateTypeLib(IN(RString) name)
: _iface(0)
{
  RString fname = acdk::io::File(name).getCanonicalPath()->convert(CCUcs2);

  BSTR bstr = S2OLE(fname);
  ACDK_CECKCOMCALL(::CreateTypeLib2(SYS_WIN32, bstr /*L"C:\\TEST.TLB" */, _iface));
  //ACDK_CECKCOMCALL(_iface->SetLcid(LANG_NEUTRAL));
  //ACDK_CECKCOMCALL(_iface->SetHelpStringDll((char*)name->c_str()));

}

CreateTypeLib::~CreateTypeLib()
{

}

void 
CreateTypeLib::commit()
{
  ACDK_CECKCOMCALL(_iface->SaveAllChanges());
}


RCreateTypeInfo 
CreateTypeLib::createTypeInfo(TypeKind kind, IN(RString) name)
{
  RICreateTypeInfo cti;
  RString wname = name->convert(CCUcs2);
  ACDK_CECKCOMCALL(_iface->CreateTypeInfo(S2OLE(wname), TKIND_INTERFACE /*TYPEKIND(kind)*/, cti));
  ACDK_CECKCOMCALL(cti->SetTypeFlags(TYPEFLAG_FOLEAUTOMATION));
  return new ::acdkx::com::CreateTypeInfo(iface_cast<ICreateTypeInfo>(cti)); 
}

} // namespace com 
} // namespace acdkx 7




