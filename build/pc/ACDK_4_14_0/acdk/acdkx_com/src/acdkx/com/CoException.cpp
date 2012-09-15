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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/CoException.cpp,v 1.7 2005/02/05 10:45:38 kommer Exp $



#include "CoException.h"

namespace acdkx {
namespace com {

RString 
CoException::getMessage()
{
  StringBuffer sb(1024);
  sb.append("Call failed: ");
  sb.append(_call);
  sb.append(": ");
  HRESULT hr = _hr;
  if (HRESULT_FACILITY(hr) == FACILITY_WINDOWS)
    hr = HRESULT_CODE(hr);
  uc2char* text;
  if (FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                    NULL, 
                    hr, 
                    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
                    (LPTSTR)&text, 0, NULL) != 0)
  {
    sb.append(text);
    LocalFree(text);
  } else
    sb.append("<unknown>");
  return sb.toString();
}

} // namespace com 
} // namespace acdkx 
