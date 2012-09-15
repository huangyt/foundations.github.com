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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/RegisterServer.h,v 1.7 2005/03/12 11:51:43 kommer Exp $

// used from book "Inside COM+"


#ifndef acdkx_com_RegisterServer_h
#define acdkx_com_RegisterServer_h

// Registry.h
// Diese Funktion wird eine Komponente registrieren.
HRESULT RegisterServer(TCHAR* szModuleName, REFCLSID clsid, TCHAR* szFriendlyName, TCHAR* szVerIndProgID, TCHAR* szProgID, TCHAR* szThreadingModel);

// Diese Funktion wird die Registrierung einer Komponente entfernen.
HRESULT UnregisterServer(REFCLSID clsid, TCHAR* szVerIndProgID, TCHAR* szProgID);

struct REG_DATA
{
    TCHAR* pszKey;
    TCHAR* pszValue;
    TCHAR* pszData;
};

HRESULT UnregisterServerEx(const REG_DATA regData[]);
HRESULT RegisterServerEx(const REG_DATA regData[], const TCHAR* szModuleName);


#endif //acdkx_com_RegisterServer_h
