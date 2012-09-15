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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/RegisterServer.cpp,v 1.9 2005/03/14 17:59:13 kommer Exp $



// code used from inside COM+
#include <acdk.h>
#include <objbase.h>
#include <assert.h>
#include "RegisterServer.h"

#if defined(UNICODE)
# define tstrlen wcslen
# define tstrcpy wcscpy
# define tstrcat wcscat
# define tstrstr wcsstr
# define tcstombs wcsncpy
#else
# define tstrlen strlen
# define tstrcpy strcpy
# define tstrcat strcat
# define tstrstr strstr
# define tcstombs wcstombs
#endif

// Schluessel und Wert setzen.
BOOL setKeyAndValue(TCHAR* pszPath, TCHAR* szSubkey, TCHAR* szValue);

// Einen Schluessel oeffnen und einen Wert setzen.
BOOL setValueInKey(TCHAR* szKey, TCHAR* szNamedValue, TCHAR* szValue);

// CLSID in eine Zeichenfolge konvertieren.
void CLSIDtochar(REFCLSID clsid, TCHAR* szCLSID, int length);

// SzKeyChild und alle Nachfolger loeschen.
LONG recursiveDeleteKey(HKEY hKeyParent, TCHAR* szKeyChild);

// Groesse einer CLSID als Zeichenfolge
const int CLSID_STRING_SIZE = 39;

// Registrieren der Komponente in der Registrierung.
HRESULT RegisterServer(TCHAR* szModuleName,     // DLL module name
                       REFCLSID clsid,               // Klassen-ID
                       TCHAR* szFriendlyName,   // Name
                       TCHAR* szVerIndProgID,   // Programmgesteuert
                       TCHAR* szProgID,         // IDs
					   TCHAR* szThreadingModel) // ThreadingModel
{
	// Server lokalisieren.
	TCHAR szModule[512];
	HMODULE hModule = GetModuleHandle(szModuleName);
	DWORD dwResult = GetModuleFileName(hModule, szModule, sizeof(szModule) / sizeof(TCHAR));
	assert(dwResult != 0);

	// CLSID in Zeichen konvertieren.
	TCHAR szCLSID[CLSID_STRING_SIZE];
	CLSIDtochar(clsid, szCLSID, sizeof(szCLSID) / sizeof(TCHAR));

	// Schluessel CLSID\\{...} erstellen
	TCHAR szKey[64];
	tstrcpy(szKey, _T("CLSID\\"));
	tstrcat(szKey, szCLSID);
  
	// CLSID zur Registrierung hinzufuegen.
	setKeyAndValue(szKey, NULL, szFriendlyName);

	if(tstrstr(szModuleName, _T(".exe")) == NULL)
	{
		setKeyAndValue(szKey, _T("InprocServer32"), szModule);
		TCHAR szInproc[64];
		tstrcpy(szInproc, szKey);
		tstrcat(szInproc, _T("\\InprocServer32"));
		setValueInKey(szInproc, _T("ThreadingModel"), szThreadingModel);
	}
	else
		setKeyAndValue(szKey, _T("LocalServer32"), szModule);

	setKeyAndValue(szKey, _T("ProgID"), szProgID);

	setKeyAndValue(szKey, _T("VersionIndependentProgID"), szVerIndProgID);

	setKeyAndValue(szVerIndProgID, NULL, szFriendlyName); 
	setKeyAndValue(szVerIndProgID, _T("CLSID"), szCLSID);
	setKeyAndValue(szVerIndProgID, _T("CurVer"), szProgID);

	setKeyAndValue(szProgID, NULL, szFriendlyName); 
	setKeyAndValue(szProgID, _T("CLSID"), szCLSID);

	return S_OK;
}


LONG UnregisterServer(REFCLSID clsid,             // Klassen-ID
                      TCHAR* szVerIndProgID, // Programmgesteuert
                      TCHAR* szProgID)       // IDs
{
	
	TCHAR szCLSID[CLSID_STRING_SIZE];
	CLSIDtochar(clsid, szCLSID, sizeof(szCLSID) / sizeof(TCHAR));

	
	TCHAR szKey[64];
	tstrcpy(szKey, _T("CLSID\\"));
	tstrcat(szKey, szCLSID);

	
	LONG lResult = recursiveDeleteKey(HKEY_CLASSES_ROOT, szKey);
	assert((lResult == ERROR_SUCCESS) || (lResult == ERROR_FILE_NOT_FOUND)); 

	
	lResult = recursiveDeleteKey(HKEY_CLASSES_ROOT, szVerIndProgID);
	assert((lResult == ERROR_SUCCESS) || (lResult == ERROR_FILE_NOT_FOUND)); 

	
	lResult = recursiveDeleteKey(HKEY_CLASSES_ROOT, szProgID);
	assert((lResult == ERROR_SUCCESS) || (lResult == ERROR_FILE_NOT_FOUND)); 

	return S_OK;
}


void CLSIDtochar(REFCLSID clsid, TCHAR* szCLSID, int length)
{
	assert(length >= CLSID_STRING_SIZE);
	// CLSID ermitteln
	LPOLESTR wszCLSID = NULL;
	HRESULT hr = StringFromCLSID(clsid, &wszCLSID);
	assert(SUCCEEDED(hr));

	// 16-Bit-Unicode in Zeichenfolge konvertieren.
	tcstombs(szCLSID, wszCLSID, length);

	// Speicher freigeben.
	CoTaskMemFree(wszCLSID);
}


LONG recursiveDeleteKey(HKEY hKeyParent,           
                        TCHAR* lpszKeyChild)  
{
	HKEY hKeyChild;
	LONG lRes = RegOpenKeyEx(hKeyParent, lpszKeyChild, 0, KEY_ALL_ACCESS, &hKeyChild);
	if(lRes != ERROR_SUCCESS)
		return lRes;

	
	FILETIME time;
	TCHAR szBuffer[256];
	DWORD dwSize = 256;
	while(RegEnumKeyEx(hKeyChild, 0, szBuffer, &dwSize, NULL, NULL, NULL, &time) == S_OK)
	{
		
		lRes = recursiveDeleteKey(hKeyChild, szBuffer);
		if(lRes != ERROR_SUCCESS)
		{
			
			RegCloseKey(hKeyChild);
			return lRes;
		}
		dwSize = 256;
	}

	
	RegCloseKey(hKeyChild);

	
	return RegDeleteKey(hKeyParent, lpszKeyChild);
}


BOOL setKeyAndValue(TCHAR* szKey, TCHAR* szSubkey, TCHAR* szValue)
{
	HKEY hKey;
	TCHAR szKeyBuf[1024];

	
	tstrcpy(szKeyBuf, szKey);

	if(szSubkey != NULL)
	{
		tstrcat(szKeyBuf, _T("\\"));
		tstrcat(szKeyBuf, szSubkey );
	}

	long lResult = RegCreateKeyEx(HKEY_CLASSES_ROOT, szKeyBuf, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, &hKey, NULL);
	if(lResult != ERROR_SUCCESS)
		return FALSE;

	if(szValue != NULL)
		RegSetValueEx(hKey, NULL, 0, REG_SZ, (BYTE *)szValue, (tstrlen(szValue) + 1) * sizeof(TCHAR));

	RegCloseKey(hKey);
	return TRUE;
}

BOOL setValueInKey(TCHAR* szKey, TCHAR* szNamedValue, TCHAR* szValue)
{
	HKEY hKey;
	TCHAR szKeyBuf[1024];

	tstrcpy(szKeyBuf, szKey);

	long lResult = RegOpenKeyEx(HKEY_CLASSES_ROOT, szKeyBuf, 0, KEY_SET_VALUE, &hKey);
	if(lResult != ERROR_SUCCESS)
		return FALSE;

	if(szValue != NULL)
		RegSetValueEx(hKey, szNamedValue, 0, REG_SZ, (BYTE*)szValue, (tstrlen(szValue) + 1)  * sizeof(TCHAR));

	RegCloseKey(hKey);
	return TRUE;
}

HRESULT RegisterServerEx(const REG_DATA regData[], TCHAR* szModuleName)
{
	for(int count = 0;; count++)
	{
		const TCHAR* pszKey = regData[count].pszKey;
		const TCHAR* pszValue = regData[count].pszValue;
		const TCHAR* pszData = regData[count].pszData;

		if(pszKey == 0 && pszValue == 0 && pszData == 0)
			break;

		if(pszData == (const TCHAR*)-1)
			pszData = szModuleName;

		HKEY hKey;
		long err = RegCreateKey(HKEY_CLASSES_ROOT, pszKey, &hKey);
		if(err == ERROR_SUCCESS)
		{
			err = RegSetValueEx(hKey, pszValue, 0, REG_SZ, (const BYTE*)pszData, (tstrlen(pszData) + 1) * sizeof(TCHAR));
			RegCloseKey(hKey);
		}
		if(err != ERROR_SUCCESS)
		{
			UnregisterServerEx(regData);
			return REGDB_E_WRITEREGDB;
		}
	}
	return S_OK;
}

HRESULT UnregisterServerEx(const REG_DATA regData[])
{
	HRESULT hr = S_OK;
  int nEntries;
	for (nEntries = 0;; nEntries++)
		if(regData[nEntries].pszKey == 0 && regData[nEntries].pszValue == 0 && regData[nEntries].pszData == 0)
			break;

	for (int count = nEntries - 1; count >= 0; count--)
		if(RegDeleteKey(HKEY_CLASSES_ROOT, regData[count].pszKey) != ERROR_SUCCESS)
			hr = S_FALSE;

	return hr;
}
