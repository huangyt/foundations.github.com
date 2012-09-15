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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/IsoTables.cpp,v 1.6 2005/03/15 21:39:27 kommer Exp $

#include <acdk.h>
#include "IsoEncoding.h"
#include "UnicodeTable.h"

namespace acdk {
namespace locale { 


unsigned short cp2uc_unmappable[256] = {
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 
0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, };


#include "8859-1_mapping.h"
#include "8859-2_mapping.h"
#include "8859-3_mapping.h"
#include "8859-4_mapping.h"
#include "8859-5_mapping.h"
#include "8859-6_mapping.h"
#include "8859-7_mapping.h"
#include "8859-8_mapping.h"
#include "8859-9_mapping.h"
#include "8859-10_mapping.h"
//#include "8859-11_mapping.h"
//#include "8859-12_mapping.h"
#include "8859-13_mapping.h"
#include "8859-14_mapping.h"
#include "8859-15_mapping.h"
#include "8859-16_mapping.h"
#include "CP1250_mapping.h"
#include "CP1251_mapping.h"
#include "CP1252_mapping.h"
#include "CP1253_mapping.h"
#include "CP1254_mapping.h"
#include "CP1255_mapping.h"
#include "CP1256_mapping.h"
#include "CP1257_mapping.h"
#include "CP1258_mapping.h"
#include "IBM-850_mapping.h"


IsoUnicodeMapping mappings[] =
{
  { "8859-1", (unsigned short*)codepage_8859_1_mapping, (unsigned short **)&codepage_8859_1_mapping_cp2uc },
  { "LATIN-1", (unsigned short*)codepage_8859_1_mapping, (unsigned short **)&codepage_8859_1_mapping_cp2uc },
  { "8859-2", (unsigned short*)&codepage_8859_2_mapping, (unsigned short**)&codepage_8859_2_mapping_cp2uc },
  { "8859-3", (unsigned short*)&codepage_8859_3_mapping, (unsigned short**)&codepage_8859_3_mapping_cp2uc },
  { "8859-4", (unsigned short*)&codepage_8859_4_mapping, (unsigned short**)&codepage_8859_4_mapping_cp2uc },
  { "8859-5", (unsigned short*)&codepage_8859_5_mapping, (unsigned short**)&codepage_8859_5_mapping_cp2uc },
  { "8859-6", (unsigned short*)&codepage_8859_6_mapping, (unsigned short**)&codepage_8859_6_mapping_cp2uc },
  { "8859-7", (unsigned short*)&codepage_8859_7_mapping, (unsigned short**)&codepage_8859_7_mapping_cp2uc },
  { "8859-8", (unsigned short*)&codepage_8859_8_mapping, (unsigned short**)&codepage_8859_8_mapping_cp2uc },
  { "8859-9", (unsigned short*)&codepage_8859_9_mapping, (unsigned short**)&codepage_8859_9_mapping_cp2uc },
  { "8859-10", (unsigned short*)&codepage_8859_10_mapping, (unsigned short**)&codepage_8859_10_mapping_cp2uc },
  { "8859-13", (unsigned short*)&codepage_8859_13_mapping, (unsigned short**)&codepage_8859_13_mapping_cp2uc },
  { "8859-14", (unsigned short*)&codepage_8859_14_mapping, (unsigned short**)&codepage_8859_14_mapping_cp2uc },
  { "8859-15", (unsigned short*)&codepage_8859_15_mapping, (unsigned short**)&codepage_8859_15_mapping_cp2uc },
  { "8859-16", (unsigned short*)&codepage_8859_16_mapping, (unsigned short**)&codepage_8859_16_mapping_cp2uc },
  { "CP1250", (unsigned short*)&codepage_CP1250_mapping, (unsigned short**)&codepage_CP1250_mapping_cp2uc },
  { "CP1251", (unsigned short*)&codepage_CP1251_mapping, (unsigned short**)&codepage_CP1251_mapping_cp2uc },
  { "CP1252", (unsigned short*)&codepage_CP1252_mapping, (unsigned short**)&codepage_CP1252_mapping_cp2uc },
  { "CP1253", (unsigned short*)&codepage_CP1253_mapping, (unsigned short**)&codepage_CP1253_mapping_cp2uc },
  { "CP1254", (unsigned short*)&codepage_CP1254_mapping, (unsigned short**)&codepage_CP1254_mapping_cp2uc },
  { "CP1255", (unsigned short*)&codepage_CP1255_mapping, (unsigned short**)&codepage_CP1255_mapping_cp2uc },
  { "CP1256", (unsigned short*)&codepage_CP1256_mapping, (unsigned short**)&codepage_CP1256_mapping_cp2uc },
  { "CP1257", (unsigned short*)&codepage_CP1257_mapping, (unsigned short**)&codepage_CP1257_mapping_cp2uc },
  { "CP1258", (unsigned short*)&codepage_CP1258_mapping, (unsigned short**)&codepage_CP1258_mapping_cp2uc },
  { "IBM-850", (unsigned short*)&codepage_IBM_850_mapping, (unsigned short**)&codepage_IBM_850_mapping_cp2uc },
  { 0, 0, 0 }
};

} // locale
} // acdk
