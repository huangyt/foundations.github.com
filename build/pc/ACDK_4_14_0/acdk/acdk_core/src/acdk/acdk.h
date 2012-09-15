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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/acdk.h,v 1.12 2005/02/07 11:17:24 kommer Exp $

#ifndef acdk_h
#define acdk_h
#define ACDK_h  // temporary item


/** 
  All ACDK-Classes are inside the acdk*-packages.
*/
namespace acdk {
  
} //namespace acdk 

#include "Config.h"

#include "Compiler.h"

#include "Platform.h"
#include "Version.h"

#include "AcdkCoreConfig.h"

#ifndef acdk_lang_sys_RefHolder_h
#include "lang/sys/RefHolder.h"
#endif // acdk_lang_sys_RefHolder_h

#ifndef acdk_lang_Lang_h
#  include "lang/lang.h"
#endif

#ifndef acdk_io_IO_h
#include "io/io.h"
#endif

#ifndef acdk_util_Util_h
#include "util/util.h"
#endif

#ifndef acdk_lang_Object_h
#include "lang/Object.h"
#include "lang/ObjectInline.h"
#endif // acdk_lang_Object_h


#ifndef acdk_lang_String_h
#include "lang/String.h"
#endif // acdk_lang_String_h

#ifndef acdk_lang_Class_h
#include "lang/Class.h"
#endif // acdk_lang_Class_h

#ifndef acdk_lang_dmi_ClazzInfo_h
#include "lang/dmi/ClazzInfo.h"
#endif // acdk_lang_dmi_ClazzInfo_h

#ifndef acdk_io_ObjectReader_h
#include "io/ObjectReader.h"
#endif //acdk_io_ObjectReader_h
#ifndef acdk_lang_dmi_DmiProxy_h
# include "lang/dmi/DmiProxy.h"
#endif
#endif //acdk_h

