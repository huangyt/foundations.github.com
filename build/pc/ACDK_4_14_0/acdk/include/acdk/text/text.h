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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/text.h,v 1.8 2005/02/07 17:16:08 kommer Exp $

#ifndef acdk_text_text_h
#define acdk_text_text_h

#include "Config.h"


ACDK_DECL_UNIT(acdk_text)

namespace acdk {
/**
  Equally to the Java package java.text.
*/ 
 namespace text {

  ACDK_DECL_CLASS(DateFormat);
  ACDK_DECL_CLASS(DateFormatSymbols);
  ACDK_DECL_CLASS(FieldPosition);
  ACDK_DECL_CLASS(FieldPosition);
  ACDK_DECL_CLASS(ParsePosition);
  ACDK_DECL_CLASS(Format);
  ACDK_DECL_CLASS(NumberFormat);
  ACDK_DECL_CLASS(ParsePosition);
  ACDK_DECL_CLASS(RegExpMatchPosition);
  ACDK_DECL_CLASS(RegExp);
  ACDK_DECL_CLASS(SimpleDateFormat);
  ACDK_DECL_CLASS(PropertyVarTemplateFilter);
  ACDK_DECL_CLASS(TemplateFilterInformation);
  ACDK_DECL_CLASS(Template);

  ACDK_DECL_INTERFACE(TemplateFilter);

  ACDK_DECL_THROWABLE(ParseException, Exception);
 }
}

#endif // acdk_text_text_h

