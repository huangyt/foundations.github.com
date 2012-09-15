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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/SimpleDateFormat.h,v 1.7 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_text_SimpleDateFormat_h
#define acdk_text_SimpleDateFormat_h

//#include <acdk/util/Locale.h>
//#include <acdk/util/Calendar.h>
//#include <acdk/util/TimeZone.h>

#include "text.h"
#include "DateFormat.h"

namespace acdk {
namespace text {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(SimpleDateFormat);
/**
 *
 */
class ACDK_TEXT_PUBLIC SimpleDateFormat
  : public acdk::text::DateFormat
{
  ACDK_WITH_METAINFO(SimpleDateFormat)
protected:
  RString _pattern;
  RDateFormatSymbols _format;
  acdk::util::RLocale _loc;
public:
  SimpleDateFormat();
  SimpleDateFormat(IN(RString) pattern);
  SimpleDateFormat(IN(RString) pattern, IN(RDateFormatSymbols) formatData);
  SimpleDateFormat(IN(RString) pattern, IN(acdk::util::RLocale) loc);

  virtual void applyLocalizedPattern(IN(RString) pattern);
  virtual void applyPattern(IN(RString) pattern);
  virtual RObject clone() { return clone(allocator()); }
  virtual RObject clone(sys::Allocator* alc);
  virtual bool equals(IN(RObject) obj);
  RString format(IN(acdk::util::RDate) date) { return DateFormat::format(date); }
  virtual RStringBuffer format(IN(acdk::util::RDate) date, IN(RStringBuffer) toAppendTo, IN(RFieldPosition) pos);
  virtual acdk::util::RDate get2DigitYearStart();
  virtual RDateFormatSymbols getDateFormatSymbols();
  virtual int hashCode();
  acdk::util::RDate parse(IN(RString) text);
  virtual acdk::util::RDate parse(IN(RString) text, IN(RParsePosition) pos);
  virtual void set2DigitYearStart(IN(acdk::util::RDate) startDate);
  virtual void setDateFormatSymbols(IN(RDateFormatSymbols) newFormatSymbols);
  virtual RString toLocalizedPattern();
  virtual RString toPattern();
};


} // text
} //acdk

#endif //acdk_text_SimpleDateFormat_h

