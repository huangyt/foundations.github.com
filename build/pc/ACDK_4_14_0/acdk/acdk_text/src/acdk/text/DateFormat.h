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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/DateFormat.h,v 1.14 2005/03/08 18:49:55 kommer Exp $

#ifndef acdk_text_DateFormat_h
#define acdk_text_DateFormat_h

#include <acdk.h>
#include <acdk/util/Calendar.h>
#include <acdk/lang/UnsupportedOperationException.h>

#include "Format.h"
#include "NumberFormat.h"
#include "ParsePosition.h"

namespace acdk {
namespace text {


using namespace acdk::lang;


ACDK_DECL_CLASS(DateFormat);

enum DateFields 
{
    ERA_FIELD = 0,
    YEAR_FIELD = 1,
    MONTH_FIELD = 2,
    DATE_FIELD = 3,  // sic, should be "DAY_OF_MONTH"...
    HOUR_OF_DAY1_FIELD = 4,
    HOUR_OF_DAY0_FIELD = 5,
    MINUTE_FIELD = 6,
    SECOND_FIELD = 7,
    MILLISECOND_FIELD = 8,
    DAY_OF_WEEK_FIELD = 9,
    DAY_OF_YEAR_FIELD = 10,
    DAY_OF_WEEK_IN_MONTH_FIELD = 11,
    WEEK_OF_YEAR_FIELD = 12,
    WEEK_OF_MONTH_FIELD = 13,
    AM_PM_FIELD = 14,
    HOUR1_FIELD = 15,
    HOUR0_FIELD = 16,
    TIMEZONE_FIELD = 17
};
ACDK_DEF_LIB_ENUM(ACDK_TEXT_PUBLIC, DateFields);

/** Constant for full style pattern. */
enum FormatStyle
{
  NoneFormatStyle = -1,
  FullFormatStyle = 0,
  LongFormatStyle = 1,
  MediumFormatStyle = 2,
  ShortFormatStyle = 3,
  DefaultFormatStyle = 4
};
ACDK_DEF_LIB_ENUM(ACDK_TEXT_PUBLIC, FormatStyle);

class ACDK_TEXT_PUBLIC DateFormat
  : public acdk::text::Format
{
  ACDK_WITH_METAINFO(DateFormat)
public:
  
 
protected:
  ::acdk::util::RCalendar _calendar;
  RNumberFormat _numberFormat;

private:
  bool _lenient;

public:
  virtual ~DateFormat();

  RStringBuffer format(IN(RObject) obj, IN(RStringBuffer) toAppendTo, 
                       IN(RFieldPosition) fieldPosition);

  virtual RStringBuffer format(IN(::acdk::util::RDate) date, IN(RStringBuffer) toAppendTo, 
                               IN(RFieldPosition) fieldPosition) = 0;

  RString format(IN(::acdk::util::RDate) date);

  ::acdk::util::RDate parse(IN(RString) text);
  virtual ::acdk::util::RDate parse(IN(RString) text, IN(RParsePosition) pos) = 0;
  
  RObject parseObject(IN(RString) source, IN(RParsePosition) pos) 
  {
    return (RObject)parse(source, pos);
  }

  static RDateFormat getTimeInstance(FormatStyle style = DefaultFormatStyle) 
  {
    return get(style, NoneFormatStyle, ::acdk::util::Locale::getDefault());
  }

  static RDateFormat getTimeInstance(FormatStyle style, IN(::acdk::util::RLocale) locale) 
  {
    return get(style, NoneFormatStyle, locale);
  }
  
  static RDateFormat getDateInstance(FormatStyle style = DefaultFormatStyle) 
  {
    return get(NoneFormatStyle, style, ::acdk::util::Locale::getDefault());
  }

  static RDateFormat getDateInstance(FormatStyle style, IN(::acdk::util::RLocale) locale) 
  {
    return get(NoneFormatStyle, style, locale );
  } 
  static RDateFormat getDateTimeInstance(FormatStyle dateStyle = DefaultFormatStyle, FormatStyle timeStyle = DefaultFormatStyle) 
  {
    return get(timeStyle, dateStyle, ::acdk::util::Locale::getDefault());
  }
  static RDateFormat getDateTimeInstance(FormatStyle dateStyle, FormatStyle timeStyle, IN(::acdk::util::RLocale) locale) 
  {
    return get(timeStyle, dateStyle, locale);
  }
  static RDateFormat getInstance() 
  {
    return getDateTimeInstance(ShortFormatStyle, ShortFormatStyle);
  }

  static ::acdk::util::RLocaleArray getAvailableLocales()
  {
    
    return new LocaleArray(0);
  }
  
  void setCalendar(IN(::acdk::util::RCalendar) newCalendar) 
  {
    _calendar = newCalendar;
  }

  ::acdk::util::RCalendar getCalendar() 
  {
    return _calendar;
  }
  
  void setNumberFormat(IN(RNumberFormat) newNumberFormat)
  {
    _numberFormat = newNumberFormat;
  }

  RNumberFormat getNumberFormat() 
  {
    return _numberFormat;
  }

  void setTimeZone(IN(::acdk::util::RTimeZone) zone) 
  {
    _calendar->setTimeZone(zone);
  }

  ::acdk::util::RTimeZone getTimeZone() 
  {
    return _calendar->getTimeZone();
  }

  void setLenient(bool lenient) {
    _lenient = lenient ;
  }
  
  bool getLenient() {
    return _lenient;
  }

  int hashCode();

  bool equals(IN(RObject) obj);

  RObject clone() 
  {
    THROW0(UnsupportedOperationException);
    return Nil;
  }

protected:
  DateFormat();

private:  
  static RDateFormat get(FormatStyle timeStyle,
                         FormatStyle dateStyle,
                         IN(::acdk::util::RLocale) loc);

};


} // namespace text
} // namespace acdk

#endif //acdk_text_DateFormat_h

