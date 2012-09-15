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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/DateFormatSymbols.h,v 1.14 2005/03/19 21:29:27 kommer Exp $

#ifndef acdk_text_DateFormatSymbols_h
#define acdk_text_DateFormatSymbols_h


#include "text.h"

#include <acdk/lang/ObjectArrayImpl.h>
#include <acdk/util/Locale.h>
#include <acdk/util/ResourceBundle.h>
#include <acdk/util/Map.h>

namespace acdk {
namespace text {

using namespace acdk::util;
using namespace acdk::lang;

ACDK_DECL_CLASS(DateFormatSymbols);

class ACDK_TEXT_PUBLIC DateFormatSymbols
: public acdk::lang::Object
{
  
private:
  
  acdk::util::RMap _weekdays; // Serialized
  acdk::util::RMap _shortWeekdays; // Serialized
  RStringArray _months; // Serialized
  RStringArray _shortMonths; // Serialized
  acdk::util::RMap _eras; // Serialized
  RStringArray _ampms; // Serialized
  acdk::util::RMap _zoneStrings; // Serialized
  RString _localPatternChars; // Serialized
  
  static RStringArray __formatPrefixes;
  static RStringArray _formatPrefixes();
  //rrk## delete this static bool _arrayEquals(RObjectArrayImpl<RObject> o1, RObjectArrayImpl<RObject> o2);

  RStringArray _dateFormats;
  RStringArray _timeFormats;
  RStringArray _formatsForKey(IN(RString) key) ;
  acdk::util::RResourceBundle _resourceBundle;
  void _init(IN(acdk::util::RLocale) locale);
public:
  DateFormatSymbols() 
  {
    _init(acdk::util::Locale::getDefault());
  }
  DateFormatSymbols(IN(acdk::util::RLocale) locale) 
  {
    _init(locale);
    
  }
  virtual RObject clone();
  virtual bool equals(IN(RObject) obj);

  RStringArray getAmPmStrings() { return _ampms; }
  acdk::util::RMap getEras() { return _eras; }
  RString getLocalPatternChars() { return _localPatternChars; }
  RStringArray getMonths()  { return _months; }
  RStringArray getShortMonths() { return _shortMonths; }
  acdk::util::RMap getWeekdays()  { return _weekdays; }
  acdk::util::RMap getShortWeekdays() { return _shortWeekdays; }
  acdk::util::RMap getZoneStrings() { return _zoneStrings; }
  void setAmPmStrings(IN(RStringArray) ampms) { _ampms = ampms; }
  void setEras(IN(acdk::util::RMap) eras) { _eras = eras; }
  void setLocalPatternChars(IN(RString) localPatternChars)  { _localPatternChars = localPatternChars;}
  void setMonths(IN(RStringArray) months) { _months = months; }
  void setShortMonths(IN(RStringArray) shortMonths) { _shortMonths = shortMonths; }
  void setWeekdays(IN(acdk::util::RMap) weekdays) { _weekdays = weekdays; }
  void setShortWeekdays(IN(acdk::util::RMap) shortWeekdays) { _shortWeekdays = shortWeekdays; }
  void setZoneStrings(IN(acdk::util::RMap)  zoneStrings) { _zoneStrings = zoneStrings; }
private:
  
};

} // namespace text 
} // namespace acdk 


#endif //acdk_text_DateFormatSymbols_h

