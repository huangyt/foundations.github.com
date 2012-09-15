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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/DateFormatSymbols.cpp,v 1.18 2005/04/08 10:53:21 kommer Exp $




#include "DateFormatSymbols.h"
#include <acdk/lang/CloneNotSupportedException.h>
#include <acdk/lang/System.h>
#include <acdk/util/MissingResourceException.h>
#include <acdk/util/ResourceBundle.h>
#include <acdk/util/PropertyResourceBundle.h>

namespace acdk {
namespace text {

using namespace acdk::util;
using namespace acdk::lang;


//static 
RStringArray DateFormatSymbols::__formatPrefixes;

//static 
RStringArray 
DateFormatSymbols::_formatPrefixes()
{
  if (__formatPrefixes != Nil)
    return __formatPrefixes;
  __formatPrefixes = new StringArray(4);
  int i = 0;
  __formatPrefixes[i++] = "short";
  __formatPrefixes[i++] = "medium";
  __formatPrefixes[i++] = "long";
  __formatPrefixes[i++] = "full";
  //__formatPrefixes[i++] = "default";
  return __formatPrefixes;
}

RStringArray 
DateFormatSymbols::_formatsForKey(IN(RString) key) 
{
  RStringArray fp = _formatPrefixes();
  RStringArray values = new StringArray(fp->length());
  for (int i = 0; i < fp->length(); i++) 
  {
    values[i] = _resourceBundle->getString(fp[i] + key);
  }
  return values;
}

/** @internal */
template <class T>
bool 
_arrayEquals(IN(RObjectArrayImpl<T>) o1, IN(RObjectArrayImpl<T>) o2)
{
  if (o1 == Nil) {
    if (o2 == Nil)
      return true;
    else
      return false;
  } else if (o2 == Nil)
    return false;
  
  // We assume ordering is important.
  for (int i = 0; i < o1->length(); i++) {
    if (instanceof(o1[i], RObjectArrayImpl<T>)) {
      if (instanceof(o2[i], RObjectArrayImpl<T>))  {
        if (!_arrayEquals(RObjectArrayImpl<T>(o1[i]), RObjectArrayImpl<T>(o2[i])))
          return false;
      } else
        return false;
    } else {
      if (o1[i] == Nil) {
        if (o2[i] == Nil)
          return true;
        else
          return false;
      } else if (o2[1] == Nil)
        return false;
      
      if (!o1[i]->equals((RObject)o2[i]))
        return false;
    }
  }
  return true;
}

void
DateFormatSymbols::_init(IN(RLocale) locale)
{
  _resourceBundle = ResourceBundle::getBundle("acdk/locale/LocaleInfo", locale);
  //RPropertyResourceBundle propresb(_resourceBundle);
  //propresb->getProperties()->list(System::out);

  _months = _resourceBundle->getStringArray("month");
  _shortMonths = _resourceBundle->getStringArray("shortMonths");
  _weekdays = _resourceBundle->getMap("weekdays");
  _shortWeekdays = _resourceBundle->getMap("shortWeekdays");
  _ampms = new StringArray(2);
  _ampms[0] = _resourceBundle->getString("am");
  _ampms[1] = _resourceBundle->getString("pm");
  _eras = _resourceBundle->getMap("era");
  _zoneStrings = _resourceBundle->getMap("zoneStrings");
  // ### TODO not used? remove this _localPatternChars = _resourceBundle->getString("localPatternChars");

  _dateFormats = _formatsForKey("DateFormat");
  _timeFormats = _formatsForKey("TimeFormat");
}

RObject 
DateFormatSymbols::clone() 
{
  try {
    return Object::clone();
  }  catch (RCloneNotSupportedException e)  {
    return Nil;
  }
}


bool 
DateFormatSymbols::equals(IN(RObject) obj)
{
  if (obj == Nil)
    return false;
  if (instanceof(obj,DateFormatSymbols) == false)
    return false;
  RDateFormatSymbols dfs = (RDateFormatSymbols)obj;
  if (_arrayEquals(getAmPmStrings(), dfs->getAmPmStrings()) == false)
    return false;
  if (getEras()->equals((RObject)dfs->getEras()) == false)
    return false;
  if (_arrayEquals(getMonths(), dfs->getMonths()) == false)
    return false;
  if (!_arrayEquals(getShortMonths(), dfs->getShortMonths()))
    return false;
  if (getWeekdays()->equals((RObject)dfs->getWeekdays()) == false)
    return false;
  if (getShortWeekdays()->equals((RObject)dfs->getShortWeekdays()) == false)
    return false;
  if (!getLocalPatternChars()->equals(dfs->getLocalPatternChars()))
    return false;
  RObject o = (RObject)getWeekdays();
  if (RObject(getZoneStrings())->equals((RObject)dfs->getZoneStrings()) == false)
    return false;
 
  return true;
}


#if defined(__BORLANDC__)
static void __initMissingRessourceException()
{
  RMissingResourceException ex;
  ex = new MissingResourceException();
  ex->getMessage();
}
#endif //defined(__BORLANDC__)


} // namespace text 
} // namespace acdk 



