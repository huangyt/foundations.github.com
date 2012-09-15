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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TimeZone.h,v 1.11 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_TimeZone_h
#define acdk_util_TimeZone_h

#include <acdk.h>

#include "Locale.h"
#include "Date.h"

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;


ACDK_DECL_CLASS(TimeZone);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC TimeZone
: extends acdk::lang::Object,
  implements acdk::io::Serializable,
  implements acdk::lang::Cloneable

{
  ACDK_WITH_METAINFO(TimeZone)
public:
  TimeZone();
  TimeZone(IN(RString) id);
  virtual ~TimeZone();
  
  virtual int getOffset(int era, int year, int month,
                        int day, int dayOfWeek, int milliseconds) = 0;

  virtual int getRawOffset() = 0;

  RString getID() 
  {
    return _id;
  }
  
  void setID(IN(RString) id) 
  {
    _id = id;
  }

  RString getDisplayName();
  
  RString getDisplayName(IN(RLocale) locale);

  RString getDisplayName(bool b, int st);

  RString getDisplayName(bool b, int st, IN(RLocale) locale);

  virtual bool useDaylightTime() = 0;

  virtual bool inDaylightTime(IN(RDate) date) = 0;

  static RTimeZone getTimeZone(IN(RString) id);

  static RStringArray getAvailableIDs(int rawOffset);

  static RStringArray getAvailableIDs();

  static RTimeZone getDefault();

  static void setDefault(RTimeZone zone) {
    _defaulZone = zone;
  }

  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc);

  static RStringArrayArray get_zoneAlliases();
  static RTimeZoneArray get_timeZone();

public:
  static int SHORT;
  static int LONG;
private:
  RString _id;
  static RTimeZone _defaulZone;
  static RStringArrayArray _zoneAlliases;
  static RTimeZoneArray _timeZone;
};


} // util
} // acdk

#endif //acdk_util_TimeZone_h

