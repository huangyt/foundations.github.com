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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SysDate.h,v 1.24 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_SysDate_h
#define acdk_util_SysDate_h

#include <acdk.h>
#include <acdk/lang/Comparable.h>
#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>
#include <acdk/lang/ClassCastException.h>

#ifdef ACDK_HAS_STRUCT_TIMEVAL
#  include <sys/time.h>
#endif 
#include <time.h>

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(SysDate);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.24 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/

class ACDK_CORE_PUBLIC SysDate
: extends acdk::lang::Object
, implements acdk::io::Serializable
, implements acdk::lang::Cloneable
, implements acdk::lang::Comparable
{
  ACDK_WITH_METAINFO(SysDate)
private:
  int _seconds;
  int _useconds;
  bool _isSyncFields;
  bool _isSyncTime;
  typedef  struct ::tm timefiles;
  timefiles _fields;
public:
  // static java methods (Date-Class)
  static jlong UTC(int year, int month, int day, int hour, int min, int sec);
  static jlong parse(IN(RString) s, IN(RString) format = Nil);

  // static acdk methods
  static RObject create_instance() { return new SysDate(); }
  
  // java constructors (Date-Class)
  SysDate();
  /**
    date are milliseconds since 1970-01-01T00:00:00:000
  */
  SysDate(jlong date);
  SysDate(IN(RString) s, IN(RString) format = Nil);  
  SysDate(int year, int month, int day);
  SysDate(int year, int month, int day, int hour, int min);
  SysDate(int year, int month, int day, int hour, int min, int sec);

  // acdk constructor
  SysDate(int sec, int usec)
  : Object(),
    _seconds(sec),
    _useconds(usec)
  {
  }

  virtual ~SysDate() { }

  // java methods (Date-Class)
  bool after(IN(RSysDate) when) { return getTime() > when->getTime(); }
  bool before(IN(RSysDate) when) { return getTime() < when->getTime(); } 
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc) { return new (alc) SysDate(_seconds, _useconds); }
  virtual int compareTo(IN(RSysDate) other)  
  { 
    return getTime() - other->getTime() > 0 ? 1 : getTime() - other->getTime() < 0 ? -1 : 0; 
  }
  /**
   * 
   * 
   * @param acdk::lang::Object The Object to compare
   */
  foreign virtual int compareTo(IN(acdk::lang::RObject) o);
  foreign virtual bool equals(IN(RObject) obj);
  foreign virtual int hashCode() {  return (int) getTime()  ^ (int) ((getTime()) >> 32); }
  /**
    return time in milliseconds since 1970
  */
  jlong getTime() 
  {
    syncTime();
    return (jlong(_seconds) * jlong(1000)) + jlong(_useconds) / jlong(1000);
  }
  foreign virtual RString toString();
  
  //DEPRECATED methods
  int getDate();
  int getDay();
  int getHours();
  int getMinutes();
  int getMonth();
  int getSeconds();
  int getTimezoneOffset();
  int getYear();
  void setDate(int date);
  void setDay(int day);
  void setHours(int hours);
  void setMinutes(int minutes);
  void setMonth(int month);
  void setSeconds(int seconds);
  void setYear(int year);
  RString toGMTString();
  RString toLocaleString();

  // acdk methods
  static jlong getTickCount();
  RString getTimeStamp(IN(RString) format = Nil);
  int getUSecs()
  {
    syncTime();
    return _useconds;
  }
  int getSecs()
  {
    syncTime();
    return _seconds;
  }
#if defined(ACDK_OS_WIN32)
  static jlong fileTimeToTime(const FILETIME& ft);
  static void timeToFileTime(jlong t, FILETIME& ft);
#endif
  /**
    create a new SysDate with offset of millisends
  */
  RSysDate addMilliseconds(jlong millies)
  {
    return new SysDate(getTime() + millies);
  }
  /**
    return the difference in milliseconds
    @param other if other is younger, returned value
           is negative
  */
  jlong diffMilliseconds(INP(RSysDate) other)
  {
    return getTime() - other->getTime();
  }

protected:
  void syncFields();
  void syncTime();
};

} // util
} // acdk

#endif //acdk_util_SysDate_h

