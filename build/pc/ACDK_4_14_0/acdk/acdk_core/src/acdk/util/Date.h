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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Date.h,v 1.17 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_Date_h
#define acdk_util_Date_h

#include <acdk.h>
#include <acdk/lang/Comparable.h>
#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>
#include <acdk/lang/ClassCastException.h>


namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(Date);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.17 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC Date
: extends acdk::lang::Object,
  implements acdk::io::Serializable,
  implements acdk::lang::Cloneable,
  implements acdk::lang::Comparable
{
  ACDK_WITH_METAINFO(Date)
private:
  jlong _date;
public:
  static RObject create_instance() { return new Date(); }
// constructors
  Date();
  /**
    date are milliseconds since 1970-01-01T00:00:00:000
  */
  Date(jlong date);
  Date(IN(RString) s);  
  Date(int sec, int usec)
  : Object(),
    _date(jlong(1000000) * sec + usec)
  {
  }
  Date(int year, int month, int day);
  Date(int year, int month, int day, int hour, int min);
  Date(int year, int month, int day, int hour, int min, int sec);


  virtual ~Date() { }
  bool after(IN(RDate) when) { return getTime() > when->getTime(); }
  bool before(IN(RDate) when) { return getTime() < when->getTime(); } 
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc) { return new (alc) Date(_date); }
  virtual int compareTo(IN(RDate) other)  
  { 
    return getTime() - other->getTime() > 0 ? 1 : getTime() - other->getTime() < 0 ? -1 : 0; 
  }
  /**
   * 
   * 
   * @param acdk::lang::Object The Object to compare
   */
  foreign virtual int compareTo(IN(RObject) o);
  foreign virtual bool equals(IN(RObject) obj);
  foreign virtual int hashCode() {  return (int) getTime()  ^ (int) ((getTime()) >> 32); }
  jlong getTime() {  return _date; }
  foreign virtual RString toString();
  
  static jlong getTickCount();
  RString getTimeStamp();
  int getUSecs() { return 1000 * (_date % 1000); }
  int getSecs() { return (int)(_date / 1000); }

  int getYear();

};

} // util
} // acdk

#endif //acdk_util_Date_h

