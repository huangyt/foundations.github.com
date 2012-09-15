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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/Timestamp.h,v 1.10 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_Timestamp_h
#define acdk_sql_Timestamp_h

#include <acdk.h>
#if !defined(ACDK_MINI)

#include "sql.h"
#include <acdk/util/Date.h>
#include <acdk/io/Serializable.h>

#include <acdk/text/SimpleDateFormat.h>

namespace acdk {
namespace sql {

using namespace acdk::lang;
/*
using namespace acdk::io;
using namespace acdk::util;
using namespace acdk::text;
*/

ACDK_DECL_CLASS(Timestamp);
/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/04/08 10:53:21 $
  
*/
class ACDK_SQL_PUBLIC Timestamp
: extends acdk::util::Date,
  implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Timestamp)
public:
  // Millisecond will have to be close enough for now.
  static acdk::text::RSimpleDateFormat __parse_sdf; //=  new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
  static acdk::text::RSimpleDateFormat parse_sdf();

  static acdk::text::RSimpleDateFormat __format_sdf;// = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
  static acdk::text::RSimpleDateFormat format_sdf();

  int _nanos;
  static RTimestamp valueOf(INP(RString) str);

  Timestamp(int year, int month, int day, int hour, int minute, int second, int nanos);
  Timestamp(jlong date);

  virtual RString toString();
  virtual int getNanos() {  return _nanos; }
  virtual void setNanos(int nanos) { _nanos = nanos; }
  virtual bool before(INP(RTimestamp) ts) { return before(*ts); }
  foreign virtual bool before(Timestamp& ts);
  virtual bool after(INP(RTimestamp) ts) { return after(*ts); }
  foreign virtual bool after(Timestamp& ts);
  virtual bool equals(INP(RObject) obj);
  virtual bool equals(INP(RTimestamp) ts)
  { 
    if (ts == Nil)
      return false;
    return equals(*ts); 
  }
  foreign virtual bool equals(Timestamp& ts);
};
    


} // sql
} // acdk
#endif //#if !defined(ACDK_MINI)

#endif //acdk_sql_Timestamp_h

