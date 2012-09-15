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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/DriverManager.cpp,v 1.10 2005/03/08 18:55:08 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/io/Reader.h>
#include <acdk/io/StreamTokenizer.h>
#include <acdk/util/StringTokenizer.h>
#include <acdk/util/Iterator.h>
#include <acdk/util/Vector.h>
#include <acdk/util/Properties.h>

#include "sql.h"
#include "DriverManager.h"
#include "SQLException.h"
#include "Connection.h"

namespace acdk {
namespace sql {

using namespace acdk::lang;
using namespace acdk::io;

int DriverManager::_loginTimeOut = 0;

#if !defined(ACDK_MINI)

RPrintWriter DriverManager::_logWriter = Nil;



acdk::util::RVector DriverManager::_drivers = Nil;

//static 
acdk::util::RVector 
DriverManager::drivers()
{
  if (_drivers == Nil) {
    _drivers = new acdk::util::Vector();
    System::registerStaticReference(_drivers);
    RString driver_string = System::getProperty("jdbc._drivers");
    if (driver_string == Nil)
      return _drivers;
    acdk::util::StringTokenizer st(driver_string, ",");
    while (st.hasMoreTokens() == 0) 
    {
      RString driver_classname = st.nextToken();
      try {
        Class::forName(driver_classname); 
        //### load an initialize here
      } catch (RException e) { 
        ; // Ignore not founds
      } 
    }
  }
  return _drivers;
}

//static 
void 
DriverManager::println(INP(RString) str)
{
  if (_logWriter != Nil) 
    _logWriter->println(str);
}
#else
//static 
OUTP(RDriverArray) 
DriverManager::drivers()
{
  static RDriverArray _drivers = new DriverArray(0);
  return _drivers;
}
#endif //#if !defined(ACDK_MINI)

//static 
RConnection 
DriverManager::getConnection(INP(RString) url) THROWS1(RSQLException)
{
  return getConnection(url, new acdk::util::Properties());
}

//static 
RConnection 
DriverManager::getConnection(INP(RString) url, INP(RString) user, INP(RString) password) THROWS1(RSQLException)
{
  acdk::util::RProperties p = new acdk::util::Properties();
  p->setProperty("user", user);
  p->setProperty("password", password);
  return getConnection(url, p);
}

//static 
RDriver 
DriverManager::getDriver(INP(RString) url) THROWS1(RSQLException)
{
#if defined(ACDK_MINI)
  RDriverArray da = drivers();
  for (int i = 0; i < da->length(); ++i)
    if (da[i]->acceptsURL(url) == true)
      return da[i];
#else
    acdk::util::RIterator it = getDrivers();
    while (it->hasNext() == true)
    {
      RDriver dr = (RDriver)it->next();
      if (dr->acceptsURL(url) == true)
        return dr;
    }
#endif
  return Nil;
}

//static 
RConnection 
DriverManager::getConnection(INP(RString) url, INP(acdk::util::RProperties) properties) THROWS1(RSQLException)
{
  RDriver d = getDriver(url);
  if (d == Nil)
    THROW1(SQLException, "Driver not found for URL: " + url); 
  return d->connect(url, properties);
}

//static 

acdk::util::RIterator 
DriverManager::getDrivers()
{
  return drivers()->iterator();
}

//static 
void 
DriverManager::registerDriver(INP(RDriver) driver)
{
#if defined(ACDK_MINI)
  if (drivers()->find(driver) == -1)
    drivers()->append(driver);
#else
  drivers()->add((RObject)driver);
#endif
}

//static 
void 
DriverManager::deregisterDriver(INP(RDriver) driver)
{
#if defined(ACDK_MINI)
  int idx;
  if ((idx = drivers()->find(driver)) != -1)
      drivers()->remove(idx);
#else
  drivers()->remove((RObject)driver);
#endif
}
} // sql
} // acdk


