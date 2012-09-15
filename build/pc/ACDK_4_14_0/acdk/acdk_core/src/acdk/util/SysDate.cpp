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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SysDate.cpp,v 1.19 2005/03/08 12:45:46 kommer Exp $




#include <acdk.h>
#include <stdio.h>
#include <acdk/util/SysDate.h>
#include "GregorianCalendar.h"
#include <time.h>
#if defined(ACDK_OS_LINUX)
# include <sys/time.h>
#endif
#ifdef ACDK_OS_WIN32
# include <windows.h>
#endif

#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/lang/IllegalArgumentException.h>


namespace acdk {
namespace util {



#define td_milliseconds(val) (jlong(1000) * jlong(val))
#define td_seconds(val) td_milliseconds(jlong(val) * jlong(1000))
#define td_minutes(val) td_seconds(jlong(val) * jlong(60))
#define td_hours(val) td_minutes(jlong(val) * jlong(60))
#define td_days(val) td_hours(jlong(val) * jlong(24))
#define td_weeks(val) td_days(jlong(val) * jlong(7))

#if defined(ACDK_OS_WIN32)


struct timezone 
{
  int tz_minuteswest;
  int tz_dsttime;
};

struct timeval {
        long    tv_sec;         /* seconds */
        long    tv_usec;        /* and microseconds */
};


jlong TIME_FACTOR = JLONG_CONSTANT(0x19db1ded53ea710);
jlong NSPERSEC = 10000000;


jlong
mm_to_clock_t(FILETIME* src, bool flag)
{
  jlong total = ((jlong) src->dwHighDateTime << 32) + (src->dwLowDateTime);
  if (flag == true)
    total -= TIME_FACTOR;
  total /= (jlong) (NSPERSEC / CLOCKS_PER_SEC);
  return total;
}

void
mm_totimeval(struct timeval *dst, FILETIME *src, int sub, bool flag)
{
  jlong x = mm_to_clock_t(src, flag);

  x *= (int) (1e6) / CLOCKS_PER_SEC; 
  x -= (jlong) sub * (int) (1e6);

  dst->tv_usec = long(x % (jlong) (1e6)); 
  dst->tv_sec = long(x / (jlong) (1e6));
}

int
mm_gettimeofday(struct timeval* p, 
                struct timezone* z)
{
  int res = 0;
  DWORD tzid;
  TIME_ZONE_INFORMATION tz;
  LONG bias;
  tzid = GetTimeZoneInformation (&tz);
  if (tzid == TIME_ZONE_ID_INVALID)
    res = -1;
  if (tzid == TIME_ZONE_ID_DAYLIGHT)
    bias = tz.Bias + tz.DaylightBias;
  else
    bias = tz.Bias + tz.StandardBias;
  
  if (p != NULL) {
    SYSTEMTIME t;
    FILETIME f;
    GetSystemTime(&t);
    if (SystemTimeToFileTime(&t, &f) == FALSE)
      res = -1;
    mm_totimeval(p, &f, 0, 1);
  }
  
  if (z != NULL) {
    z->tz_minuteswest = bias;
    z->tz_dsttime = (tzid == TIME_ZONE_ID_DAYLIGHT);
  }
  return res;
}

const jlong TIMETOFILETIMEOFFSET = JLONG_CONSTANT(0x019db1ded53e8000);
                                                  
const jlong FILETIMEMILLISECTICS = JLONG_CONSTANT(10000);

jlong fileTimeToLong(const FILETIME& ft)
{
  LARGE_INTEGER li;
  li.LowPart = ft.dwLowDateTime;
  li.HighPart = ft.dwHighDateTime;
  return li.QuadPart;
}

FILETIME longToFileTime(jlong l)
{
  LARGE_INTEGER li;
  li.QuadPart = l;
  FILETIME ft;
  ft.dwLowDateTime = li.LowPart;
  ft.dwHighDateTime = li.HighPart;
  return ft;
}

//static 
jlong 
SysDate::fileTimeToTime(const FILETIME& ft)
{
  return (fileTimeToLong(ft) - TIMETOFILETIMEOFFSET) / FILETIMEMILLISECTICS;
}

//static 
void 
SysDate::timeToFileTime(jlong t, FILETIME& ft)
{
  ft = longToFileTime((t * FILETIMEMILLISECTICS) + TIMETOFILETIMEOFFSET);
}

#else
# define mm_gettimeofday gettimeofday
#endif //defined(ACDK_OS_WIN32) 


SysDate::SysDate()
: Object(),
  _seconds(0),
  _useconds(0)
{
  struct timeval time;
  mm_gettimeofday(&time, 0);
  _seconds = time.tv_sec;
  _useconds = time.tv_usec;
  _isSyncTime = true;
  memset(&_fields, 0, sizeof(struct tm));
  _isSyncFields = false;
}


SysDate::SysDate(jlong date)
  : Object(),
    _seconds(0),
    _useconds(0)
{
  _seconds  = date / jlong(1000);
  _useconds = (date % jlong(1000)) * 1000;
  _isSyncTime = true;
  memset(&_fields, 0, sizeof(struct tm));
  _isSyncFields = false;
}

SysDate::SysDate(IN(RString) s, IN(RString) format)
  : Object(),
    _seconds(0),
    _useconds(0)
{
  memset(&_fields, 0, sizeof(struct tm));
  _seconds = SysDate::parse(s, format);
  _isSyncFields = false;
  _isSyncTime = true;
}



SysDate::SysDate(int year, int month, int day)
  : Object(),
    _seconds(0),
    _useconds(0)
{
  memset(&_fields, 0, sizeof(struct tm));
  _fields.tm_sec = 0;
  _fields.tm_min = 0;
  _fields.tm_hour = 0;
  _fields.tm_mon = month;
  _fields.tm_mday = day;
  _fields.tm_year = year - 1900;
  _isSyncFields = true;
  _isSyncTime = false;
}

SysDate::SysDate(int year, int month, int day, int hour, int min)
  : Object(),
    _seconds(0),
    _useconds(0)
{
  memset(&_fields, 0, sizeof(struct tm));
  _fields.tm_sec = 0;
  _fields.tm_min = min;
  _fields.tm_hour = hour;
  _fields.tm_mon = month;
  _fields.tm_mday = day;
  _fields.tm_year = year - 1900;
  _isSyncFields = true;
  _isSyncTime = false;
}
 
SysDate::SysDate(int year, int month, int day, int hour, int min, int sec)
  : Object(),  
    _seconds(0),
    _useconds(0)
{
  memset(&_fields, 0, sizeof(struct tm));
  _fields.tm_sec = sec;
  _fields.tm_min = min;
  _fields.tm_hour = hour;
  _fields.tm_mon = month;
  _fields.tm_mday = day;
  _fields.tm_year = year - 1900;
  _isSyncFields = true;
  _isSyncTime = false;
}


//virtual 
int
SysDate::compareTo(IN(RObject) o)
{
  if (instanceof(o, SysDate) == false)
    THROW1(ClassCastException, "SysDate::compareTo(RObject o)");
  return getTime() - RSysDate(o)->getTime() > 0 ? -1 : getTime() - RSysDate(o)->getTime() < 0 ? 1 : 0;
}

//virtual 
bool 
SysDate::equals(IN(RObject) obj)
{
  if (instanceof(obj, SysDate) == false)
    return false;
  return getTime() == RSysDate(obj)->getTime();
}

//virtual 
RString 
SysDate::toString()
{
  time_t ut = time_t(getSecs());
  struct tm* t = localtime(&ut);
  if (t == 0) 
    return "Time not compatible or before January 1, 1900";
  char buffer[256];
  //strftime(buffer, 256, "%a %b %d %H:%M:%S %Y", t);
  strftime(buffer, 256, "%Y-%m-%dT%H:%M:%S", t);
  return new String(buffer, NormalSST | CCAscii);
}

//static
jlong
SysDate::UTC(int year, int month, int day, int hour, int min, int sec)
{
  THROW0(UnsupportedOperationException);
  return -1;
}


//static
jlong
SysDate::parse(IN(RString) s, IN(RString) format)
{
  const char *frmt;
  const char *str = s->c_str();
  if (format == Nil)
  {
    // millis are added below!
    frmt = "%Y-%m-%d_%H-%M-%S";
  } else {
    frmt = format->c_str();
  }
  int str_idx = 0;
  int str_len = strlen(str);
  int frmt_idx = 0;
  int frmt_len = strlen(frmt);
  struct tm tf;
  memset(&tf, 0, sizeof(struct tm));

  while (frmt_idx < frmt_len)
  {
    if (frmt[frmt_idx] == '%')
    {
      // match fields ...
      frmt_idx++;
      switch (frmt[frmt_idx])
      {
        case 'Y':
          // Year
          if (sscanf(str + str_idx, "%4d", &tf.tm_year) != 1)
            THROW1(IllegalArgumentException, RString("Error in %Y field"));
          tf.tm_year -= 1900;
          str_idx += 4;
          break;
        case 'm':
          // Month
          if (sscanf(str + str_idx, "%2d", &tf.tm_mon) != 1)
            THROW1(IllegalArgumentException, RString("Error in %m field"));
          str_idx += 2;
          break;
        case 'd':
          // Day
          if (sscanf(str + str_idx, "%2d", &tf.tm_mday) != 1)
            THROW1(IllegalArgumentException, RString("Error in %d field"));
          str_idx += 2;
          break;
        case 'H':
          // Hour (24)
          if (sscanf(str + str_idx, "%2d", &tf.tm_hour) != 1)
            THROW1(IllegalArgumentException, RString("Error in %H field"));
          str_idx += 2;
          break;
        case 'I':
          // Hour (12)
          if (sscanf(str + str_idx, "%2d", &tf.tm_hour) != 1)
            THROW1(IllegalArgumentException, RString("Error in %I field"));
          str_idx += 2;
          break;
        case 'p':
          // AM/PM
          char stamp[16];
          if (sscanf(str + str_idx, "%2s", stamp) != 1)
            THROW1(IllegalArgumentException, RString("Error in %p field"));
          str_idx += 2;
          if (strcmp(stamp,"AM") == 0)
            break;
          if (strcmp(stamp,"PM") == 0)
          {
            tf.tm_hour += 12;
            break;
          }
            THROW1(IllegalArgumentException, RString("Error in %p field"));
        case 'M':
          // Minute
          if (sscanf(str + str_idx, "%2d", &tf.tm_min) != 1)
            THROW1(IllegalArgumentException, RString("Error in %M field"));
          str_idx += 2;
          break;
        case 'S':
          // second
          if (sscanf(str + str_idx, "%2d", &tf.tm_sec) != 1)
            THROW1(IllegalArgumentException, RString("Error in %S field"));
          str_idx += 2;
          break;
        case 'j':
          // second
          if (sscanf(str + str_idx, "%3d", &tf.tm_yday) != 1)
            THROW1(IllegalArgumentException, RString("Error in %j field"));
          str_idx += 3;
          break;
        case 'w':
          // second
          if (sscanf(str + str_idx, "%1d", &tf.tm_wday) != 1)
            THROW1(IllegalArgumentException, RString("Error in %w field"));
          str_idx += 1;
          break;
        default:
          THROW1(IllegalArgumentException, "Option %" + String::valueOf(frmt[frmt_idx]) + " is not Supported!");
      } // case option_char
    } // if match %option_char
    else
    {
      // match normal chars
      if (str[str_idx] != frmt[frmt_idx])
      {
        // oops missmatch
        THROW1(IllegalArgumentException, "Error matching character " + String::valueOf(frmt_idx) +
                                         " of Format-String (" + frmt + ")");
      }
      str_idx++;
    } // else match normal chars
    frmt_idx++;
  } // while
  jlong time = mktime(&tf);
  if (time == -1)
  {
    THROW1(Exception, "Sorry invalide Date or < 1970-01-01");
  }

  // Millibased Systime is returned
  return jlong(1000) * time;
}


int
SysDate::getDate()
{
  return _fields.tm_mday;
}

int
SysDate::getDay()
{
  syncFields();
  return _fields.tm_mday;
}

int
SysDate::getHours()
{
  syncFields();
  return _fields.tm_hour;
}

int
SysDate::getMinutes()
{
  syncFields();
  return _fields.tm_min;
}

int
SysDate::getMonth()
{
  syncFields();
  return _fields.tm_mon;
}

int
SysDate::getSeconds()
{
  syncFields();
  return _fields.tm_sec;
}

int
SysDate::getTimezoneOffset()
{
  return 0;
}

int
SysDate::getYear()
{
  syncFields();
  return _fields.tm_year;
}

void
SysDate::setDate(int date)
{
  _fields.tm_mday = date;
  _isSyncTime = false;
}

void
SysDate::setDay(int day)
{
  _fields.tm_mday = day;
  _isSyncTime = false;
}

void
SysDate::setHours(int hours)
{
  _fields.tm_hour = hours;
  _isSyncTime = false;
}

void
SysDate::setMinutes(int minutes)
{
  _fields.tm_min = minutes;
  _isSyncTime = false;
}

void
SysDate::setMonth(int month)
{
  _fields.tm_mon = month;
  _isSyncTime = false;
}

void
SysDate::setSeconds(int seconds)
{
  _fields.tm_sec = seconds;
  _isSyncTime = false;
}

void
SysDate::setYear(int year)
{
  _fields.tm_year = year - 1900;
  _isSyncTime = false;
}

RString
SysDate::toGMTString()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

RString
SysDate::toLocaleString()
{
  THROW0(UnsupportedOperationException);
  return toString();
}

RString 
SysDate::getTimeStamp(IN(RString) format)
{
  const char* frmt;
  if (format == Nil)
  {
    // millis are added below!
    frmt = "%Y-%m-%d_%H-%M-%S";
  } else {
    frmt = format->c_str();
  }
  time_t ut = time_t(getSecs());
  struct tm* t = localtime(&ut);
  if (t == 0) 
    return "Time not compatible or before January 1, 1900";
  char buffer[256];
  strftime(buffer, 256, frmt, t);

  // The default format has millis!
  if (format == Nil)
  {
    char* ptr = buffer + strlen(buffer);
    sprintf(ptr,"_%03d", (int)(getUSecs() / 1000));
  }
  return new String(buffer, NormalSST | CCAscii);
}

//static 
jlong 
SysDate::getTickCount()
{
  return SysDate().getTime();
}

//protected
void
SysDate::syncFields()
{
  if (_isSyncFields == false)
  {
    time_t ut = time_t(getSecs());
    struct tm* t = localtime(&ut);
    memcpy(&_fields, t, sizeof(struct tm));
    _isSyncFields = true;
  }
}

void
SysDate::syncTime()
{
  if (_isSyncTime == false)
  {
    _seconds  = mktime(&_fields);
    _useconds = 0;
    if (_seconds == -1)
    {
      THROW1(Exception, "Sorry invalide Date or < 1970-01-01");
    }
    _isSyncTime = true;
  }
}


} // util
} // acdk

