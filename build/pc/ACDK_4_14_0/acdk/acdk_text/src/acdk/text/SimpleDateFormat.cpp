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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/SimpleDateFormat.cpp,v 1.22 2005/04/08 10:53:21 kommer Exp $



/** 
 the headers have to be included in this order.
 maybe they have to be moved to SimpleDateFormat.h 
 for distribution. (jb)
*/

#include "NumberFormat.h"
#include "ParsePosition.h"
#include "FieldPosition.h"
#include "ParseException.h"
#include "SimpleDateFormat.h"
#include "FieldPosition.h"
#include "DateFormatSymbols.h"

#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Byte.h>
#include <acdk/util/ResourceBundle.h>
#include <acdk/util/GregorianCalendar.h>
#include <acdk/util/SysDate.h>

#include <stdio.h>

namespace acdk {
namespace text {

using namespace acdk::util;
using namespace acdk::lang;
using namespace acdk::io;

SimpleDateFormat::SimpleDateFormat()
: DateFormat(),
  _pattern(Nil),
  _format(Nil),
  _loc(::acdk::util::Locale::getDefault())
{
   _format = new DateFormatSymbols(_loc);
}

SimpleDateFormat::SimpleDateFormat(IN(RString) pattern)
: DateFormat(),
  _pattern(pattern),
  _format(Nil),
  _loc(::acdk::util::Locale::getDefault())
{
  _format = new DateFormatSymbols(_loc);
}

SimpleDateFormat::SimpleDateFormat(IN(RString) pattern, IN(RDateFormatSymbols) formatData)
: DateFormat(),
  _pattern(pattern),
  _format(formatData),
  _loc(::acdk::util::Locale::getDefault())
{
  
}

SimpleDateFormat::SimpleDateFormat(IN(RString) pattern, IN(RLocale) loc)
: DateFormat(),
  _pattern(pattern),
  _format(Nil),
  _loc(loc)
{
  _format = new DateFormatSymbols(_loc);
}

//virtual 
void 
SimpleDateFormat::applyLocalizedPattern(IN(RString) pattern)
{
  _pattern = pattern;
}
 
//virtual 
void 
SimpleDateFormat::applyPattern(IN(RString) pattern)
{
  _pattern = pattern;
}

//virtual 
RObject 
SimpleDateFormat::clone(sys::Allocator* alc)
{
  SimpleDateFormat* sdf = new (alc) SimpleDateFormat(_pattern, _loc);
  sdf->setDateFormatSymbols(_format);
  return sdf;
}


bool _equals(IN(RObject) f, IN(RObject) s)
{
  if (f == Nil && s == Nil)
    return true;
  if (f == Nil || s == Nil)
    return false;
  if (f == s)
    return true;
  return f->equals(s);
}
//virtual 
bool 
SimpleDateFormat::equals(IN(RObject) obj)
{
  if (instanceof(obj, SimpleDateFormat) == false)
    return false;
  RSimpleDateFormat other = (RSimpleDateFormat)obj;
  if (_equals((RObject)_pattern, (RObject)other->toPattern()) == false)
    return false;
  if (_equals((RObject)_format, (RObject)other->getDateFormatSymbols()) == false)
    return false;
  return true;
}


namespace {

/** @internal */
  inline int getTokenCount(String::iterator& fit, String::iterator fend)
{
  int tkcount = 1;
  while ((fit + 1) != fend && *(fit + 1) == *fit) 
  {
      ++tkcount;
      ++fit;
  }
  return tkcount;
}

/** @internal */
  bool scanQuotedText(String::iterator& fid,  String::iterator fend,
                    String::iterator& tit, String::iterator tend)
{
   //const char* startptr = ptr;

  while (fid != fend && tit != tend) 
  {
    if (*fid == '\'') 
    {
      if ((fid + 1) != fend && *(fid + 1) == '\'') 
      {
        if (*tit != '\'')
          return false;
        ++fid;
      } 
      else 
      {
        return true;
      }
    } 
    ++tit;
    ++fid;
  }
  return false;
}


/** @internal */
  bool scanText(IN(RString) text, String::iterator& tit, OUT(RString) erg)
{
  String::iterator start = tit;
  String::iterator tend = text->end();
  while (tit != tend) 
  {
    if (isalnum(*tit) == false) 
    {
      int istart = start - text->begin();
      int iend = istart + (tit - start);
      erg = text->substr(istart, iend);
      return true;
    }
    ++tit;
  }
  erg = text->substr(start - text->begin(), start - tit);
  return true;
}


/**
   if a given year has only 2 digits, 
   the century will be calculated
   80 after current year, or 20 before
   current year.
*/
/** @internal */
  int getWidenYear(int nowYear, int shortYear)
{
   int lowYear = (nowYear - 20) / 100;
   int highYear = (nowYear + 80)  / 100;
   lowYear = (lowYear * 100) + shortYear;
   highYear = (highYear * 100) + shortYear;
   if (nowYear - lowYear <= 20)
      return lowYear;
   else
      return highYear;
}

/** @internal */
  bool parseInt(int tcount, int& terg, IN(RString) text, String::iterator& tit)
{
  String::iterator startptr = tit;
  String::iterator tend = text->end();
  while (tit != tend) 
  {
    if (Character::isDigit(*tit) == false)
      return false;
    --tcount;
    if (tcount == 0) 
    {
      int starti = startptr - text->begin();
      int leni = tit - startptr + 1;
      RString tstr = text->substr(starti, starti + leni);
      terg = Integer::parseInt(tstr);
      ++tit;
      return true;
    }
    ++tit;
  }
  RString tstr = text->substr(tit - text->begin(), tit - startptr);
  terg = atoi(tstr->c_str());
  return false;
}

/** @internal */
  int mapMonth(IN(RString) month, DateFormatSymbols& dfs)
{
  StringArray& sa = *dfs.getMonths();
  for (int i = 0; i < sa.length(); ++i)
  {
    if (sa[i]->equals(month) == true)
      return i + 1;
  }
  return 0;
}

/** @internal */
  int getAmPm(IN(RString) str, DateFormatSymbols& dfs)
{
  StringArray& sa = *dfs.getAmPmStrings();
  for (int i = 0; i < sa.length(); ++i)
  {
    if (sa[i]->equals(str) == true)
      return i;
  }
  return 0;
  
}

/** @internal */
#define WARN(msg) sys::coreout << msg << sys::eofl


/** @internal */
  RString formatInt(int c, int i)
{
   char ibuf[128];
  ::sprintf(ibuf, "%i", i);
  if (c == (int)strlen(ibuf))
      return SCS(ibuf);
  if (c < (int)strlen(ibuf))
    return SCS(ibuf + strlen(ibuf) - c);
  char buffer[128];
  memset(buffer, 0, 128);
  memset(buffer, '0', c);
  strcpy(buffer + c - strlen(ibuf), ibuf);
  return SCS(buffer);
}

/** @internal */
  bool parseDate(IN(RString) format, IN(RString) text, Calendar& gc, DateFormatSymbols& dfs)
{
  String::iterator fit = format->begin();
  String::iterator fend = format->end();
  String::iterator tit = text->begin();
  String::iterator tend = text->end();
      
  RString pt = "";
   int terg;
   bool success = true;
   
   while (fit != fend) 
   {
     int tkcount = 1;
     ucchar cchar = *fit;
     switch (cchar) 
     {
      case 'G' : 
      {
        tkcount = getTokenCount(fit, fend);
        if (scanText(text, tit, pt) == false)
          return false;
        acdk::util::RMap eramap = dfs.getEras();
        acdk::util::RIterator it = eramap->entrySet()->iterator();
        while (it->hasNext() == true)
        {
          acdk::util::RMapEntry me(it->next());
          if (me->getValue()->equals(&pt) == true)
          {
            int i = Integer::parseInt(RString(me->getKey()));
            gc.set(Calendar::ERA, i);
            break;  
          }
        }
        /* old code 
        RStringArray eras = dfs.getEras();
        
        for (int i = 0; i < eras.length(); ++i) 
        {
          if (eras[i]->equals(pt) == true) 
          {
            gc.set(Calendar::ERA, i);
            break;
          }
        }
        */
        break;
      }
      case 'y' :
         tkcount = getTokenCount(fit, fend);
         if (parseInt(tkcount, terg, text, tit) == false) 
            return false;            
         
         if (tkcount < 3) {
            gc.set(Calendar::YEAR, getWidenYear(SysDate().getYear(), terg));
         } else
            gc.set(Calendar::YEAR, terg);
         break;
      case 'M' :
         tkcount = getTokenCount(fit, fend);
         if (tkcount >= 3) {
            if (scanText(text, tit, pt) == false)
               return false;
            gc.set(Calendar::MONTH, mapMonth(pt, dfs));
         } else {
            if (parseInt(tkcount, terg, text, tit) == false) 
               return false;            
            gc.set(Calendar::MONTH, terg - 1);
         }
         break;
      case 'd':
         tkcount = getTokenCount(fit, fend);
         if (parseInt(tkcount, terg, text, tit) == false) 
            return false;            
         gc.set(Calendar::DAY_OF_MONTH, terg);
         break;
      case 'k' : // no break;
      case 'h' : // 12 hour
      case 'K': // no break
      case 'H':
        tkcount = getTokenCount(fit, fend);
        if (parseInt(tkcount, terg, text, tit) == false) 
           return false;
        if (cchar == 'k')
          gc.set(Calendar::HOUR_OF_DAY, terg - 1);
        else if (cchar == 'h')
          gc.set(Calendar::HOUR, terg - 1);
        else if (cchar == 'K')
           gc.set(Calendar::HOUR, terg);
        else if (cchar == 'H')
           gc.set(Calendar::HOUR_OF_DAY, terg);
        break; 
      case 'm' :
         tkcount = getTokenCount(fit, fend);
         if (parseInt(tkcount, terg, text, tit) == false) 
            return false;
         gc.set(Calendar::MINUTE, terg);
         break;
      case 's' :
         tkcount = getTokenCount(fit, fend);
         if (parseInt(tkcount, terg, text, tit) == false) 
            return false;
         gc.set(Calendar::SECOND, terg);
         break;
      case 'S' :
         tkcount = getTokenCount(fit, fend);
         if (parseInt(tkcount, terg, text, tit) == false) 
            return false;
         gc.set(Calendar::MILLISECOND, terg);
         break;
      case 'E' : //day in week             (Text)              
         tkcount = getTokenCount(fit, fend);
         /* not supported
         if (parseInt(tkcount, terg, text, tit) == false) 
            return false;
         //???
         */
         break;
      case 'D' : // day in year             
         tkcount = getTokenCount(fit, fend);
         /* not supported
         if (parseInt(tkcount, terg, text, tit) == false) 
            return false;
         //???
         */
         break;
      case 'F' : //F        day of week in month    (Number)            
         tkcount = getTokenCount(fit, fend);
         /* not supported

         if (parseInt(tkcount, terg, text, tit) == false) 
            return false;
         //???
         */
         break;
      case 'w' : //        week in year            (Number)            
         tkcount = getTokenCount(fit, fend);
         /* not supported

         if (parseInt(tkcount, terg, text, tit) == false) 
            return false;
         //???
         */
         break;
      case 'W' : //        week in month           (Number)            2
         tkcount = getTokenCount(fit, fend);
         /* not supported
         if (parseInt(tkcount, terg, text, tit) == false) 
            return false;
         //???
         */
         break;
      case 'a' :
         tkcount = getTokenCount(fit, fend);
         if (scanText(text, tit, pt) == false)
            return false;
          gc.set(Calendar::AM_PM, getAmPm(pt, dfs));
         
         break;
      case 'z' :
         tkcount = getTokenCount(fit, fend);
         if (tkcount >= 4) {
            if (scanText(text, tit, pt) == false)
               return false;
            if (scanText(text, tit, pt) == false)
               return false;
            if (scanText(text, tit, pt) == false)
               return false;
         } else {
            if (scanText(text, tit, pt) == false)
               return false;
         }  
         break;
      case '\'':
         if (scanQuotedText(++fit, fend, tit, tend) == false)
            return false;
         
         break;
      default :
         if ((*fit < 'a' || *fit > 'z') &&  (*fit < 'A' || *fit > 'Z')) 
         {
            if (*fit != *tit)
               return false;
            ++tit;
         } 
         else
            WARN("Unknown Token: " << *fit);
         break;

      }
      ++fit;
   }
   return success;
}






/** @internal */
void 
formatDate(String::iterator fit, String::iterator fend, StringBuffer& sb
               , Calendar& gc, DateFormatSymbols& dfs)
{
  String::iterator fstart = fit;
  int terg = 0;
  while (fit != fend) 
  {
    int tkcount = 1;
    char cchar = *fit;
    switch (cchar) 
    {
    case 'G' :
      tkcount = getTokenCount(fit, fend);
      if (gc.get(Calendar::ERA) == 1)
        sb.append(dfs.getEras()->get(&RString("1")));
      else
        sb.append(dfs.getEras()->get(&RString("0")));
      break;
    case 'y' :
      tkcount = getTokenCount(fit, fend);
      if (tkcount == 2)
        sb.append(formatInt(tkcount, gc.get(Calendar::YEAR) % 100));
      else if (tkcount == 4)
        sb.append(formatInt(tkcount, gc.get(Calendar::YEAR)));
      else
        WARN("Format of Year can have 2 or 4 digits");
      break;
    case 'M' :
      tkcount = getTokenCount(fit, fend);
      if (tkcount >= 3) 
      {
        int m = gc.get(Calendar::MONTH);
        sb.append(dfs.getMonths()[m]);
      } else
        sb.append(formatInt(tkcount, gc.get(Calendar::MONTH) + 1));
      break;
    case 'd':
      tkcount = getTokenCount(fit, fend);
      sb.append(formatInt(tkcount, gc.get(Calendar::DAY_OF_MONTH)));
      break;
      
    case 'k' : // no break;
    case 'h' : // 12 hour
    case 'K': // no break
    case 'H':
      tkcount = getTokenCount(fit, fend);
      if (cchar == 'k')
        terg = gc.get(Calendar::HOUR_OF_DAY) + 1;
      else if (cchar == 'h')
        terg = gc.get(Calendar::HOUR) + 1;
      else if (cchar == 'K')
        terg = gc.get(Calendar::HOUR);
      else if (cchar == 'H')
        terg = gc.get(Calendar::HOUR_OF_DAY);
      sb.append(formatInt(tkcount, terg));
      break; 
    case 'm' :
      tkcount = getTokenCount(fit, fend);
      sb.append(formatInt(tkcount, gc.get(Calendar::MINUTE)));
      break;
    case 's' :
      tkcount = getTokenCount(fit, fend);
      sb.append(formatInt(tkcount, gc.get(Calendar::SECOND)));
      break;
    case 'S' :
      tkcount = getTokenCount(fit, fend);
      sb.append(formatInt(tkcount, gc.get(Calendar::MILLISECOND)));
      break;
    case 'E' : //day in week             (Text)              
               /*
               tkcount = getTokenCount(fit, fend);
               os << "<WeekDay>"; // ###FIXME
      */
      break;
    case 'D' : // day in year             
               /* not supported
               tkcount = getTokenCount(fit, fend);
               os << std::setw(tkcount) << std::setfill('D') 
               << 0;
      */
      break;
    case 'F' : //F        day of week in month    (Number)            
               /* not supported
               tkcount = getTokenCount(fit, fend);
               os << std::setw(tkcount) << std::setfill('F') 
               << 0;
      */
      break;
    case 'w' : //        week in year            (Number)            
               /* not supported
               tkcount = getTokenCount(fit, fend);
               os << std::setw(tkcount) << std::setfill('w') 
               << 0;
      */
      break;
    case 'W' : //        week in month           (Number)            2
               /* not supported
               tkcount = getTokenCount(fit, fend);
               os << std::setw(tkcount) << std::setfill('W') 
               << 0;
      */
      break;
    case 'a' :
      tkcount = getTokenCount(fit, fend);
      if (gc.get(Calendar::HOUR) > 11) 
        sb.append(dfs.getAmPmStrings()[1]);
      else
        sb.append(dfs.getAmPmStrings()[0]);
      break;
    case 'z' :
    /* not supported
    tkcount = getTokenCount(fit, fend);
    if (tkcount >= 4)
    os << "Central European Time";
    else
    os << "CET";
      */
      break;
    case '\'':
      if (fit + 1 != fend && *(fit + 1) == '\'') 
      {
        sb.append('\'');
        ++fit;
      } 
      else 
      {
        ++fit;
        while (fit != fend) 
        {
          if (*fit == '\'') 
          {
            if ((fit + 1) != fend && *(fit + 1) == '\'') 
            {
              sb.append('\'');
              ++fit;
            } 
            else 
            {
              break;
            }
          } 
          else 
          {
            sb.append(*fit);
            ++fit;
          }
        }
      }
      break;
    default :
      if ((*fit < 'a' || *fit > 'z') &&  (*fit < 'A' || *fit > 'Z'))
        sb.append(*fit);
      else
        WARN("Unknown Token: " << *fit);
      break;
      
      }
      ++fit;
   }
}


} // anon namespace





//virtual 
RStringBuffer 
SimpleDateFormat::format(IN(RDate) date, IN(RStringBuffer) toAppendTo, IN(RFieldPosition) pos)
{
  
  GregorianCalendar gc;
  gc.setTime(date);

  formatDate(_pattern->begin(), _pattern->end(), *toAppendTo, gc, *_format);
  return toAppendTo;
}

//virtual 
RDate 
SimpleDateFormat::get2DigitYearStart()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

//virtual 
RDateFormatSymbols 
SimpleDateFormat::getDateFormatSymbols()
{
  return _format;
}
 
//virtual 
int 
SimpleDateFormat::hashCode()
{
  int result = DateFormat::hashCode();
  result = 31 * result + (_pattern == Nil ? 0 : _pattern->hashCode());
  result = 31 * result + (_format == Nil ? 0 : _format->hashCode());  
  //result = 31 * result + (_loc == Nil ? 0 : _loc->hashCode());  
  //result = 31 * result + (_fields == Nil ? 0 : _fields->hashCode());  
  return result;
}

RDate
SimpleDateFormat::parse(IN(RString) text)
{
  GregorianCalendar gc;
  parseDate(_pattern, text, gc, *_format);
  return gc.getTime();
}


//virtual 
RDate 
SimpleDateFormat::parse(IN(RString) text, IN(RParsePosition) pos)
{
  GregorianCalendar gc;
  int ipos = 0;
  if (pos != Nil)
    ipos = pos->getIndex();
  parseDate(_pattern, text->substring(ipos), gc, *_format);
  return gc.getTime();
}

//virtual 
void 
SimpleDateFormat::set2DigitYearStart(IN(RDate) startDate)
{
  THROW0(UnsupportedOperationException);
}

//virtual 
void 
SimpleDateFormat::setDateFormatSymbols(IN(RDateFormatSymbols) newFormatSymbols)
{
  _format = newFormatSymbols;
}

//virtual 
RString 
SimpleDateFormat::toLocalizedPattern()
{
  return _pattern;
}

//virtual 
RString 
SimpleDateFormat::toPattern()
{
  return _pattern;
}

} // text
} // acdk


