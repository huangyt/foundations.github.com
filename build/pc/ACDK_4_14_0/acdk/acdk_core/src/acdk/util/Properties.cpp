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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Properties.cpp,v 1.36 2005/04/06 10:03:29 kommer Exp $


#include <acdk.h>
#include "Properties.h"
#include "DoubleIterator.h"
#include "Date.h"
#include "HashSet.h"
#include "TreeSet.h"
#include "StringTokenizer.h"

#include <acdk/io/PrintWriter.h>
#include <acdk/io/BufferedReader.h>
#include <acdk/io/InputReader.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/System.h>
#include <acdk/io/CharToByteWriter.h>
#include <acdk/io/ByteToCharReader.h>

#include <acdk/io/LineNumberCharReader.h>
#include <acdk/io/File.h>
#include <acdk/locale/AsciiUtfEncoding.h>
#include <acdk/locale/CEscapeEncoding.h>

namespace acdk {
namespace util {

using namespace acdk::lang;

Properties::Properties(IN(RProperties) def)
: HashMap(),
  _defaults(def)
{
}

RString
Properties::getProperty(IN(RString) key)
{
  return getProperty(key, Nil);
}

RString
Properties::getProperty(IN(RString) key, IN(RString) defaultValue, bool withDefaults)
{
  RProperties prop = this;
  do {
    RString value = RString(prop->get((RObject)key));
    if (value != Nil || withDefaults == false)
      return value;
    prop = prop->defaults();
  } while (prop != Nil);
  return defaultValue;
}

RMap 
Properties::getMapProperty(IN(RString) keyStart, bool withDefaults)
{
  RIterator it = propertyNames(withDefaults);
  RString ks = keyStart + ".";
  RMap m = new TreeMap();
  while (it->hasNext() == true)
  {
    RString ck = (RString)it->next();
    if (ck->startsWith(ks) == false)
      continue;
    RString val = getProperty(ck);
    m->put(&ck->substr(ks->length()), &val);
  }
  return m;
}

void 
Properties::_deleteKeys(IN(RString) keyStart)
{
  RIterator it = propertyNames(false);
  while (it->hasNext() == true)
  {
    RString ck = (RString)it->next();
    if (ck->startsWith(keyStart) == false)
      continue;
    remove(&ck);
  }
}

void 
Properties::setPropertyMap(IN(RString) keyStart, IN(RMap) map)
{
  _notifyListener(PropChangeSetMapProperty, keyStart, (RObject)map);

  RString ks = keyStart + ".";
  _deleteKeys(ks);
  if (map == Nil)
    return;
  RIterator it = map->entrySet()->iterator();
  while (it->hasNext() == true)
  {
    RMapEntry me = (RMapEntry)it->next();
    RString mk = (RString)me->getKey();
    RString mo = (RString)me->getValue();
    RString pk = ks + mk;
    put(&pk, &mo);
  }
}

RStringArray 
Properties::getArrayProperty(IN(RString) keyStart, int start, bool withDefaults)
{
  RStringArray sa = new StringArray(0);
  do 
  {
    RString ks = keyStart + "." + Integer::toString(start);
    RString v = getProperty(ks, Nil, withDefaults);
    if (v == Nil)
      break;
    sa->append(v);
    ++start;
  } while (true);
  return sa;
}

void 
Properties::setArrayProperty(IN(RString) keyStart, IN(RStringArray) values, int start)
{
  _notifyListener(PropChangeSetArrayProperty, keyStart, &values);
  _deleteKeys(keyStart + ".");
  if (values == Nil)
    return;
  for (int i = 0; i < values->length();++i)
  {
    RString nk = keyStart + "." + Integer::toString(start);
    put(&nk, &values[i]);
    ++start;
  }
}

RString 
Properties::eval(IN(RString) expr, bool recursive)
{
  RString resultExpr = expr;
restart:
  int slen = resultExpr->length();
  int i = 0;
  StringBuffer sb;
  bool replaced = false;
  while (i < slen)
  {
    if (resultExpr->charAt(i) == '$')
    {
      ++i;
      if (i == slen)
      {
        sb.append('$');
        break;
      }
      if (resultExpr->charAt(i) != '(')
      {
        sb.append('$');
        continue;
      }
      RString ex = resultExpr->substr(i + 1);
      int eidx = ex->indexOf(')');
      if (eidx == -1)
      {
        sb.append('(');
        sb.append(ex);
        break;
      }
      RString k = ex->substr(0, eidx);
      RString s = getProperty(k);
      if (s == Nil)
      {
        sb.append("$(");
        sb.append(k);
        sb.append(')');
        i += eidx + 2;
        continue;
      }
      else
      {
        sb.append(s);
        i += eidx + 2;
        replaced = true;
        continue;
      }
    }
    else
    {
      sb.append(resultExpr->charAt(i));
      ++i;
    }
  }
  if (replaced == false)
    return resultExpr;
  if (recursive == true)
  {
    resultExpr = sb.toString();
    goto restart;
  }
  return sb.toString(); 
}

void
Properties::list(IN(RPrintWriter) out, bool withDefaults)
{
  if (withDefaults == true && _defaults != Nil)
    _defaults->list(out, withDefaults);
  RObjectArray oa = keySet()->toArray();
  Arrays::sort(oa);
  
  
  //RIterator entries = oa->iterator();
  RWriter rawout = out->getWriter();
  for (int i = 0; i < oa->length(); ++i)
  {
    RString k = (RString)oa[i];
    RString l = _format(k, getProperty(k));
    rawout->write((const byte*)l->c_str(), 0, l->length());
    rawout->write('\n');
  }
}


RString encodePropString(IN(RString) str)
{
  RString s = str->encodeAscUnicode();
  StringBuffer result((int)(s->length() * 1.3));
  String::iterator it = s->begin();
  String::iterator end = s->end();
  for (; it < end; ++it)
  {
    switch(*it)
    {
    case '!':
    case '#':
    case ':':
    case '=':
      result << '\\' << *it;
      break;
    default:
      result << *it;
    }
  }
  return result.toString();
}

RString decodePropString(IN(RString) str)
{
  RString s = str->encodeAscUnicode();
  StringBuffer result(s->length());
  String::iterator it = s->begin();
  String::iterator end = s->end();
  for (; it < end; ++it)
  {
    if (*it == '\\')
    {
      ++it;
      result << *it;
      continue;
    }
    result << *it;
  }
  return result.toString();
}

//static
RString
Properties::_format(IN(RString) key, IN(RString) value)
{
  return encodePropString(key) + "=" + encodePropString(value);
}

int seekForKeyDevider(IN(RString) line)
{
  String::iterator it = line->begin();
  String::iterator begin = it;
  String::iterator end = line->end();
  for (; it < end; ++it)
  {
    if (*it == '\\')
    {
      ++it;
      continue;
    }
    if (*it == ':' || *it == '=')
      return it - begin;
  }
  return -1;
}

RString trimPropLine(IN(RString) line)
{
  return line->trim();
  /* does not working, if for example # is element of a value

  if (line->indexOf('!') == -1 && line->indexOf('#') == -1)
    return line->trim();
  String::iterator it = line->begin();
  String::iterator begin = it;
  String::iterator end = line->end();
  for (; it < end; ++it)
  {
    if (*it == '\\')
    {
      ++it;
      continue;
    }
    if (*it == '#' || *it == '!')
      return line->substr(0, it - begin)->trim();
  }
  return line->trim();
  */
}


namespace {
  //using namespace acdk::locale;

class PropertyLineDecoder
: extends acdk::locale::AsciiUtfDecoder
{
public:
  int _nonReturned;
  PropertyLineDecoder(IN(acdk::locale::REncoding) encoding = acdk::locale::AsciiUtfEncoding::getAsciiUtfCEscapeEncoding(),
                      acdk::locale::CodingErrorAction onMalformed = acdk::locale::ReportCodingError,
                      acdk::locale::CodingErrorAction onUnmappable = acdk::locale::ReplaceCodingError)
  : ACDK_FQ_SUPER_QUALIFIER(acdk::locale::, AsciiUtfDecoder)(true, encoding, onMalformed, onUnmappable)
  , _nonReturned(-1)
  {
  }
  int  decodeToChar(IN(acdk::io::RReader) in);
};

int
PropertyLineDecoder::decodeToChar(IN(acdk::io::RReader) in)
 {
   if (_nonReturned != -1)
   {
     int ret = _nonReturned;
     _nonReturned = -1;
     return ret;
   }
   int c = in->read();
   if (c == -1)
     return -1;
   ++_bytesReaded;
   if (c == '\\')
   {
     c = in->read();
     ++_bytesReaded;
     if (c == 'u')
     {
       byte buffer[4];
       int count = in->read(buffer, 0, 4);
       if (count < 4)
         return -2;
       _bytesReaded += 4;
       int erg = _decode(buffer);
       if (erg == -1)
         return -2;
       return erg;
   }
   else
   {
     int erg = acdk::locale::CEscapeDecoder::decodeEscapeByte(c);
     if (erg != c) // is mapped
       return erg;
     int nc = in->read();
     switch(nc)
     {
     case '!':
     case '#':
     case ':':
     case '=':
       return nc;
     default:
       _nonReturned = nc;
       return erg;
     }
   }
 }
 if (c < 0x80)
   return c;
DecodeErrror: // ### TODO exception
 return c;

}

} // anon namespace

void
Properties::load(IN(RReader) in)
{
  acdk::locale::RDecoder dec = new PropertyLineDecoder();
  acdk::io::RByteToCharReader  btchr = new acdk::io::ByteToCharReader(in, dec);
  RInputReader reader = new InputReader(&btchr);

  RString line;
  while ((line = reader->readLine()) != Nil)
  {
    StringBuffer complLine;
    //line = line->trim();
    if (line->length() == 0)
      continue;
    while (line->charAt(line->length() - 1) == '\\')
    {
      complLine.append(line->substr(0, line->length() - 1));
      line = reader->readLine();
    }
    if (complLine.length() > 0)
      line = complLine.append(line)->toString();
    line = trimPropLine(line);
    if (line->length() == 0)
      continue;
    uc2char c = line->charAt(0);
    if (c == '#' || c == '!')
      continue;
    int idx = seekForKeyDevider(line);
    if (idx == -1)
    {
      THROW1(IllegalArgumentException, "illegal line in property file. line: " + line);
    }
    RString key = line->substr(0, idx)->trim();
    RString value = line->substr(idx + 1)->trim();

    setProperty(key, value);
  }
}



RIterator
Properties::propertyNames(bool withDefaults)
{
  if (_defaults == Nil || withDefaults == false)
    return keySet()->iterator();
  return new DoubleIterator(keySet()->iterator(), _defaults->propertyNames());

}
/*
void
Properties::save(IN(RWriter) out, IN(RString) header, bool withDefaults)
{
  store(out, header, withDefaults);
}*/

RObject
Properties::setProperty(IN(RString) key, IN(RString) value)
{
  _notifyListener(PropChangeSetProperty, key, &value);
  return put((RObject)key, (RObject)value);
}

void
Properties::store(IN(RWriter) out, IN(RString) header, bool withDefaults)
{
  RPrintWriter writer = new PrintWriter((RCharWriter)new acdk::io::CharToByteWriter(out, acdk::locale::AsciiUtfEncoding::getAsciiUtfEncoding()->getEncoder()));

  if (header != Nil)
  {
    RStringArray hl = StringTokenizer(header, "\n").allToken();
    for (int i = 0; i < hl->length(); ++i)
    {
      writer->print(RString("#") + hl[i]);
      out->write('\n');
    }
    out->write('\n');
  }
  writer->print(RString("#") + (new Date())->toString());
  out->write('\n');
  list(writer, withDefaults);
  writer->flush();
}

void
Properties::mergeProperties(IN(acdk::util::RProperties) props)
{
  RIterator it = props->propertyNames();
  while (it->hasNext() == true)
  {
    RString key = (RString)it->next();
    setProperty(key, props->getProperty(key));
  }
}

RObject
Properties::clone(sys::Allocator* alloc)
{
  RProperties newprops = new (alloc) Properties();
  RIterator it = propertyNames();
  while (it->hasNext() == true)
  {
    RObject o = it->next();
    if (instanceof(o, String) == false)
      continue;
    RString key = (RString)o;
    newprops->setProperty(key, getProperty(key));
  }
  if (_defaults != Nil)
    newprops->_defaults = (RProperties)_defaults->clone(alloc);
  return &newprops;
}

//static
RProperties
Properties::loadProperties(IN(RString) name)
{
  RString acdk_home = System::getAcdkHome();
  RString fqname = acdk_home + acdk::io::File::separator() + "cfg" +
      acdk::io::File::separator() + name->replace('.', acdk::io::File::separatorChar()) + ".properties";
  acdk::io::File f(fqname);
  if (f.exists() == false)
    return Nil;
  RProperties props = new Properties();
  props->load(f.getReader());
  return props;
}

void 
Properties::addPropertyChangeListener(IN(RPropertiesChangeListener) listener)
{
  if (_listener == Nil)
    _listener = new PropertiesChangeListenerArray(0);
  _listener->append(listener);
}

void 
Properties::removePropertyChangeListener(IN(RPropertiesChangeListener) listener)
{
  if (_listener == Nil)
    return;
  for (int i = 0; i < _listener->length(); ++i)
  {
    if (_listener[i] == listener)
    {
      if (_listener->length() == 1)
        _listener = Nil;
      else
        _listener->remove(i);
    }
  }
}

void
Properties::_notifyListener2(PropertiesChangeAction action, IN(RString) key, IN(RObject) obj)
{
  if (_listener == Nil)
      return;
  for (int i = 0; i < _listener->length(); ++i)
    _listener[i]->propertyChanged(action, this, key, obj);
}

} // util
} // acdk


