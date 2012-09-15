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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/RegExp.cpp,v 1.20 2005/03/08 18:49:55 kommer Exp $




#include "text.h"
#include "RegExp.h"
#include "ParseException.h"

#include "ParseException.h"

#include <acdk/io/ObjectReader.h> // for instantiation purposes
#include <acdk/io/MemWriter.h>
#include <acdk/io/BytePtrReader.h>
#include <acdk/locale/UTF8Encoding.h>
#include <acdk/lang/StringInternals.h>

extern "C" 
void* acdk_alloc(size_t size)
{
  return ::operator new (size);
}

extern "C" 
void  acdk_free(void* ptr)
{
  ::operator delete(ptr);
}


namespace acdk {
namespace text {

# define MAX_REGEXARGS 256

RegExp::RegExp(IN(RString) expression, int cflags/* = 0*/) 
  //throw(RParseException, RThrowable)
: Object()
, _errorCode(0)
, _pcre_extra(0)
{
  const char* errtext = 0;
  int offset = -1;
  RString expr = expression->convert(CCUtf8);
  _pcre = pcre_compile(expr->c_str(), cflags | PCRE_UTF8, &errtext, &offset, 0);
  if (_pcre == 0)
    THROW1(ParseException, "Error compiling RegExp: [" + expression + "] at " + offset + ": " + errtext);
}

RegExp::~RegExp()
{
  if (_pcre != 0)
  {
    pcre_free(_pcre);
    _pcre = 0;
  }
}
  
//virtual 
bool 
RegExp::test(IN(RString) str, int eflags)
{
   int margs[MAX_REGEXARGS];
  for (int i = 0; i < MAX_REGEXARGS; ++i)
    margs[i] = -1;
  acdk::io::MemWriter out;
  acdk::locale::UTF8Encoder enc;
  enc.encode(&out, str);
  int ret = pcre_exec(_pcre, _pcre_extra, (const char*)out.getBuffer()->data(), out.getBuffer()->length(), 0, eflags, margs, MAX_REGEXARGS);
  return ret == 0;
  //return regexec(&_regexpr,  str->c_str(), 0,   0, eflags) != REG_NOMATCH;
}

inline
int
utf8ByteToCharOffset(const char* begin, int offset)
{
  Utf8CharIterator it1(begin);
  Utf8CharIterator it2(begin + offset);
  return it2 - it1;
}

void 
utf8ByteToCharOffsets(const char* begin, int offscount, int* margs)
{
  for (int i = 0; i < offscount; ++i)
  {
    margs[i * 2] = utf8ByteToCharOffset(begin, margs[i * 2]);
    margs[i * 2 + 1] = utf8ByteToCharOffset(begin, margs[i * 2 + 1]);
  }
}

int 
RegExp::_matchSize(const char* it, const char* end, int eflags)
{
  int margs[MAX_REGEXARGS];
  for (int i = 0; i < MAX_REGEXARGS; ++i)
    margs[i] = -1;

  int erg = pcre_exec(_pcre, _pcre_extra, it, end - it, 0, eflags, margs, MAX_REGEXARGS);
  if (erg < 0)
    return -1;
  int mcount = 0;
  for (mcount = 0; mcount < MAX_REGEXARGS; ++mcount)
  {
    if (margs[mcount] == -1)
      break;
  }
  utf8ByteToCharOffsets(it, mcount, margs);
  return margs[1] - margs[0];
}

int 
RegExp::matchSize(IN(RString) text, int eflags)
{
   acdk::io::MemWriter out;
  acdk::locale::UTF8Encoder enc;
  enc.encode(&out, text);
  const char* begin = (const char*)out.getBuffer()->data();
  const char* end = begin + out.getBuffer()->length();
  return _matchSize(begin, end, eflags);
}



int 
RegExp::_match(const char* it, const char* end, int* slots, int slotnum, int eflags)
{
  for (int i = 0; i < slotnum; ++i)
    slots[i] = -1;
  int erg = pcre_exec(_pcre, _pcre_extra, it, end - it, 0, eflags, slots, slotnum);
  if (erg < 0)
    return -1;
  int mcount = 0;
  for (mcount = 0; mcount < MAX_REGEXARGS; ++mcount)
  {
    if (slots[mcount] == -1)
      break;
  }
  utf8ByteToCharOffsets(it, mcount, slots);
  return mcount / 2;
}

int 
RegExp::match(IN(RString) text, int* slots, int slotnum, int eflags)
{
  acdk::io::MemWriter out;
  acdk::locale::UTF8Encoder enc;
  enc.encode(&out, text);
  const char* begin = (const char*)out.getBuffer()->data();
  const char* end = begin + out.getBuffer()->length();
  return _match(begin, end, slots, slotnum, eflags);
}



//virtual 
RStringArray 
RegExp::match(IN(RString) str, int eflags)
{
  int margs[MAX_REGEXARGS];
  int mcount = match(str, margs, MAX_REGEXARGS, eflags);
  if (mcount == -1)
    return new StringArray(0);
  RStringArray sa = new StringArray(mcount);
  for (int i = 0; i < mcount; ++i)
  {
    sa[i] = str->substr(margs[i * 2], margs[i * 2 + 1]);
  }
  return sa;
}

//virtual 
RRegExpMatchPositionArray
RegExp::matchPos(IN(RString) str, int eflags)
{
  int margs[MAX_REGEXARGS];
  int mcount = match(str, margs, MAX_REGEXARGS, eflags);
  if (mcount == -1)
    return new RegExpMatchPositionArray(0);
  RRegExpMatchPositionArray ret = new RegExpMatchPositionArray(mcount);
  for (int i = 0; i < mcount; ++i)
  {
    ret[i] = new RegExpMatchPosition(margs[i * 2], margs[i * 2 + 1]);
  }
  return ret;
}


RString 
RegExp::replace(IN(RString) text, IN(RString) replwith, bool replaceAll)
{
  RString rest = text;
  RRegExpMatchPositionArray sa = matchPos(rest);

  RString erg;
  while (sa != Nil && sa->length() > 0)
  {
    //System::out << sa << endln;
    erg = erg + rest->substr(0, sa[0]->start) + replwith;
    rest = rest->substr(sa[0]->end);
    if (replaceAll == false)
      break;
    sa = matchPos(rest);
  }
  erg = erg + rest;
  return erg;
}


//static 
RString 
RegExp::escape(IN(RString) str)
{
  StringBuffer sb;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  while (it < end)
  {
    if ((*it >= 'a' && *it <= 'z') ||
        (*it >= 'A' && *it <= 'Z') ||
        (*it >= '0' && *it <= '9')
       )
      sb.append(*it);
    else
    {
      sb.append('\\');
      sb.append(*it);
    }
    ++it;
  }
  return sb.toString();
}

} // namespace text 
} // namespace acdk 


