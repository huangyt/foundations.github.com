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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/PrintWriter.cpp,v 1.19 2005/03/08 12:45:36 kommer Exp $





#include <acdk.h>
#include "PrintWriter.h"
#include "CharToByteWriter.h"
namespace acdk {
namespace io {


PrintWriter::PrintWriter()
: AbstractCharFilterWriter(new CharToByteWriter(Nil, Nil, Nil))
, _flushOnNewLine(true)
{
}

PrintWriter::PrintWriter(IN(RWriter) out, IN(acdk::locale::REncoder) encoder, IN(RObject) lock)
: AbstractCharFilterWriter(new CharToByteWriter(out, encoder, lock))
, _flushOnNewLine(true)
{
}

PrintWriter::PrintWriter(IN(RCharWriter) out)
: AbstractCharFilterWriter(out)
, _flushOnNewLine(true)
{
}

//virtual 
void
PrintWriter::print(bool c)
{
  print(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::print(char c)
{
  writeChar(c);
}

//virtual 
void 
PrintWriter::print(ucchar c)
{
  writeChar(c);
}

//virtual 
void 
PrintWriter::print(byte c)
{
  print(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::print(short c)
{
  print(String::valueOf(c));
  
}


//virtual 
void 
PrintWriter::print(int c)
{
  print(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::print(long c)
{
  print(String::valueOf(int(c)));
  
}

//virtual 
void 
PrintWriter::print(jlong c)
{
  print(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::print(float c)
{
  print(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::print(double c)
{
  print(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::print(IN(RObject) c)
{
  print(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::print(IN(RString) str)
{
  writeString(str);
}

//virtual 
void 
PrintWriter::print(const String& str)
{
  writeString(&str);
}

//virtual 
void 
PrintWriter::print(const char* str)
{
  String tstr(str);
  writeString(&tstr);
}
  
//virtual 
void
PrintWriter::println()
{
  String tstr("\n");
  writeString(&tstr);
  //_out->write((const byte*)"\n", 0, 1);
  if (_flushOnNewLine == true)
    _out->flush();
  
}

//virtual 
void 
PrintWriter::println(bool c)
{
  println(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::println(char c)
{
  char cbuf[2]; cbuf[0] = c; cbuf[1] = 0;
  println(cbuf);
}

//virtual 
void 
PrintWriter::println(ucchar c)
{
  println(String::valueOf(c));
}


//virtual 
void 
PrintWriter::println(byte c)
{
  println(String::valueOf(c));
}

//virtual 
void 
PrintWriter::println(short c)
{
  println(String::valueOf(c));
}


//virtual 
void 
PrintWriter::println(int c)
{
  println(String::valueOf(c));
}

//virtual 
void 
PrintWriter::println(long c)
{
  println(String::valueOf(int(c)));
  
}

//virtual 
void 
PrintWriter::println(jlong c)
{
  println(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::println(float c)
{
  println(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::println(double c)
{
  println(String::valueOf(c));
  
}

//virtual 
void 
PrintWriter::println(IN(RObject) c)
{
  if (c == Nil)
    println("Nil");
  else
    println(c->toString());
  
}


//virtual 
void
PrintWriter::println(IN(RString) str)
{
  String endl("\n");
  if (str == Nil) 
    writeString(&endl);
  else {
    writeString(str); //_out->write((const byte*)str->byte_begin(), 0, str->length());
    writeString(&endl);
  }
  if (_flushOnNewLine == true)
    _out->flush();
}

//virtual 
void
PrintWriter::println(const String& str)
{
  String endl("\n");
  writeString(&str); 
  writeString(&endl);
  if (_flushOnNewLine == true)
    _out->flush();
}

//virtual 
void 
PrintWriter::println(const char* str)
{
  String tstr(str);
  println(tstr);
  /*
  _out->write((const byte*)str, 0, strlen(str));
  _out->write((const byte*)"\n", 0, 1);
  _out->flush();
  */
  
}

//virtual 
void 
PrintWriter::printQuoted(IN(RString) str)
{
  
  String::iterator it = str->begin();
  String::iterator end = str->end();
  writeString("\"");
  while (it != end)
  {
    if (*it == '\\')
    {
      writeString("\\\\");
    } else if (*it == '\"') {
      writeString("\\\"");
    } else if (*it == '\n') {
      writeString("\\n");
    } else if (*it == '\t') {
      writeString("\\t");
    } else
      writeChar(*it);
    ++it;
  }
  writeString("\"");
}

StreamEndline endln;

} // io
} // acdk
