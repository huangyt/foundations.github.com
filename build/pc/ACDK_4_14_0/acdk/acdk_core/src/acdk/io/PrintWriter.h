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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/PrintWriter.h,v 1.29 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_PrintWriter_h
#define acdk_io_PrintWriter_h


#include "AbstractCharFilterWriter.h"

namespace acdk {
namespace io {


using namespace acdk::lang;

class PrintWriter;
ACDK_DECL_CLASS(PrintWriter);

/**
  PrintWriter provides formated character output.

  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.29 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC PrintWriter
: extends AbstractCharFilterWriter
{
  ACDK_WITH_METAINFO(PrintWriter)
protected:
  bool _flushOnNewLine;
  PrintWriter();
public:
  static RObject create_instance() { return new PrintWriter(); }

  PrintWriter(IN(RWriter) out, IN(acdk::locale::REncoder) encoder = Nil, IN(RObject) lock = Nil);
  PrintWriter(IN(RCharWriter) out);

// pure virtual from Writer
  /// implements form Writer
  foreign virtual void flush() {  _out->flush(); }
  /// implements form CharWriter
  foreign virtual void close() { _out->close(); }
  /*
  /// implements form Writer
  foreign virtual void write(byte c) { AbstractFilterWriter::write(c); }
  /// implements form Writer
  foreign virtual void write(const byte* cstr, int offset, int len) 
  {
    _out->write(cstr, offset, len);
  }
  /// implements form Writer
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1)
  {
    write(ch->data(), offset, len == -1 ? ch->length() - offset : len);
  }
  */
// PrintWriter 
  virtual void print(bool c);
  virtual void print(char c);
  virtual void print(ucchar c);
  virtual void print(byte c);
  virtual void print(short c);
  virtual void print(int c);
  virtual void print(jlong c);
  virtual void print(long c);
  virtual void print(float c);
  virtual void print(double c);
  virtual void print(IN(RObject) c);
  virtual void print(IN(RString) str);
  virtual void print(const String& str);
  virtual void print(const char* str);
  virtual void println();
  virtual void println(bool c);
  virtual void println(char c);
  virtual void println(ucchar c);
  virtual void println(byte c);
  virtual void println(short c);
  virtual void println(int c);
  virtual void println(long c);
  virtual void println(jlong c);
  virtual void println(float c);
  virtual void println(double c);
  virtual void println(IN(RObject) c);
  virtual void println(IN(RString) str);
  virtual void println(const String& str);
  virtual void println(const char* str);
  /// used for dmi
  RPrintWriter operator_lt_lt(bool c) { print(c); return this; }
  RPrintWriter operator_lt_lt(char c) { print(c); return this; }
  RPrintWriter operator_lt_lt(ucchar c) { print(c); return this; }
  RPrintWriter operator_lt_lt(byte c) { print(c); return this; }
  RPrintWriter operator_lt_lt(short c) { print(c); return this; }
  RPrintWriter operator_lt_lt(int c) { print(c); return this; }
  RPrintWriter operator_lt_lt(jlong c) { print(c); return this; }
  RPrintWriter operator_lt_lt(float c) { print(c); return this; }
  RPrintWriter operator_lt_lt(double c) { print(c); return this; }
  RPrintWriter operator_lt_lt(IN(RObject) c) { print(c); return this; }
  RPrintWriter operator_lt_lt(IN(RString) c) { print(c); return this; }
  


  /** 
    print C quote string 
    @code
    Text = 'a string is a "string"
            next line.
    // will be 
    text = '"a string is a \"string\"\nnext line."'
    @endcode
    @see InputReader::readQuoted
  */
  virtual void printQuoted(IN(RString) str);
  /**
    should on println flush the underlying stream.
    by default (after construction) this is true
    for performance reason this may changed to false
  */
  bool getFlushOnNewLine() const { return _flushOnNewLine;  }
  void setFlushOnNewLine(bool doflush)  { _flushOnNewLine = doflush;  }
};

class ACDK_CORE_PUBLIC StreamEndline
{
};

extern ACDK_CORE_PUBLIC StreamEndline endln;


template <typename T>
inline
PrintWriter& operator<<(PrintWriter& p, const RefHolder<T>& b)
{
  p.print((RObject)&b);
  return p;
}

inline
PrintWriter& operator<<(PrintWriter& p, const RString& b)
{
  p.print(b);
  return p;
}


template <typename T>
inline
PrintWriter& operator<<(RPrintWriter& rp, const RefHolder<T>& b)
{
  PrintWriter& p = *rp;
  p.print((RObject)&b);
  return p;
}


inline
PrintWriter& operator<<(RPrintWriter& rp, const RString& b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}




/*
template <typename T>
inline
PrintWriter& operator<<(PrintWriter& p, const T& b)
{
  p.print(b);
  return p;
}

template <typename T>
inline
PrintWriter& operator<<(RPrintWriter& rp, const T& b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}
*/
template <class T>
inline
PrintWriter& operator<<(PrintWriter& p, T* b)
{
  RObject obj = b;
  p.print(obj);
  return p;
}

template <class T>
inline
PrintWriter& operator<<(RPrintWriter& rp, T* b)
{
  PrintWriter& p = *rp;
  RObject obj = b;
  p.print(obj);
  return p;
}


inline
PrintWriter& operator<<(PrintWriter& p, bool b)
{
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(PrintWriter& p, char b)
{
  p.print(b);
  return p;
}


inline
PrintWriter& operator<<(PrintWriter& p, byte b)
{
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(PrintWriter& p, short b)
{
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(PrintWriter& p, int b)
{
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(PrintWriter& p, jlong b)
{
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(PrintWriter& p, float b)
{
  p.print(b);
  return p;
}


inline
PrintWriter& operator<<(PrintWriter& p, double b)
{
  p.print(b);
  return p;
}


inline
PrintWriter& operator<<(RPrintWriter& rp, bool b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(RPrintWriter& rp, char b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}


inline
PrintWriter& operator<<(RPrintWriter& rp, byte b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(RPrintWriter& rp, short b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(RPrintWriter& rp, int b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(RPrintWriter& rp, jlong b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(RPrintWriter& rp, float b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}


inline
PrintWriter& operator<<(RPrintWriter& rp, double b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(PrintWriter& p, const char* b)
{
  p.print(b);
  return p;
}

inline
PrintWriter& operator<<(PrintWriter& p, char* b)
{
  p.print(b);
  return p;
}


inline
PrintWriter& operator<<(RPrintWriter& rp, const char* b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}


inline
PrintWriter& operator<<(RPrintWriter& rp, char* b)
{
  PrintWriter& p = *rp;
  p.print(b);
  return p;
}




inline
PrintWriter& operator<<(PrintWriter& p, const StreamEndline& b)
{
  p.print("\n"); 
  if (p.getFlushOnNewLine() == true)
    p.flush();
  return p;
}

inline
PrintWriter& operator<<(const RPrintWriter& pp, const StreamEndline& b)
{
  acdk::io::PrintWriter& p = *pp;
  p.print("\n"); 
  if (p.getFlushOnNewLine() == true)
    p.flush();
  return p;
}


} // io
} // acdk



#endif //acdk_io_PrintWriter_h

