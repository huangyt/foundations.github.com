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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/LispTokenizer.h,v 1.8 2005/02/05 10:45:12 kommer Exp $

#ifndef acdk_lisp_LispTokenizer_h
#define acdk_lisp_LispTokenizer_h

#include "lisp.h"

#include <acdk/io/StreamTokenizer.h>

namespace acdk {
namespace lisp {


ACDK_DECL_CLASS(LispTokenizer);


class ACDK_ACDK_LISP_PUBLIC LispTokenizer
: public ::acdk::io::StreamTokenizer
{
  /** contains the current parsed line */
  RStringBuffer _currentLine;
  /** contains last parsed white space */
  RString _wss;
  
public:
  LispTokenizer(IN(::acdk::io::RCharReader) in);
  int nextToken();
  bool isLispIdentifier(char c);
  /** 
    returns the the current parsing Line as Line reference in the form "file(line)"
  */
  RString currentLineReference();
  /// returns the the current parsing Line
  RString currentLine();
  bool eof() { return _eof; }
protected:
  int _nextToken();
  bool _readIdentifier();
  RString _readLispComment();
};



} // namespace lisp
} // namespace acdk

#endif //acdk_lisp_LispTokenizer_h

