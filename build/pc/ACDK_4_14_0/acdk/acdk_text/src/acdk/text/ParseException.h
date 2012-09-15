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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/ParseException.h,v 1.8 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_text_ParseException_h
#define acdk_text_ParseException_h

#include "text.h"

#include <acdk/lang/Exception.h>

namespace acdk {
namespace text {

using namespace acdk::lang;

ACDK_DECL_THROWABLE(ParseException, Exception);

class ACDK_TEXT_PUBLIC ParseException
  : public acdk::lang::Exception
{
  ACDK_WITH_METAINFO(ParseException)
public:
  
  /**
   * Default Constructor
   */
  ParseException()
  : Exception()
  {
  }
  ParseException(IN(RString) msg)
  : Exception(msg)
  {
  }

  /**
   * This method initializes a new instance of ParseException with a detailed 
   * error message and a error position.
   * 
   * @param msg The descriptive message decribing the error.
   * @param offset The position where error was encountered.
   *
   */
  ParseException(IN(RString) msg, int offset)
    : Exception(msg),
      _offset(offset)
    {}


  /**
   * Destructor
   */
  virtual ~ParseException(){}

  /**
   * This method returns porition where the error occured.
   * 
   */
  int getErrorOffset() { return _offset; }
private:
  int _offset;
};


} // text
} // acdk

#endif

