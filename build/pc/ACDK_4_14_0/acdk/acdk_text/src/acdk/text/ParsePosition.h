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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/ParsePosition.h,v 1.8 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_text_ParsePosition_h
#define acdk_text_ParsePosition_h

#include "text.h"

namespace acdk {
namespace text {

using namespace acdk::lang;

ACDK_DECL_CLASS(ParsePosition);

/**
 * This class is used to keep track of the current position during parsing 
 * operations.
 */

class ACDK_TEXT_PUBLIC ParsePosition
  : public acdk::lang::Object
{
  ACDK_WITH_METAINFO(ParsePosition)
public:
  /**
   *
   */
  ParsePosition();

  /**
   * This method inizializes a new instance of ParsePosition to have specified
   * initial index value. 
   * 
   * @param index The initial parsing index.
   *
   */
  ParsePosition(int index);
  
  /**
   *
   */
  ~ParsePosition();
  
  /**
   * This method returns the current parsing index.
   *
   */
  int getIndex() { return _index; }

  /**
   * This method sets the curent parsing index to the specified value.
   * 
   * @param index The new parsing index.
   *
   */
  void setIndex(int index) { _index = index; }

  /**
   * This method returns the error index value. This value defaults to -1 
   * unless explicitly set to another value.
   *
   * @return The error index.
   */
  int getErrorIndex() { return _errorIndex; }

  /**
   * This method sets the error index to the specified value.
   *
   * @param errorIndex The new error index.
   *
   */
  void setErrorIndex(int errorIndex) { _errorIndex = errorIndex; }

public:
  
  /**
   * This method tests the specified object for quality with this object. The
   * two object will be considered equal if and only if all of the following
   * conditions are met.
   *   The specified object is not null.
   *   The specified object is an instance of ParsePosition.
   *   The specified object has the same index and error index as this object.
   *
   * @param obj The to test for quality against this object.
   *
   * @return true if the specified object is equal to this object, false 
   * otherwise.
   */
  bool equals(IN(RObject) obj);

  /**
   * This method returns a String representation of this object.
   * 
   *
   * @return A String that represents this object.
   */
  RString toString();

protected:

private:
  int _index;
  int _errorIndex;
};


} // Text
} //MM

#endif //acdk_text_ParsePosition_h

