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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/DecimalFormat.h,v 1.12 2005/04/19 13:49:46 kommer Exp $

#ifndef acdk_text_DecimalFormat_h
#define acdk_text_DecimalFormat_h


#include <acdk/io/Serializable.h>
#include <acdk/lang/Cloneable.h>

#include "NumberFormat.h"
#include "DecimalFormatSymbols.h"

namespace acdk {
namespace text {

using namespace acdk::lang;





ACDK_DECL_CLASS(DecimalSubpatternProperties);

/**
  Used to store the formatting options of DecimalFormat
  @author Roger Rene Kommer
*/
class ACDK_TEXT_PUBLIC DecimalSubpatternProperties
: extends ::acdk::lang::Object
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(DecimalSubpatternProperties)
public:
  /** is the pattern of the subpattern */
  RString _pattern;
  RString _prefix;
  RString _suffix;
  /** grouping 000.000.000 */
  int _groupingSize; 
  /** if % or permill */
  int _multiplier;
  bool _leftPercent;
  int _maximumFractionDigits;
  int _minimumFractionDigits;
  int _maximumIntegerDigits;
  int _minimumIntegerDigits;
  int _exponent;
  
  DecimalSubpatternProperties(IN(RString) pattern)
  : Object()
  , _pattern(pattern)
  , _prefix("")
  , _suffix("")
  , _groupingSize(0) 
  , _multiplier(1)
  , _leftPercent(true)
  , _maximumFractionDigits(0)
  , _minimumFractionDigits(0)
  , _maximumIntegerDigits(0)
  , _minimumIntegerDigits(0)
  , _exponent(-1)
  {
    if (_pattern != Nil && _pattern->length() > 0)
      _analysePattern();
  }
  
  bool equals(RObject other) { return serialized_equals(other, false); }
  RObject clone() { return serialized_clone(false); }
  
private:
  void _analysePattern();
};

ACDK_DECL_CLASS(DecimalFormat);

/**
  Corresponds the Java implementation.
  
  @author Roger Rene Kommer
  @note Not finished yet.
        - pattern 'E' not supported
        - parse not supported
        - toPattern() & toLocalizedPattern() not supported
*/

class ACDK_TEXT_PUBLIC DecimalFormat
: public acdk::text::NumberFormat
, implements acdk::io::Serializable
, implements acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(DecimalFormat)
private:
  RDecimalFormatSymbols _dateFormatSymbols;
  RString _pattern;
  RDecimalSubpatternProperties _positiv;
  RDecimalSubpatternProperties _negativ;
public:
  DecimalFormat();
  DecimalFormat(IN(RDecimalFormatSymbols) symbols);
 

  void applyLocalizedPattern(IN(RString) pattern);
  void applyPattern(IN(RString) pattern);
  
  RObject clone();
  foreign virtual RObject clone(sys::Allocator* alloc) { return Nil; } //#### implement and check concept
  bool equals(IN(RObject) obj);
  
  RStringBuffer format(double number, IN(RStringBuffer) result, IN(RFieldPosition) fieldPosition);
  RStringBuffer format(jlong number, IN(RStringBuffer) result, IN(RFieldPosition) fieldPosition) ;
  
  RStringBuffer format(IN(RObject) obj, IN(RStringBuffer) sb, IN(RFieldPosition) pos);

  RNumber parse(IN(RString) text, IN(RParsePosition) parsePosition);

  RDecimalFormatSymbols getDecimalFormatSymbols();
  
  int getGroupingSize() { return _positiv->_groupingSize; }
  void setGroupingSize(int newValue) 
  { 
    _positiv->_groupingSize = newValue; 
  }
  /*
  int getMultiplier() { return _multiplier; }
  void setMultiplier(int newValue) { _multiplier = newValue; }

  RString getNegativePrefix() { return _negativePrefix; }
  void setNegativePrefix(RString newValue) { _negativePrefix = newValue; }
  
  RString getNegativeSuffix() { return _negativeSuffix; }
  void setNegativeSuffix(RString newValue) { _negativeSuffix = newValue;} 
  
  
  RString getPositivePrefix() { return _positivePrefix; }
  void setPositivePrefix(RString newValue) { _positivePrefix = newValue; }
  RString getPositiveSuffix() { return _positiveSuffix; }
  void setPositiveSuffix(RString newValue) { _positiveSuffix = newValue; }

  
  bool isDecimalSeparatorAlwaysShown();

  
  void setDecimalFormatSymbols(RDecimalFormatSymbols newSymbols);
  void setDecimalSeparatorAlwaysShown(bool newValue);
  */
  /* already in NumberFormat
  void setMaximumFractionDigits(int newValue) { _maximumFractionDigits = newValue; }
  void setMaximumIntegerDigits(int newValue) { _maximumIntegerDigits = newValue; }
  void setMinimumFractionDigits(int newValue) { _minimumFractionDigits = newValue; }
  void setMinimumIntegerDigits(int newValue) { _minimumIntegerDigits = newValue; }
  */
  
  int hashCode();
  
  
  /*
    RString toLocalizedPattern();
    RString toPattern();
    */
protected:
 

};


} // text
} // acdk

#endif //acdk_text_DecimalFormat_h

