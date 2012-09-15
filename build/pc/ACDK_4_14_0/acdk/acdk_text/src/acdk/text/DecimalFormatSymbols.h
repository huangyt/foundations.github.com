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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/DecimalFormatSymbols.h,v 1.10 2005/04/14 06:28:05 kommer Exp $

#ifndef acdk_text_DecimalFormatSymbols_h
#define acdk_text_DecimalFormatSymbols_h


#include "text.h"
#include <acdk/lang/ObjectArrayImpl.h>
#include <acdk/util/Locale.h>
#include <acdk/util/ResourceBundle.h>

namespace acdk {
namespace text {


ACDK_DECL_CLASS(DecimalFormatSymbols);

class ACDK_TEXT_PUBLIC DecimalFormatSymbols
: public acdk::lang::Object
{
  ACDK_WITH_METAINFO(DecimalFormatSymbols)
private:
  RString _currencySymbol;
  uc2char _decimalSeparator;
  uc2char _digit;
  uc2char _groupingSeparator;
  RString _infinity;
  RString _currency;
  uc2char _minusSign;
  uc2char _monetaryDecimalSeperator;
  RString _naN;
  uc2char _patternSeparator;
  uc2char _percent;
  uc2char _perMill;
  uc2char _zeroDigit;
  void _init(IN(::acdk::util::RLocale) locale);
public:
  DecimalFormatSymbols() 
  : Object()
  {
    _init(::acdk::util::Locale::getDefault());
  }
  DecimalFormatSymbols(IN(::acdk::util::RLocale) locale) 
  : Object()
  {
    _init(locale);
  }
  RObject clone() { return serialized_clone(false); }
  bool equals(IN(RObject) obj) { return serialized_equals(obj, false); }
  
  RString getCurrencySymbol() { return _currencySymbol; }
  void setCurrencySymbol(IN(RString) currency) { _currencySymbol = currency; }
  uc2char getDecimalSeparator() { return _decimalSeparator; }
  void setDecimalSeparator(uc2char decimalSeparator) { _decimalSeparator = decimalSeparator; }
  uc2char getDigit() { return _digit; }
  void setDigit(uc2char digit) { _digit = digit; }
  uc2char getGroupingSeparator() { return _groupingSeparator; }
  void setGroupingSeparator(uc2char groupingSeparator) { _groupingSeparator = groupingSeparator; }
  RString getInfinity() { return _infinity; }
  void setInfinity(IN(RString) infinity) { _infinity = infinity; }
  RString getInternationalCurrencySymbol() { return _currency; }
  void setInternationalCurrencySymbol(IN(RString) currency) { _currency = currency; }
  uc2char getMinusSign() { return _minusSign; }
  void setMinusSign(uc2char minusSign) { _minusSign = minusSign; }
  uc2char getMonetaryDecimalSeparator() { return _monetaryDecimalSeperator; }
  void setMonetaryDecimalSeparator(uc2char monetaryDecimalSeperator) 
  {
    _monetaryDecimalSeperator = monetaryDecimalSeperator;
  }
  RString getNaN() { return _naN; }
  void setNaN(IN(RString) naN) { _naN = naN; }
  uc2char getPatternSeparator() { return _patternSeparator; }
  void setPatternSeparator(uc2char patternSeparator) 
  {
    _patternSeparator = patternSeparator; 
  }

  uc2char getPercent() { return _percent; }
  void setPercent(uc2char percent) { _percent = percent; }
  uc2char getPerMill() { return _perMill; }
  void setPerMill(uc2char perMill) { _perMill = perMill; }
  uc2char getZeroDigit() { return _zeroDigit; }
  void setZeroDigit(uc2char zeroDigit) { _zeroDigit = zeroDigit; }
  //### to implement int hashCode()
};

} // namespace text 
} // namespace acdk 


#endif //acdk_text_DecimalFormatSymbols_h

