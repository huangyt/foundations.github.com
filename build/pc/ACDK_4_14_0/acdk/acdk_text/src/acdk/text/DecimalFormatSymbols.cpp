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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/DecimalFormatSymbols.cpp,v 1.8 2005/03/19 21:29:27 kommer Exp $



#include "DecimalFormatSymbols.h"

namespace acdk {
namespace text {


using namespace acdk::lang;


void 
DecimalFormatSymbols::_init(IN(::acdk::util::RLocale) locale)
{
  ::acdk::util::RResourceBundle rb 
      = ::acdk::util::ResourceBundle::getBundle("acdk/locale/LocaleInfo", locale);
  if (rb->hasValue("currency.default") == true)
    _currencySymbol = rb->getString("currency.default");
  else
    _currencySymbol = "";
  
  _decimalSeparator = rb->getString("decimalSeparator")->charAt(0);
  _digit = rb->getString("digit")->charAt(0);
  _groupingSeparator = rb->getString("groupingSeparator")->charAt(0);
  _infinity = rb->getString("infinity");
  if (rb->hasValue("currency.intlCurrencySymbol") == true)
    _currency  = rb->getString("currency.intlCurrencySymbol");
  else
    _currency  = "";
  _minusSign =rb->getString("minusSign")->charAt(0);
  _monetaryDecimalSeperator = _decimalSeparator;//rb->getString("monetarySeparator")->charAt(0);
  _naN  = rb->getString("NaN");
  _patternSeparator = rb->getString("patternSeparator")->charAt(0);
  _percent = rb->getString("percent")->charAt(0);
  _perMill = rb->getString("perMill")->charAt(0);
  //_zeroDigit = rb->getString("zeroDigit")->charAt(0);
  _zeroDigit = 0; //?
}

} // namespace text 
} // namespace acdk 




