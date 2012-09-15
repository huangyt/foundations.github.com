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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/NumberFormat.h,v 1.12 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_text_NumberFormat_h
#define acdk_text_NumberFormat_h

#include <acdk.h>
#include <acdk/util/Locale.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Number.h>

#include "Config.h"
#include "Format.h"
#include "ParsePosition.h"

namespace acdk {
namespace text {


using namespace acdk::lang;
USING_CLASS(::acdk::util::, Locale);

ACDK_DECL_CLASS(NumberFormat);

class ACDK_TEXT_PUBLIC NumberFormat
: public acdk::text::Format
  //implements acdk::io::Serializable,
  //implements acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(NumberFormat)
protected:
  bool _groupingUsed;
  int _maximumFractionDigits;
  int _maximumIntegerDigits;
  int _minimumFractionDigits;
  int _minimumIntegerDigits;
  RByte _maxFractionDigits;
  

  bool _parseIntegerOnly;

public:
  NumberFormat();

  ~NumberFormat();
  
  static int INTEGER_FIELD;
  static int FRACTION_FIELD;
  
  
  static RNumberFormat getInstance(IN(RLocale) locale = Locale::getDefault()) 
  {
    return getNumberInstance(locale);
  }

  
  static RNumberFormat getNumberInstance(IN(RLocale) locale);

  static RNumberFormat getCurrencyInstance() 
  {
    return getCurrencyInstance(Locale::getDefault());
  }

  static RNumberFormat getCurrencyInstance(IN(RLocale) locale);

  static RNumberFormat getPercentInstance() 
  {
    return getPercentInstance(Locale::getDefault());
  }
  
  static RNumberFormat getPercentInstance(IN(RLocale) locale);

  static RObjectArrayImpl<RLocale> getAvailableLocales();

  int getMaximumIntegerDigits() 
  {
    return _maximumIntegerDigits;
  }
  
  void setMaximumIntegerDigits(int maximumIntegerDigits) 
  {
    _maximumIntegerDigits = maximumIntegerDigits;
    if (getMinimumIntegerDigits() > maximumIntegerDigits)
      setMaximumIntegerDigits(maximumIntegerDigits);
  }

  int getMinimumIntegerDigits() 
  {
    return _minimumIntegerDigits;
  }

  void setMinimumIntegerDigits(int minimumIntegerDigits) 
  {
    _minimumIntegerDigits = minimumIntegerDigits;
    if (getMaximumIntegerDigits() < minimumIntegerDigits)
      setMaximumIntegerDigits(minimumIntegerDigits);
  }

  int getMaximumFractionDigits() 
  {
    return _maximumFractionDigits;
  } 
  
  void setMaximumFractionDigits(int maximumFractionDigits) 
  {
    _maximumFractionDigits = maximumFractionDigits;
    if (getMinimumFractionDigits() > maximumFractionDigits)
      setMinimumFractionDigits(maximumFractionDigits);
  }
  
  int getMinimumFractionDigits() 
  {
    return _minimumFractionDigits;
  }

  void setMinimumFractionDigits(int minimumFractionDigits) 
  {
    _minimumFractionDigits = minimumFractionDigits;
    
    if (getMaximumFractionDigits() < minimumFractionDigits)
      setMaximumFractionDigits(minimumFractionDigits);
  }
  
  bool isGroupingUsed() 
  {
    return _groupingUsed;
  }
  
  void setGroupingUsed(bool groupingUsed) 
  {
    _groupingUsed = groupingUsed;
  } 

  bool isParseIntegerOnly() 
  {
    return _parseIntegerOnly;
  }

  void setParseIntegerOnly(bool parseIntegerOnly) 
  {
    _parseIntegerOnly = parseIntegerOnly;
  }
  

  RString format(jlong number); 

  RString format(double number);
  
  virtual RStringBuffer format(jlong number, IN(RStringBuffer) sb, IN(RFieldPosition) pos) = 0;

  virtual RStringBuffer format(double number, IN(RStringBuffer) sb, IN(RFieldPosition) pos) = 0;

  RNumber parse(IN(RString) str);
  
  virtual RNumber parse(IN(RString) str, IN(RParsePosition) pp) = 0;

  RObject parseObject(IN(RString) str, IN(RParsePosition) pp) 
  {
    return (RObject)parse(str,pp);
  }

  int hashCode();

  bool equals(IN(RObject) obj);

};


} // text
} // acdk

#endif //acdk_text_NumberFormat_h

