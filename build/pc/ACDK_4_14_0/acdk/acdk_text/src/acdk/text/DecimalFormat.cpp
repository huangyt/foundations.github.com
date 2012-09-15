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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/DecimalFormat.cpp,v 1.15 2005/04/19 13:49:44 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/IllegalArgumentException.h>
#include <acdk/io/IOException.h>
#include "DecimalFormat.h"
#include "DecimalFormatSymbols.h"
#include "FieldPosition.h"

#if defined(__BORLANDC__)

namespace {
//  is needed to resolves a linker/template bug
void foo()
{
  try { 
    THROW1_FQ(::acdk::io::, IOException, "asdf");
  } catch (::acdk::io::RIOException ex){
  }
}

}



#endif

namespace acdk {
namespace text {

  /*
  class DigitNumber
: public ::acdk::lang::Object
{
public:
  int _decimalIdx;
  int _size;
  byteArray _bytes;
  enum {
    MaxLongDigits = 19
  };

  DigitNumber()
  : Object()
  , _decimalIdx(0)
  , _size(0)
  , _bytes(MaxLongDigits)
  {
  }
  void set(jlong v, int maxl);
  bool needRoundUp(int maxl);
  void round(int maxl);
};
*/

namespace {

/** @internal */
  int findTokenPos(IN(RString) pattern, char ch)
{
  const char* ptr = pattern->c_str();
  while (*ptr != 0) {
    if (*ptr == '\'')
      ++ptr;
    else if (*ptr == ';')
      return ptr - pattern->c_str();
    ++ptr;
  }
  return -1;
}

/** @internal */
  RString getPattern(bool isNeg, IN(RString) pattern)
{
  int negpatternidx = findTokenPos(pattern, ';');
  if (negpatternidx == -1)
    return pattern;
  if (isNeg == true) 
    return pattern->substr(negpatternidx + 1);
  else  
    return pattern->substr(0, negpatternidx);
}



} //anon namespace


/*
void 
DigitNumber::set(jlong v, int maxl)
{
  int left = MaxLongDigits;
  int right = 0;
  while (v > 0) 
  {
    _bytes[--left] = (byte) (((jlong) '0') + (v % 10));
    v /= 10;
  }
  _decimalIdx = MaxLongDigits - left;
  for (right = MaxLongDigits - 1; _bytes[right] == (byte) '0'; --right) 
  {
    //nothing
  }
  _size = right - left + 1;
  System::arraycopy(RbyteArray(&_bytes), left, RbyteArray(&_bytes), 0, _size);
  if (maxl > 0) 
    round(maxl);
}

bool
DigitNumber::needRoundUp(int maxl) 
{
  bool doIncrement = false;
  if (_bytes[maxl] > '5') 
    return true;
  if (_bytes[maxl] != '5' ) 
    return false;
  
  for (int i = maxl + 1; i < _size; ++i) {
    if (_bytes[i] != '0') {
      return true;
    }
  }
  if (maxl == 0)
    return true;
  return (_bytes[maxl - 1] % 2 != 0);
}

void 
DigitNumber::round(int maxl)
{
  if (maxl <= 0 || maxl >= _size) 
    return;
  if (needRoundUp(maxl)) {
    while (true)  {
      --maxl;
      if (maxl < 0) {
        _bytes[0] = (byte) '1';
        ++_decimalIdx;
        maxl = 0; 
        break;
      }
      ++_bytes[maxl];
      if (_bytes[maxl] <= '9') 
        break;
      
    }
    ++maxl; // Increment for use as _size
  }
  _size = maxl;
}
  

*/

DecimalFormat::DecimalFormat()
: _dateFormatSymbols(new DecimalFormatSymbols())
, _positiv(new DecimalSubpatternProperties(""))
, _negativ(new DecimalSubpatternProperties(""))
{
  
}

DecimalFormat::DecimalFormat(IN(RDecimalFormatSymbols) symbols)
: _dateFormatSymbols(symbols)
, _positiv(new DecimalSubpatternProperties(""))
, _negativ(new DecimalSubpatternProperties(""))
{
}

void 
DecimalFormat::applyLocalizedPattern(IN(RString) pattern)
{
}

void 
DecimalFormat::applyPattern(IN(RString) pattern)
{
  _pattern = pattern;
  if (pattern->indexOf(';') != -1) {
    _negativ = new DecimalSubpatternProperties(getPattern(true, pattern));
    _positiv = new DecimalSubpatternProperties(getPattern(false, pattern));
  } else
    _negativ = _positiv = new DecimalSubpatternProperties(_pattern);
}

RObject 
DecimalFormat::clone()
{
  return Object::serialized_clone(false);
}

bool 
DecimalFormat::equals(IN(RObject) obj)
{
  return Object::serialized_equals(obj, false);
}

/** @internal */
void checkGroup(const char* pattern, const char* pptr, const char* grouppos, int& groupsize)
{
  if (grouppos == 0)
    return;
  int diff = pptr - grouppos;
  if (groupsize != 0 && groupsize != diff)
    THROW1(IllegalArgumentException, RString("in sub pattern [") 
                    + pattern + "] grouping size must me always the same");
  groupsize = diff;
}

//foreign 
void 
DecimalSubpatternProperties::_analysePattern()
{
  const char* pptr = _pattern->c_str();

  const char* grouppos = 0;

  StringBuffer suffix("");
  StringBuffer prefix("");
  enum State 
  {
    Prefix = 0,
    Integer,
    Fraction,
    Suffix
  };
  State state = Prefix;
  while (*pptr) {
    switch (*pptr) {
    case '0':
      if (state == Prefix)
        state = Integer;
      if (state == Integer)
        ++_minimumIntegerDigits;
      else
        ++_minimumFractionDigits;
      break;
    case '#':
      if (state == Prefix)
        state = Integer;
      if (state == Integer)
        ++_maximumIntegerDigits;
      else
        ++_maximumFractionDigits;
      break;
    case ',' :
      if (grouppos != 0) {
        int diff = pptr - grouppos;
        if (_groupingSize != 0 && diff != _groupingSize)
          THROW1(IllegalArgumentException, RString("in sub pattern [") 
                    + _pattern + "] grouping size must me always the same");
        _groupingSize = diff;
      } else
        grouppos = pptr;
      break;
    case '-' :
      if (state == Fraction)
        state = Suffix;
      if (state == Prefix)
        prefix.append('-');
      else if (state == Suffix)
        suffix.append('-');
      break;
    case '\'':
      if (state > Prefix) {
        checkGroup(_pattern->c_str(), pptr, grouppos, _groupingSize);
        state = Suffix;
      }
      ++pptr;
      if (*pptr == '\'') {
        if (state == Prefix)
          prefix.append(*pptr);
        else
          suffix.append(*pptr);
        break;
      }
      while (*pptr != 0 && *pptr != '\'')
      {
        if (state == Prefix)
          prefix.append(*pptr);
        else
          suffix.append(*pptr);
        ++pptr;
      }
      break;
    case '.':
      if (state > Integer)
        THROW1(IllegalArgumentException, RString("in sub pattern [") 
                    + _pattern + "] only one '.' allowed");
      checkGroup(_pattern->c_str(), pptr, grouppos, _groupingSize);
      state = Fraction;
      break;
    case 'E':
      ++pptr;
      if (*pptr != 0) 
        THROW1(IllegalArgumentException, RString("in sub pattern [") 
                    + _pattern + "]: only exponent 0 is supported");
      _exponent = 0;
      break;
    case '%' :
      if (state == Fraction)
        state = Suffix;
      if (state == Prefix)
        _leftPercent = true; 
      else if (state == Suffix)
        _leftPercent = false; 
      else
        THROW1(IllegalArgumentException, RString("in sub pattern [") 
                    + _pattern + "]: '%' can only be in suffix or prefix");
      _multiplier = 100;
      break;
    default:
      THROW1(IllegalArgumentException, RString("in sub pattern [") 
                    + _pattern + "]: '" + *pptr + "' unknown pattern character");
      break;
    }
    ++pptr;
  }
  _suffix = suffix.toString();
  _prefix = prefix.toString();
  
}

/*
//foreign 
void 
DecimalFormat::format(DigitNumber& dn, 
                      StringBuffer& result, 
                      FieldPosition& fieldPosition,
                      bool is_int, bool is_negativ)
{
  
  //result.append(is_negativ ? _negativePrefix : _positivePrefix);

}
*/


/** @internal */
RString renderInteger(jlong num, int mind, int maxd, int radix)
{
  RString erg = Long::toString(num, radix);
  if (num < 0) // first char is '-'
    erg = erg->substr(1);
  int diff = mind - erg->length();
  while (diff-- > 0) 
  {
    erg = "0" + erg;
    
  }
  return erg;
}


/** @internal */
RString renderFraction(double num, int mind, int maxd)
{
  jlong lp = jlong(num);
  double fp = num - lp;
  RString erg = Double::toString(fp);
  RString fract = erg->substr(erg->indexOf('.') + 1);
  while (fract->length() < mind) {
    fract = fract + "0";
  }
  if (maxd > 0) {
    if (fract->length() > maxd) {
      fract = fract->substr(0, maxd);
    }
  } else {
    if (fract->length() > mind) {
      fract = fract->substr(0, mind);
    }
  }
  return fract;
}

/** @internal */
RString 
applyGrouping(IN(RString) text, int groupsize, char groupsymbol)
{
  if (groupsize == 0)
    return text;
  if (text->length() <= groupsize)
    return text;
  StringBuffer sb;
  const char* textptr = text->c_str();
  const char* eptr = textptr + text->length() - 1;
  for (int i = 0; eptr >= textptr; --eptr, ++i) 
  {
    if (i == groupsize - 1) {
      sb.insert(0, groupsymbol);
      i = 0;
    }
    sb.insert(0, *eptr);
  }
  return sb.toString();
}


RStringBuffer 
DecimalFormat::format(jlong number, IN(RStringBuffer) result, IN(RFieldPosition) fieldPosition)
{
  RDecimalSubpatternProperties props = _positiv;
  if (number < 0)
    props = _negativ;
  result->append(props->_prefix);
  if (number < 0)
    result->append(_dateFormatSymbols->getMinusSign());
  if (props->_multiplier != 1 && props->_leftPercent == true)
    result->append(props->_multiplier == 100 
                    ? _dateFormatSymbols->getPercent()
                    : _dateFormatSymbols->getPerMill());

  RString integ = renderInteger(number, props->_minimumIntegerDigits,  
                               props->_maximumIntegerDigits, 
                               props->_exponent == -1 ? 10 : props->_exponent);
  integ = applyGrouping(integ, props->_groupingSize, 
                        _dateFormatSymbols->getGroupingSeparator());
  result->append(integ);
  
  RString fract = renderFraction(0, props->_minimumFractionDigits,  
                                   props->_maximumFractionDigits);
  if (fract->length() > 0) {
    result->append(_dateFormatSymbols->getDecimalSeparator());
    result->append(fract);
  }
  if (props->_multiplier != 1 && props->_leftPercent == false)
    result->append(props->_multiplier == 100 
                    ? _dateFormatSymbols->getPercent()
                    : _dateFormatSymbols->getPerMill());
  result->append(props->_suffix);


  return result;
}


RStringBuffer 
DecimalFormat::format(double number, IN(RStringBuffer) result, IN(RFieldPosition) fieldPosition)
{
  RDecimalSubpatternProperties props = _positiv;
  if (number < 0)
    props = _negativ;
  result->append(props->_prefix);
  if (Double::isNaN(number) == true) {
    result->append(_dateFormatSymbols->getNaN());
  } else if (Double::isInfinite(number) == true) {
    result->append(_dateFormatSymbols->getInfinity());
  } else {
    if (number < 0)
      result->append(_dateFormatSymbols->getMinusSign());
    if (props->_multiplier != 1 && props->_leftPercent == true)
      result->append(props->_multiplier == 100 
                    ? _dateFormatSymbols->getPercent()
                    : _dateFormatSymbols->getPerMill());

    number = number * props->_multiplier;
    RString integ = renderInteger((jlong)number, props->_minimumIntegerDigits,  
                                  props->_maximumIntegerDigits, 
                                  props->_exponent == -1 ? 10 : props->_exponent);
    integ = applyGrouping(integ, props->_groupingSize, 
                        _dateFormatSymbols->getGroupingSeparator());
    result->append(integ);
    RString fract = renderFraction(number, props->_minimumFractionDigits,  
                                   props->_maximumFractionDigits);
    if (fract->length() > 0) {
      result->append(_dateFormatSymbols->getDecimalSeparator());
      result->append(fract);
    }
  }
  if (props->_multiplier != 1 && props->_leftPercent == false)
    result->append(props->_multiplier == 100 
                    ? _dateFormatSymbols->getPercent()
                    : _dateFormatSymbols->getPerMill());
  result->append(props->_suffix);
  return result;
}


RDecimalFormatSymbols 
DecimalFormat::getDecimalFormatSymbols()
{
  return _dateFormatSymbols; 
}
 
RStringBuffer 
DecimalFormat::format(IN(RObject) obj, IN(RStringBuffer) sb, IN(RFieldPosition) pos)
{
  return Nil;
}

int 
DecimalFormat::hashCode()
{
  return serialized_hashCode(false);
}

RNumber 
DecimalFormat::parse(IN(RString) text, IN(RParsePosition) parsePosition)
{
  //THROW1(
  return Nil; 
}


/*
bool 
DecimalFormat::isDecimalSeparatorAlwaysShown()
{
  return false; // fixme
}


void 
DecimalFormat::setDecimalFormatSymbols(RDecimalFormatSymbols newSymbols)
{
}

void 
DecimalFormat::setDecimalSeparatorAlwaysShown(bool newValue)
{
}
*/



/*
RString 
DecimalFormat::toLocalizedPattern()
{
  return Nil; //fixme
}

RString 
DecimalFormat::toPattern()
{
  return _pattern ; 
}

*/
} // text
} // acdk



