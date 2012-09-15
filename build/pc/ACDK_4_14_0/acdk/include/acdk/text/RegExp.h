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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/RegExp.h,v 1.17 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_text_RegExp_h
#define acdk_text_RegExp_h

#include "text.h"
//#include "regexp/pcre/pcreposix.h" // ## posix layer
#include "regexp/pcre/pcre.h"

namespace acdk {
namespace text {

enum RegExpFlags
{
  IgnoreCase    = PCRE_CASELESS,
  NoNewLine     = PCRE_MULTILINE,
  DotAll        =  PCRE_DOTALL,
  Extended      = PCRE_EXTENDED,
  Anchored      = PCRE_ANCHORED,
  DollarEndOnly = PCRE_DOLLAR_ENDONLY,
  Extra         = PCRE_EXTRA,
  NotBol        = PCRE_NOTBOL,
  NotEol        = PCRE_NOTEOL,
  UnGreedy      = PCRE_UNGREEDY,
  NotEmpty      = PCRE_NOTEMPTY,
  Utf8          = PCRE_UTF8
};
ACDK_DEF_LIB_ENUM(ACDK_TEXT_PUBLIC, RegExpFlags);


ACDK_DECL_CLASS(RegExpMatchPosition);

/** 
  Holds the Offsets of matching regular expresseions
  API: ACDK<br>
  
  @author Roger Rene Kommer
  @version $Revision: 1.17 $
  @date $Date: 2005/04/08 10:53:21 $
  @see RegExp
*/
class ACDK_TEXT_PUBLIC RegExpMatchPosition
: public ::acdk::lang::Object
{
public:
  int start;
  int end;
  RegExpMatchPosition(int s, int e) 
  : Object(),
    start(s),
    end(e)
  {
  }
  RegExpMatchPosition()
  : Object(),
    start(-1),
    end(-1)
  {
  }
  RString toString()
  {
    StringBuffer sb; sb << "start=[" << start << "]; end=[" << end << "]";
    return sb.toString();
  }
};

ACDK_DECL_CLASS(RegExp);
      
/** 
  Implements Regular Expression
  API: ACDK<br>
  
  @author Roger Rene Kommer
  @version $Revision: 1.17 $
  @date $Date: 2005/04/08 10:53:21 $
*/
class ACDK_TEXT_PUBLIC RegExp 
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(RegExp)
private:
  /** 
    stores the POSIX-error Code
  */
  int _errorCode; 
  /**
     from posix
  */
  pcre* _pcre; 
  pcre_extra* _pcre_extra;
public:
  /** 
    @param expression the regular expression
    @param cflags a combination of RegExpCompileFlags
  */
  RegExp(IN(RString) expression, int cflags = 0); //throw(RParseException, RThrowable);
  ~RegExp();
  /**
    Tests if the pattern matches at given str
    @str 
    @param eflags any combinations of RegExpEvaluateFlags
    @return true if matches
  */
  virtual bool test(IN(RString) str, int eflags = 0);

  
  int matchSize(IN(RString) text, int eflags = 0);
  /**
    returns the matching substring in slots
    @param text text to match
    @param slots array of ints.
           slot[0] until slot[1]: match of regular expression
           slot[n] until slot[n + 1]: matches n'th subexpression
    @param slotnum size of array slots
    @return -1 if not matching
  */
  foreign int match(IN(RString) text, int* slots, int slotnum, int eflags = 0);
  /**
    
    @param eflags any combinations of RegExpEvaluateFlags
    @return an StringArray: [0] substring matched
                            [1 - n] safed expression with '()' 
  */
  virtual RStringArray match(IN(RString) str, int eflags = 0);
  /**
    
    @param eflags any combinations of RegExpEvaluateFlags
    @return an RegExpMatchPosition: [0] substring matched
                                    [1 - n] safed expression with '()' 
  */
  virtual RRegExpMatchPositionArray matchPos(IN(RString) str, int eflags = 0);
  RString replace(IN(RString) text, IN(RString) with, bool replaceAll);

  /**
    Escapes all meta charakters
  */
  static RString escape(IN(RString) str);
protected:
  /**
    returns the matching substring in slots
    @return -1 if not matching, otherwise count of filled slots
    @slots return ajustes Character offset (not byte-offset of the UTF8-Stream
  */
  
  foreign int _match(const char* it, const char* end, int* slots, int slotnum, int eflags = 0);
  foreign int _matchSize(const char* it, const char* end, int eflags = 0);
};


} // namespace text 
} // namespace acdk 

#endif //acdk_text_RegExp_h

