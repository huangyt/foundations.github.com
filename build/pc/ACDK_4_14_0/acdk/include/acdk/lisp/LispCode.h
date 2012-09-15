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


#ifndef acdk_lisp_LispCode_h
#define acdk_lisp_LispCode_h

#include "LispFunction.h"
#include "LispList.h"
#include "LispCallBack.h"
#include "LispAtom.h"
#include "LispSymbol.h"
#include "LispArray.h"
#include <acdk/text/ParseException.h>
#include <acdk/util/ArrayList.h>
#include <acdk/util/Collections.h>

namespace acdk {
namespace lisp {

USING_CLASS(::acdk::text::, ParseException);

template <class T>
class Stack
: public acdk::lang::Object
{
  RObjectArrayImpl<T> _list;
  int _top;
public:
  Stack() 
  : Object(),
    _list(new ObjectArrayImpl<T>(0)),
    _top(0)
  {
  }
  void push(IN(T) t)
  {
    if (_top == _list->length()) {
      _list->resize(_top + 1);
    }
    _list[_top] = t;
    _top++;
  }
  T pop()
  {
    if (_top == 0)
      THROW1(ParseException, "Stack underflow");
    T ret = _list[_top - 1];
    _list[_top - 1] = Nil;
    --_top;
    return ret;
  }
  T top()
  {
    if (_top == 0)
      THROW1(ParseException, "Stack underflow");
    return _list[_top - 1];
  }
  T getFromTop(int idx)
  {
    if (idx > _top)
      THROW1(ParseException, "Stack underflow");
    return _list[_top - idx];
  }
  T getFromBottom(int idx)
  {
    if (idx > _top)
      THROW1(ParseException, "Stack overflow");
    return _list[idx];
  }
  bool empty() { return _top == 0; }
  int size() { return _top; }

  acdk::util::RIterator iterator()
  {
    acdk::util::RArrayList container = new acdk::util::ArrayList();
    ::acdk::util::Collections::addAll((acdk::util::RCollection)container, (RObjectArray)_list);
    return container->iterator();
  }
  
};

template <class T>
class StackHolder
{
  Stack<T>& _stack;
public:
  StackHolder(Stack<T>& stack, IN(T) var)
  : _stack(stack)
  {
    _stack.push(var);
  }
  ~StackHolder()
  {
    _stack.pop();
  }
};

ACDK_DECL_CLASS(LispCode);

class ACDK_ACDK_LISP_PUBLIC LispCode
: public acdk::lang::Object
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(LispCode)
private:
  /** the begin code */
  RLispList _code; 
  RLispList _last; 
  
public:
  /// for serialization
  static RObject create_instance() { return new LispCode(); }
  /**
    Used while parsing code. 

    0 = not quoted
    '\'' quote
    '`' backquote
    ',' comma
    '@' commaat
    'M' defmacro
    'm' macro usage
  */
  int quotech;

  LispCode() 
  : Object()
  , quotech(0)

  {
  }
  void append(IN(RLispVar) var)
  {
    if (_code == Nil) {
      _code = new LispList(var);
      _last = _code;
    } else {
      _last->_cdr = new LispList(var);
      _last = _last->_cdr;
    }
  }
  RLispVar last() 
  {
    if (_last == Nil)
      return Nil;
    return _last->_car;
  }
  RLispList code() 
  { 
    return _code; 
  }
  
};


inline RLispList LispCallBack::getDefinition() { return _declCode; }
inline RLispList LispCallBack::getDeclDefinition() { return _declCode; }

} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispCode_h
