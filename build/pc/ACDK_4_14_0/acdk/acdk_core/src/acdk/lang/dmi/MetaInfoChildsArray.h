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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/MetaInfoChildsArray.h,v 1.8 2005/02/05 10:44:58 kommer Exp $

#ifndef acdk_lang_dmi_MetaInfoChildsArray_h
#define acdk_lang_dmi_MetaInfoChildsArray_h

#include <acdk.h>

namespace acdk {
namespace lang {
namespace dmi {



template <typename T>
class MetaInfoChildsArray
{
  T**& _ptr;
  int _size;
  int _capacity;
  typedef MetaInfoChildsArray<T> ThisType ;
public:
  MetaInfoChildsArray(T**& p)
  : _ptr(p)
  , _size(-1)
  , _capacity(-1)
  {
    if (_ptr != 0)
    {
      _size = size();
      _capacity = capacity();
    }
    else
      _size = 0;
  }
  MetaInfoChildsArray(T**& p, int initcap) 
  : _ptr(p)
  , _size(-1)
  , _capacity(initcap)
  {
    _ptr = ::new T*[_capacity + 2];
    memset(_ptr, 0, sizeof(T*) * _capacity);
    _ptr[1] = ACDK_CAST_INT2PTR(T, _capacity);
  }
  
  inline int capacity() const 
  { 
    if (_capacity != -1)
      return _capacity;
    if (_ptr == 0)
      return 0;
    int s = size();
    return  const_cast<ThisType*>(this)->_capacity = ACDK_CAST_PTR2INT(_ptr[s + 1]);
  }
  inline int size() const
  {
    if (_size != -1)
      return _size;
    return const_cast<ThisType*>(this)->_size = calcSize();
  }
  int calcSize() const
  {
    int i;
    for (i = 0; _ptr != 0 && _ptr[i] != 0; ++i)
	;
    return i;
  }
  void ensureCapacity(int newcap)
  {
    int oldsize = size();
    T** np = ::new T*[newcap + 2];
    memset(np, 0, sizeof(T*) * newcap + 2);
    if (_ptr != 0)
    {
      memcpy(np, _ptr, sizeof(T*) * oldsize);
      delete [] _ptr;
    }
    np[oldsize + 1] = ACDK_CAST_INT2PTR(T, newcap);
    _ptr = np;
  }
  
  void push_back(T* p)
  {
    int s = size();
    if (capacity() < s + 1)
      ensureCapacity(s + 10);
    _ptr[s] = p;
    _ptr[s + 2] = _ptr[s + 1];
    _ptr[s + 1] = 0;
    ++_size;
  }
  void remove(int idx)
  {
    int oldsize = size();
    int newsize = oldsize - 1;
    T** np = ::new T*[newsize + 2];
    memcpy(np, _ptr, idx * sizeof(T*));
    // size == 3, remove 1  copy 2 - n to 1
    memcpy(np + idx, _ptr + idx + 1, ((oldsize - idx) - 1) * sizeof(T*));

    np[newsize + 1] = np[newsize + 2]; // capacity
    _ptr[newsize] = 0;
    --_size;
  }
  void remove(T* p)
  {
    int idx = 0;
    int csize = size();
    for (int i = 0; i < csize; ++i)
    {
      if (_ptr[i] == p)
      {
        remove(i);
        return;
      }
    }
  }
  void pop_back() { remove(size() - 1); }
  void dispose(bool withChilds = true)
  {
    if (_ptr != 0)
    {
      if (withChilds == true)
      {
        for (int i = 0; _ptr[i] != 0; ++i)
          _ptr[i]->dispose();
      }
      delete [] _ptr;
      _ptr = 0;
    }
  }
  void copyTo(T**& targetptr, bool deep)
  {
    if (_ptr == 0)
      return;
    int s = size();
    MetaInfoChildsArray<T> t(targetptr, s);
    for (int i = 0; i < s; ++i)
    {
      if (deep == true)
        t.push_back(_ptr[i]->clone(deep));
      else
        t.push_back(_ptr[i]);
    }
  }
};

template <typename T>
T* createMetaInfo()
{
  T* t = new T();
  memset(t, 0, sizeof(T));
  return t;
}

} // dmi
} // lang
} // acdk

#endif //acdk_lang_dmi_MetaInfoChildsArray_h

