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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_hashmap.h,v 1.9 2005/04/25 13:19:34 kommer Exp $

#ifndef acdk_lang_sys_core_hashmap_h
#define acdk_lang_sys_core_hashmap_h

#include "core_pair.h"
#include "core_prim.h"

namespace acdk {
namespace lang {
namespace sys {

template <class IK, class IV> class hashmap_iterator;

/** 
  very basic & specialized implementation of an hashmap hashmap.
  @author Roger Rene Kommer  
  This implementention is designed to store
  Keys and Values, which are basic types, pointers
  or dump structure.
  There are some asumptions:
  *((*int)(K*)) == 0 is an invalid key;
  Requirements:
  K and V: copy constructors.
  K: operator==
  V constructor V(int i = 0)
  int hash(const K& k);
*/

template <class K, class V> 
class core_flathashmap
{
private:
  /** allows multiple entries */
  bool _multimap;
  int _size;
public:
  typedef hashmap_iterator<K, V> iterator;
  typedef acdk::lang::sys::core_pair<K, V> value_type;
   struct container 
  {
    char flags;
    K key;
    V value;
    void setKey(const K& k) { new ((void*)&key) K(k); }
    void setValue(const V& v) { new ((void*)&value) V(v); }
    value_type& valtype() { return *((value_type*)&key); }
    const value_type& valtype() const { return *((value_type*)&key); }
    void destroy()
    {
      if (flags == 0)
        return;
      key.~K();
      value.~V();
      flags = 0;
    }
  };
private:
  container* _buffer;
  /** more slots than  */
  float _treshhold;
  /// doesn't allow copy construction
  core_flathashmap(const core_flathashmap<K, V>& other);
 
public:
  /** intitialize it with double size of maximum expected members */
  core_flathashmap(int size = 10, bool asmultimap = false, float treshhold = 2.0)
  : _multimap(asmultimap)
  , _size(acdk::lang::sys::core_get_next_prim(int(float(size) * treshhold)))
  , _buffer(0)
  , _treshhold(treshhold)
  {
    _buffer = (container*)new char[_size * sizeof(container)];
    memset(_buffer, 0, _size * sizeof(container));
  }
  ~core_flathashmap()
  {
    _destroy();
  }
  
  int _hash(const K& k, int maxsize)
  {
    int erg = hash(k, maxsize);
    return erg < 0 ? -erg : erg;
  }
  void put(const K& k, const V& v)
  {
    if (_put(k, v, _buffer, _size) == true) 
      return;
    _resize(int(float(_size) * 2 * _treshhold));
    _put(k, v, _buffer, _size);
  }
  void insert(const value_type& v)
  {
    put(v.first, v.second);
  }
  bool _put(const K& k, const V& v, container* buffer, int maxsize)
  {
    container* ptr;
    int tries = 0;
    int inithash =  _hash(k, maxsize);
    //sys::coreout << "[K[" << (int)k << "] V[" <<  (int)v <<  "] ih[" << inithash << "] s[" << maxsize - _valreserved << "]]" << sys::eofl;
    for (int i = inithash; i < maxsize; i++) 
    {
      container* c = buffer + i;
      if (c->flags == 0)
      {
        c->setKey(k);
        c->setValue(v);
        c->flags = 1;
        /*        
        if (tries > 2) 
        {
          //sys::coreout << "[K[" << (int)k << "] V[" <<  (int)v <<  "] ih[" << inithash << "] s[" << maxsize << "]]" << sys::eofl;
          //sys::coreout << "hashmap::puttries: " << tries << sys::eofl;
        }*/
        return true;
      } 
      else 
      {
        if (_multimap == false) 
        {
          if (c->key == k)
          {
            c->setValue(v);
            return true;
          }
        } 
      }
      if (k != c->key)
        ++tries;
    }
    return false;
  }
 
  /** in case of multimap only return first */
  iterator get(const K& k)
  {
    container* ptr;
    for (int i = _hash(k, _size); i < _size; i++) 
    {
      ptr = _buffer + i;
      if (ptr->flags == 1)
        return iterator(i, *this);
    } 
    return end();
  }
  bool _isValide(int pos)
  {
    if (pos >= _size)
      return false;
    container* ptr = _buffer + pos;
    return ptr->flags != 0;
  }
  K& key(int pos) 
  {
    return (_buffer + pos)->key;
  }
  const K& key(int pos) const
  {
    return (_buffer + pos)->key;
  }
  V& value(int pos) 
  {
    return (_buffer + pos)->value;
  }
  const V& value(int pos) const
  {
    return (_buffer + pos)->value;
  }
  
  int capacity() const { return _size; }
  value_type& get_value_type(int pos) 
  {
    return (_buffer + pos)->valtype();
  }
  const value_type& get_value_type(int pos) const
  {
    return (_buffer + pos)->valtype();
  }
  int size() const
  {
    int count = 0;
    for (int i = 0; i < _size; i++) 
    {
      if ((_buffer + i)->flags != 0)
        ++count;
    }
    return count;
  }
  
  
  iterator begin()
  {
    return iterator(0, *this);
  }
  iterator end()
  {
    return iterator(_size, *this);
  }
  iterator find(const K& key)
  {
    container* ptr;
    for (int i = _hash(key, _size); i < _size; i++) 
    {
      if ((_buffer + i)->key == key)
        return iterator(i, *this);
    }
    return end();
  }
  void erase(iterator it)
  {
    (_buffer + it._curpos)->destroy();
  }
  void erase(iterator it, iterator& e)
  {
    for (; it != e; ++it)
      (_buffer + it._curpos)->destroy();
  }
  /** in case of multmap, erases all k */
  void eraseAll(hashmap_iterator<K, V>& it)
  {
    for (int i = it._curpos; i < _size; i++) 
    {
      container* ptr = _buffer + it._curpos;
      if (ptr->key == it.key())
      {
        (_buffer + i).destroy();
      }
      else if (ptr.flags == 0)
        break;
    }
  }
protected:
  void _destroy()
  {
    if (_buffer == 0)
      return;
    for (int i = 0; i < _size; i++) 
    {
      (_buffer + i)->destroy();
    } 
    char* ptr = (char*)_buffer;
    delete[] ptr;
    _buffer = 0;
  }
  void _resize(int newsize)
  {
    newsize = acdk::lang::sys::core_get_next_prim(newsize);
    //sys::coreout << "[core_flathashmap] resize. old=[" << _size << "] new[" << newsize << "]" << sys::eofl;
    container* newbuf = (container*)new char[newsize * sizeof(container)];
    memset(newbuf, 0, newsize * sizeof(container));
    container* ptr;
    for (int i = 0; i < _size; i++) 
    {
      ptr = _buffer + i;
      if (ptr->flags != 0)
      {
        _put(ptr->key, ptr->value, newbuf, newsize);
      }
    } 
    _destroy();
    _buffer = newbuf;
    _size = newsize;
  }
};

template <class IK, class IV>
class hashmap_iterator
{
  int _curpos;
  core_flathashmap<IK, IV>& _map;
public:
  typedef acdk::lang::sys::core_pair<IK, IV> value_type;
  hashmap_iterator(int pos, core_flathashmap<IK, IV>& map)
      : _curpos(pos),
      _map(map)
  {
    seekNext();      
  }
  void seekNext()
  {
    for (;_curpos < _map.capacity(); _curpos++) 
      if (_map._isValide(_curpos) == true)
        break;
  }
  bool operator==(const hashmap_iterator<IK, IV>& other) const { return _curpos == other._curpos; }
  bool operator!=(const hashmap_iterator<IK, IV>& other) const { return _curpos != other._curpos; }
  bool operator<(const hashmap_iterator<IK, IV>& other) const { return _curpos < other._curpos; }
  bool operator<=(const hashmap_iterator<IK, IV>& other) const { return _curpos <= other._curpos; }
  bool operator>(const hashmap_iterator<IK, IV>& other) const { return _curpos > other._curpos; }
  bool operator>=(const hashmap_iterator<IK, IV>& other) const { return _curpos >= other._curpos; }
  void operator++()
  {
    if (_curpos + 1 <= _map.capacity()) {
      ++_curpos;
      seekNext();
    } else
      _curpos = _map.capacity();
  }
  
  IK& key() { return _map.key(_curpos);  }
  IV& value() { return _map.value(_curpos);  }
  value_type& operator*() { return _map.get_value_type(_curpos); }
  friend class core_flathashmap<IK, IV>;
};
  
}
}
}

#endif //acdk_lang_sys_core_hashmap_h



