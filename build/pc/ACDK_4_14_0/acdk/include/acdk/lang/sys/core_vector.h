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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_vector.h,v 1.19 2005/03/14 12:14:31 kommer Exp $

#ifndef acdk_lang_sys_core_vector_h
#define acdk_lang_sys_core_vector_h

#include <string.h>
namespace acdk {
namespace lang {
namespace sys {

template <class IT> class core_vector_iterator;
template <class IT> class core_vector_reverse_iterator;

void ACDK_CORE_PUBLIC core_vector_throw_out_of_index(int size, int idx);

/**
  replacement for the std::vector class.
  access outside valid range will throw acdk::lang::RIndexOutOfBoundsException
*/

template <class T> 
class core_vector 
{
private:
  T* _begin;
  T* _end;
  int _capacity;
public:
  typedef T value_type;
  typedef T* iterator;
  typedef const T* const_iterator;
  
  core_vector()
  : _begin(0)
  , _end(0)
  , _capacity(10)
  {
    _end = _begin = _allocData(_capacity);
  }
  explicit core_vector(int initsize, const T& fillwith = T())
  : _begin(0)
  , _end(0)
  , _capacity(initsize)
  {
    if (_capacity < 10)
      _capacity = 10;
    _end = _begin = _allocData(_capacity);
    for (int i = 0; i < initsize; i++)
    {
      ::new ((void*)(_end))T(fillwith);
      ++_end;
    }
  }
  core_vector(int initsize, int initcap, const T& fillwith)
  : _begin(0)
  , _end(0)
  , _capacity(0)
  {
    if (initcap < initsize)
      initcap = initsize;
    if (initcap <= 0)
      initcap = 10;

    _end = _begin = _allocData(initcap);
    _capacity = initcap;
    
    for (int i = 0; i < initsize; i++)
    {
      ::new ((void*)(_end))T(fillwith);
      ++_end;
    }
  }
  /** 
    copy constructor
  */
  core_vector(const core_vector<T>& other)
  : _begin(0)
  , _end(0)
  , _capacity(other._capacity)
  {
    _end = _begin = _allocData(_capacity);
    _copy2end(other.begin(), other.end());
  }
  ~core_vector()
  {
    _destroy();
  }
  core_vector<T>& operator=(const core_vector<T>& other)
  {
    if (this == &other)
      return *this;
    _destroy();
    _capacity  = other._capacity;
    _end = _begin = _allocData(sizeof(T) * _capacity );
    _copy2end(other.begin(), other.end());
    return *this;
  }
  bool operator==(const core_vector<T>& other) const
  {
    if (size() != other.size())
      return false;
    const_iterator it1 = _begin;
    const_iterator end1 = _end;
    const_iterator it2 = other._begin;
    const_iterator end2 = other._end;
    for (; it1 < end1; ++it1, ++it2)
      if (*it1 != *it2)
        return false;
    return true;
  }
  bool operator!=(const core_vector<T>& other) const
  {
    return operator==(other) == false;
  }
  void assign(const_iterator b, const_iterator e)
  {
    if (_begin == b && _end == e)
      return;
    _destroy();
    _ensureCapacity(e - b);
    _begin = _end;
    _copy2end(b, e);
  }
  void insert(iterator it, const_iterator first, const_iterator last)
  {
    int inspos = it - _begin;
    _ensureCapacity(size() + last - first);
    it = _begin + inspos;
    int tsize = size();
    int diff = last - first;
   
    int mvs = _end - it;
    memmove(it + diff, it, sizeof(T) * (_end - it));
    _end += diff;
    iterator mit = _begin + inspos;
    while (first < last)
    {
      new ((void*)mit) T(*first);
      ++first;
      ++mit;
    }
  }
  void insert(int idx, const T& elem)
  {
    if (idx < 0 || idx > size())
      core_vector_throw_out_of_index(size(), idx);
    insert(begin() + idx, &elem, &elem + 1);
  }
  /**
    ensure that the size of the vector is at least
    given newize. Fills new empty elements with fillwi
    th
  */
  void ensureSize(int newsize, const T& fillwith = T())
  {
    ensureCapacity(newsize);
    for (int i = size(); i < newsize; ++i)
    {
      ::new ((void*)(_end))T(fillwith);
      ++_end;
    }
  }
  /**
    resize the array on given newsize
    if newsize > as size() new slots will be
    filled with fillwith
  */
  void resize(int newsize, const T& fillwith = T()) 
  { 
    if (newsize > size())
      ensureSize(newsize, fillwith); 
    else
    {
      erase(begin() + newsize, end());
    }
  }

  void ensureCapacity(int newcap)
  {
    if (_capacity >= newcap)
      return;
    if (newcap < _capacity * 2)
      newcap = _capacity * 2;
    if (newcap < 10) 
      newcap = 10;
    else if (newcap < (int)((float)_capacity * 2))
      newcap = (int)((float)_capacity * 2);
    _ensureCapacity(newcap);
  }
  
  /// STL compatibility
  void reserve(int newcap) { ensureCapacity(newcap); }
  void _ensureCapacity(int newcap)
  {
    if (_capacity >= newcap)
      return;
    int tsize = size();
    T* newData = _allocData(newcap);
    memcpy(newData, _begin, sizeof(T) * tsize);
    _freeData();
    _begin = newData;
    _end = _begin + tsize;
    _capacity = newcap;
  }
  
  inline T& operator[](int idx)
  {
    check_range(idx);
    return _begin[idx];
  }
  inline const T& operator[](int idx) const
  {
    check_range(idx);
    return _begin[idx];
  }
  inline T& at(int idx) 
  { 
    check_range(idx);
    return _begin[idx]; 
  }
  inline const T& at(int idx) const 
  { 
    check_range(idx);
    return _begin[idx]; 
  }

  void add(const T& el)
  {
    ensureCapacity(size() + 1);
    ::new ((void*)(_end)) T(el);
    ++_end;
  }
  void push_back(const T& el) { add(el); }
  void addLast(const T& el) { add(el); }
  void addFirst(const T& el)
  {
    insert(_begin, &el, &el + 1);
  }
  inline int size() const { return _end - _begin; }
  inline int capacity() const { return _capacity; }
  inline bool empty() const { return _begin == _end; }
  inline T& front() 
  { 
    check_range(0);
    return *_begin; 
  }
  inline const T& front() const 
  { 
    check_range(0);
    return *_begin; 
  }
  inline T& back() 
  { 
    check_range(0);
    return *(_end - 1); 
  }
  const T& back() const 
  { 
    check_range(0);
    return *(_end - 1); 
  }
  
  inline void pop_back() 
  { 
    check_range(0);
    erase(end() - 1); 
  }
  inline void pop_front() 
  { 
    check_range(0);
    erase(begin(), begin() + 1); 
  }

  // set _size  to 0, doesn't care of any constructor/destructor 
  /*
  void reset_hard(int newsize = 0)
  {
    _size = newsize;
  }*/
  inline bool hasElement(const T& el) const
  {
    return find(el) != _end; 
  }
  inline void clear() { erase(begin(), end()); }
  
  inline iterator erase(iterator it)
  {
    return erase(it, it + 1);
  }
  iterator erase(iterator it, iterator eit)
  {
    if (it < _begin || it >= _end)
      return _end;
    
    _destroy(it, eit);
    
    int diff = eit - it;
    
    while (it + diff < _end) 
    {
      memcpy(it, it + diff, sizeof(T));
      ++it;
    }
    _end -= diff;
    return eit;
  }
  inline bool deleteElement(const T& el) 
  {
    iterator it = find(el);
    if (it == _end)
      return false;
    erase(it);
    return true;
  }
  const_iterator find(const T& t) const
  {
    
    for (const_iterator it = _begin;
         it < _end;
         ++it)
    {
      if (*it == t)
        return it;
    }
    return _end;
  }
  iterator find(const T& t) 
  {
    
    for (iterator it = _begin;
         it < _end;
         ++it)
    {
      if (*it == t)
        return it;
    }
    return _end;
  }
  friend class core_vector_iterator<T>;  
  inline iterator begin() { return _begin; }
  inline iterator end() { return _end; }
  inline const_iterator begin() const { return _begin;  }
  inline const_iterator end() const { return _end; }

  friend class core_vector_reverse_iterator<T>;  
  typedef core_vector_reverse_iterator<T> reverse_iterator;
  typedef core_vector_iterator<T> const_reverse_iterator; // ## short hack
  inline reverse_iterator rbegin() { return core_vector_reverse_iterator<T>(*this, size() - 1); }
  inline reverse_iterator rend() { return core_vector_reverse_iterator<T>(*this, -1); }
  inline T* data() { return _begin; }

  void swap(iterator first, iterator second)
  {
    // make dump copy
    char buff[sizeof(T)];
    ::memcpy(buff, first, sizeof(T));
    ::memcpy((char*)first, (char*)second, sizeof(T));
    ::memcpy(second, buff, sizeof(T));
  }
  inline void check_range(int idx) const
  {
    if (idx < 0 || idx >= size())
      core_vector_throw_out_of_index(size(), idx);
  }
protected:
  
  void _destroy()
  {
    _destroy(_begin, _end);
    _freeData();
    _begin = _end = 0;
  }
  void _destroy(iterator s, iterator e)
  {
    while (s < e) 
    {
      s->~T();
      ++s;
    }
  }
  T* _allocData(int elemcount)
  {
    return static_cast<T*>(operator new(sizeof(T) * elemcount));
  }
  void _freeData()
  {
    operator delete(_begin);
  }
  void _copy2end(const_iterator oit, const_iterator eit)
  {
    for (const_iterator it = oit;
         it < eit;
         ++it)
    {
      ::new ((void*)(_end)) T(*it);
      ++_end;
    }
  }

};


template <typename T>
inline
core_vector<T> 
make_core_vector(const T& t1, const T& t2, const T& t3)
{
  core_vector<T> ret(3);
  ret[0] = t1;
  ret[1] = t2;
  ret[2] = t3;
  return ret;
}

template <typename T>
inline
core_vector<T> 
make_core_vector(const T& t1, const T& t2)
{
  core_vector<T> ret(2);
  ret[0] = t1;
  ret[1] = t2;
  return ret;
}


template <typename T>
inline
core_vector<T> 
make_core_vector(const T& t1)
{
  core_vector<T> ret(1);
  ret[0] = t1;
  return ret;
}

/**
  reverse iterator vor core_vector
*/
template <class IT>
class core_vector_reverse_iterator
  { 
    core_vector<IT>& _vec;
    int _pos;
  public:
    core_vector_reverse_iterator(core_vector<IT>& vec, int start)
    : _vec(vec),
      _pos(start)
    {
    }
    core_vector_reverse_iterator(const core_vector_reverse_iterator<IT>& other)
    : _vec(other._vec),
      _pos(other._pos)
    {
    }
    core_vector_reverse_iterator& operator=(const core_vector_reverse_iterator<IT>& other)
    {
      _vec = other._vec;
      _pos = other._pos;
      return *this;
    }
    core_vector_reverse_iterator<IT>& operator++()
    {
      if (_pos <= 0)
        return *this;
      --_pos;
      return *this;
    }
    core_vector_reverse_iterator<IT>& operator--()
    {
      if (_pos >= _vec._size)
        return *this;
      ++_pos;
      return *this;
    }
    bool operator==(const core_vector_reverse_iterator<IT>&other) const { return _pos == other._pos; }
    bool operator!=(const core_vector_reverse_iterator<IT>&other) const { return _pos != other._pos; }
    bool operator<(const core_vector_reverse_iterator<IT>&other) const { return _pos > other._pos; }
    bool operator<=(const core_vector_reverse_iterator<IT>&other) const { return _pos > other._pos; }
    bool operator>(const core_vector_reverse_iterator<IT>&other) const { return _pos < other._pos; }
    bool operator>=(const core_vector_reverse_iterator<IT>&other) const { return _pos <= other._pos; }
    IT& operator*() { return _vec[_pos]; }
    const IT& operator*() const { return _vec[_pos]; }
};


/**
  Basic stack based on core_vector
*/
template <class T>
class core_stack 
{
  typedef core_vector<T> Container;
public:
  Container _vec;
  typedef typename Container::iterator iterator;
  typedef typename Container::const_iterator const_iterator;
  inline core_stack()
  {
  }
  inline core_stack(int initsize, int initcap, const T& fillwith = T())
  : _vec(initsize, initcap, fillwith)
  {
  }
  inline void push(const T& t)
  {
    _vec.push_back(t);
  }
  inline T pop()
  {
    T ret = _vec[_vec.size() - 1];
    _vec.erase(_vec.end() - 1);
    return ret;
  }
  inline void pop_noret()
  {
    _vec.check_range(0);
    _vec.erase(_vec.end() - 1);
  }
  inline T& top() { return _vec.back(); }
  inline const T& top() const { return _vec.back(); }
  inline T& bottom() { return _vec.front(); }
  inline const T& bottom() const { return _vec.front(); }
  inline int size() const { return _vec.size(); }
  inline bool empty() const { return _vec.empty(); }
  inline int capacity() const { return _vec.capacity(); }
  inline T& peek(int fromtop = 0) { return _vec[_vec.size() - 1 - fromtop]; }
  inline const T& peek(int fromtop = 0) const { return _vec[_vec.size() - 1 - fromtop]; }
  inline T& operator[](int idx) { return _vec[idx]; }
  inline const T& operator[](int idx) const { return _vec[idx]; }
};


/**
  use core_vector stack as scope.
  all items in core_stack 
  will be erased in destructor of core_stack_scope
  are inserted since construction
*/
template <class T>
class core_stack_scope
{
  core_stack<T>& _stack;
  typedef typename core_stack<T>::iterator iterator;
  int _savedTop;
  
public:
  core_stack_scope(core_stack<T>& stack)
  : _stack(stack)
  , _savedTop(stack.size())
  
  {
  }
  ~core_stack_scope()
  {
    _stack._vec.erase(_stack._vec.begin() + _savedTop, _stack._vec.end());
  }
  void commit() { _savedTop = _stack.size(); }
  void push(const T& t) { _stack.push(t); }
  T pop() { return _stack.pop(); }
  T& top() { return _stack.top(); }
};


} // sys
} // lang
} // acdk

#endif //acdk_lang_sys_core_vector_h

