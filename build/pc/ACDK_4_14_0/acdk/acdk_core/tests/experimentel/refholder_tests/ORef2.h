// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 


#ifndef ORef2_h
#define ORef2_h
#if !defined(DOXYGENONLY)


template <class T>
class Referer
{
 public:
  int _refCount;
  int _flags;
  T* _iptr;
  Referer(T* impl)
    : _refCount(0)
    , _flags(0)
    , _iptr(impl)
  {
    
  }
  void addRef()
  {
    ++_refCount;
  }
  void releaseRef()
  {
    --_refCount;
  }
  T* operator->() { return _iptr; }
};

template <class T>
class ORef
{
 public:
  T* _iptr;
  Referer<T>* _impl;

  ORef(const Referer<T>* o)
    : _iptr(0)
    , _impl(o)
  {
    if (_impl == 0)
      return;
    _iptr = _impl->_iptr;
    _impl->addRef();
  }
  
  T* operator->() { return _iptr; }
  operator T* ()
  {
    return _impl->_iptr;
  }
};


template <class I>
class IRef
{
  T* _iptr;
  Referer<Object>* _impl;
public:

};

#endif //!defined(DOXYGENONLY)
#endif //ORef2_h
