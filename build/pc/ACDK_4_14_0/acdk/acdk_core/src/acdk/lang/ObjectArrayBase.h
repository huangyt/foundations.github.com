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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ObjectArrayBase.h,v 1.50 2005/04/17 11:20:11 kommer Exp $

#ifndef acdk_lang_ObjectArrayBase_h
#define acdk_lang_ObjectArrayBase_h


namespace acdk {
namespace io {

  template <class T> void readObjectArray(ObjectArrayImpl<T>* This, ::acdk::io::RObjectReader in);
}
namespace util {
  ACDK_DECL_INTERFACE(Iterator);
  ACDK_DECL_INTERFACE(List);
}
}

/*

Redesign of ObjectArray and BasicArray
Targets:
  - Support for casting 
  - better DMI integration
  - SubArrays (equal solution to StringArrays with copy on write mechanism)
    (Not yet implemented)
  - less generated template code
  - Collection interface
*/
namespace acdk {
namespace lang {

ACDK_DECL_CLASS(ObjectArrayBaseImpl);
/**
  Object implements the array of Object similar to the 
  Java Object[] construct

  Normally will not used directly
  - and instance of template <class RefHolderClass> class RObjectArrayImpl;
  - or via DMI


  Java usage: Integer[] iarray = new Integer[3]; iarray[0] = new Integer(1);
  ACDK usage: RIntegerArray iarray = new IntegerArray(3); iarray[0] = new Integer(1);

  @author Roger Rene Kommer
  @version $Revision: 1.50 $
  @see gw_ref[acdk_hb_lang_arrays].
 
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiCiArray)) 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzNameAttribute("[")) 
class ACDK_CORE_PUBLIC ObjectArrayBaseImpl
: extends Object
{
  ACDK_WITH_METAINFO(ObjectArrayBaseImpl)
public:
  foreign typedef RObject* array_iterator;
protected:
  foreign array_iterator _begin;
  int _length;
  foreign array_iterator _endCap;
  foreign const acdk::lang::dmi::ClazzInfo* _elementClazzInfo;
public:
  
  /**
    creates an Array with given size
  */
  ACDK_METHODATTRIBUTE(acdk.tools.mc.SetDispatchAttribute("ObjectArrayBaseImpl::dmiConstructor", false))
  ObjectArrayBaseImpl(int size = 0)
  : _begin(0)
  , _length(size)
  , _endCap(0)
  , _elementClazzInfo(Object::clazzInfo())
  {
    _init(size);
  }
  /**
    Creating generic ObjectArray with given elemtype as member types
    Used for DMI
  */
  foreign ObjectArrayBaseImpl(const acdk::lang::dmi::ClazzInfo* elemtype, int size)
  : _begin(0)
  , _length(size)
  , _endCap(0)
  , _elementClazzInfo(elemtype)
  {
    _init(size);
  }
  foreign ~ObjectArrayBaseImpl()
  {
    _dispose();
  }
  foreign virtual Object* _cast(const acdk::lang::dmi::ClazzInfo* ci);
  static RObject create_instance();
  
  /**
    return the count of elements in this array
  */
  inline int length() const { return _length; }
  /**
    STL like array_iterator
  */
  foreign inline array_iterator begin() { return _begin; }
  /**
    STL like iterator
  */
  foreign inline array_iterator end() { return _begin + _length; }
  /**
    STL like iterator
  */
  foreign inline array_iterator endCap() { return _endCap; }

  /**
    return the element at idx position
    @throw ArrayIndexOutOfBoundsException if idx not in valid range
  */
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("get", 1))
  IN(RObject) getElement(int idx)
  {
    _checkIndexAccess(idx);
    return _begin[idx]; // before: .impl(), but why?
  }
  /**
    return the element at idx position as reference (left side value)
    @throw ArrayIndexOutOfBoundsException if idx not in valid range
  */
  
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("getref", 1))
  OUT(RObject) getElementRef(int idx) 
  {
    _makeSetSlot(idx);
    return _begin[idx];
  }
  
  OUT(RObject) operator[](int idx) { return getElementRef(idx); }

   /**
    set the element at idx position (method name in DMI is set)
    @throw ArrayIndexOutOfBoundsException if idx not in valid range
  */
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("set", 2))
  void setElement(int idx, IN(RObject) value)
  {
    _makeSetSlot(idx);
    _begin[idx] = _castAssignmentElement(value);
  }
  /**
    Set element at idx to Nil
    @throw ArrayIndexOutOfBoundsException if idx not in valid range
  */
  foreign void set(int idx, NilRef nil) 
  { 
    _makeSetSlot(idx);
    _begin[idx] = nil; 
  }
  /**
    append value to this array
  */
  void append(IN(RObject) value)
  {
    _makeAppendSlot();
    *(end() - 1) = _castAssignmentElement(value);
  }
  /** for DMI operator[](int idx) access */
  OUT(RObject) operator_bo(int idx)
  {
    return getElementRef(idx);
  }
  /**
    concat other array to this array
  */
  void concat(IN(RObjectArrayBaseImpl) other);
  /**
    insert obj into idx possition
    @throw IndexOutOfBoundsException if idx not in valid range
  */
  void insert(int idx, IN(RObject) value)
  {
    _makeInsertSlot(idx);
    *(_begin + idx) = _castAssignmentElement(value);
  }
  /**
    removes element at idx position
    @throw IndexOutOfBoundsException if idx not in valid range
  */
  void remove(int idx);
  /**
    Find equals element (using el->equals())
    If el == Nil find first element which also Nil.
    @return found index position
            or -1
  */
  int find(IN(RObject) el)
  {
    for (int i = 0; i < _length; ++i)
    {
      if (el == Nil &&  _begin[i]  == Nil)
        return i;
      if (el != Nil && _begin[i] != Nil)
        if (el->equals(_begin[i]) == true)
          return i;
    }
    return -1;
  }
  /**
    alias for find(IN(RObject) el)
  */
  int findFirst(IN(RObject) el) { return find(el); }
  /**
    find the element from starting with the last element
    using equals()
    @return found index position
            or -1
  */
  int findLast(IN(RObject) el)
  {
    int l = length();
    for (int i = l - 1; i >= 0; --i)
    {
      RObject d = _begin[i];
      if (el == Nil &&  d == Nil)
        return i;
      if (el != Nil && d != Nil)
        if (el->equals(d) == true)
          return i;
    }
    return -1;
  }
  /**
    find same object instance (using == instead of equals())
    
    @return found index position
            or -1 of not found
  */
  int findSame(IN(RObject) el)
  {
    int l = length();
    for (int i = 0; i < l; ++i)
      if (_begin[i] == el)
        return i;
    return -1;
  }
  /* 
    removes element element, which is equals
    @return Nil if not found
  */
  RObject removeEqualElement(IN(RObject) el)
  {
    int idx = find(el);
    if (idx == -1)
      return Nil;
    RObject d = _begin[idx];
    remove(idx);
    return d;
  }
  /**
    remove element which is same instance
    @return Nil if not found
  */
  RObject removeSameElement(IN(RObject) el)
  {
    int idx = findSame(el);
    if (idx == -1)
      return Nil;
    RObject d = _begin[idx];
    remove(idx);
    return d;
  }
  /** creates a read only iterator from this array */
  
  acdk::util::RIterator iterator();
  /** 
    create this as an container (ArrayList)
    @param copy take over this ObjectArray or crate internally an own
  */
  acdk::util::RList asContainer(bool copy = false);
  /**
    return the capacity of this array
  */
  inline int capacity() const { return _endCap - _begin; }
  /**
    make sure that internal storage capacity 
    is at least newsize elements
  */
  void ensureCapacity(int newsize);
  /// @internal
  void makeCapacity(int newsize)
  {
    _makeCapacity(newsize);
  }
  /**
    return the hashCode of this array
  */
  virtual int hashCode();

  virtual bool equals(IN(RObject) o);
  /**
    hard resize of this array. if newSize is less than
    current size elements will be deleted.
    if newSize is greater thatn current size new
    element will be set to Nil
  */
  void resize(int newSize);
  /** 
    return stringified representation [element, element] of this array
  */
  RString toString();
  RObject clone() { return clone(allocator()); }
  foreign RObject clone(::acdk::lang::sys::Allocator* alc);
  /**
    remove element from back and return the removed value
  */
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("popBack", 0))
  RObject pop_back_element() 
  {
    RObject l = getElement(length() - 1);
    remove(length() - 1);
    return l;
  }
  /**
    remove element from front (first element) and return the removed value
  */
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("popFront", 0))
  RObject pop_front_element()
  {
    RObject t = getElement(0);
    remove(0);
    return t;
  }
  /** return the last element */
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("back", 0))
  IN(RObject) back_element() { return getElement(length() - 1); }
  
  /// STL style function
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("front", 0))
  IN(RObject) front_element() { return getElement(0); }
  
  /// STL style function
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("pushFront", 1))
  void push_front_element(IN(RObject) obj) { insert(0, obj); }

  /// STL style function
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("pushBack", 1))
  void push_back_element(IN(RObject) obj) { append(obj); }
  /**
    get the element from index beginning with last element
  */
  ACDK_METHODATTRIBUTE(acdk.tools.mc.MethodAltNameAttribute("peekBack", 1))
  IN(RObject) peekElementBack(int idx) { return getElement((length() - 1) - idx); }
  /// @internal
  foreign static const ObjectArrayBaseImpl* _tryCast(const acdk::lang::dmi::ClazzInfo* toClazz, const ObjectArrayBaseImpl* other);
  foreign const acdk::lang::dmi::ClazzInfo* getElementClazzInfo() { return _elementClazzInfo; }
  foreign void setElementClazzInfo(const acdk::lang::dmi::ClazzInfo* ci) { _elementClazzInfo = ci; }
protected:
  /// @internal
  foreign void _init(int size)
  {
    ensureCapacity(size);
  }
  /// @internal
  foreign void _dispose();
  /// @internal
  foreign inline void _checkIndexAccess(int idx) const
  {
    if (idx >= _length || idx < 0)
      _throwArrayIndexOutOfBoundsException(idx, _length, "ObjectArrayImpl::get()");
  }
  /// @internal
  foreign inline RObject* _createData(int newsize)
  {
    RObject* data = new RObject[newsize];
    memset(data, 0, sizeof(RObject) * newsize);
    return data;
  }
  /// @internal
  foreign inline void _destroyData()
  {
    RObject* data = _begin;
    if (data != 0)
    {
      delete [] data;
      data = 0;
    }
  }
  /// @internal
  foreign void _makeCapacity(int newCap);
  /// @internal
  foreign void _makeSetSlot(int idx) const
  {
    _checkIndexAccess(idx);
  }
  /// @internal
  foreign void _makeInsertSlot(int idx);
  /// @internal
  foreign void _makeAppendSlot()
  {
    ensureCapacity(length() + 1);
    ++_length;
  }
  /**
    throws ClassCastException if value is not assignable to array element type
  */
  foreign RObject _castAssignmentElement(IN(RObject) value) const;
  
public:
  /// @internal
  foreign static const ::acdk::lang::dmi::ClazzMethodInfo* dmiConstructor(
                                                        ::acdk::lang::Object* This, 
                                                         IN(acdk::lang::RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf);
};

// ****************************************** ObjectArrayBase  **************************

/**
  base DMIable implementation of Arrays of Objects
  Normally this class will not be used by application code
*/
class ACDK_CORE_PUBLIC ObjectArrayBase
: extends ObjectArrayBaseImpl
{
public:
  ObjectArrayBase(int size = 0)
  : ObjectArrayBaseImpl(size)
  {
  }
  foreign ObjectArrayBase(const acdk::lang::dmi::ClazzInfo* elemtype, int size)
  : ObjectArrayBaseImpl(elemtype, size)
  {
  }
  /// @internal
  virtual acdk::lang::dmi::ClazzInfo* getClazzInfo();
  /// @internal
  virtual ::acdk::lang::RClass getClass();
  /// @internal
  virtual void getCollectableFields(FieldReferences& fields);
  /// @internal
  virtual ::acdk::lang::dmi::SysFields getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz = 0);
};

} // namespace lang
} // namespace acdk

// ****************************************** ObjectArrayImpl<T>  **************************

/// @internal
template <class T> class RObjectArrayImpl;


/**
  template implementation of Object Arrays
  This will be used via the ACDK_DECL_CLASS() 
  macro.
  Different to Java Object arrays in ACDK are resizable
  @ingroup acdksmartptr
*/
template <class T>
class ObjectArrayImpl
: extends ::acdk::lang::ObjectArrayBase

{

public:
  /// @internal
  static acdk::lang::dmi::ClazzInfo* clazzInfo();
  /// @internal
  static ::acdk::lang::RClass GetClass();
  
  typedef T* array_iterator;
  //typedef T* iterator; NOT because there is also a method
  ObjectArrayImpl(int size = 0)
  : ObjectArrayBase(T::clazzInfo(), size)
  {
  }
  ObjectArrayImpl(const acdk::lang::dmi::ClazzInfo* elemtype, int size)
    : ObjectArrayBase(elemtype, size)
  {
  }
  /** 
    return element at index idx
  */
  const T& get(int idx) const
  {
    _checkIndexAccess(idx);
    return _getData()[idx];
  }
  T& getref(int idx) const
  {
   _makeSetSlot(idx);
    return _getData()[idx];
  }
  void set(int idx, const T& value)
  {
    _makeSetSlot(idx);
    _getData()[idx] = value;
  }
  T& operator[](int idx) const { return getref(idx) ; }

  void append(const T& value)
  {
    _makeAppendSlot();
    _getData()[length() - 1] = value;
  }

  void insert(int idx, const T& value)
  {
    _makeInsertSlot(idx);
    _getData()[idx] = value;
  }
  /**
    Find equals element (using el->equals())
    If el == Nil find first element which also Nil.
    @return found index position
            or -1 of not found
  */
  int find(IN(T) el)
  {
    int l = length();
    for (int i = 0; i < l; ++i)
    {
      T d = _getData()[i];
      if (el == Nil &&  d == Nil)
        return i;
      if (el != Nil && d != Nil)
        if (el->equals((::acdk::lang::RObject)&d) == true)
          return i;
    }
    return -1;
  }
  /**
    alias for findFirst(IN(T) el)
  */
  int findFirst(IN(T) el) { return find(el); }
  /** 
    find the element (using equals()) from behind
    @return found index position or -1 if not found
  */
  int findLast(IN(T) el) 
  { 
    int l = length();
    for (int i = l - 1; i >= 0; --i)
    {
      T d = _getData()[i];
      if (el == Nil &&  d == Nil)
        return i;
      if (el != Nil && d != Nil)
        if (el->equals((::acdk::lang::RObject)&d) == true)
          return i;
    }
    return -1;
  }
  /**
    find same object
    @return found index position
            or -1 of not found
  */
  int findSame(IN(T) el)
  {
    int l = length();
    for (int i = 0; i < l; ++i)
      if (_getData()[i] == el)
        return i;
    return -1;
  }
  /* 
    removes element element, which is equals
    @return Nil if not found
  */
  T removeEqualElement(IN(T) el)
  {
    int idx = find(el);
    if (idx == -1)
      return Nil;
    T d = _getData()[idx];
    remove(idx);
    return d;
  }
  /**
    remove element which is same instance
    @return Nil if not found
  */
  T removeSameElement(IN(T) el)
  {
    int idx = findSame(el);
    if (idx == -1)
      return Nil;
    T d = _getData()[idx];
    remove(idx);
    return d;
  }
  /** append given other Array to this array */
  void concat(IN(RObjectArrayImpl<T>) other)
  {
    ensureCapacity(length() + other->length());
    array_iterator oit = other->begin();
    array_iterator oend = other->end();
    for (; oit < oend; ++oit)
      append(*oit);
  }
  /// STL style function
  array_iterator begin() { return _getData(); }
  /// STL style function
  array_iterator end() { return _getData() + length(); }

  virtual void readObject(IN(::acdk::io::RObjectReader) in)
  {
    ::acdk::io::readObjectArray(this, in);
  }
  /// STL style function
  T pop_back() 
  {
    T l = get(size() - 1);
    remove(size() - 1);
    return l;
  }
  /// STL style function
  T popBack() { return pop_back(); }
  /// STL style function
  T pop_front()
  {
    T t = get(0);
    remove(0);
    return t;
  }
  T popFront() { return pop_front(); }
  /// STL style function
  const T&  back() const { return get(size() - 1); }
  /// STL style function
  const T& front() const { return get(0); }
  /// STL style function
  void push_front(const T& t) { insert(0, t); }
  void pushFront(const T& t) { insert(0, t); }
  /// STL style function
  void push_back(const T& t) { append(t); }
  void pushBack(const T& t) { append(t); }
  /// STL style function
  int size() const { return length(); }
  /// STL style function
  inline bool empty() const { return length() == 0; }
  const T& peekBack(int idx) { return get((length() - 1) - idx); }
protected:
  T* _getData() const { return reinterpret_cast<T*>(_begin); }
};

// ****************************************** RObjectArrayImpl  **************************

/**
  reference holder for an array
  @ingroup acdksmartptr
*/
template <class T>
class RObjectArrayImpl
: public RefHolder<ObjectArrayImpl<T> >
{
public:
  
  static acdk::lang::dmi::ClazzInfo* clazzInfo();
  static ::acdk::lang::RClass GetClass();

  RObjectArrayImpl(NilRef nil = Nil) 
  : RefHolder<ObjectArrayImpl<T> >()
  {
  }
  
  RObjectArrayImpl(size_t size) 
  : RefHolder<ObjectArrayImpl<T> >(new ObjectArrayImpl<T>(size))
  {
  }
  
  template <class OT>
  upcast_explicit
  RObjectArrayImpl(ObjectArrayImpl<OT> *implptr) 
  : RefHolder<ObjectArrayImpl<T> >() 
  { 
    if (implptr == 0) 
      return;
    ObjectArrayImpl<T>* t = dynamic_cast<ObjectArrayImpl<T> *>(implptr);
    if (t == 0)
    {
      t = (ObjectArrayImpl<T>*)ObjectArrayBase::_tryCast(T::clazzInfo(), implptr);
      if (t == 0)
      {
        _copyArray(implptr);
        return;
      }
    }
    RObjectArrayImpl<T>::_impl = implptr->_getObjectPtr();
    RObjectArrayImpl<T>::_iptr = const_cast<ObjectArrayImpl<T>*>(t);
    RObjectArrayImpl<T>::_impl->addRef();
  }

  RObjectArrayImpl(ObjectArrayImpl<T> *impl) 
  : RefHolder<ObjectArrayImpl<T> >(impl) 
  { 
  }
 
  template <class OT>
  upcast_explicit
  RObjectArrayImpl(const RObjectArrayImpl<OT>& anotherarray)
  : RefHolder<ObjectArrayImpl<T> >() 
  {
    if (anotherarray.impl() == 0)
      return;
    
    ObjectArrayImpl<T>* t = dynamic_cast<ObjectArrayImpl<T> *>(anotherarray.impl());
    if (t == 0)
    {
      t = (ObjectArrayImpl<T>*)ObjectArrayBase::_tryCast(T::clazzInfo(), anotherarray.iptr());
      if (t == 0)
      {
        _copyArray(anotherarray.iptr());
        return;
      }
    }
    RObjectArrayImpl<T>::_impl = t;
    RObjectArrayImpl<T>::_iptr = t;
    RObjectArrayImpl<T>::_impl->addRef();
  }
  
  RObjectArrayImpl(const RObjectArrayImpl<T> &array) 
  : RefHolder<ObjectArrayImpl<T> >() 
  { 
    if (array.iptr() == 0)
      return;
    RObjectArrayImpl<T>::_impl = array.impl();
    RObjectArrayImpl<T>::_iptr = array.iptr();
    RObjectArrayImpl<T>::impl()->addRef();
  }
  /** 
  copy/cast constructor 
  */
  template <class OT> 
  upcast_explicit
  RObjectArrayImpl(const RefHolder<OT>& other) 
    : RefHolder<ObjectArrayImpl<T> >() 
  {
    if (other.impl() == 0)
      return;
    ::acdk::lang::Object* o = const_cast< ::acdk::lang::Object*>(other.impl());
    ObjectArrayImpl<T>* t = dynamic_cast<ObjectArrayImpl<T>*>(o);
    if (t == 0)
    {
      ObjectArrayBaseImpl* oab = dynamic_cast<ObjectArrayBaseImpl*>(o);
      if (oab == 0)
        ::acdk::lang::ObjectBase::_throwBadCastException(); 
      t = (ObjectArrayImpl<T>*)ObjectArrayBase::_tryCast(T::clazzInfo(), oab);
      if (t == 0)
      {
        _copyArray((ObjectArrayImpl<acdk::lang::RObject>*)other.impl());
        return;
      }
    }
    RObjectArrayImpl<T>::_impl = t;
    RObjectArrayImpl<T>::_iptr = t;
    RObjectArrayImpl<T>::_impl->addRef();
  }
  
  /* don't do this, it is too dangerous
  RObjectArrayImpl(const ::acdk::lang::dmi::ScriptVar& sv)
  : RefHolder<ObjectArrayImpl<T> >() 
  {
    *this = (RObjectArrayImpl<T>) sv.getObjectVar();      
  }
  */
#ifdef ACDK_CORE_NEED_ROBJECT_CONVERTER
  /** 
      because there is no implizite upcast functionality
  */
  operator ::acdk::lang::RObject () { return ::acdk::lang::RObject(*this); }
#endif //ACDK_CORE_NEED_ROBJECT_CONVERTER  

  
  template <class OT>
  RObjectArrayImpl<T>& operator=(const ObjectArrayImpl<OT> *anotherarray) 
  {
    _forceCompilerCastError(anotherarray);
    if (anotherarray == 0) 
    {
      setImpl(0);
      return *this;
    }
    const ObjectArrayImpl<T>* t = dynamic_cast<const ObjectArrayImpl<T> *>(anotherarray);
    if (t == 0) 
    {
      t = (ObjectArrayImpl<T>*)ObjectArrayBase::_tryCast(T::clazzInfo(), anotherarray);
      if (t == 0)
      {
        _copyArray(anotherarray);
        return;
      }
    }
    setImpl(t);
    return *this;
  }
  RObjectArrayImpl<T>& operator=(const ObjectArrayImpl<T> *anotherarray) 
  {
    setImpl(const_cast<ObjectArrayImpl<T>*>(anotherarray));
    return *this;
  }
  template <class OT>
  RObjectArrayImpl<T>& operator=(const RObjectArrayImpl<OT>& array) 
  {
    if (RObjectArrayImpl<T>::_iptr == 0 && array.iptr() == 0)
      return *this;
    return operator=(array.iptr());
  }
  
  RObjectArrayImpl<T>& operator=(NilRef nil)
  {
    RefHolder<ObjectArrayImpl<T> >::operator=(nil);
    return *this;
  }
  
  ObjectArrayImpl<T>* operator->() const { return RObjectArrayImpl<T>::getIPtr(); }
  //const ObjectArrayImpl<T>* operator->() const { return getIPtr(); }
  
  int length() const { return RObjectArrayImpl<T>::getIPtr()->length(); }
  void add(const T& t) { RObjectArrayImpl<T>::getIPtr()->add(t); }
  const T& get(int idx) const
  {
    return RObjectArrayImpl<T>::getIPtr()->get(idx);
  }
  T& get(int idx) 
  {
    return RObjectArrayImpl<T>::getIPtr()->getref(idx);
  }
  void set(int idx, T& t)
  {
    RObjectArrayImpl<T>::getIPtr()->set(idx, t);
  }
  T& operator[](int idx) const
  {
    return RObjectArrayImpl<T>::getIPtr()->getref(idx);
  }
  
  RObjectArrayImpl<T>* _ref_this() { return this; }
protected:
  /// only used to force compiler cast error
  void _forceCompilerCastError(const ObjectArrayImpl<T>* optr)
  {}

  void setImpl(const ObjectArrayImpl<T>* optr)
  {
    if (RObjectArrayImpl<T>::_iptr == optr)
      return;
    if (RObjectArrayImpl<T>::_iptr != 0)
      RObjectArrayImpl<T>::_impl->releaseRef();
    RObjectArrayImpl<T>::_impl =  const_cast<ObjectArrayImpl<T>*>(optr);
    RObjectArrayImpl<T>::_iptr =  const_cast<ObjectArrayImpl<T>*>(optr);
    if (RObjectArrayImpl<T>::_iptr != 0)
      RObjectArrayImpl<T>::_impl->addRef();
  }

protected:
  template <class OT> 
  void _copyArray(const RObjectArrayImpl<OT>& array)
  {
    _copyArray(array.iptr());
  }
  template <class OT> 
  void _copyArray(ObjectArrayImpl<OT>* oim)
  {
    if (oim == 0) {
      if (RObjectArrayImpl<T>::_iptr != 0)
        RObjectArrayImpl<T>::impl()->releaseRef();
      RObjectArrayImpl<T>::_impl = 0;
      RObjectArrayImpl<T>::_iptr = 0;
      return;
    } 
    if (oim->length() == 0)
    {
      ObjectArrayImpl<T>* noa = new ObjectArrayImpl<T>(0);
      RObjectArrayImpl<T>::setImpl(noa);
      return;
    }
    if (oim->get(0).impl()) 
    {
      T testref = (T)(oim->get(0)); // if OT and T not = compatible, a BadCastException will thrown here
      if ((void*)testref.iptr() != (void*)oim->get(0).iptr()) 
      {
        int nsize = oim->length();
        ObjectArrayImpl<T>* noa = new ObjectArrayImpl<T>(nsize);
        for (int i = 0; i < nsize; i++) {
          noa->set(i, (T)oim->get(i));
        }
        setImpl(noa);
        return;
      }
    }
    //## may not correctly, especially if virtual base classes are involved
    ObjectArrayImpl<T>* noa = reinterpret_cast<ObjectArrayImpl<T>*>(oim);
    if (RObjectArrayImpl<T>::_iptr == noa)
      return;
    if (RObjectArrayImpl<T>::_iptr != 0)
      RObjectArrayImpl<T>::impl()->releaseRef();
    RObjectArrayImpl<T>::_impl = noa;
    RObjectArrayImpl<T>::_iptr = noa;
    if (RObjectArrayImpl<T>::_iptr != 0) {
      RObjectArrayImpl<T>::impl()->addRef();
    }
  }
};




#endif //acdk_lang_ObjectArrayBase_h
