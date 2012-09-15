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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/BasicArray.h,v 1.61 2005/04/17 11:20:10 kommer Exp $

#ifndef acdk_lang_sys_BasicArray_h
#define acdk_lang_sys_BasicArray_h

#include <acdk.h>
#include <acdk/lang/dmi/ClazzInfo.h>
#include <acdk/lang/dmi/AcdkDmiClient.h>
#include <acdk/lang/reflect/Modifier.h>





/**
This template is only to wrap arrays with Basig types, like char, int, float
*/


template <class T> class BasicArray;
template <class T> class RBasicArray;

#if !defined(DOXYGENONLY)

extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields__length;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_bool_data;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_char_data;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_ucchar_data;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_byte_data;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_short_data;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_int_data;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_jlong_data;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_float_data;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_double_data;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_unknown_data;

#endif //!defined(DOXYGENONLY)

namespace acdk {
namespace io {
  // is implemented in ObjectReader.h
#ifdef ACDK_METROWORKS
  class ObjectReader;
  typedef ::InterfaceHolder<ObjectReader> RObjectReader;
#endif
  
  template <class T> void readBasicArray(BasicArray<T>* This, IN(::acdk::io::RObjectReader) in);
  //template <> void readBasicArray<byte>(BasicArray<bool>* This, IN(::acdk::io::RObjectReader) in);
  //template <> void readBasicArray<byte>(BasicArray<char>* This, IN(::acdk::io::RObjectReader) in);
  //template <> void readBasicArray<byte>(BasicArray<uc2char>* This, IN(::acdk::io::RObjectReader) in);
  //template <> void readBasicArray<byte>(BasicArray<uc4char>* This, IN(::acdk::io::RObjectReader) in);
  // template <> void readBasicArray<byte>(BasicArray<byte>* This, IN(::acdk::io::RObjectReader) in);
  //template <> void readBasicArray<byte>(BasicArray<short>* This, IN(::acdk::io::RObjectReader) in);
  //template <> void readBasicArray<byte>(BasicArray<int>* This, IN(::acdk::io::RObjectReader) in);
  //template <> void readBasicArray<byte>(BasicArray<jlong>* This, IN(::acdk::io::RObjectReader) in);
  //template <> void readBasicArray<byte>(BasicArray<float>* This, IN(::acdk::io::RObjectReader) in);
  //template <> void readBasicArray<byte>(BasicArray<double>* This, IN(::acdk::io::RObjectReader) in);
}
}

/*
#if 0 // note; just to make life a little bit easier

class BasicArray
: public ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(BasicArray)
public:
  virtual int length();
  jlong get();
  void set(jlong v);
  void ensureCapacity(int newSize);
  void resize(int newSize);
};
#endif //0
*/

#if !defined(DOXYGENONLY)
#ifndef ACDK_NOMETAINFO


/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_length_I;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_get_I_D;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_getref_I_D;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_set_I_D_V;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_ensureCapacity_I_V;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_resize_I_V;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_append_D_V;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_remove_I_V;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_methods_insert_I_D_V;
/// @internal
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_operator_bo_bc_I_D;

extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_findFirst_D_I;
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_findLast_D_I;

#endif //ACDK_NOMETAINFO
#endif //!defined(DOXYGENONLY)
/**
  BasicArray implements Java like arrays for basic types
  bool, char, byte, short, int, jlong, float and double
  Java usage: int[] iarray = new int[3]; iarray[0] = 1;
  ACDK usage: RintArray iarray = new intArray(3); iarray[0] = 1;

  @author Roger Rene Kommer
  @version $Revision: 1.61 $
  @see gw_ref[acdk_hb_lang_arrays].
*/
template <class T>
class ACDK_CORE_PUBLIC BasicArray
: extends ::acdk::lang::Object
{
protected:
  T* _data;
  int _length;
  int _capacity;
public :
  typedef T ElementType;
  typedef BasicArray<T> ArrayType;
  typedef T* iterator;
  /*
  BasicArray<T>()
  : Object(),
    _data(0),
    _length(0)
  {
#ifdef ACDK_DEBUG
    T t = 0; // to avoid, that Object used to inst template
    t++;
#endif
  }
  */
  /// @internal
  static ::acdk::lang::RObject create_instance() { return new BasicArray<jlong>(0); }
  /// @internal
  static ::acdk::lang::RObject create_arrayInstance(T dummy) { return new BasicArray<T>(int(0)); }
  /**
    standard constructor.
    if count > 0 the elements will be undefied
  */
  BasicArray<T>(int count = 0)
    : ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)(),
    _data(0),
    _length(count),
    _capacity(count)
  {
#ifdef _DEBUG
    T t = 0; // to avoid, that Object used to inst template
    t++;
#endif
    if (_capacity > 0) {
      //_data = new (allocator()) T[count];
      _data = (T*)allocate(_capacity * sizeof(T), acdk::lang::sys::DumpBufferMem);
    }
  }
  /**
    create array with given count of elements and fill with firstelem
  */
  BasicArray<T>(int count, T firstelem)
    : ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)(),
    _data(0),
    _length(count),
    _capacity(count)
  {
    if (_capacity > 0) {
      _data = (T*)allocate(_capacity * sizeof(T), acdk::lang::sys::DumpBufferMem);
      ::memset(_data, '\0', _capacity * sizeof(T));
    }
  }
  /**
    create pair array 
    count should be 2
  */
  BasicArray<T>(int count, T firstelem, T secelem)
    : ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)(),
      _data(0),
      _length(count),
      _capacity(count)

  {
    if (_capacity > 1) {
      _data = (T*)allocate(_capacity * sizeof(T), acdk::lang::sys::DumpBufferMem);
      set(0, firstelem);
      set(1, secelem);
    }
  }
  /**
    construct basic array from native T pointer/array
  */
  BasicArray<T>(const T *t, int size)
  : ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)(),
    _data(0),
    _length(size),
    _capacity(size)
  {
    if (_capacity > 0) {
      _length = size;
      _data = (T*)allocate(_capacity * sizeof(T), acdk::lang::sys::DumpBufferMem);
      for (int i = 0; i < _length; i++)
        _data[i] = t[i];
    }
  }
  /**
    cast constructor
  */
  template <class OT>
  BasicArray<T>(const BasicArray<OT>& other)
  : ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)(),
    _data(0),
    _length(other.length()),
    _capacity(other._capacity)
  {
    _data = (T*)allocate(_capacity * sizeof(T), acdk::lang::sys::DumpBufferMem);
    for (int i = 0; i < _length; i++)
      _data[i] = other[i];
  }
  /**
    cast constructor
  */
  BasicArray<T>(const BasicArray<T>& other)
  : ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)(),
    _data(0),
    _length(other.length()),
    _capacity(other._capacity)
  {
    _data = (T*)allocate(_capacity * sizeof(T), acdk::lang::sys::DumpBufferMem);
    for (int i = 0; i < _length; i++)
      _data[i] = other[i];
  }
  /// @internal
  virtual ~BasicArray()
  {
    if (_data) {
      deallocate(_data, acdk::lang::sys::DumpBufferMem);
      _data = 0;
    }
    _length = 0;
  }
  /**
    returns the length of the array
  */
  int length() const { return _length; }
  /**
    return the capacity (>= length) of the array
  */
  int capacity() const { return _capacity; }
  /**
    returns a native pointer to the underlying array storage
  */
  const T* data() const { return _data; }
  /**
    returns a native pointer to the underlying array storage
  */
  T* data() { return _data; }
  /**
    sets element at given position
    @throw ArrayIndexOutOfBoundsException
  */
  T& set(int idx, const T& t)
  {
    if (idx < 0 || idx >= _length)
      _throwArrayIndexOutOfBoundsException(idx);
    return _data[idx] = t;

  }
  /**
    get element at given position
    @throw ArrayIndexOutOfBoundsException
  */
  const T& get(int idx) const
  {
    if (idx < 0 || idx >= _length)
      _throwArrayIndexOutOfBoundsException(idx);
    return _data[idx] ;
  }
  /**
    get element at given position
    @throw ArrayIndexOutOfBoundsException
  */
  T& get(int idx)
  {
    if (idx < 0 || idx >= _length)
      _throwArrayIndexOutOfBoundsException(idx);
    return _data[idx] ;
  }
  /**
    get element at given position
    @throw ArrayIndexOutOfBoundsException
  */
  T& operator[](int idx) const
  {
    if (idx < 0 || idx >= _length)
      _throwArrayIndexOutOfBoundsException(int(idx));
    return _data[idx];
  }
  /*
  const T& operator[](int idx) const
  {
    if (idx < 0 || idx >= _length)
      ::acdk::lang::ObjectBase::_throwArrayIndexOutOfBoundsException(idx, _length, "BasicArray<T>::operator[]()");
    return _data[idx];
  }*/
  /**
    return true if size of arrays are equal and all elements are equal
  */
  virtual bool equals(IN(RBasicArray<T>) ta)
  {
    if (ta->length() != length())
      return false;
    for (int i = 0; i < length(); i++)
    {
      if (get(i) != ta->get(i))
        return false;
    }
    return true;
  }
  /**
    return true if object has same type of this array and 
    size of arrays are equal and all elements are equal
  */
  virtual bool equals(IN(::acdk::lang::RObject) o)
  {
    if (instanceof(o, ArrayType) == false)
      return false;
    if (o.impl() == this)
      return true;
    RBasicArray<T> ta = RBasicArray<T>(o);
    return equals(ta);
  }
  /** creates a copy of this array */
  virtual ::acdk::lang::RObject clone() { return clone(allocator()); }
  /** creates a copy of this array */
  virtual ::acdk::lang::RObject clone(::acdk::lang::sys::Allocator* alc)
  {
    BasicArray<T> *ni = new (alc) BasicArray<T>(_length);
    // memset here
    for (int i = 0; i < _length; i++)
      (*ni)[i] = (*this)[i];
    return ni;
  }
  /**
    set all elements of this array to given d
  */
  void memset(T d) { memset(d, 0, length() * sizeof(T)); }
  /**
    set all elements of this array to given d
  */
  void memset(T d, int offset, int l)
  {
    if (l <= 0 || l + offset > _length)
      return;
    while (l--)
      _data[offset++] = d;
  }
  /**
    copy given native array pointer data from given offset and length
    into this array
  */
  void memcpy(T* d, int offset, int len)
  {
    ::memcpy(d, _data + offset, len * sizeof(T));
  }
  /**
    ensure that this array has given newSize
    this method doesn't modify the size of the array
  */
  void ensureCapacity(int newSize)
  {
    if (newSize <= _capacity)
      return;
    makeCapacity(newSize > _capacity * 2 ? newSize : _capacity * 2);
  }
  /// @internal
  void makeCapacity(int newSize)
  {
    T* newdata = (T*)allocate(newSize * sizeof(T), acdk::lang::sys::DumpBufferMem);
    ::memset(newdata, '\0', newSize * sizeof(T));
    ::memcpy(newdata, _data, (_length <= newSize ? _length : newSize) * sizeof(T));
    T* olddata = _data;
    _data = newdata;
    _capacity = newSize;
    if (olddata)
      deallocate(olddata, acdk::lang::sys::DumpBufferMem);
  }
  /**
    resize this array to given size
    if newSize is > as length() the values
    of the new elements is undefined
  */
  void resize(int newSize)
  {
    if (newSize == _length)
      return;
    ensureCapacity(newSize);
    _length = newSize;
  }
  /**
    append the given value to this array
  */
  void append(T d)
  {
    ensureCapacity(_length + 1);
    _data[_length] = d;
    ++_length;
  }
  /**
    insert the given newelem into given idx
  */
  void insert(int idx, T newelem)
  {
    if (idx > _length)
      _throwIndexOutOfBoundsException(idx, _length, "BasicArray::insert()");
    ensureCapacity(_length + 1);
    for (int i = _length; i > idx; --i)
    {
      _data[i] = _data[i - 1];
    }
    _data[idx] = newelem;
    ++_length;
  }
  /**
    removes element at given position
  */
  void remove(int idx)
  {
    if (idx >= _length)
      _throwIndexOutOfBoundsException(idx, _length, "BasicArray::remove()");
    for (int i = idx; i < _length - 1; ++i)
    {
      _data[i] = _data[i + 1];
    }
    --_length;
  }
  /**
    append all elements of given array to this array
  */
  void concat(IN(RBasicArray<T>) s)
  {
    if (s == Nil || s->length() == 0)
      return;

    int newlength = _length + s->length();
    ensureCapacity(newlength);

    int i; int j;
    for (i = 0, j = _length; j < newlength; i++, j++)
      _data[j] = s[i];
    _length = newlength;
  }
  /**
    see in StringInline.h
  */
  ::acdk::lang::RString toString();
  /** get hascode of this array */
  int hashCode()
  {
    int erg = 0;
    for (int i = 0; i < length(); i++)
      erg = 31 * erg + int(get(i));
    return erg;
  }
  /** native STL-like iterator */
  iterator begin() { return _data == 0 ? 0 : &_data[0]; }
  /** native STL-like iterator */
  iterator end() { return _data == 0 ? 0 : &_data[_length]; }

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
  /// STL style function
  T popFront() { return pop_front(); }
  /// STL style function
  T back() const { return get(size() - 1); }
  /// STL style function
  T front() const { return get(0); }
  /// STL style function
  void push_front(const T& t) { insert(0, t); }
  /// STL style function
  void pushFront(const T& t) { insert(0, t); }
  /// STL style function
  void push_back(const T& t) { append(t); }
  /// STL style function
  void pushBack(const T& t) { append(t); }
  /// STL style function
  int size() const { return length(); }
  /// STL style function
  inline bool empty() const { return length() == 0; }
  /// STL style function
  T peekBack(int idx) { return get((length() - 1) - idx); }
  /**
    find the first element it is equal to given el
    return the index position of found element, return -1 if not found
  */
  int findFirst(const T& el) const
  {
    int al = length();
    for (int i = 0; i < al; ++i)
      if (_data[i] == el)
        return i;
    return -1;
  }
  /**
    find the last element it is equal to given el
    return the index position of found element, return -1 if not found
  */
  int findLast(const T& el) const
  {
    int al = length();
    for (int i = al - 1; i >= 0; --i)
      if (_data[i] == el)
        return i;
    return -1;
  }
  /**
    return the ClazzInfo of the element
    @internal
  */
  const ::acdk::lang::dmi::ClazzInfo* getElementClazzInfo()
  {
    T t = 0;
    return ::acdk::lang::dmi::getBasicTypeClazz<T>(t);
  }
  /// @internal
  static acdk::lang::dmi::ClazzInfo* clazzInfo();
  /// @internal
  virtual   ::acdk::lang::dmi::ClazzInfo* getClazzInfo()
  {
    return clazzInfo();
  }
  /// @internal
  static ::acdk::lang::RClass GetClass();
  /// @internal
  virtual ::acdk::lang::RClass getClass();
  /// @internal
  ::acdk::lang::RClass getMemberClass();
#ifndef ACDK_NOMETAINFO
  /// @internal
  virtual void readObject(IN(::acdk::io::RObjectReader) in)
  {
    ::acdk::io::readBasicArray(this, in);
  }
  /// @internal
  virtual void getCollectableFields(FieldReferences& fields)
  {
  }
  ::acdk::lang::dmi::ClazzFieldInfo* getArrayField()
  {
    if (typeid(T) == typeid(bool))
      return &BasicArray_fields_bool_data;
    if (typeid(T) == typeid(char))
      return &BasicArray_fields_char_data;
    if (typeid(T) == typeid(ucchar))
      return &BasicArray_fields_ucchar_data;
    if (typeid(T) == typeid(byte))
      return &BasicArray_fields_byte_data;
    if (typeid(T) == typeid(short))
      return &BasicArray_fields_short_data;
    if (typeid(T) == typeid(int))
      return &BasicArray_fields_int_data;
    if (typeid(T) == typeid(jlong))
      return &BasicArray_fields_jlong_data;
    if (typeid(T) == typeid(float))
      return &BasicArray_fields_float_data;
    if (typeid(T) == typeid(double))
      return &BasicArray_fields_double_data;
    return &BasicArray_fields_unknown_data;
  }
  /// @internal
  virtual acdk::lang::dmi::SysFields getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz = 0)
  {
    ::acdk::lang::dmi::SysFields __fields; //(length() + 1 =  Object::getInternalFields(flags);
    __fields.reserve(length() + 1);

    __fields.push_back(::acdk::lang::dmi::SysField(&BasicArray_fields__length, &_length));
    ::acdk::lang::dmi::ClazzFieldInfo* fi = getArrayField();
    for (int i = 0; i < _length; i++) {
      __fields.push_back(::acdk::lang::dmi::SysField(fi, &_data[i]));
    }
    return getImplFields(__fields);
  }
  /// @internal
  const ::acdk::lang::dmi::ClazzMethodInfo* lookupFunction(IN(acdk::lang::RString) fname_,
                                                           ::acdk::lang::dmi::ScriptVarArray& args,
                                                           int flags,
                                                            const ::acdk::lang::dmi::ClazzMethodInfo* methinf
                                                           );
  /// @internal
  virtual const ::acdk::lang::dmi::ClazzMethodInfo* standardDispatch(IN(acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret,
                                                         ::acdk::lang::dmi::ScriptVarArray& args,
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs, int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo = 0,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0);
#endif // ACDK_NOMETAINFO
};

/**
  Smart Pointer wrapper for BasicArray<T>
  @ingroup acdksmartptr
*/
template <class T>
class RBasicArray
: public ::RefHolder<BasicArray<T> >
{
public :
  RBasicArray<T>() : RefHolder<BasicArray<T> >() { }
  RBasicArray<T>(NilRef nil) : RefHolder<BasicArray<T> >(nil) { }
  RBasicArray<T>(BasicArray<T> *im) : RefHolder<BasicArray<T> >(im) { }
  explicit RBasicArray<T>(int c)
  : RefHolder<BasicArray<T> >(new BasicArray<T>(c))
  {
  }
  ~RBasicArray()
  {
  }
  explicit RBasicArray<T>(const T t[])
  : RefHolder<BasicArray<T> >(new BasicArray<T>(t, sizeof(t) / sizeof(T)))
  {
  }
  RBasicArray<T>(const T t[], int size)
  : RefHolder<BasicArray<T> >(new BasicArray<T>(t, size))
  {
  }
  template <class OT>
  upcast_explicit
  RBasicArray<T>(const RBasicArray<OT>& anotherarray)
  : RefHolder<BasicArray<T> >()
  {
    _arrayCastAssign(anotherarray);
  }

  /** copy/cast constructor */
  template <class OT>
  upcast_explicit
  RBasicArray(const RefHolder<OT>& other)
    : RefHolder<BasicArray<T> >()
  {
    ::acdk::lang::Object* o = const_cast< ::acdk::lang::Object*>((const ::acdk::lang::Object*)other.impl());
    if (o == 0)
      return;
    BasicArray<T>* t = dynamic_cast<BasicArray<T>*>(o);
    if (t != 0)
    {
      setImpl(t);
      return;
    }
    t = try_dmi_cast<BasicArray<T> >(other);
    if (t != 0)
    {
      setImpl(t);
      return;
    }
    T ttype = 0;
    badBasicArrayCast(::acdk::lang::dmi::getBasicTypeClazz<T>(ttype), o);
}

   /**
	  copy constructor
	*/

  RBasicArray<T>(const RBasicArray<T> &array)
    : RefHolder<BasicArray<T> >(array) { }

  /* don't do this, to dangerous of misuse

    RBasicArray(const ::acdk::lang::dmi::ScriptVar& sv)
  : RefHolder<BasicArray<T> >()
  {
    *this = (RBasicArray<T>) sv.getObjectVar();
  }
  */
  template <class OT>
  RBasicArray<T>& operator=(const RBasicArray<OT>& array)
  {
    _arrayCastAssign(array);
    return *this;
  }
  RBasicArray<T>& operator=(const RBasicArray<T>& array)
  {
    setImpl(array.iptr());
    return *this;
  }
  RBasicArray<T>& operator=(const BasicArray<T> *array)
  {
    setImpl(array);
    return *this;
  }
  RBasicArray<T>& operator=(NilRef nil)
  {
    setImpl(0);
    return *this;
  }

#ifdef ACDK_CORE_NEED_ROBJECT_CONVERTER
  /**
   because there is no implizite upcast functionality
   */
  operator ::acdk::lang::RObject () { return ::acdk::lang::RObject(*this); }
#endif

  /*
  static RBasicArray<T> cast(::acdk::lang::RObject other)
  {
    return RBasicArray<T>(other);
  }*/
  BasicArray<T>* operator->() const { return RBasicArray<T>::getIPtr(); }
  //const BasicArray<T>* operator->() const { return getIPtr(); }

  int length() const { return RBasicArray<T>::getIPtr()->length(); }

  T& set(int idx, T t) { return RBasicArray<T>::getIPtr()->set(idx, t); }
  const T& get(int idx) { return RBasicArray<T>::getIPtr()->get(idx); }
  T& operator[](int idx) const { return RBasicArray<T>::getIPtr()->operator[](idx); }
  RBasicArray<T>* _ref_this() { return this; }

public :

  // actually Casts from RBasicArray<String> to RBasicArray<RObject> doesn't work
  //virtual BasicArray<T>* impl() { return dynamic_cast<BasicArray<T>* >(RefHolder<Object>::impl()); }
  //virtual const BasicArray<T>* impl() const { return dynamic_cast<const BasicArray<T>* >(RefHolder<Object>::impl()); }
protected:
  void setImpl(const BasicArray<T>* t)
  {
    if (t == this->_iptr)
      return;
    if (this->_iptr != 0)
      RBasicArray<T>::impl()->releaseRef();
    this->_impl = (::acdk::lang::Object*)t;

    this->_iptr = (BasicArray<T>*)t;
    if (this->_iptr != 0)
      RBasicArray<T>::impl()->addRef();
  }
private:

  // casting from Array<String> to Array<RObject> doesn't work properly, because
  // they have no relation ship. Casting from String to Object is allowed.
  // Because
  // virtual classes are needed, an reinterpret_cast<> is not allowed in this situation
  //
  template <class OT>
  void _arrayCastAssign(const RBasicArray<OT>& a)
  {
    const BasicArray<OT>* ot = a.iptr();
    const BasicArray<T>* t = dynamic_cast<const BasicArray<T>* >(ot);
    if (t != 0 || ot == 0) {
      RefHolder<BasicArray<T> >::operator=(const_cast<BasicArray<T>*>(t));
      return;
    }

    int l = a->length();
    BasicArray<T>* ni = new BasicArray<T>(l);
    for (int i = 0; i < l; i++) {
      (*ni)[i] = T(a[i]);
    }
    setImpl(ni);
  }

};

/// standard basic type array type
typedef BasicArray<char> charArray;
/// standard basic type array type
typedef BasicArray<ucchar> uccharArray;
/// standard basic type array type
typedef BasicArray<short> shortArray;
/// standard basic type array type
typedef BasicArray<int> intArray;
/// standard basic type array type
typedef BasicArray<jlong> longArray;
/// standard basic type array type
typedef BasicArray<float> floatArray;
/// standard basic type array type
typedef BasicArray<double> doubleArray;
/// standard basic type array type
typedef BasicArray<bool> boolArray;
/// standard basic type array type
typedef BasicArray<bool> booleanArray;
/// standard basic type array type
typedef BasicArray<byte> byteArray;

/// standard basic type array reference type
typedef RBasicArray<char> RcharArray;
/// standard basic type array reference type
typedef RBasicArray<ucchar> RuccharArray;
/// standard basic type array reference type
typedef RBasicArray<short> RshortArray;
/// standard basic type array reference type
typedef RBasicArray<int> RintArray;
/// standard basic type array reference type
typedef RBasicArray<jlong> RlongArray;
/// standard basic type array reference type
typedef RBasicArray<float> RfloatArray;
/// standard basic type array reference type
typedef RBasicArray<double> RdoubleArray;
/// standard basic type array reference type
typedef RBasicArray<bool> RboolArray;
/// standard basic type array reference type
typedef RBasicArray<bool> RbooleanArray;
/// standard basic type array reference type
typedef RBasicArray<byte> RbyteArray;



/** helper function to create a BasicArray with a single expression */
template <class T>
inline
RBasicArray<T>  makeBasicArray(IN(T) a0)
{
  RBasicArray<T> t = new BasicArray<T>(1);
  t[0] = a0;
  return t;
}

/** helper function to create a BasicArray with a single expression */

template <class T>
inline
RBasicArray<T>  makeBasicArray(IN(T) a0, IN(T) a1)
{
  RBasicArray<T> t = new BasicArray<T>(2);
  t[0] = a0;
  t[1] = a1;
  return t;
}

/** helper function to create a BasicArray with a single expression */
template <class T>
inline
RBasicArray<T>  makeBasicArray(IN(T) a0, IN(T) a1, IN(T) a2)
{
  RBasicArray<T> t = new BasicArray<T>(3);
  t[0] = a0;
  t[1] = a1;
  t[2] = a2;
  return t;
}

/** helper function to create a BasicArray with a single expression */
template <class T>
inline
RBasicArray<T>  makeBasicArray(IN(T) a0, IN(T) a1, IN(T) a2, IN(T) a3)
{
  RBasicArray<T> t = new BasicArray<T>(4);
  t[0] = a0;
  t[1] = a1;
  t[2] = a2;
  t[3] = a3;
  return t;
}

/** helper function to create a BasicArray with a single expression */
template <class T>
inline
RBasicArray<T>  makeBasicArray(IN(T) a0, IN(T) a1, IN(T) a2, IN(T) a3, IN(T) a4)
{
  RBasicArray<T> t = new BasicArray<T>(5);
  t[0] = a0;
  t[1] = a1;
  t[2] = a2;
  t[3] = a3;
  t[4] = a4;
  return t;
}

#endif //acdk_lang_sys_BasicArray_h
