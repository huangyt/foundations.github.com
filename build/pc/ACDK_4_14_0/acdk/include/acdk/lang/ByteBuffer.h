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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ByteBuffer.h,v 1.6 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_ByteBuffer_h
#define acdk_lang_ByteBuffer_h

#include <acdk.h>
#include "Number.h"
#include "Comparable.h"

#include "Exception.h"
#include "ClassCastException.h"
#include "NumberFormatException.h"
#include "../locale/Encoding.h"

namespace acdk {
namespace lang {

ACDK_DECL_INTERFACE(ByteBuffer);

/**
  a ByteBuffer holds a a sequence of bytes
*/
class ACDK_CORE_PUBLIC ByteBuffer
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ByteBuffer)
public:
  virtual int length() = 0;
  /**
    for performance reasons the ByteBuffer implementation should support this method
  */
  foreign virtual byte* begin() { THROW0(UnsupportedOperationException); return 0; }
  /**
    for performance reasons the ByteBuffer implementation should support this method
  */
  foreign virtual byte* end() { THROW0(UnsupportedOperationException); return 0; }
  /**
    begin() and end() are supported
  */
  virtual bool supportNativeIterator() { return false; }
  virtual RObject clone() = 0;
};

/**
  defines the Type of ByteBuffer slice
  @see ByteBuffer
*/
enum SliceType
{
  /**
    changes on slices writes also change original buffer
  */
  ShadowSlice      = 0x0000,
  /**
    if a slice will be change, make a copy/clone of the slice
  */
  CopyOnWriteSlice = 0x0001,
  /** create a copy/clone of the slice */
  ClonedSlice      = 0x0002
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, SliceType);


ACDK_DECL_INTERFACE(ReadByteBuffer);

/**
  a readable ByteBuffer
*/
class ACDK_CORE_PUBLIC ReadByteBuffer
: implements ByteBuffer
{
  ACDK_WITH_METAINFO(ReadByteBuffer)
public:
  /**
    returns the byte from given position
    May throw RuntimeException or ArrayIndexOutOfBoundsException
  */
  virtual byte get(int idx) = 0;
  /**
    creates a slice of this ByteBuffer
  */
  virtual RReadByteBuffer createReadSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice) = 0;
  
};

ACDK_DECL_INTERFACE(WriteByteBuffer);

/**
  a writeable ByteBuffer
*/
class ACDK_CORE_PUBLIC WriteByteBuffer
: implements ByteBuffer
{
  ACDK_WITH_METAINFO(WriteByteBuffer)
public:
  /**
    set the byte at given position
    May throw RuntimeException or ArrayIndexOutOfBoundsException
  */
  virtual void set(int idx, byte t) = 0;
  /**
    creates a slice of this ByteBuffer
  */
  virtual RWriteByteBuffer createWriteSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice) = 0;
};

ACDK_DECL_INTERFACE(ReadWriteByteBuffer);

/**
  A readable and writeable ByteBuffer 
*/
class ACDK_CORE_PUBLIC ReadWriteByteBuffer
: implements ReadByteBuffer
, implements WriteByteBuffer
{
  ACDK_WITH_METAINFO(ReadWriteByteBuffer)
public:
  /**
    creates a slice of this ByteBuffer
  */
  virtual RReadWriteByteBuffer createReadWriteSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice) = 0;
};

ACDK_DECL_INTERFACE(FlexByteBuffer);

/**
  A FlexBuffer can shrink and grow
  The FlexBuffer may also contains 
  an internal limit
*/
class ACDK_CORE_PUBLIC FlexByteBuffer
: implements ReadWriteByteBuffer
{
  ACDK_WITH_METAINFO(FlexByteBuffer)
public:
  /** 
    append the given byte 
  */
  virtual void append(byte t) = 0;
  /** 
    delete byte at given position and return it 
  */
  virtual byte deleteAt(int idx) = 0;
  /** 
    insert a given byte into given position 
  */
  virtual void insertAt(int idx, byte t) = 0;
  /**
    return -1 if this buffer has no write limit
  */
  virtual int limit() = 0;
  /**
    for performance: ensure capacity
  */
  virtual void ensureCapacity(int cap) = 0;
  /**
    set size of buffer
    if new size > as current size use fill byte to fill new elements
  */
  virtual void resize(int size, byte fill = 0) = 0;
  /**
    alias for the append(byte t) method
  */
  virtual void push(byte t) { append(t); }
  /**
    removes the last byte and return it
  */
  virtual byte pop() { return deleteAt(length() - 1); }

};  


ACDK_DECL_CLASS(SlicedReadByteBuffer);
/**
  holds a slice from another ReadByteBuffer
*/
class ACDK_CORE_PUBLIC SlicedReadByteBuffer
: extends Object
, implements ReadByteBuffer
{
  ACDK_WITH_METAINFO(SlicedReadByteBuffer)
protected:
  /// original byte buffer
  RReadByteBuffer _parent;
  // offset where the slice starts
  int _start;
  // offset where the slice ends
  int _end;
public:
  /**
    creates a slice from given ReadByteBuffer
    with given (absolut) start and end offsets
  */
  SlicedReadByteBuffer(IN(RReadByteBuffer) buf, int start = 0, int end = -1)
  : _parent(buf)
  , _start(start)
  , _end(end == -1 ? buf->length() - start : end)
  {
    if (end >= length())
      THROW0(IndexOutOfBoundsException);
  }
   virtual RObject clone()
  {
    return new SlicedReadByteBuffer((RReadByteBuffer)_parent->clone(), _start, _end);
  }
  virtual int length() { return _end - _start; }
  virtual byte get(int idx) 
  { 
    if (idx < 0 || idx > length())
      THROW0(IndexOutOfBoundsException);
    return _parent->get(idx + _start); 
  }
  virtual RReadByteBuffer createReadSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice);
  foreign virtual byte* begin() { return _parent->begin() + _start; }
  foreign virtual byte* end() { return _parent->begin() + _end; }
  virtual bool supportNativeIterator() { return _parent->supportNativeIterator(); }
};

ACDK_DECL_CLASS(SlicedReadWriteByteBuffer);
/**
  holds a slice from another ReadWriteByteBuffer
*/
class ACDK_CORE_PUBLIC SlicedReadWriteByteBuffer
: extends Object
, implements ReadWriteByteBuffer
{
  ACDK_WITH_METAINFO(SlicedReadWriteByteBuffer)
protected:
  RReadWriteByteBuffer _parent;
  int _start;
  int _end;
  SliceType _sliceType;
public:

  SlicedReadWriteByteBuffer(IN(RReadWriteByteBuffer) buf, int start, int end, SliceType sliceType)
  : _parent(buf)
  , _start(start)
  , _end(end)
  , _sliceType(sliceType)
  {
  }
  virtual RObject clone()
  {
    return new SlicedReadWriteByteBuffer((RReadWriteByteBuffer)_parent->clone(), _start, _end, ClonedSlice);
  }
  virtual int length() { return _end - _start; }
  virtual byte get(int idx) 
  { 
    if (idx < 0 || idx > length())
      THROW0(IndexOutOfBoundsException);

    return _parent->get(idx + _start); 
  }
  virtual void set(int idx, byte t) 
  { 
    if (idx < 0 || idx > length())
      THROW0(IndexOutOfBoundsException);
    if (_sliceType == CopyOnWriteSlice)
    {
      _parent = (RReadWriteByteBuffer)_parent->clone();
      _sliceType = ClonedSlice;
    }
    _parent->set(idx + _start, t);
  }
  virtual RReadByteBuffer createReadSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice);
  virtual RWriteByteBuffer createWriteSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice);
  virtual RReadWriteByteBuffer createReadWriteSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice);
};

/**
  internal helper implements standard methods
  for ByteBuffer, which implements native begin() and end() methods

*/
template <typename T>
class  SimplePtrItBuffer
{
protected:
  T* _begin;
  T* _end;
public:
  SimplePtrItBuffer()
    : _begin(0)
    , _end(0)
  {
  }
  SimplePtrItBuffer(T* begin, T* end)
    : _begin(begin)
    , _end(end)
  {
  }
  int length() { return _end - _begin; }
  T get(int idx) { return *(_begin + idx); }
  void set(int idx, T t)
  {
    *(_begin + idx) = t;
  }
};

ACDK_DECL_CLASS(ArrayReadByteBuffer);

/**
  wrapps a a byteArray as ByteBuffer
*/
class ACDK_CORE_PUBLIC ArrayReadByteBuffer
: extends Object
, implements ReadByteBuffer
, foreign public SimplePtrItBuffer<byte>
{
  ACDK_WITH_METAINFO(ArrayReadByteBuffer)
protected:
  typedef SimplePtrItBuffer<byte> PtrBufferType;
  RbyteArray _ba;
public:
  /**
    creates a ReadByteBuffer from a byteArray with given absolut
    start and end offsets
  */
  ArrayReadByteBuffer(IN(RbyteArray) ba, int start = 0, int end = -1)
  : _ba(ba)
  {
    _begin = ba->begin() + start;
    if (end == -1)
      end = ba->length() - start;
    _end = _begin + end;
  }
  RObject clone() { return new ArrayReadByteBuffer((RbyteArray)_ba->clone(), startOffset(), endOffset()); }
  int length() { return PtrBufferType::length(); }
  byte get(int idx) { return  PtrBufferType::get(idx); }
  
  virtual RReadByteBuffer createReadSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice);

  int startOffset() { return _ba->begin() - _begin; }
  int endOffset() { return startOffset() + length(); }
  foreign virtual byte* begin() { return _begin; }
  foreign virtual byte* end() { return _end; }
  virtual bool supportNativeIterator() { return true; }

  //void set(int idx, byte v) { PtrBufferType::set(idx, v); }
};

ACDK_DECL_CLASS(ArrayReadWriteByteBuffer);
/**
  wrapps a a byteArray as ByteBuffer
*/
class ACDK_CORE_PUBLIC ArrayReadWriteByteBuffer
: extends ArrayReadByteBuffer
, implements ReadWriteByteBuffer
{
  ACDK_WITH_METAINFO(ArrayReadWriteByteBuffer)
public:
  /**
    creates a ReadWriteByteBuffer from a byteArray with given absolut
    start and end offsets
  */
  ArrayReadWriteByteBuffer(IN(RbyteArray) ba, int start = 0, int end = -1)
  : ArrayReadByteBuffer(ba, start, end)
  {
  }
  RObject clone() { return new ArrayReadWriteByteBuffer((RbyteArray)_ba->clone(), startOffset(), endOffset()); }
  void set(int idx, byte v) { PtrBufferType::set(idx, v); }
  virtual RWriteByteBuffer createWriteSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice);
  virtual RReadWriteByteBuffer createReadWriteSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice);
};


ACDK_DECL_CLASS(CoreByteBuffer);
/**
  CoreByteBuffer is a standard implementation 
  for a FlexByteBuffer
*/
class ACDK_CORE_PUBLIC CoreByteBuffer
: extends acdk::lang::Object
, implements FlexByteBuffer
, implements ReadWriteByteBuffer
{
  ACDK_WITH_METAINFO(CoreByteBuffer)
protected:
  typedef sys::core_vector<byte> VectorType;
  VectorType _array;
public:
  /**
    create a buffer on given size
    element values are undefined
  */
  CoreByteBuffer(int size)
    : _array(size)
  {
  }
  /**
    create a byte buffer on given size and with initial capicity
    the elements are initialized with fillWith byte
  */
  CoreByteBuffer(int size, int initCap, byte fillWith = 0)
    : _array(size, initCap, fillWith)
  {
  }
  /// @internal
  foreign CoreByteBuffer(const VectorType& array)
    : _array(array)
  {
  }
  /// @internal
  foreign CoreByteBuffer(const VectorType& array, int start, int end = -1);
  RObject clone() { return new CoreByteBuffer(_array); }

  foreign virtual byte* begin() { return _array.begin(); }
  foreign virtual byte* end() { return _array.end(); }
  virtual bool supportNativeIterator() { return true; }
  virtual int length() { return _array.size(); }
  byte get(int idx) { return _array[idx]; }
  void set(int idx, byte b) { _array[idx] = b; }
  virtual RReadByteBuffer createReadSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice)
  {
    if (sliceType == ClonedSlice)
      return new CoreByteBuffer(_array, start, end);
    return new SlicedReadByteBuffer(const_cast<CoreByteBuffer*>(this), start, end);
  }
  virtual RWriteByteBuffer createWriteSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice)
  {
    if (sliceType == ClonedSlice)
      return new CoreByteBuffer(_array, start, end);
    return new SlicedReadWriteByteBuffer(const_cast<CoreByteBuffer*>(this), start, end, sliceType);
  }
  virtual RReadWriteByteBuffer createReadWriteSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice)
  {
    if (start == 0 && (end == -1 || end == length()) && sliceType == ShadowSlice)
      return (ReadWriteByteBuffer*)this;
    if (sliceType == ClonedSlice)
      return new CoreByteBuffer(_array, start, end);
    return new SlicedReadWriteByteBuffer(const_cast<CoreByteBuffer*>(this), start, end, sliceType);
  }
  virtual void append(byte t) { _array.push_back(t); }
  virtual byte deleteAt(int idx) { byte t = _array[idx]; _array.erase(_array.begin() + idx); return t; }
  virtual void insertAt(int idx, byte t) { _array.insert(idx, t); }
  virtual int limit() { return -1; }
  virtual void resize(int size, byte fill = 0)  { _array.resize(size, fill); }
  virtual void ensureCapacity(int cap) { _array.ensureCapacity(cap); }
  virtual void push(byte t) { _array.push_back(t); }
  virtual byte pop() { byte t = _array.back(); _array.pop_back(); return t; }
};

ACDK_DECL_CLASS(StringReadByteBuffer);

/**
  This class holds a String, whereas the String::characterClass() must already
  have the correct encoding
*/
class ACDK_CORE_PUBLIC StringReadByteBuffer
: extends Object
, implements ReadByteBuffer
, foreign public SimplePtrItBuffer<byte>
{ 
  ACDK_WITH_METAINFO(StringReadByteBuffer)
protected:
  RString _str;
  typedef SimplePtrItBuffer<byte> PtrBufferType;
public:
  StringReadByteBuffer(IN(RString) str, int start = 0, int end = -1)
    : _str(str)
  {
    _begin = str->byte_begin() + start;
    if (end == -1)
      end = str->length() - start;
    _end = _begin + end;
  }
  RObject clone() { return new StringReadByteBuffer((RString)_str->clone(), startOffset(), endOffset()); }
  int length() { return PtrBufferType::length(); }
  byte get(int idx) { return  PtrBufferType::get(idx); }
  
  virtual RReadByteBuffer createReadSlice(int start = 0, int end = -1, SliceType sliceType = ShadowSlice);

  int startOffset() { return _str->byte_begin() - _begin; }
  int endOffset() { return startOffset() + length(); }
  foreign virtual byte* begin() { return _begin; }
  foreign virtual byte* end() { return _end; }
  virtual bool supportNativeIterator() { return true; }
  
};

ACDK_DECL_CLASS(Buffers);

/**
  helper methods in connection with ByteBuffers
*/
class ACDK_CORE_PUBLIC Buffers
: extends Object
{
  ACDK_WITH_METAINFO(Buffers)
public:
  /// internal only
  RString toString() { return Object::toString(); }
  /**
    return a array of hex values
  */
  static RString toString(IN(RByteBuffer) buf);
  /** 
    creates a ReadByteBuffer from given byteArray
  */
  static RReadByteBuffer getReadByteBuffer(IN(RbyteArray) ba);
  /** 
    creates a ReadWriteByteBuffer from given byteArray
  */
  static RReadWriteByteBuffer getReadWriteByteBuffer(IN(RbyteArray) ba, int startIdx = 0, int endIdx = -1);
  /** 
    Converts given ReadByteBuffer to a ReadByteBuffer which supports the native begin() and end() 
    methods. If the given buffer already supports these methods return the given buffer
  */
  static RReadByteBuffer getNativeReadByteBuffer(IN(RReadByteBuffer) buffer);
  /** 
    Converts given WriteByteBuffer to a WriteByteBuffer which supports the native begin() and end() 
    methods. If the given buffer already supports these methods return the given buffer
  */
  static RWriteByteBuffer getNativeWriteByteBuffer(IN(RWriteByteBuffer) buffer);
  /**
    converts the given String to a ReadByteBuffer using the given encoding
  */
  static RReadByteBuffer getReadByteBuffer(IN(RString) str, IN(acdk::locale::REncoding) enc);
  /**
    creates a Reader wrapper from the given ReadByteBuffer
  */
  static acdk::io::RReader getReader(IN(RReadByteBuffer) buffer);
  /**
    creates a Writer wrapper from the given WriteByteBuffer
  */
  static acdk::io::RWriter getWriter(IN(RWriteByteBuffer) buffer);
  /**
    creates a Writer wrapper from the given FlexByteBuffer
  */
  static acdk::io::RWriter getAppendWriter(IN(RFlexByteBuffer) buffer);
  /**
    copy source buffer into target
    stops if source is copied or target is full
    @return count of copied bytes
  */
  static int copyBuffer(IN(RReadByteBuffer) source, IN(RWriteByteBuffer) target);
  /**
    append all bytes from source to buffer
  */
  static void appendBuffer(IN(RReadByteBuffer) source, IN(RFlexByteBuffer) buffer);
};


} // lang
} // acdk



#endif //acdk_lang_ByteBuffer_h

