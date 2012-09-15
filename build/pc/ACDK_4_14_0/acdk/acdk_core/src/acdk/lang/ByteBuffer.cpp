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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ByteBuffer.cpp,v 1.4 2005/04/09 19:26:48 kommer Exp $


#include "ByteBuffer.h"
#include "Integer.h"
#include "Math.h"
#include "../locale/UCS2Encoding.h"
#include "../locale/UTF8Encoding.h"
#include "../io/ByteBufferReader.h"
#include "../io/ByteBufferWriter.h"

namespace acdk {
namespace lang {


RReadByteBuffer 
SlicedReadByteBuffer::createReadSlice(int start, int end, SliceType sliceType)
{
  if (end == -1)
    end = _end;
  else
    end = _start + end;
  return _parent->createReadSlice(_start + start, end, sliceType);
}

RReadByteBuffer 
ArrayReadByteBuffer::createReadSlice(int start, int end, SliceType sliceType)
{
  if (end == -1)
    end = length() - start;
  switch (sliceType)
  {
  case ShadowSlice:
    return new ArrayReadByteBuffer(_ba,  start + startOffset(), end);
  case ClonedSlice:
    return new ArrayReadByteBuffer((RbyteArray)_ba->clone(),  start + startOffset(), end);
  default:
    THROW0(UnsupportedOperationException);
  }
  return Nil;
}

RReadByteBuffer 
SlicedReadWriteByteBuffer::createReadSlice(int start, int end, SliceType sliceType)
{
  if (end == -1)
    end = length() - start;
  else
    end = _start + end;
  return new SlicedReadWriteByteBuffer(_parent, _start + start, end, sliceType);
}

RWriteByteBuffer 
SlicedReadWriteByteBuffer::createWriteSlice(int start, int end, SliceType sliceType)
{
  if (end == -1)
    end = length() - start;
  else
    end = _start + end;
  return new SlicedReadWriteByteBuffer(_parent, _start + start, end, sliceType);
}

RReadWriteByteBuffer 
SlicedReadWriteByteBuffer::createReadWriteSlice(int start, int end, SliceType sliceType)
{
  if (end == -1)
    end = length() - start;
  else
    end = _start + end;
  return new SlicedReadWriteByteBuffer(_parent, _start + start, end, sliceType);
}

RWriteByteBuffer 
ArrayReadWriteByteBuffer::createWriteSlice(int start, int end, SliceType sliceType)
{
   switch (sliceType)
  {
  case ShadowSlice:
    return new ArrayReadWriteByteBuffer(_ba,  start + startOffset(), end);
  default:
    THROW0(UnsupportedOperationException);
  }
  return Nil;
}

RReadWriteByteBuffer 
ArrayReadWriteByteBuffer::createReadWriteSlice(int start, int end, SliceType sliceType)
{
  switch (sliceType)
  {
  case ShadowSlice:
    return new ArrayReadWriteByteBuffer(_ba,  start + startOffset(), end);
  default:
    THROW0(UnsupportedOperationException);
  }
  return Nil;
}


CoreByteBuffer::CoreByteBuffer(const VectorType& array, int start, int end)
: _array(end == -1 ? start - array.size() : end - start)
{
  if (end == -1)
    end = start - array.size();
  const byte* sit = array.begin() + start;
  const byte* send = array.begin() + end;
  byte* tid = _array.begin();
  for (; sit != send; ++tid, ++sit)
    *tid = *sit;
}

RReadByteBuffer 
StringReadByteBuffer::createReadSlice(int start, int end, SliceType sliceType)
{
  if (start == 0 && (end == -1 || end == length()) && sliceType == ShadowSlice)
    return (ReadByteBuffer *)this;
  RString tstr = _str->substr(start, end);
  if (sliceType == ClonedSlice)
    tstr = (RString)tstr->clone();
  return new StringReadByteBuffer(tstr);
}

//static 
RString 
Buffers::toString(IN(RByteBuffer) buf)
{
  if (buf->supportNativeIterator() == true)
  {
    byte* it = buf->begin();
    byte* end = buf->end();
    StringBuffer sb;
    sb << "[";
    bool isFirst = true;
    for (; it != end; ++it)
    {
      if (isFirst == true)
        isFirst = false;
      else
        sb << ",";
      sb << Integer::toHexString(*it);
    }
    sb << "]";
    return sb.toString();
  }
  // ### TODO check if is ReadByteBuffer
  return "";
}


RReadByteBuffer 
Buffers::getReadByteBuffer(IN(RbyteArray) ba)
{
  return new ArrayReadByteBuffer(ba);
}

//static 
RReadWriteByteBuffer 
Buffers::getReadWriteByteBuffer(IN(RbyteArray) ba, int startIdx, int endIdx)
{
  return new ArrayReadWriteByteBuffer(ba, startIdx, endIdx);
}

//static 
RReadByteBuffer 
Buffers::getNativeReadByteBuffer(IN(RReadByteBuffer) buffer)
{
  if (buffer->supportNativeIterator() == true)
    return buffer;
  THROW0(UnsupportedOperationException);
  return Nil;
}

//static 
RWriteByteBuffer 
Buffers::getNativeWriteByteBuffer(IN(RWriteByteBuffer) buffer)
{
  if (buffer->supportNativeIterator() == true)
    return buffer;
  THROW0(UnsupportedOperationException);
  return Nil;
}

bool
hasStringEncoding(IN(RString) str, IN(acdk::locale::REncoding) enc)
{
  if (enc == acdk::locale::Encoding::getAsciiEncoding() && str->characterClass() == CCAscii)
    return true;
  if (enc == acdk::locale::UCS2Encoding::getUCS2NativeEncoding() && str->characterClass() == CCUcs2)
    return true;
  if (enc == acdk::locale::UTF8Encoding::getUTF8Encoding() && str->characterClass() == CCUtf8)
    return true;
  return false;
}

//static 
RReadByteBuffer 
Buffers::getReadByteBuffer(IN(RString) str, IN(acdk::locale::REncoding) enc)
{
  if (hasStringEncoding(str, enc) == true)
    return new StringReadByteBuffer(str);

  return new ArrayReadByteBuffer(str->getBytes(enc->getEncoder()));
}

//static 
acdk::io::RReader 
Buffers::getReader(IN(RReadByteBuffer) buffer)
{
  if (buffer->supportNativeIterator() == true)
    return new acdk::io::ByteBufferPtrReader(buffer);
  return new acdk::io::ByteBufferReader(buffer);
}

acdk::io::RWriter 
Buffers::getWriter(IN(RWriteByteBuffer) buffer)
{
  if (buffer->supportNativeIterator() == true)
    return new acdk::io::BytePtrWriter(buffer->begin(), buffer->end(), (RObject)buffer);
  return new acdk::io::ByteBufferWriter(buffer);
}

//static 
acdk::io::RWriter 
Buffers::getAppendWriter(IN(RFlexByteBuffer) buffer)
{
  return new acdk::io::ByteBufferAppendWriter(buffer);
}

//static 
int 
Buffers::copyBuffer(IN(RReadByteBuffer) source, IN(RWriteByteBuffer) target)
{
  if (source->supportNativeIterator() == true && target->supportNativeIterator() == true)
  {
    const byte* sit = source->begin();
    const byte* send = source->end();
    byte* tit = target->begin();
    byte* tend = target->end();
    int ml = Math::min(send - sit, tend - tit);
    memcpy(tit, sit, ml);
    return ml;
  }
  else
  {
    int sl = source->length();
    int tl = target->length();
    int ml = Math::min(sl, tl);
    for (int i = 0; i < ml; ++i)
      target->set(i, source->get(i));
    return ml;
  }
}

void 
Buffers::appendBuffer(IN(RReadByteBuffer) source, IN(RFlexByteBuffer) target)
{
  if (source->supportNativeIterator() == true && target->supportNativeIterator() == true)
  {
    const byte* sit = source->begin();
    const byte* send = source->end();
    int tl = target->length();
    target->resize(tl + (send - sit));
    byte* ipos = target->begin() + tl;
    memcpy(ipos, sit, (send - sit));
  }
  else
  {
    int sl = source->length();
    for (int i = 0; i < sl; ++i)
      target->append(source->get(i));
  }
}

} // lang
} // acdk



