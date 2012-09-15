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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/BitSet.cpp,v 1.13 2005/03/12 10:54:08 kommer Exp $



#include <acdk.h>
#include "BitSet.h"
#include <acdk/lang/System.h>
#include <acdk/lang/Math.h>
#include <acdk/lang/StringBuffer.h>

#include <acdk/lang/ArrayIndexOutOfBoundsException.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

BitSet::BitSet()
  : _bits(new (allocator()) charArray(1))
{}
  
BitSet::BitSet(int nBits) 
{
  if (nBits%8 == 0)
    _bits = new (allocator()) charArray(nBits >> 3); // nBits >> 3 == nBits / 8 
  else 
    _bits = new (allocator()) charArray((nBits >> 3) + 1); // nBits >> 3 == nBits / 8 
}

BitSet::~BitSet() 
{
}

//private
RcharArray
BitSet::getBits()
{
  return _bits;
}

#ifdef max 
# undef max
#endif

// private
void
BitSet::grow(int toSize)
{
  int nextSize = Math::max((_bits->length())*2, toSize);
  RcharArray newBits = new (allocator()) charArray(nextSize);
  System::arraycopy(_bits, 0, newBits, 0, _bits->length());
  _bits = newBits;
}

#if !defined(ACDK_HAS_ALTERNATIVE_TOKEN) 
void 
BitSet::and(IN(RBitSet) set) 
{ 
  bit_and(set); 
}

void 
BitSet::xor(IN(RBitSet) set) 
{ 
  bit_xor(set); 
}

void
BitSet::or(IN(RBitSet) set) 
{ 
  bit_or(set); 
}
#endif 

void 
BitSet::bit_and(IN(RBitSet) set)
{
  SYNCTHIS();
  { // synchronized block
    SYNCOBJECT(set);

    // ## langsame impl.
    RcharArray setBits = set->getBits();
    for (int i = 0; i < _bits->length() && i < setBits->length(); i++) 
      {
      _bits[i] &= setBits[i];
    }
  }
}

void
BitSet::andNot(IN(RBitSet) set)
{
  SYNCTHIS();
  { // synchronized block
    SYNCOBJECT(set);
    // ## langsame impl.
    RcharArray setBits = set->getBits();
    for (int i=0; i<_bits->length() && i < setBits->length(); i++) {
      _bits[i] &= ~setBits[i];
    }
  }

}

void 
BitSet::clear(int bitIndex) THROWS1(RArrayIndexOutOfBoundsException)
{
  SYNCTHIS();
  if (_bits->length() * 8 < bitIndex)
    THROW0(ArrayIndexOutOfBoundsException);
  int coffset = bitIndex / 8;
  int boffset = 7 - (bitIndex % 8);
  unsigned char cbitmask = 1 << boffset;
  *(_bits->data() + coffset) &=  ~cbitmask ;
}

RObject 
BitSet::clone(sys::Allocator* alc)  
{
  THROW0(UnsupportedOperationException);
  return Nil;  

}

bool 
BitSet::equals(IN(RObject) obj)  
{
  THROW0(UnsupportedOperationException);
  //if (RObject(this) == obj) return true;
  //if (instanceof(obj,BitSet) == false)
  //  return false;
  SYNCTHIS();
  { // synchronized block
    SYNCOBJECT(obj);
  }

  return false;  
}

bool
BitSet::get(int bitIndex)  
{
  if (_bits->length() * 8 < bitIndex)
    THROW0(ArrayIndexOutOfBoundsException);
  int coffset = bitIndex / 8;
  int boffset = 7 - (bitIndex % 8);
  
  char data = *(_bits->data() + coffset);
  unsigned char cbitmask = 1 << boffset;
  return (data & cbitmask) == cbitmask;
}

int 
BitSet::hashCode()
{
  SYNCTHIS();
  int result = 0;
  result = 31 * result + _bits->hashCode();
  return result;  
}

// Gibt Index von hoechstgesetzte Bit + 1 zurueck
int
BitSet::length()
{
  int byteIndex = _bits->length()-1; 
  int currentLength;
  while (byteIndex >= 0 && _bits[byteIndex] == 0)
    byteIndex--;

  if (byteIndex < 0)
    currentLength = 0;
  else {
    int bitIndex = ((byteIndex+1) * 8) - 1;
    while (bitIndex >= 0 && this->get(bitIndex) == false)
      bitIndex--;
    currentLength = bitIndex + 1;
  }
 
  return currentLength;
}


void 
BitSet::bit_or(IN(RBitSet) set)
{
  SYNCTHIS();
  { // synchronized block
    SYNCOBJECT(set);
    // ## langsame impl.
    RcharArray setBits = set->getBits();

    int setLength = set->length() ;
    int max = setLength >> 3;
    while (max >=0 && setBits[max] == 0) {
      max--;
    }
    if (max >= this->length())
      grow(max+1);
    for (int i=0; i<max; i++) {
      _bits[i] |= setBits[i];
    }
  }
}


void
BitSet::set(int bitIndex)
{
  SYNCTHIS();
  if (_bits->length() * 8 < bitIndex)
    THROW0(ArrayIndexOutOfBoundsException);
  int coffset = bitIndex /  8;
  int boffset = 7 - (bitIndex % 8);
  unsigned char cbitmask = 1 << boffset;
  *(_bits->data() + coffset) |=  cbitmask ;

}


int 
BitSet::size()
{
  return (_bits->length())<<3;  
}

RString 
BitSet::toString()
{
  RStringBuffer result = new StringBuffer("[");
  RString comma = String::emptyString();
  int i;
  for (i = 0; i < (_bits->length()) * 8; i++) {
    if (get(i) == true) {
      result->append(comma)->append("1");
    } else {
      result->append(comma)->append("0");
    }
    i++;
    if (i % 8 == 0)
      comma = " ";
    else 
      comma = "";
  }
  result->append("]");
  return result->toString();
}

void 
BitSet::bit_xor(IN(RBitSet) set)
{
  SYNCTHIS();
  { // synchronized block
    SYNCOBJECT(set);
    // ## langsame impl.
    RcharArray setBits = set->getBits();
   
    int setLength = set->length() ;
    int max = setLength >> 3;
    while (max >=0 && setBits[max] == 0) {
      max--;
    }
    if (max >= length())
      grow(max + 1);
    for (int i=0; i<max; i++) {
      _bits[i] ^= setBits[i];
    }
  }    
}




} // util
} // acdk

