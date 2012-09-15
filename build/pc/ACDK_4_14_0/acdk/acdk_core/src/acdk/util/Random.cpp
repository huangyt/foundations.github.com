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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Random.cpp,v 1.11 2005/03/08 12:45:45 kommer Exp $


#include <acdk.h>
#include "Random.h"

#include <acdk/lang/System.h>
#include <acdk/lang/Math.h>

#include <acdk/lang/IllegalArgumentException.h>

namespace acdk {
namespace util {

Random::Random()
: _haveNextNextGaussian(false),
  _nextNextGaussian(0),
  _seed(0)
{
  setSeed(System::currentTimeMillis());
}

Random::Random(jlong seed)
: _haveNextNextGaussian(false),
  _nextNextGaussian(0),
  _seed(0)
{
  setSeed(seed);
}

//virtual 
bool 
Random::nextBoolean()
{
  return _nextBits(1) != 0;
}



//virtual 
void 
Random::nextBytes(IN(RcharArray) bytes) THROWS1(RIllegalArgumentException)
{
  if (bytes == Nil)
    THROW0(IllegalArgumentException);
  int random;
  int max = bytes->length() & ~0x3;
  for (int i = 0; i < max; i += 4) {
    random = _nextBits(32);
    bytes[i]   = (char)  random;
    bytes[i+1] = (char) (random >>  8);
    bytes[i+2] = (char) (random >> 16);
    bytes[i+3] = (char) (random >> 24);
  }
  if (max < bytes->length()) {
    random = _nextBits(32);
    for (int j = max; j < bytes->length(); j++) {
      bytes[j] = (char) random;
      random >>= 8;
    }
  }
}

double 
Random::nextDouble()
{
  SYNCTHIS();
  return (( ((jlong)_nextBits(26)) << 27) + _nextBits(27)) / 
                              (double)(jlong(1) << 53);
}

float 
Random::nextFloat()
{
  return _nextBits(24) / ((float)(1 << 24));
}

double 
Random::nextGaussian()
{
  SYNCTHIS();
  if (_haveNextNextGaussian == true) {
    _haveNextNextGaussian = false;
    return _nextNextGaussian;
  } 
  double v1, v2, s;
  do {
    v1 = 2 * nextDouble() - 1; // between -1.0 and 1.0
    v2 = 2 * nextDouble() - 1; // between -1.0 and 1.0
    s = v1 * v1 + v2 * v2;
  } while (s >= 1);
  double norm = Math::sqrt(-2 * Math::log(s) / s);
  _nextNextGaussian = v2 * norm;
  _haveNextNextGaussian = true;
  return v1 * norm;
}

int 
Random::nextInt()
{
  return _nextBits(32);
}

int 
Random::nextInt(int n)
{
  SYNCTHIS();
  if (n <= 0)
    THROW0(IllegalArgumentException);
  if ((n & -n) == n)  // i.e., n is a power of 2
    return (int)((n * (long)_nextBits(31)) >> 31);
  int bits, val;
  do {
    bits = _nextBits(32);
    val = bits % n;
  } while(bits - val + (n-1) < 0);
  return val;
}


jlong 
Random::nextLong()
{
  SYNCTHIS();
  return ((jlong)_nextBits(32) << 32) + jlong(_nextBits(32));
}


#ifdef ACDK_HAVE_LONG_LONG
const jlong scrampleconstant = 0x5DEECE66DLL;
#else
const jlong scrampleconstant = 0x5DEECE66D;
#endif

int 
Random::_nextBits(int bits)
{
  if (bits < 1 || bits > 32)
    THROW0(IllegalArgumentException);

  _seed = (_seed * scrampleconstant) & ((jlong(1) << 48) - 1);
  return (int) (_seed >> (48 - bits));
}

void 
Random::setSeed(jlong seed) 
{
  SYNCTHIS();
  _seed = (seed ^ scrampleconstant) & ((jlong(1) << 48) - 1);
  _haveNextNextGaussian = false;
}

} // Util
} // acdk


