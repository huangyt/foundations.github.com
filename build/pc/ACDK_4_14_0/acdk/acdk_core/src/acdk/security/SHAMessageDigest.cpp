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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/security/SHAMessageDigest.cpp,v 1.7 2005/03/12 10:54:08 kommer Exp $



#include "SHAMessageDigest.h"

namespace acdk {
namespace security {

using namespace acdk::lang;

SHAMessageDigest::SHAMessageDigest()
: MessageDigest("SHA")
{
  _init();
}

SHAMessageDigest::SHAMessageDigest(SHAMessageDigest& other)
: MessageDigest("SHA"),
  _count_lo(other._count_lo),
  _count_hi(other._count_hi),
  _local(other._local)
{
  int i;
  for (i = 0; i < 5; i++)
    _digest[i] = other._digest[i];
  for (i = 0; i < engineGetDigestLength(); i++)
    _data[i] = other._data[i];
}

//virtual 
RObject 
SHAMessageDigest::clone(sys::Allocator* alc)
{
  return new (alc) SHAMessageDigest(*this);
}

//virtual 
RbyteArray 
SHAMessageDigest::engineDigest()
{
  _final();
  return new byteArray((const byte*)_digest, engineGetDigestLength());
}

//virtual 
int 
SHAMessageDigest::engineDigest(IN(RbyteArray) buf, int offset, int len)
{
  int i;
  for (i = offset; i < offset + len && (i - offset) < engineGetDigestLength(); i++)
    buf[i] = _data[i - offset];
  return i - offset;
}

//virtual 
int 
SHAMessageDigest::engineGetDigestLength()
{
  return 16;
}
  
//virtual 
void 
SHAMessageDigest::engineReset()
{
  _init();
}

//virtual 
void 
SHAMessageDigest::engineUpdate(byte input)
{
  _update(&input, 1);
}

//virtual 
void 
SHAMessageDigest::engineUpdate(IN(RbyteArray) buf, int offset, int len)
{
  byte* data = buf->data() + offset;
  _update(data, len);
}


#define f1(x,y,z)       ((x & y) | (~x & z))
#define f2(x,y,z)       (x ^ y ^ z)
#define f3(x,y,z)       ((x & y) | (x & z) | (y & z))
#define f4(x,y,z)       (x ^ y ^ z)


#define CONST1          0x5a827999L
#define CONST2          0x6ed9eba1L
#define CONST3          0x8f1bbcdcL
#define CONST4          0xca62c1d6L

#define ROT32(x,n)      ((x << n) | (x >> (32 - n)))

#define FUNC(n,i)                                               \
    temp = ROT32(A,5) + f##n(B,C,D) + E + W[i] + CONST##n;       \
    E = D; D = C; C = ROT32(B,30); B = A; A = temp

#define SHA_BLOCKSIZE           64
#define SHA_DIGESTSIZE          20



void 
SHAMessageDigest::_transform()
{
  int i;
  int temp, A, B, C, D, E, W[80];
  for (i = 0; i < 16; ++i) {
    W[i] = _data[i];
  }
  for (i = 16; i < 80; ++i) {
    W[i] = W[i-3] ^ W[i-8] ^ W[i-14] ^ W[i-16];
#ifdef USE_MODIFIED_SHA
    W[i] = ROT32(W[i], 1);
#endif /* USE_MODIFIED_SHA */
  }
  A = _digest[0];
  B = _digest[1];
  C = _digest[2];
  D = _digest[3];
  E = _digest[4];
  /* Aufruf der Funktionen abhaengig von der Zahl
  * der Operationen */
  for (i = 0; i < 20; ++i) {
    FUNC(1,i);
  }
  for (i = 20; i < 40; ++i) {
    FUNC(2,i);
  }
  for (i = 40; i < 60; ++i) {
    FUNC(3,i);
  }
  
  for (i = 60; i < 80; ++i) {
    FUNC(4,i);
  }
  
  /* Addieren der gehashten Teilbloecke zu den
  * Verkettungsvariablen */
  
  _digest[0] += A;
  _digest[1] += B;
  _digest[2] += C;
  _digest[3] += D;
  _digest[4] += E;
}



#ifdef ACDK_LITTLEENDIAN

/* change endianness of data */
void 
maybe_byte_reverse(int *buffer, int count)
{
  
  int i;
  byte ct[4], *cp;
  count /= sizeof(int);
  cp = (byte *) buffer;
  for (i = 0; i < count; ++i) {
    ct[0] = cp[0];
    ct[1] = cp[1];
    ct[2] = cp[2];
    ct[3] = cp[3];
    cp[0] = ct[3];
    cp[1] = ct[2];
    cp[2] = ct[1];
    cp[3] = ct[0];
    cp += sizeof(int);
  }
}

#else // ACDK_LITTLEENDIAN 

#define maybe_byte_reverse(a,b) 

#endif // ACDK_LITTLEENDIAN 



/* Initialisierung des SHA-Kontext */

void 
SHAMessageDigest::_init()
{
  _digest[0] = 0x67452301L;
  _digest[1] = 0xefcdab89L;
  _digest[2] = 0x98badcfeL;
  _digest[3] = 0x10325476L;
  _digest[4] = 0xc3d2e1f0L;
  _count_lo = 0L;
  _count_hi = 0L;
  _local = 0;
}

/* Den SHA-Kontext in Bloecke teilen*/
void 
SHAMessageDigest::_update(byte *buffer, int count)
{
  int i;
  if ((_count_lo + ((int) count << 3)) < _count_lo) {
    ++_count_hi;
  }
  _count_lo += (int) count << 3;
  _count_hi += (int) count >> 29;
  if (_local) {
    i = SHA_BLOCKSIZE - _local;
    if (i > count) {
      i = count;
    }
    memcpy(((byte *) _data) + _local, buffer, i);
    count -= i;
    buffer += i;
    _local += i;
    if (_local == SHA_BLOCKSIZE) {
      maybe_byte_reverse(_data, SHA_BLOCKSIZE);
      _transform();
    } else 
      return;
  }
  while (count >= SHA_BLOCKSIZE) {
    memcpy(_data, buffer, SHA_BLOCKSIZE);
    buffer += SHA_BLOCKSIZE;
    count -= SHA_BLOCKSIZE;
    maybe_byte_reverse(_data, SHA_BLOCKSIZE);
    _transform();
  }
  memcpy(_data, buffer, count);
  _local = count;
}



/* Hashwert berechnen */
void 
SHAMessageDigest::_final()
{
  int count;
  int lo_bit_count, hi_bit_count;
  lo_bit_count = _count_lo;
  hi_bit_count = _count_hi;
  count = (int) ((lo_bit_count >> 3) & 0x3f);
  ((byte *) _data)[count++] = 0x80;
  if (count > SHA_BLOCKSIZE - 8) {
    memset(((byte *) _data) + count, 0, SHA_BLOCKSIZE - count);
    maybe_byte_reverse(_data, SHA_BLOCKSIZE);
    _transform();
    memset((byte *) _data, 0, SHA_BLOCKSIZE - 8);
  } else {
    memset(((byte *) _data) + count, 0, SHA_BLOCKSIZE - 8 - count);
  }
  maybe_byte_reverse(_data, SHA_BLOCKSIZE);
  _data[14] = hi_bit_count;
  _data[15] = lo_bit_count;
  _transform();
}

} // security
} // acdk

