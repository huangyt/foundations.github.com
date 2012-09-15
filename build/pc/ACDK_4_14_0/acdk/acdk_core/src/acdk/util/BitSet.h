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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/BitSet.h,v 1.13 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_BitSet_h
#define acdk_util_BitSet_h

#include <acdk.h>
#include <acdk/lang/ArrayIndexOutOfBoundsException.h>
#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>

namespace acdk {
namespace util {

using namespace acdk::lang;


ACDK_DECL_CLASS(BitSet);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC BitSet
: extends acdk::lang::Object,
  implements acdk::lang::Cloneable,
  implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(BitSet)
private:
  RcharArray _bits;
public:
  BitSet();
  BitSet(int nBits);
  ~BitSet();

  void bit_and(IN(RBitSet) set);
  void bit_xor(IN(RBitSet) set);
  void bit_or(IN(RBitSet) set);
#if !defined(ACDK_HAS_ALTERNATIVE_TOKEN)
  foreign void and(IN(RBitSet) set);
  foreign void xor(IN(RBitSet) set);
  foreign void or(IN(RBitSet) set);
#endif
  void andNot(IN(RBitSet) set);

  void clear(int bit) THROWS1(RArrayIndexOutOfBoundsException);
  virtual RObject clone() { return clone(allocator()); }
  virtual RObject clone(sys::Allocator* alc);
  bool equals(IN(RObject) obj);

  bool get(int bitIndex);

  int hashCode();

  int length();
  
  void set(int bitIndex);

  int size();

  RString toString();

  RcharArray getBits();
private:
  void grow(int toSize);
};


} // util
} // acdk

#endif //acdk_util_BitSet_h

