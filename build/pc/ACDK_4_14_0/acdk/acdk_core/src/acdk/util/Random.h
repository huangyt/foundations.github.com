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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Random.h,v 1.12 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_Random_h
#define acdk_util_Random_h

#include <acdk/io/Serializable.h>

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(Random);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC Random
: extends acdk::lang::Object,
  implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Random)
private:
  bool _haveNextNextGaussian;
  double _nextNextGaussian;
  transient jlong _seed;
public:
  static RObject create_instance() { return new Random(); }
  Random();
  Random(jlong seed);
  virtual bool nextBoolean();
  virtual void nextBytes(IN(RcharArray) bytes) THROWS1(RIllegalArgumentException);
  double nextDouble();
  float nextFloat();
  double nextGaussian();
  int nextInt();
  int nextInt(int n);
  jlong nextLong();
  void setSeed(jlong seed) ;
protected:
  int _nextBits(int bits);

};

} // util
} // acdk

#endif //acdk_util_Random_h

