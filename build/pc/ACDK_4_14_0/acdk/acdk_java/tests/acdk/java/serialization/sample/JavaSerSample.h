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
// $Header: /cvsroot/acdk/acdk/acdk_java/tests/acdk/java/serialization/sample/JavaSerSample.h,v 1.3 2005/02/05 10:45:12 kommer Exp $

#ifndef tests_acdk_java_serialization_JavaSerSample_h
#define tests_acdk_java_serialization_JavaSerSample_h

#include <acdk.h>
#include <acdk/io/Serializable.h>

ACDK_DECL_CLASS(JavaSerSample);

class JavaSerSample
: extends ::acdk::lang::Object
, implements ::acdk::io::Serializable
{
  // need meta info for serialization
  ACDK_WITH_METAINFO(JavaSerSample)
private:
  int _x;
  int _y;
  // don't use this in serialization
  mutable RString _privateString;
public:
  // needed for serialization, alternativelly also a default constructor can be defined

  static RObject create_instance() { return new JavaSerSample(0, 0); }

  JavaSerSample(int x, int y) 
  : _x(x)
  , _y(y)
  {
  }
  int getX() { return _x; }
  int getY() { return _y; }
  RString toString() 
  { 
    StringBuffer sb;
    sb << "x=" << _x << ", y=" << _y;
    return sb.toString();
  }
};

#endif //tests_acdk_java_serialization_JavaSerSample_h
