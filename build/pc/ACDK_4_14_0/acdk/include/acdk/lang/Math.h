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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Math.h,v 1.9 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_Math_h
#define acdk_lang_Math_h

#include <math.h>
#include <time.h>
#include <stdlib.h>

namespace acdk {
namespace lang {

#ifdef min
#  undef min
#endif
#ifdef max
#  undef max
#endif
#ifdef abs
#  undef abs
#endif

#ifdef MATHFUNCNS
#undef MATHFUNCNS
#endif
#ifdef ACDK_METROWORKS
#define MATHFUNCNS std
#else
#define MATHFUNCNS 
#endif


/**
  Some common math operations.
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:48 $
  @bug asin, IEEEremainder, rint not implemented
*/  
class ACDK_CORE_PUBLIC Math 
{
  static bool _rand_inited;
public :
  template <class T> template_static T abs(T t) { return (t < 0) ? (t * -1) : (t); }
  template <class T> template_static T min(T t1, T t2) { return (t1 < t2) ? t1 : t2; }
  template <class T> template_static T max(T t1, T t2) { return (t1 > t2) ? t1 : t2; }


  static double acos(double a) { return MATHFUNCNS::acos(a); }
  //NIY static double asin(double a) { return ::asin(a); }
  static double atan(double a) { return MATHFUNCNS::atan(a); }
  static double atan2(double a, double b) { return MATHFUNCNS::atan2(a, b); }
  static double ceil(double a) { return MATHFUNCNS::ceil(a); }
  static double cos(double a) { return MATHFUNCNS::cos(a); }
  static double exp(double a) { return MATHFUNCNS::exp(a); }
  static double floor(double a) { return MATHFUNCNS::floor(a); }
  //NIY static double IEEEremainder(double f1, double f2) 
  static double log(double a) { return MATHFUNCNS::log(a); }
  static double pow(double a, double b) { return MATHFUNCNS::pow(a, b); }
  static double random() 
  {
    if (_rand_inited == false) {
      srand((unsigned)time(NULL));
      _rand_inited = true;
    }
    double erg = double(rand()) / double(RAND_MAX);
    return erg;
  }
  //NIY static double rint(double a) 
  static int round(float a) 
   {
    return (int)floor(a + 0.5f);
  }
  static int round(double a) 
  {
    return (int)floor(a + 0.5f);
  }
  static double sin(double a) { return MATHFUNCNS::sin(a); }
  static double sqrt(double a) {  return MATHFUNCNS::sqrt(a);  }
  static double tan(double a) { return MATHFUNCNS::tan(a);  }
  static double toDegrees(double angrad) { return angrad / 0.017453292519943295; }
  static double toRadians(double angdeg) { return angdeg * 0.017453292519943295; }
};


} // lang
} // acdk

#endif //acdk_lang_Math_h


