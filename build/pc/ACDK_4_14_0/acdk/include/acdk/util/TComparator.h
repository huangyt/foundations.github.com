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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TComparator.h,v 1.7 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TComparator_h
#define acdk_util_TComparator_h


namespace acdk {
namespace util {

using namespace acdk::lang;

#define ACDK_DECL_COMPARATOR(Type1, RType1) \
typedef ::acdk::util::TComparator<RType1> Type1##Comparator; \
typedef Type1##Comparator::RefType R##Type1##Comparator;

/**
  Interface to compare to elements.
  @see Java: http://java.sun.com/j2se/1.3/docs/api/java/util/Comparator.html
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/

template <class T>
class TComparator 
      ACDK_INTERFACEBASE
{
  
public:
  typedef T RValueType;
  typedef TComparator<RValueType> ThisType;
  typedef InterfaceHolder<ThisType> RThisType;
  typedef RThisType RefType;

  virtual int compare(IN(RValueType) o1, IN(RValueType) o2) = 0;

};


} // util
} // acdk

#endif //acdk_util_TComparator_h

