// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Parts of this class are ported from the of GNU Classpath project 
//  (http://www.gnu.org/software/classpath/classpath.html)
//   with following copyright statement:

// Collections.java -- Utility class with methods to operate on collections
//
// Copyright (c) 1998 by Stuart Ballard (stuart.ballard@mcmail.com)
// Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Library General Public License as published
// by the Free Software Foundation, version 2. (see COPYING.LIB)
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Library General Public License for more details.
//
// You should have received a copy of the GNU Library General Public License
// along with this program; if not, write to the Free Software Foundation
// Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA

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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Collections.h,v 1.16 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_Collections_h
#define acdk_util_Collections_h


#include <acdk.h>


#include "Random.h"
#include "Enumeration.h"
#include "Iterator.h"
#include "Comparator.h"
#include "List.h"
#include "Set.h"
#include "SortedSet.h"
#include "SortedMap.h"

namespace acdk {
namespace util {

#ifdef max
# undef max
#endif
#ifdef min
# undef min
#endif

/**
  Utility class with methods to operate on collections

  
  @author of the original Classpath implementation: 
    Copyright (c) 1998 by Stuart Ballard (stuart.ballard@mcmail.com)
    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

  @author (ACDK) Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @author Stuart Ballard (stuart.ballard@mcmail.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC Collections
: extends acdk::lang::Object
{
  static RList _EMPTY_LIST;
  static RSet _EMPTY_SET;
public:
  static int binarySearch(IN(RList) list, IN(RObject) key, IN(RComparator) comparator = Nil);
  static void copy(IN(RList) dest, IN(RList) source);
  static REnumeration enumeration(IN(RCollection) c);
  static void fill(IN(RList) list, IN(RObject) val);
  static RObject max(IN(RCollection) coll);
  /** 
    API: ACDK
  */
  static void addAll(IN(RCollection) coll, IN(RObjectArray) array);
  /**
    @author of the original Java implementation Stuart Ballard (stuart.ballard@mcmail.com)
  */
  static RObject max(IN(RCollection) coll, IN(RComparator) order);
  /**
    @author of the original Java implementation Stuart Ballard (stuart.ballard@mcmail.com)
  */
  static RObject min(IN(RCollection) coll);
  /**
    @author of the original Java implementation Stuart Ballard (stuart.ballard@mcmail.com)
  */
  static RObject min(IN(RCollection) coll, IN(RComparator) order);
  /** not implemented yet */
  static RList nCopies(int count, IN(RObject) object);
  static void reverse(IN(RList) list);
  static RComparator reverseOrder();
  static void shuffle(IN(RList) list);
  /**
    @author of the original Java implementation Stuart Ballard (stuart.ballard@mcmail.com)
  */
  static void shuffle(IN(RList) list, IN(RRandom) rnd);
  static RSet singleton(IN(RObject) o);
  static void sort(IN(RList) list, IN(RComparator) comparator = Nil);
  static RCollection synchronizedCollection(IN(RCollection) coll, IN(RObject) lock = Nil);
  static RList synchronizedList(IN(RList) list, IN(RObject) lock = Nil);
  static RSet synchronizedSet(IN(RSet) set, IN(RObject) lock = Nil);
  static RSortedSet synchronizedSortedSet(IN(RSortedSet) sortedset, IN(RObject) lock = Nil);
  static RMap synchronizedMap(IN(RMap) map, IN(RObject) lock = Nil);
  static RSortedMap synchronizedSortedMap(IN(RSortedMap) sortedmap, IN(RObject) lock = Nil);
  static RCollection unmodifiableCollection(IN(RCollection) c); 
  static RList unmodifiableList(IN(RList) list);
  static RMap unmodifiableMap(IN(RMap) m);
  static RSet unmodifiableSet(IN(RSet) s);
  static RSortedMap unmodifiableSortedMap(IN(RSortedMap) m);
  static RSortedSet unmodifiableSortedSet(IN(RSortedSet) s);
           


  // implementation defined
  static RList get_EMPTY_LIST();
  static RSet get_EMPTY_SET();
private:
  static int  _defaultSearch(IN(RList) list, IN(RObject) key, IN(RComparator) c);
  inline static int _compare(IN(RObject) o1, IN(RObject) o2, IN(RComparator) comparator) 
   {
     if (comparator == Nil) 
      return RComparable(o1)->compareTo(o2);
    return comparator->compare(o1, o2);
  }
};

ACDK_DECL_CLASS(IteratorEnumeration);

/** 
  a thin layer over Iterator 
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/
class ACDK_CORE_PUBLIC IteratorEnumeration
: extends acdk::lang::Object,
  implements Enumeration
{
  ACDK_WITH_METAINFO(IteratorEnumeration)
private:
  RIterator _it;
public:
  IteratorEnumeration(IN(RIterator) it)
  :  _it(it)
  {
  }
  bool hasMoreElements()
  {
    return _it->hasNext();
  }
  RObject nextElement() 
  {
    return _it->next();
  }
};

} // util
} // acdk

#endif //acdk_util_Collections_h

