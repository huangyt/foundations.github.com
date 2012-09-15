// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 

// Parts of this class are ported from the of GNU Classpath project 
//  (http://www.gnu.org/software/classpath/classpath.html)
//   with following copyright statement:

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
// END

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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Arrays.cpp,v 1.21 2005/03/08 12:45:45 kommer Exp $



#include <acdk.h>
#include "Arrays.h"
#include "ArrayList.h"
#include "ListIterator.h"

#include <acdk/lang/System.h>
namespace acdk {
namespace util {

//static 
RObjectArray 
ArraysImpl::removeElement(IN(RObjectArray) array, int idx) 
  {
    RObjectArray na = new ObjectArray(array->length() - 1);
    int i;
    for (i = 0; i < idx; i++)
      na[i] = array[i];
    for (i = idx; i < na->length(); i++)
      na[i] = array[i + 1];
    return na;
  }

//static 
RList 
ArraysImpl::asList(IN(RObjectArray) a)
{
  int length = a->length();
  RArrayList list = new ArrayList(length);
  for (int i = 0; i < length; i++)
    list->add(i, a[i]);
  return (RList)list;
}

//static 
int 
ArraysImpl::binarySearch(IN(RObjectArray) a, IN(RObject) key, IN(RComparator) comp)
{
  int lo = 0;
  int hi = a->length() - 1;
  int mid = 0;
  while (lo <= hi) 
  {
    mid = (lo + hi) >> 1;
    int d = _compare(key, a[mid], comp);
    if (d == 0)
      return mid;
    else if (d < 0) 
      hi = mid - 1;
    else
      lo = mid + 1;
  }
  return -1;
}


//static 
bool 
ArraysImpl::equals(IN(RObjectArray) a1, IN(RObjectArray) a2) 
{
  if (a1 == a2) 
    return true;
  if (a1 == Nil || a2 == Nil)
    return false;
  if (a1->length() != a2->length())
    return false;
  for (int i = 0; i < a1->length(); i++) {
    if (!(a1[i] == Nil ? a2[i] == Nil : a1[i]->equals(a2[i]))) 
      return false;
  }
  return true;
}

//static
void 
ArraysImpl::_mergeSort(IN(RObjectArray) a, IN(RComparator) comp)
{
  
  int n = a->length();
  RObjectArray x = a;
  RObjectArray y = new ObjectArray(n);
  RObjectArray t = Nil;
  for (int sizenf = 1; sizenf < n; sizenf <<= 1) {
    int size = sizenf;
    
    for (int startnf = 0; startnf < n; startnf += size << 1) {
      int start = startnf; 
      int size2 = n - start - size < size ? n - start - size : size;
      if (size2 <= 0 ||
        _compare(x[start + size - 1], x[start + size], comp) <= 0) {
        System::arraycopy(x, start, y, start, size + size2);
      } else if (_compare(x[start], x[start + size + size2 - 1], comp) >= 0) {
        System::arraycopy(x, start, y, start + size2, size);
        System::arraycopy(x, start + size, y, start, size2);
      } else {
        int p1 = start;
        int p2 = start + size;
        int i = start;
        int d1;
        
        int d2 = -1;
        
        while ((d1 = start + size - p1) > 0 &&
          (d2 = start + size + size2 - p2) > 0) {
          y[i++] = x[(_compare(x[p1], x[p2], comp) <= 0) ? p1++ : p2++];
        }
        System::arraycopy(x, d1 > 0 ? p1 : p2, y, i, d1 > 0 ? d1 : d2);
      } 
    }
    t = x; x = y; y = t; // swap x and y ready for the next merge
  }
  if (x != a) 
    System::arraycopy(x, 0, a, 0, n);
}

template <class T> 
void 
__forceInstantiateBasicArrayFuncs(T val)
{
  RBasicArray<T> ca = new BasicArray<T>(2);
  Arrays::equals(ca, ca);
  Arrays::fill(ca, val);
  Arrays::sort(ca);
}

void
__forceInstantiateBasicArrayFunctions()
{
  __forceInstantiateBasicArrayFuncs<char>(0);
  __forceInstantiateBasicArrayFuncs<bool>(false);
  __forceInstantiateBasicArrayFuncs<byte>(0);
  __forceInstantiateBasicArrayFuncs<short>(0);
  __forceInstantiateBasicArrayFuncs<int>(0);
  __forceInstantiateBasicArrayFuncs<jlong>(JLONG_CONSTANT(0));
  __forceInstantiateBasicArrayFuncs<float>(0.0);
  __forceInstantiateBasicArrayFuncs<double>(0.0);
}

template <class T>
void
__forceInstantiateObjectArrayFuncs(IN(T) val)
{
  RObjectArrayImpl<T> oa = new ObjectArrayImpl<T>(2);
  ArraysImpl::equals((RObjectArray)oa, (RObjectArray)oa);
  ArraysImpl::fill((RObjectArray)oa, (RObject)val);
  ArraysImpl::sort((RObjectArray)oa);
}

void
__forseInstantiateObjectArrayFunctions()
{
  __forceInstantiateObjectArrayFuncs(RObject(new Object()));
  __forceInstantiateObjectArrayFuncs(RString(new String("asdf")));
}

} // Util
} // acdk

