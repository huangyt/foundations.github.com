// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//

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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Arrays.h,v 1.32 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_Arrays_h
#define acdk_util_Arrays_h
#include <acdk.h>

#include <acdk/lang/NullPointerException.h>
#include <acdk/lang/ArrayIndexOutOfBoundsException.h>
#include <acdk/lang/Math.h>
#include "Comparator.h"
#include "List.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_CLASS(ArraysImpl);

/**
  API: Java<br/>
  @author for parts of the orignal Classpath implementation: Copyright (c) 1998 by Stuart Ballard (stuart.ballard@mcmail.com)
            Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.32 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/
class ACDK_CORE_PUBLIC ArraysImpl
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(ArraysImpl)
public:
  static RList asList(IN(RObjectArray) a);
  static int binarySearch(IN(RObjectArray) a, IN(RObject) key, IN(RComparator) comp = Nil); 
  virtual bool equals(IN(RObject) obj) { return Object::equals(obj); }
  static bool equals(IN(RObjectArray) a1, IN(RObjectArray) a2);
  static void sort(IN(RObjectArray) array, IN(RComparator) comp = Nil)
  {
    _mergeSort(array, comp);
  }
  static void _mergeSort(IN(RObjectArray) array, IN(RComparator) comp);
  
  static void fill(IN(RObjectArray) array, IN(RObject) val) 
  {
    fill(array, 0, array->length(), val);
  }
  static void fill(IN(RObjectArray)  array, int fromIndex, int toIndex, IN(RObject) val) 
  {
    for (int i = fromIndex; i < toIndex; i++) 
      array[i] = val;
  }
  static RObjectArray removeElement(IN(RObjectArray) array, int idx);
  /**
    Removes first element where element->equals(obj) == true
  */
  static RObjectArray removeFirstElement(IN(RObjectArray) array, IN(RObject) obj)
  {
    for (int i = 0; i < array->length(); ++i)
    {
      if (array[i] == Nil && obj == Nil)
        return removeElement(array, i);
      else if (array[i] != Nil && array[i]->equals(obj) == true)
        return removeElement(array, i);
    }
    return array;
  }
  static void stripElement(IN(RObjectArray) array, int idx)
  {
    for (int i = idx; i < array->length(); i++)
    {
      if (i + 1 < array->length())
        array[i] = array[i + 1];
      else
        array[i] = Nil;
    }
    array->resize(array->length() - 1);
  }
private:
  inline static int _compare(IN(RObject) o1, IN(RObject) o2, IN(RComparator) comp) 
  {
    if (comp == Nil) 
      return RComparable(o1)->compareTo(o2);
    return comp->compare(o1, o2);
  }
};

/** 
  Helper class as representant to a Nil type
*/
template <class T>
class NilComparator
: extends acdk::lang::Object
{
public:
  virtual int compare(IN(T) o1, IN(T) o2) { return 0; }
};


/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.32 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class Arrays
{
public:
  /**
    append all elements from second array to first element
  */
  template <class T>
  template_static 
  RObjectArrayImpl<T> append(IN(RObjectArrayImpl<T>) farray, IN(RObjectArrayImpl<T>) sarray) 
  {
    int sl = sarray->length();
    farray->ensureCapacity(farray->length() + sl);
    for (int i = 0; i < sl; ++i)
      farray->append(sarray[i]);
    return farray;
  }
  template <class T>
  template_static 
  RObjectArrayImpl<T> removeElement(IN(RObjectArrayImpl<T>) array, int idx) 
  {
    RObjectArrayImpl<T> na = new ObjectArrayImpl<T>(array->length() - 1);
    int i;
    for (i = 0; i < idx; i++)
      na[i] = array[i];
    for (i = idx; i < na->length(); i++)
      na[i] = array[i + 1];
    return na;
  }
  /**
    Removes first element where element->equals(obj) == true
  */
  
  template <class T>
  template_static
  RObjectArrayImpl<T> removeFirstElement(IN(RObjectArrayImpl<T>) array, IN(T) obj)
  {
    for (int i = 0; i < array->length(); ++i)
    {
      if (array[i] == Nil && obj == Nil)
        return removeElement(array, i);
      else if (array[i] != Nil && array[i]->equals(obj) == true)
        return removeElement(array, i);
    }
    return array;
  }
  template <class T>
  template_static
  RBasicArray<T> removeElement(IN(RBasicArray<T>) array, int idx) 
  {
    RBasicArray<T> na = new BasicArray<T>(array->length() - 1);
    int i;
    for (i = 0; i < idx; i++)
      na[i] = array[i];
    for (i = idx; i < na->length(); i++)
      na[i] = array[i + 1];
    return na;
  }
  template <class T> template_static void sort(IN(RBasicArray<T>) array) 
  {
    _qsort(array, 0, array->length());
  }
  template <class T> 
  template_static int binarySearch(IN(RBasicArray<T>) array, T key)
  {
    int low = 0;
    int hi = array->length() - 1;
    int mid = 0;
    while (low <= hi) {
      mid = (low + hi) >> 1;
      T d = array[mid];
      if (d == key) {
        return mid;
      } else if (d > key) {
        hi = mid - 1;
      } else {
        low = ++mid; // This gets the insertion point right on the last loop
      }
    }
    return -mid - 1;
  }
  
  template <class T> 
  template_static bool equals(IN(RBasicArray<T>) a1, IN(RBasicArray<T>) a2) 
  {
    if (a1 == a2) 
      return true;
    if (a1 == Nil || a2 == Nil)
      return false;
    if (a1->length() != a2->length()) 
      return false;
    for (int i = 0; i < a1->length(); i++) {
      if (a1[i] != a2[i]) 
        return false;
    }
    return true;
  }
  
  template <class T> 
  template_static void fill(IN(RBasicArray<T>) array, T val) 
  {
    fill(array, 0, array->length(), val);
  }
  template <class T> 
  template_static void fill(IN(RBasicArray<T>) array, int fromIndex, int toIndex, T val) 
  {
    for (int i = fromIndex; i < toIndex; i++) 
      array[i] = val;
  }
  template <class T>
  static void sort(IN(RObjectArrayImpl<T>) array)
  {
    RefHolder<NilComparator<T> > comp;
    _mergesort(array, comp);
  }
  template <class T, class C>
  static void sort(IN(RObjectArrayImpl<T>) array, IN(C) comp)
  {
    _mergesort(array, comp);
  }
  template <class T> template_static void _swap(int i, int j, IN(RBasicArray<T>) a) 
  {
    T c = a[i];
    a[i] = a[j];
    a[j] = c;
  }
  template <class T> template_static int _med3(int a, int b, int c, IN(RBasicArray<T>) d) 
  {
    return _cmp(d[a], d[b]) < 0 ? 
      (_cmp(d[b], d[c]) < 0 ? b : _cmp(d[a], d[c]) < 0 ? c : a)
    : (_cmp(d[b], d[c]) > 0 ? b : _cmp(d[a], d[c]) > 0 ? c : a);
  }
  template <class T> template_static short _cmp(T i, T j) 
  {
    return (short)(i - j);
  }
  template <class T> template_static void _vecswap(int i, int j, int n, IN(RBasicArray<T>) a) 
  {
    for (; n > 0; i++, j++, n--)
      _swap(i, j, a);
  }
  template <class T> template_static void _qsort(IN(RBasicArray<T>) a, int start, int n) 
  {
    if (n < 7) {
      for (int i = start + 1; i < start + n; i++)
        for (int j = i; j > 0 && _cmp(a[j-1], a[j]) > 0; j--)
          _swap(j, j-1, a);
      return;
    }
     int pm = n / 2;
    if (n > 7) {
      int pl = start;
      int pn = start + n-1;

      if (n > 40) { 
        int s = n/8;
        pl = _med3(pl, pl + s, pl + 2 * s, a);
        pm = _med3(pm - s, pm, pm + s, a);
        pn = _med3(pn - 2 * s, pn - s, pn, a);
      }
      pm = _med3(pl, pm, pn, a); // mid-size, med of 3
    }

    int pa, pb, pc, pd, pv;
    short r;

    pv = start; 
    _swap(pv, pm, a);
    pa = pb = start;
    pc = pd = start + n-1;
    
    for (;;) {
      while (pb <= pc && (r = _cmp(a[pb], a[pv])) <= 0) {
        if (r == 0) { 
          _swap(pa, pb, a); 
          pa++; 
        }
        pb++;
      }
      while (pc >= pb && (r = _cmp(a[pc], a[pv])) >= 0) {
        if (r == 0) { 
          _swap(pc, pd, a); 
          pd--; 
        }
        pc--;
      }
      if (pb > pc) 
        break;
      _swap(pb, pc, a);
      pb++;
      pc--;
    }
    int pn = start + n;
    int s;
    s = Math::min(pa-start, pb-pa); 
    _vecswap(start, pb-s, s, a);
    s = Math::min(pd-pc, pn-pd-1); 
    _vecswap(pb, pn-s, s, a);
    if ((s = pb-pa) > 1) 
      _qsort(a, start, s);
    if ((s = pd-pc) > 1) 
      _qsort(a, pn-s, s);
  }
    /** copy from System.h cause include conflicts */
  template <class T>
  template_static
  void 
  arraycopy(IN(RObjectArrayImpl<T>) src, int srcpos, IN(RObjectArrayImpl<T>) dst, int dstpos, int length)
  {
    if (src == Nil || dst == Nil)
      THROW0(NullPointerException);
    
    if ((srcpos < 0 || srcpos + length > int(src.length())) ||
      (dstpos < 0 || dstpos + length > int(dst.length())) ||
      (length < 0)) {
      THROW0(ArrayIndexOutOfBoundsException);
    }
    int di = dstpos;
    int si = srcpos;
    int i = 1;
    if (src == dst && srcpos < dstpos ){ // overlapping copy
      i=-1;
      di += (length-1);
      si += (length-1);
    }
    for (; length > 0; di+=i, si+=i, length--)
      dst[di] = src[si];
  }
  template <class T, class C>
  template_static void _mergesort(IN(RObjectArrayImpl<T>) a)
  {
    RefHolder<NilComparator<T> > comp;
    _mergesort(a, comp);
  }
  template <class T, class C>
  template_static void _mergesort(IN(RObjectArrayImpl<T>) a, IN(C) comp)
  {
    int n = a->length();
    RObjectArrayImpl<T> x = a;
    RObjectArrayImpl<T> y = new ObjectArrayImpl<T>(n);
    RObjectArrayImpl<T> t = Nil;
    for (int sizenf = 1; sizenf < n; sizenf <<= 1) {
      int size = sizenf;
      
      for (int startnf = 0; startnf < n; startnf += size << 1) {
        int start = startnf; 
        int size2 = n - start - size < size ? n - start - size : size;
        if (size2 <= 0 ||
          _compare(x[start + size - 1], x[start + size], comp) <= 0) {
          arraycopy(x, start, y, start, size + size2);
        } else if (_compare(x[start], x[start + size + size2 - 1], comp) >= 0) {
          arraycopy(x, start, y, start + size2, size);
          arraycopy(x, start + size, y, start, size2);
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
          arraycopy(x, d1 > 0 ? p1 : p2, y, i, d1 > 0 ? d1 : d2);
        } 
      }
      t = x; x = y; y = t; // swap x and y ready for the next merge
    }
    if (x != a) 
      arraycopy(x, 0, a, 0, n);
  }

  template <class T>
  template_static int sequenceSearch(IN(RObjectArrayImpl<T>) array, IN(T) find)
  {
    int arlen = array->length();
    for (int i = 0; i < arlen; ++i)
    {
      T o = array[i];
      if (o != Nil && find != Nil && o->equals(find) == true)
        return i;
      if (o == Nil && find == Nil)
        return i;
    }
    return -1;
  }
  template <class T, class C>
  template_static int sequenceSearch(IN(RObjectArrayImpl<T>) array, IN(T) find, IN(C) comp)
  {
    int arlen = array->length();
    for (int i = 0; i < arlen; ++i)
    {
      T o = array[i];
      if (o != Nil && find != Nil && comp->compare(find, o) == 0)
        return i;
      if (o == Nil && find == Nil)
        return i;
    }
    return -1;
  }
  template <class T, class C>
  inline template_static 
  int _compare(IN(T) o1, IN(T) o2, IN(C) comp) 
  {
    if (comp == Nil) 
      return o1->compareTo(o2);
    return comp->compare(o1, o2);
  }
  template <class T, class C>
  template_static int 
  binarySearch(IN(RObjectArrayImpl<T>) a, IN(T) key, IN(C) comp)
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
};


} // util
} // acdk

#endif //acdk_util_Arrays_h

