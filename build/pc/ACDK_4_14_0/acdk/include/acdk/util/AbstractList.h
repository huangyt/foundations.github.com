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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractList.h,v 1.21 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_AbstractList_h
#define acdk_util_AbstractList_h

#include <acdk.h>
#include <acdk/lang/IndexOutOfBoundsException.h>

#include "AbstractCollection.h"
#include "List.h"


namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_CLASS(AbstractList);

/**
  API: Java<br/>
  @author of the original Classpath implementation Stuart Ballard (stuart.ballard@mcmail.com)
          Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:56 $
*/
class ACDK_CORE_PUBLIC AbstractList 
: extends AbstractCollection,
  implements List
{
  ACDK_WITH_METAINFO(AbstractList)
protected:
  mutable int _modCount;
  AbstractList()
  : _modCount(0)
  {
  }
  
public:
  virtual ~AbstractList() { }

  foreign virtual RObject get(int index) = 0;
  virtual void add(int index, IN(RObject) o);
  virtual bool add(IN(RObject) o);
  virtual bool addAll(int index, IN(RCollection) c);
  virtual void clear();
  virtual bool equals(IN(RObject) o);
  virtual int hashCode();
  virtual int indexOf(IN(RObject) o);
  virtual RIterator iterator();
  virtual int lastIndexOf(IN(RObject) o);
  virtual RListIterator listIterator(int index = 0);
  virtual RObject remove(int index);
  virtual RObject set(int index, IN(RObject) o);
  virtual int size();
  virtual RList subList(int fromIndex, int toIndex);
  virtual bool isEmpty() { return AbstractCollection::isEmpty(); }
  virtual bool contains(IN(RObject) obj) { return AbstractCollection::contains(obj); }
  virtual bool containsAll(IN(RCollection) c) { return AbstractCollection::containsAll(c); }
  virtual bool remove(IN(RObject) obj) { return AbstractCollection::remove(obj); }
  virtual bool removeAll(IN(RCollection) c) { return AbstractCollection::removeAll(c); }
  virtual bool addAll(IN(RCollection) c) { return AbstractCollection::addAll(c); }
  virtual bool retainAll(IN(RCollection) c) { return AbstractCollection::retainAll(c); }
  virtual RObjectArray toArray();
  virtual RObjectArray toArray(IN(RObjectArray) a);
  virtual RString toString() { return AbstractCollection::toString(); }

  friend class AbstractListIterator;
  friend class AbstractListListIterator;
  friend class AbstractListSubList;
protected:
  virtual void removeRange(int fromIndex, int toIndex);

};

} // util
} // acdk



#endif //acdk_util_AbstractList_h

