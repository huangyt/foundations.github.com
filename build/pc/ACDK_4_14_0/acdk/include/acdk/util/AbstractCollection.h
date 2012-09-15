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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractCollection.h,v 1.15 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_AbstractCollection_h
#define acdk_util_AbstractCollection_h

#include "Collection.h"
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

using namespace acdk::lang;


ACDK_DECL_CLASS(AbstractCollection);

/**
  API: Java<br/>
  @author of the orignal GNU Classpath implementation: Stuart Ballard (stuart.ballard@mcmail.com)
          Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/
class ACDK_CORE_PUBLIC AbstractCollection 
: extends ::acdk::lang::Object,
  implements acdk::util::Collection
{
  ACDK_WITH_METAINFO(AbstractCollection)
public:
  /// reimplemented from Collection
  foreign virtual RIterator iterator() = 0;

  /// reimplemented from Collection
  foreign virtual bool add(IN(RObject) o) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }

  /// reimplemented from Collection
  virtual bool addAll(IN(RCollection) c);

  /// reimplemented from Collection
  virtual void clear();

  /// reimplemented from Collection
  virtual bool contains(IN(RObject) o);

  /// reimplemented from Collection
  virtual bool containsAll(IN(RCollection) c);

  /// reimplemented from Collection
  virtual bool isEmpty() { return size() == 0; }

  /// reimplemented from Collection
  virtual bool remove(IN(RObject) o);

  /// reimplemented from Collection
  virtual bool removeAll(IN(RCollection) c);

  /// reimplemented from Collection
  virtual bool retainAll(IN(RCollection) c);

  /// reimplemented from Collection
  virtual RObjectArray toArray();

  /// reimplemented from Collection
  virtual RObjectArray toArray(IN(RObjectArray) a);

  virtual bool equals(IN(RObject) c) = 0;
  virtual int hashCode() = 0;
  virtual RString toString();
};


} // util
} // acdk



#endif //acdk_util_AbstractCollection_h

