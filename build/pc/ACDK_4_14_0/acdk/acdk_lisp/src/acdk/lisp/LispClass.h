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


#ifndef acdk_lisp_LispClass_h
#define acdk_lisp_LispClass_h


#include "LispVar.h"
#include <acdk/lang/CloneNotSupportedException.h>
#include <acdk/util/HashMap.h>

namespace acdk {
namespace lisp {


ACDK_DECL_CLASS(LispClass);
ACDK_DECL_CLASS(LispSlot);

class ACDK_ACDK_LISP_PUBLIC LispSlot
: extends ::acdk::lang::Object
, implements ::acdk::lang::Cloneable
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(LispSlot)
public:
  /// for serialization
  static RObject create_instance() { return new LispSlot(Nil); }
  RString name;
  RString initarg;
  bool isStatic;
  /**
    In case of static also contains class varaible
  */
  RLispVar initform;
  LispSlot(IN(RString) nam, bool isstatic = false,
                             IN(RString) inita = Nil,
                             IN(RLispVar) initval = Nil)
  : name(nam)
  , initarg(inita)
  , isStatic(isstatic)
  , initform(initval)
  {
  }
  RString toCode();
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc)
  {
    return new (alc) LispSlot(name, isStatic, initarg, initform);
  }

};

/**

(defclass (super list)
  (slotname :initarg :namedarg)
  (slotname :initarg :namedarg :initform initval)
)

*/
class ACDK_ACDK_LISP_PUBLIC LispClass 
: extends LispVar
{
  ACDK_WITH_METAINFO(LispClass)
private:
  
  RString _className;
  RLispClassArray _superClasses;
  RLispSlotArray _slots;
public:
  /// for serialization
  static RObject create_instance() { return new LispClass(Nil); }
  LispClass(IN(RString) classname)
  : LispVar()
  , _className(classname)
  , _superClasses(new LispClassArray(0))
  , _slots(new LispSlotArray(0))
  {
  }
  RString className() { return _className; }
  RLispClassArray supers() { return _superClasses; }
  RLispSlotArray slots() { return _slots; }
  void addClass(IN(RLispClass) cls)
  {
    _superClasses->append(cls);
  }
  void addSlot(IN(RLispSlot) slot)
  {
    _slots->append(slot);
  }
  /**
    returns false if slot not found
  */
  
  foreign virtual RString toString();
  foreign virtual RString toCode();
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc);
  
};



} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispClass_h
