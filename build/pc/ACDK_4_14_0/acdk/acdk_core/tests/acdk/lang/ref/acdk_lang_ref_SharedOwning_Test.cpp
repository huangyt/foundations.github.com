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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/ref/acdk_lang_ref_SharedOwning_Test.cpp,v 1.7 2005/03/07 17:52:10 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/System.h>
#include <acdk/lang/ref/WeakReference.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/ref/SharedOwning.h>

namespace tests {
namespace acdk {
namespace lang {
namespace dmi {

BEGIN_DECLARE_TEST( SharedOwning_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( indirect )
END_DECLARE_TEST( SharedOwning_Test  )

BEGIN_DEFINE_TEST( SharedOwning_Test )
  ADD_TEST( SharedOwning_Test, standard ) 
  ADD_TEST( SharedOwning_Test, indirect ) 
  
END_DEFINE_TEST( SharedOwning_Test )

using namespace ::acdk::lang::ref;
using namespace ::acdk::lang::sys;


#define DOUT(msg) \
sys::coreout << msg << sys::eofl
//::acdk::lang::System::out->println(SBSTR(msg))


using namespace ::acdk::lang::ref;
using namespace ::acdk::lang::sys;

ACDK_DECL_CLASS(MyElement);

class MyElement
: extends ::acdk::lang::Object
{
  
public:
  RObject _parent;
  MyElement(IN(RObject) parent) 
  : _parent(parent)
  {
  }
  ~MyElement()
  {
    DOUT((void*)this << " ~MyElement()");
    //_releaseParent();
  }
  RObject getParent() { return _parent; }
  RString toString() 
  { 
    return SBSTR("MyElement(" << (int)this << ")");
  }
};


ACDK_DECL_CLASS(MyOwner);

class MyOwner
: extends ::acdk::lang::Object
{

public:
  SharedOwning _sharedOwning;
  RMyElementArray _oarray;
  //RMyElement other;
  //RMyElement other2;

  MyOwner()
  {
    ACDK_SAFE_CONSTRUCTOR();
    

    _oarray = new MyElementArray(0); 
    /*
    RMyElement other = new MyElement(this);
    _sharedOwning.registerSharedObject(other, (RObject*)other->_parent._ref_this(), false);
    _oarray->append(other);
    _sharedOwning.registerSharedObject(this, (RObject*)_oarray[0]._ref_this(), true);
    other = new MyElement(this);
    _sharedOwning.registerSharedObject(other, (RObject*)other->_parent._ref_this(), false);
    _oarray->append(other);
    _sharedOwning.registerSharedObject(this, (RObject*)_oarray[1]._ref_this(), true);
  */
    RMyElement other = new MyElement(this);
    _sharedOwning.registerSharedObject(this, (RObject*)other->_parent._ref_this(), true);
    _oarray->append(other);
    _sharedOwning.registerSharedObject(other, (RObject*)_oarray[0]._ref_this(), false);
    other = new MyElement(this);
    _sharedOwning.registerSharedObject(this, (RObject*)other->_parent._ref_this(), true);
    _oarray->append(other);
    _sharedOwning.registerSharedObject(other, (RObject*)_oarray[1]._ref_this(), false);
    
    //_sharedOwning.registerSharedObject(other, this, false);
    //_sharedOwning.registerSharedObject(other2, this, false);
    /*
    _sharedOwning.registerSharedObject(this, (RObject*)other._ref_this(), true);
    _sharedOwning.registerSharedObject(this, (RObject*)other2._ref_this(), true);
    _sharedOwning.registerSharedObject(other, (RObject*)other->_parent._ref_this(), false);
    _sharedOwning.registerSharedObject(other2, (RObject*)other2->_parent._ref_this(), false);
    */
/*
    _sharedOwning.registerSharedObject(other, this, false);
    _sharedOwning.registerSharedObject(other2, this, false);
    */
  }
  ~MyOwner()
  {
    DOUT((void*)this << " ~MyOwner()");
    _oarray[0] = Nil;
    _oarray[1] = Nil;
  }
  RString toString() 
  { 
    return SBSTR("MyOwner(" << (int)this << ")");
  }
  RMyElement getElement() { return _oarray[0]; }
  void setElement(IN(RMyElement) obj)
  {
    _oarray[0] = obj;
    /*
    if (_oarray[0] == obj)
      return;
    _sharedOwning.unregisterSharedObject(_oarray[0], this);
    _oarray[0] = obj;
    if (obj != Nil)
      _sharedOwning.registerSharedObject(_oarray[0], this, false);
      */
  }
  RMyElement getOther() { return _oarray[1]; }
};


#define EXEC_STATEMENT(statement) do { DOUT(#statement); statement } while(false)

void
SharedOwning_Test::standard()
{
  /*
  {
    RMyOwner own = new MyOwner();
  }
  */
  /*
  {
    RMyElement el;
    {
      RMyOwner own = new MyOwner();
      el = own->other;
    }

    RMyOwner to = (RMyOwner)el->getParent();
  }
  */
  //goto test4;
  {
    RMyElement el;
    RMyOwner own;
    own = new MyOwner();
    el = own->getElement();
    own = Nil;
    el = Nil;
  }
test2:
  {
    RMyElement el;
    RMyOwner own;
    own = new MyOwner();
    el = own->getElement();
    own = Nil;
    own = (RMyOwner)el->getParent();
    //own = Nil;
    el = Nil;

    el = own->getElement();
    own = Nil;
    own = (RMyOwner)el->getParent();
    // el; = Nil;
    EXEC_STATEMENT(own->setElement(Nil); );
    EXEC_STATEMENT( el = Nil; );
    EXEC_STATEMENT( own->getElement(); );
    el = own->getOther();
    own = Nil;
    el = Nil;
  }
test3:
  {
    RMyElement el;
    RMyElement el2;
    RMyOwner own;
    own = new MyOwner();
    el = own->getElement();
    own = Nil;
    el2 = RMyOwner(el->getParent())->getOther();
    el = Nil;
    //own = RMyOwner(el2->getParent());
    el2 = Nil;
    own = Nil;
  }
test4:
  {
    RMyElement el;
    RMyElement el2;
    RMyOwner own;
    own = new MyOwner();
    // own->other2 = Nil;
    el = own->getElement();
    own = Nil;
    el = Nil;
  }
}


ACDK_DECL_CLASS(Master2);
ACDK_DECL_CLASS(Child);

ACDK_DECL_CLASS(Child);
class Child
: extends ::acdk::lang::Object
{
public:
  RObject _obj;
  Child(){}
  ~Child()
  {
    sys::coreout << (void*)this << "; ~Child" << sys::eofl;
  }
};

class Master2
: extends Child
{
public:
  SharedOwning _sharedOwning;
  RChild _obj1;
  RChild _obj2;
  RChildArray _oarray;
  Master2()
    : _oarray(new ChildArray(0))
  {
  }
  ~Master2()
  {
    sys::coreout << (void*)this << "; ~Master2" << sys::eofl;
  }
  void setChildRef(INOUT(RChild) mr, IN(RChild) c)
  {
    if (mr != Nil)
    {
      mr->_obj = Nil;
      // unregister here
    }
    mr = c;
    mr->_obj = this;
    _sharedOwning.registerSharedObjectRefs((RObject*)mr->_obj._ref_this(), (RObject*)mr._ref_this());
  }
  void setChild1(IN(RChild) c)
  {
    setChildRef(_obj1, c);
  }

  void setChild2(IN(RChild) c)
  {
    setChildRef(_obj2, c);
  }
  void addChild(IN(RChild) c)
  {
    _oarray->append(Nil);
    setChildRef(_oarray[_oarray->size() - 1], c);
  }
};

void
SharedOwning_Test::indirect()
{
  /*
  {
    RMaster2 own = new Master2();
    own->setChild1(new Child());
    own->setChild2(new Child());
    RChild c = own->_obj1;
    ow = Nil;
    own =  RMaster2(c->_obj);
    c = Nil;
    own = Nil;
  }
  
  {
    RMaster2 own = new Master2();
    own->setChild1(new Child());
    own->setChild2(new Child());
    own->_obj1 = Nil;
    own->_obj2 = Nil;
    own = Nil;
  }
  
  {
    RMaster2 own = new Master2();
    own->setChild1(new Child());
    own->setChild2(new Child());
    RChild c = own->_obj1;
    own = Nil;
    c = RMaster2(c->_obj)->_obj2;
    c = Nil;
  }
  */
  /*
    
  {
    RMaster2 own = new Master2();
    own->setChild1(new Child());
    RMaster2 own2 = new Master2();
    own2->setChild1(new Child());
    own->setChild2(&own2);
    RChild o1c2 = own->_obj2;

    own = Nil;
    own2 = Nil;
    RChild c2 = RMaster2(o1c2)->_obj1;
    o1c2 = Nil;
    c2 = (RChild)c2->_obj;
    c2 = Nil;
    
  }
  */
  {
    RMaster2 b = new Master2();
    RMaster2 a = new Master2();
    a->setChild1(new Child());
    b->setChild1(&a);
    a = Nil;
    RChild c = RMaster2(b->_obj1)->_obj1;
    c = Nil;
    b = Nil;
  }
}

} // namespace lang 
} // namespace dmi 
} //namespace acdk 
} //namespace tests 




