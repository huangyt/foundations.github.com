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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/namesrv/CosNamingImpl.cpp,v 1.18 2005/02/05 10:45:40 kommer Exp $


#include <acdkx/orb/namesrv/CosNamingImpl.h>


namespace acdkx {
namespace orb {
namespace namesrv {

using namespace ::org::omg::CosNaming;

//virtual 
void 
NamingContextImpl::bind(IN(RNameComponentArray) name, IN(::org::omg::CORBA::RObject) obj) 
          THROWS4(::org::omg::CosNaming::RNotFound, ::org::omg::CosNaming::RCannotProceed
                 , ::org::omg::CosNaming::RInvalidName, ::org::omg::CosNaming::RAlreadyBound)
{
  
  if (name->length() < 1)
    THROW2(CannotProceed, this, namingContextAsString(*name));

  if (name->length() > 1) {
    RNamingContext nc = (RNamingContext)_childs.get((RObject)name[0]);
    if (nc == Nil)
      THROW2(NotFound, not_context, namingContextAsString(*name));
    
    nc->bind(shift(*name), obj);
    return;
  }

  if (_entries.get((RObject)name) != Nil)
    THROW0(AlreadyBound);
  
  _entries.put((RObject)name, (RObject)obj);
}

//virtual 
void 
NamingContextImpl::rebind(IN(RNameComponentArray) name, IN(::org::omg::CORBA::RObject) obj) 
                THROWS3(::org::omg::CosNaming::RNotFound, ::org::omg::CosNaming::RCannotProceed
                      , ::org::omg::CosNaming::RInvalidName)
{
  if (name.length() < 1)
    THROW2(CannotProceed, this, namingContextAsString(*name));
  
  if (name.length() > 1) {
    RNamingContext nc = (RNamingContext)_childs.get((RObject)name[0]);
    if (nc == Nil)
      THROW2(NotFound, not_context, namingContextAsString(*name));
    
    nc->rebind(shift(*name), obj);
    return;
  }
  _entries.put(&name[0], (RObject)obj);
}


//virtual 
void 
NamingContextImpl::bind_context(IN(RNameComponentArray) name, IN(RNamingContext) nc) 
                        THROWS4(::org::omg::CosNaming::RNotFound, ::org::omg::CosNaming::RCannotProceed
                               , ::org::omg::CosNaming::RInvalidName, ::org::omg::CosNaming::RAlreadyBound)
{
  if (name.length() < 1)
    THROW2(CannotProceed, this, namingContextAsString(*name));
  
  RNamingContext cnc = (RNamingContext)_childs.get((RObject)name[0]);
  if (name.length() > 1) {
    if (cnc == Nil)
      THROW2(NotFound, not_context, namingContextAsString(*name));
    cnc->bind_context(shift(*name), nc);
    return;
  }
  if (cnc != Nil)
    THROW0(AlreadyBound);
  _childs.put((RObject)name[0], (RObject)nc);
}

//virtual 
void 
NamingContextImpl::rebind_context(IN(RNameComponentArray) name, IN(RNamingContext) nc)     
  THROWS3(::org::omg::CosNaming::RNotFound, ::org::omg::CosNaming::RCannotProceed, ::org::omg::CosNaming::RInvalidName)
{
  if (name.length() < 1)
    THROW2(CannotProceed, this, namingContextAsString(*name));
  
  RNamingContext cnc = (RNamingContext)_childs.get(&name[0]);
  if (name.length() > 1) {
    if (cnc == Nil)
      THROW2(NotFound, not_context, namingContextAsString(*name));
    cnc->bind_context(shift(*name), nc);
    return;
  }
  _childs.put((RObject)name[0], (RObject)nc);
}

//virtual 
::org::omg::CORBA::RObject 
NamingContextImpl::resolve(IN(RNameComponentArray) name)
                                THROWS3(::org::omg::CosNaming::RNotFound, ::org::omg::CosNaming::RCannotProceed, ::org::omg::CosNaming::RInvalidName)
{
  if (name.length() < 1)
    THROW2(CannotProceed, this, namingContextAsString(*name));
  
  if (name.length() > 1) {
    RNamingContext cnc = (RNamingContext)_childs.get(&name[0]);
    if (cnc == Nil)
      THROW2(NotFound, not_context, namingContextAsString(*name));
    return cnc->resolve(shift(*name));
  }
  if (_entries.containsKey(&name[0]) == false)
    THROW2(NotFound, not_object, namingContextAsString(*name));

  return (::org::omg::CORBA::RObject )_entries.get((RObject)name[0]);
}

//virtual 
void 
NamingContextImpl::unbind(IN(RNameComponentArray) name) THROWS3(::org::omg::CosNaming::RNotFound
                                                              , ::org::omg::CosNaming::RCannotProceed
                                                              , ::org::omg::CosNaming::RInvalidName)
{
  if (name.length() < 1)
    THROW2(CannotProceed, this, namingContextAsString(*name));
  if (name.length() > 1) {
    RNamingContext cnc = (RNamingContext)_childs.get(&name[0]);
    if (cnc == Nil)
      THROW2(NotFound, not_context, namingContextAsString(*name));
    cnc->unbind(shift(*name));
    return;
  }
  if (_entries.containsKey(&name[0]) == false)
    THROW2(NotFound, not_object, namingContextAsString(*name));
  _entries.remove(&name[0]);
}

//virtual 
RNamingContext 
NamingContextImpl::new_context()
{
  return new NamingContextImpl();
}

//virtual 
RNamingContext 
NamingContextImpl::bind_new_context(IN(RNameComponentArray) name)
            THROWS4(::org::omg::CosNaming::RNotFound, ::org::omg::CosNaming::RAlreadyBound
                  , ::org::omg::CosNaming::RCannotProceed, ::org::omg::CosNaming::RInvalidName)
{
  if (name->length() < 1)
    THROW2(CannotProceed, this, namingContextAsString(*name));
  
  RNamingContextImpl nci = new NamingContextImpl(this);
  
  RNameComponent ncomp = name[0];
  
  
  if (_childs.containsKey(&ncomp) == true)
    THROW0(AlreadyBound);
  
  RNamingContextImpl nc = (RNamingContextImpl)new_context();
  nc->_parent = this;
  
  _childs.put(&ncomp, &nc);
  if (name->length() == 1)
    return &nc;
  nc->bind_new_context(shift(*name));
  return &nc;
}

//virtual 
void 
NamingContextImpl::destroy() THROWS1(::org::omg::CosNaming::RNotEmpty)
{
  if (_childs.size() != 0)
    THROW0(NotEmpty);
  if (_entries.size() != 0)
    THROW0(NotEmpty);
  if (_parent == Nil)
    return;
  //### how to implement _parent->unbind(this);
}

//virtual 
void 
NamingContextImpl::list(int how_many, OUT(RBindingArray) bl, OUT(RBindingIterator) bi)
{
  _throwNotImplementedYet("NamingContextImpl::list()");
  int maxbinds = getBindingSize();
  if (maxbinds == Nil) {
    bl = Nil;
    bi = Nil;
    return;
  }
  int maxbindings = Math::min(how_many, maxbinds);
  RBindingIteratorImpl it = new BindingIteratorImpl(this);
  for (int i = 0; i < maxbindings; i++) {
    
  }
  
  
    
  
}


int 
NamingContextImpl::getBindingSize()
{
  int ret = _entries.size();
  ::acdk::util::RIterator it = _childs.iterator();
  while (it->hasNext() == true) {
    ret += RNamingContextImpl(it->next())->getBindingSize();
  }
  return ret;
}

BindingIteratorImpl::BindingIteratorImpl(IN(RNamingContextImpl) cont)
: _root(cont)
, _current(cont)
, _entryPos(0)
, _childPos(0)
{
}
  
//virtual 
bool 
BindingIteratorImpl::next_one(OUT(RBinding) b)
{
  return false;
}

//virtual 
bool 
BindingIteratorImpl::next_n(int how_many, OUT(RBindingArray) bl)
{
  
  return false;
}

//virtual 
void 
BindingIteratorImpl::destroy()
{
}

ACDK_BCC_RTHROWABLE_DEFINITION_FQ(::org::omg::CORBA::portable::, ApplicationException)
ACDK_BCC_RTHROWABLE_DEFINITION_FQ(::org::omg::CORBA::portable::, RemarshalException)

} // namespace namesrv
} // namespace orb
} // namespace acdkx




