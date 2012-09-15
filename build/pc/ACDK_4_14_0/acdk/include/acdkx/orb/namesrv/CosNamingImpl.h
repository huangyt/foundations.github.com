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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/namesrv/CosNamingImpl.h,v 1.17 2005/03/07 18:59:46 kommer Exp $

#ifndef acdkx_orb_namesrv_CosNamingImpl_h
#define acdkx_orb_namesrv_CosNamingImpl_h

#include <acdk.h>
#include <org/omg/CosNaming/CosNaming.h>
#include <acdk/util/HashMap.h>

namespace acdkx {
namespace orb {
namespace namesrv {

using namespace ::org::omg::CosNaming;

ACDK_DECL_CLASS(NamingContextImpl);

ACDK_DECL_CLASS(BindingIteratorImpl);

class BindingIteratorImpl
: extends ::acdkx::orb::ServerDelegate
, implements BindingIterator
{
  RNamingContextImpl _root;
  RNamingContextImpl _current;
  int _entryPos;
  int _childPos;
public:
  BindingIteratorImpl(IN(RNamingContextImpl) cont);
  virtual bool next_one(OUT(RBinding) b);
  virtual bool next_n(int how_many, OUT(RBindingArray) bl);
  virtual void destroy();
};



class NamingContextImpl
: extends ::acdkx::orb::ServerDelegate
, implements NamingContext
{
  DECL_ACDK_DEFAULT_METACLASS(NamingContext)
private:
  RNamingContext _parent;
  ::acdk::util::HashMap _entries;
  ::acdk::util::HashMap _childs;
public:
  NamingContextImpl(IN(RNamingContext) parent = Nil)
  : ServerDelegate()
  ,_parent(parent)
  , _entries()
  , _childs()
  {
  }
  RString namingContextAsString(NameComponentArray& name)
  { 
    StringBuffer sb;
    for (int i = 0; i < name.length(); i++) {
      sb.append(name[i]->toString());
      sb.append(" / ");
    }
    return sb.toString();
  }
  RNameComponentArray shift(NameComponentArray& nc)
  {
    RNameComponentArray ret = new NameComponentArray(nc.length() - 1);
    for (int i = 1; i < nc.length(); i++) {
      ret[i - 1] = nc[i];
    }
    return ret;
  }
  void bind(IN(RNameComponentArray) name, IN(::org::omg::CORBA::RObject) obj) 
    THROWS4( ::org::omg::CosNaming::RNotFound
          , ::org::omg::CosNaming::RCannotProceed
          , ::org::omg::CosNaming::RInvalidName
          , ::org::omg::CosNaming::RAlreadyBound);
  
  virtual void rebind(IN(RNameComponentArray) n, IN(::org::omg::CORBA::RObject) obj) 
    THROWS3(::org::omg::CosNaming::RNotFound
          , ::org::omg::CosNaming::RCannotProceed
          , ::org::omg::CosNaming::RInvalidName);

  virtual void bind_context(IN(RNameComponentArray) n, IN(RNamingContext) nc) THROWS4(::org::omg::CosNaming::RNotFound
                                                                                     , ::org::omg::CosNaming::RCannotProceed
                                                                                     , ::org::omg::CosNaming::RInvalidName
                                                                                     , ::org::omg::CosNaming::RAlreadyBound);
  virtual void rebind_context(IN(RNameComponentArray) n, IN(RNamingContext) nc) THROWS3(::org::omg::CosNaming::RNotFound
                                                                                       , ::org::omg::CosNaming::RCannotProceed
                                                                                       , ::org::omg::CosNaming::RInvalidName);
  virtual ::org::omg::CORBA::RObject resolve(IN(RNameComponentArray) n)	THROWS3(::org::omg::CosNaming::RNotFound
                                                                              , ::org::omg::CosNaming::RCannotProceed
                                                                              , ::org::omg::CosNaming::RInvalidName);
  virtual void unbind(IN(RNameComponentArray) n) THROWS3(::org::omg::CosNaming::RNotFound
                                                       , ::org::omg::CosNaming::RCannotProceed
                                                       , ::org::omg::CosNaming::RInvalidName);
  virtual RNamingContext new_context();
  virtual RNamingContext bind_new_context(IN(RNameComponentArray) n) THROWS4(::org::omg::CosNaming::RNotFound
                                                                           , ::org::omg::CosNaming::RAlreadyBound
                                                                           , ::org::omg::CosNaming::RCannotProceed
                                                                           , ::org::omg::CosNaming::RInvalidName);
  virtual void destroy() THROWS1(RNotEmpty);
  virtual void list (int how_many, OUT(RBindingArray) bl, OUT(RBindingIterator) bi);
protected:
  int getBindingSize();
};

} // namespace namesrv
} // namespace orb
} // namespace acdkx



#endif //acdkx_orb_namesrv_CosNamingImpl_h
