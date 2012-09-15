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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/orb/acdk/AdressBookImpl.h,v 1.12 2005/02/05 10:45:41 kommer Exp $


#ifndef tests_acdkx_orb_AdressBookImpl_h
#define tests_acdkx_orb_AdressBookImpl_h


#include <acdk.h>
#include <acdk/util/HashMap.h>

#include "IdlMappingTest.h"

namespace tests {
namespace acdkx {
namespace orb {

ACDK_DECL_INTERFACE(AdressBookImpl);

using ::acdkx::orb::ServerDelegate;

/** 
  class to test with other orb 
*/

class AdressBookImpl
: extends ::acdkx::orb::ServerDelegate,
  implements AdressBook
{
  // not needed ACDK_WITH_METAINFO
private:
  /**
    key: RString
    value: RAdressInfo
  */
  ::acdk::util::RHashMap _map;
  RAdressBook _other;
public:
  virtual RClass getClass() { return AdressBook::GetClass(); }

  AdressBookImpl()
  : ServerDelegate(),
    _map(new ::acdk::util::HashMap())
  {
  }

  virtual void ping()
  {
    System::out->println("AdressBookImpl::ping()");
  }
  virtual void testArray(IN(RintArray) longs, OUT(RStringArray) strings)
  {
    System::out->println("AdressBookImpl::testArray():");
    for (int i = 0; i < longs->length(); i++) {
      System::out->println(RString("longs[") + i + "] = " + longs[i]);
    }
    RStringArray tstrings = new StringArray(3);
    tstrings[0] = RString("Hello");
    tstrings[1] = RString("from");
    tstrings[2] = RString("ACDK ORB");
    strings = tstrings;
  }
  virtual RAdressInfo getAddressInfoA(RString name)
  {
    return (RAdressInfo)_map->get((RObject)name);
  }
  virtual void getAddressInfoB(RString name, OUT(RAdressInfo) adressinfo)
  {
    adressinfo = (RAdressInfo)_map->get((RObject)name);
  }
  virtual void setAddressInfo(RString name, IN(RAdressInfo) adressinfo)
  {
    _map->put((RObject)name, (RObject)(RAdressInfo)adressinfo);
  }
  virtual RAdressInfoArray getAllAdressInfos() 
  {
    RAdressInfoArray ia = new AdressInfoArray(_map->size());
    acdk::util::RIterator it = _map->entrySet()->iterator();
    for (int i = 0; it->hasNext() == true; ++i)
    {
      ia[i] = RAdressInfo(acdk::util::RMapEntry(it->next())->getValue());
    }
    return ia;
  }
  virtual void setOtherAdressBook(IN(RAdressBook) other)
  {
    _other = other;
  }
  virtual void getOtherAdressBook(OUT(RAdressBook) other)
  {
    other = _other;
  }
};

} // namespace orb 
} // namespace acdkx 
} // namespace tests 


#endif //tests_acdkx_orb_AdressBookImpl_h
