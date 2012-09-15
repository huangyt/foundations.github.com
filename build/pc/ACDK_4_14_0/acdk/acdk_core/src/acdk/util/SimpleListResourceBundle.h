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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SimpleListResourceBundle.h,v 1.15 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_util_SimpleListResourceBundle_h
#define acdk_util_SimpleListResourceBundle_h

#include <acdk.h>
#include <acdk/io/IOException.h>
#include "ResourceBundle.h"
#include "DoubleIterator.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_CLASS(SimpleListResourceBundle);

/** 
  A more simple ListResourcebundle with const char* as key and value *
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.15 $
  @date $Date: 2005/04/08 10:53:20 $
  @see ListResourcebundle
*/
class ACDK_CORE_PUBLIC SimpleListResourceBundle 
: extends ResourceBundle
{
  ACDK_WITH_METAINFO(SimpleListResourceBundle)  
private:
  friend class SimpleListResourceBundleIterator;
public:
  SimpleListResourceBundle() THROWS1(acdk::io::RIOException)
  : ResourceBundle()
  {
  }
  foreign virtual RIterator getKeys();
  
  foreign struct KeyValues
  {
    const char* key;
    const char* value;
  };

protected:

  /**
    Different to Java this returns a null terminated list:
    @code
    class MyRessourceBundle 
    : public SimpleListResourceBundle
    {
    public:
      MyRessourceBundle() 
      : SimpleListResourceBundle()
      {
      }
      static SimpleListResourceBundle::KeyValues _values[];
      virtual SimpleListResourceBundle::KeyValues* getContents()
      {
        return _values;
      }
    };
    //static 
    SimpleListResourceBundle::KeyValues MyRessourceBundle::_values[] = {
      { "key1", "val1" },
      { "key2", "val2" },
      { "key3", "val3" },
      { 0, 0 }
    };
  */
  typedef KeyValues* KeyValuesPtr;
  /**
    has to be overwritten
  */
  foreign virtual KeyValuesPtr getContents() 
  { 
    ObjectBase::_throwNotImplementedYet("SimpleListResourceBundle::getContents");  
    return 0; 
  }
  virtual RObject handleGetObject(IN(RString) key) ;
  
};
  

} // namespace util 
} //namespace acdk 



#endif //acdk_util_SimpleListResourceBundle_h

