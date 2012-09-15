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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/PropertyResourceBundle.h,v 1.17 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_PropertyResourceBundle_h
#define acdk_util_PropertyResourceBundle_h

#include <acdk.h>
#include <acdk/io/IOException.h>

#include "ResourceBundle.h"
#include "Properties.h"

namespace acdk {
namespace util {

using namespace acdk::lang;


ACDK_DECL_CLASS(PropertyResourceBundle);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.17 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC PropertyResourceBundle 
: extends ResourceBundle
{
  ACDK_WITH_METAINFO(PropertyResourceBundle)  
private:
  /** the underlying data */
  RProperties _properties;
public:
  PropertyResourceBundle(IN(acdk::io::RReader) in) THROWS1(acdk::io::RIOException);
  foreign virtual RIterator getKeys();
  RProperties getProperties() { return _properties; }
  virtual bool hasValue(IN(RString) key);
  virtual RStringArray getStringArray(IN(RString) key) THROWS1(RMissingResourceException);
  virtual RMap getMap(IN(RString) key) THROWS1(RMissingResourceException);
protected:
  RObject handleGetObject(IN(RString) key)
  {
    return (RObject)_properties->getProperty(key);
  }
};
  

} // namespace util 
} //namespace acdk 



#endif //acdk_util_PropertyResourceBundle_h

