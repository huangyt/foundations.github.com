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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/ListResourceBundle.h,v 1.15 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_ListResourceBundle_h
#define acdk_util_ListResourceBundle_h

#include <acdk.h>
#include <acdk/io/IOException.h>

#include "ResourceBundle.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_CLASS(ListResourceBundle);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC ListResourceBundle 
: extends ResourceBundle
{
  ACDK_WITH_METAINFO(ListResourceBundle)  
private:
  friend class ListResourceBundleIterator;
public:
  ListResourceBundle() THROWS1(acdk::io::RIOException)
  : ResourceBundle()
  {
  }
  virtual RObjectArrayArray getContents() = 0;
  virtual RIterator getKeys();
protected:
  virtual RObject handleGetObject(IN(RString) key);
  
};
  

} // namespace util 
} //namespace acdk 



#endif //acdk_util_ListResourceBundle_h

