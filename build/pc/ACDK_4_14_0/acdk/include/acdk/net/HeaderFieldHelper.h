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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/HeaderFieldHelper.h,v 1.11 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_HeaderFieldHelper_h
#define acdk_net_HeaderFieldHelper_h

#include "net.h"
#include <acdk/util/ArrayList.h>

namespace acdk {
namespace net {

using namespace acdk::lang;
USING_CLASS(::acdk::util::, ArrayList);

ACDK_DECL_CLASS(HeaderFieldHelper);

class ACDK_NET_PUBLIC HeaderFieldHelper
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(HeaderFieldHelper)
private:
  RArrayList _headerFieldKeys;
  RArrayList _headerFieldValues;

public:
  
  HeaderFieldHelper(int size = 10)
  : Object(),
    _headerFieldKeys(new ArrayList(size)),
    _headerFieldValues(new ArrayList(size))
  {
  }

  virtual void addHeaderField(IN(RString) key, IN(RString) value)
  {
    _headerFieldKeys->add((RObject)key);
    _headerFieldValues->add((RObject)value);
  }
  virtual RString getHeaderFieldKeyByIndex(int index);
  virtual RString getHeaderFieldValueByIndex(int index);
  virtual int getNumberOfEntries()
  {
    return(_headerFieldKeys->size());
  }
protected:
};

} // namespace acdk
} // namespace net

#endif //acdk_net_HeaderFieldHelper_h


