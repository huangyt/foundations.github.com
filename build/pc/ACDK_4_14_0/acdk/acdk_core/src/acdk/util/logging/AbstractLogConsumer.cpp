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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/AbstractLogConsumer.cpp,v 1.7 2005/02/05 10:45:07 kommer Exp $


#include "AbstractLogConsumer.h"

namespace acdk {
namespace util {
namespace logging {

//foreign virtual 
void 
AbstractLogConsumer::configure(IN(RString) propnameprefix, IN(RProperties) props) 
{
  RString filter = props->getProperty(propnameprefix + ".filter");
  if (filter != Nil)
  {

  }
  RString formatter = props->getProperty(propnameprefix + ".formatter");
  if (formatter != Nil)
  {
    _formatter = (RFormatter)Class::forName(formatter)->newInstance();
    _formatter->configure(propnameprefix + ".formatter", props);
  } else {
    _formatter = new (allocator()) StdFormatter();
  }
}


} // namespace logging
} // namespace util
} // namespace acdk


