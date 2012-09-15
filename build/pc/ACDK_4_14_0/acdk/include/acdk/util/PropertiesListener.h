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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/PropertiesListener.h,v 1.7 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_PropertiesListener_h
#define acdk_util_PropertiesListener_h

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include <acdk/util/Properties.h>

namespace acdk {
namespace util {

ACDK_DECL_CLASS(PropertiesListener);

/**
  Implements a deamon thread, which will update the given
  Property if a SIGHUB will be encountered.
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
*/
class ACDK_CORE_PUBLIC PropertiesListener
: extends ::acdk::lang::Thread
{
  ACDK_WITH_METAINFO(PropertiesListener)
protected:
  RString _configFile;
  RProperties _properties;
public:
  static bool reconfigure;
  PropertiesListener(IN(RString) cfgFile, IN(RProperties) props);
  void reload();
  void run();
};



} // util
} // acdk


#endif //acdk_util_PropertiesListener_h

