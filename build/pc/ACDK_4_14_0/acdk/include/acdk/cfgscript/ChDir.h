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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/cfgscript/ChDir.h,v 1.5 2005/02/05 10:44:52 kommer Exp $

#ifndef acdk_cfgscript_ChDir_h
#define acdk_cfgscript_ChDir_h

#include "Config.h"
#include <acdk/io/File.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace cfgscript {

/**
  Change directory in constructor
  and change back to previsous directory
  in desctructor.
*/
struct ChDir
{
  RString _oldcwd;
  ChDir(IN(RString) newcwd)
  {
    if (newcwd == Nil)
      return;
    RString cwd = acdk::io::File::getCWD();
    RString fqnewcwd = acdk::io::File(newcwd).getCanonicalPath();
    if (fqnewcwd->equals(cwd) == true)
      return;
    _oldcwd = cwd;
    if (acdk::io::File::setCWD(newcwd) == false)
    {
      ACDK_NLOG("acdk.make", Error, "Cannot change to directory: " + newcwd);
    } 
    else
    {
      ACDK_NLOG("acdk.make", Trace, "Changed to directory: " + newcwd);
    }

  }
  ~ChDir()
  {
    if (_oldcwd == Nil)
      return;
    if (acdk::io::File::setCWD(_oldcwd) == false)
    {
      ACDK_NLOG("acdk.make", Error, "Cannot change to directory: " + _oldcwd);
    }
    else
    {
      ACDK_NLOG("acdk.make", Trace, "Changed to directory: " + _oldcwd);
    }
  }
};


} 
}
#endif //acdk_cfgscript_ChDir_h

