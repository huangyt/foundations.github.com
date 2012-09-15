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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/IOException.cpp,v 1.10 2005/02/05 10:44:54 kommer Exp $




#include <acdk.h>
#include "IOException.h"
#include <errno.h>

namespace acdk {
namespace io {

using namespace acdk::lang;

IOException::IOException(bool withErrno) 
: Exception() 
{ 
  if (withErrno == true)
    _what = strerror(errno);
}

IOException::IOException(IN(RString) what, bool withErrno) 
: Exception() 
{ 
  if (withErrno == true)
    _what = what + ": " + strerror(errno);
  else
    _what = what;
}

namespace {

void foo()
{
  try {
    THROW1(IOException, "dummy");
  } catch (RIOException ex) {

  }
}
} // anon namespace

} // io
} // acdk


