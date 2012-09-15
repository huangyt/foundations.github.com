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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractFilterWriter.cpp,v 1.7 2005/03/08 12:45:35 kommer Exp $




#include <acdk.h>

#include "AbstractFilterWriter.h"
#include "AbstractFilterReader.h"
#include "TeeWriter.h"

#include "Storage.h"

#include "IOException.h"

namespace acdk {
namespace io {

//virtual 
RStorage 
TeeWriter::getStorage()
{
  if (instanceof(_out1, Storage) == true)
    return RStorage(_out1);
  else if (instanceof(_out1, FilterWriter) == true) {
    return RFilterWriter(_out1)->getStorage();
  } 
  THROW1(IOException, "Unknown Writer type for getOut" + ((RObject)_out1)->toString());
  return Nil;
}


//virtual 
RStorage 
AbstractFilterWriter::getStorage()
{
  if (instanceof(_out, Storage) == true)
    return RStorage(_out);
  else if (instanceof(_out, FilterWriter) == true) {
    return RFilterWriter(_out)->getStorage();
  }
  THROW1(IOException, "Unknown Writer type for getOut" + ((RObject)_out)->toString());
  return Nil;
}


//virtual 
RStorage 
AbstractFilterReader::getStorage()
{
  if (instanceof(_in, Storage) == true)
    return RStorage(_in);
  else if (instanceof(_in, FilterReader) == true)
    return RFilterReader(_in)->getStorage();
  // Interface Writer to RefHolder<Object>

  THROW1(IOException, "Unknown Reader type for getIn" + ((RObject)_in)->toString());
  return Nil;
}

} // io
} // acdk
