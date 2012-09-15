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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/FileConsumer.cpp,v 1.11 2005/02/05 10:45:07 kommer Exp $


#include "FileConsumer.h"
#include <acdk/io/CharToByteWriter.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Process.h>
#include <acdk/io/BufferedWriter.h>

namespace acdk {
namespace util {
namespace logging {

FileConsumer::FileConsumer(INP(RString) fname, INP(RFormatter) formatter, int minll, int maxll, bool buffered)
: AbstractLogConsumer(formatter, minll, maxll)
, _fname(fname)
, _buffered(buffered)
, _flushLevel(Note)
{
  RString modname = acdk::lang::System::getModuleName();
  int pid = (int)acdk::lang::Process::getProcessId();
  _fname = _fname->replace(acdk::lang::RString("$(PNAME)"), modname)
                 ->replace(acdk::lang::RString("$(PID)"), acdk::lang::String::valueOf(pid));
}


void 
FileConsumer::_openWriter()
{
  if (_buffered == true)
    _writer = new ::acdk::io::PrintWriter((::acdk::io::RWriter)new BufferedWriter(new ::acdk::io::FileWriter(_fname + ".1.log", false)));
  else
    _writer = new ::acdk::io::PrintWriter((::acdk::io::RWriter)new ::acdk::io::FileWriter(_fname + ".1.log", false));
}


//foreign virtual 
void 
FileConsumer::configure(IN(RString) propnameprefix, IN(RProperties) props)
{
  AbstractLogConsumer::configure(propnameprefix, props);
  _fname = props->getProperty(propnameprefix + ".filename");
  RString appendstr = props->getProperty(propnameprefix + ".append");
  bool append = true;
  if (appendstr == Nil || Boolean::getBoolean(appendstr) == true)
    append  = true;
  else
    append = false;
  _writer = new ::acdk::io::CharToByteWriter(new ::acdk::io::FileWriter(_fname, append));
}


} // namespace logging
} // namespace util
} // namespace acdk


