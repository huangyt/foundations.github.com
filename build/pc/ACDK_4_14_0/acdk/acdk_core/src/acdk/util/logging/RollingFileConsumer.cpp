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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/RollingFileConsumer.cpp,v 1.4 2005/02/05 10:45:07 kommer Exp $


#include "RollingFileConsumer.h"
#include <acdk/io/File.h>
#include <acdk/io/BufferedWriter.h>
#include <acdk/io/CharToByteWriter.h>

namespace acdk {
namespace util {
namespace logging {

  
void 
RollingFileConsumer::rollOver()
{
  RFormatter formatter = getFormatter();
  
  if (_writer != Nil)
  {
    _writer->writeString(formatter->getFooter());
    _writer->close();
    _writer = Nil;
  }
  if (_maxBackupIndex > 0)
  {
    for (int i = _maxBackupIndex; i > 1; --i)
    {
      RString oldname = SBSTR(_fname << "." << i - 1 << ".log");
      RString newname = SBSTR(_fname << "." << i << ".log");
      acdk::io::File oldfile(oldname);
      if (oldfile.exists() == true)
      {
        oldfile.renameTo(SR_FQ(acdk::io::, File, newname));
      }
    }
  }
  _curRecordNum = 0;
  if (_buffered == true)
    _writer = new ::acdk::io::CharToByteWriter(new acdk::io::BufferedWriter(new ::acdk::io::FileWriter(_fname + ".1.log", false)));
  else
    _writer = new ::acdk::io::CharToByteWriter(new ::acdk::io::FileWriter(_fname + ".1.log", false));
  _writer->writeString(formatter->getHeader());
}


//foreign virtual 
void 
RollingFileConsumer::configure(IN(RString) propnameprefix, IN(RProperties) props)
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


