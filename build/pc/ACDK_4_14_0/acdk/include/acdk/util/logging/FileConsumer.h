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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/FileConsumer.h,v 1.11 2005/04/09 19:26:59 kommer Exp $

#ifndef acdk_util_logging_FileConsumer_h
#define acdk_util_logging_FileConsumer_h

#include "AbstractLogConsumer.h"
#include <acdk/lang/System.h>
#include <acdk/io/FileWriter.h>

namespace acdk {
namespace util {
namespace logging {


ACDK_DECL_CLASS(FileConsumer);


/**
  Writes LogRecord into a file
  @see acdk::util::logging::RollingFileConsumer
  @author Roger Rene Kommer
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC FileConsumer
: extends AbstractLogConsumer
{
  ACDK_WITH_METAINFO(FileConsumer)
protected:
  RString _fname;
  ::acdk::io::RCharWriter _writer;
  bool _buffered;
  int _flushLevel;
public:
  FileConsumer(INP(RFormatter) formatter = Nil, int minll = AllSys, int maxll = None, bool buffered = false)
  : AbstractLogConsumer(formatter, minll, maxll)
  , _buffered(buffered)
  , _flushLevel(Note)
  {
  }
  /**
    @param fname the base file name. 
          "$(PNAME)" will be replaced by executalle name
          "$(PID)" will be replaced by process id

      
  */
  FileConsumer(INP(RString) fname, INP(RFormatter) formatter = Nil, int minll = All, int maxll = None, bool buffered = false);
  foreign void publish(INP(RLogRecord) rec) 
  {
    if (_buffered == true && (rec->level == TransCommit || rec->level >= _flushLevel))
      _writer->flush();
    if (wantPublish(rec) == false) 
      return;
    SYNCTHIS();
    _ensureWriter();
    getFormatter()->format(rec, &_writer);
  }
  void _ensureWriter() 
  {
    if (_writer != Nil)
      return;
    _openWriter();
  }
  foreign virtual void configure(IN(RString) propnameprefix, IN(RProperties) props);
   void _openWriter();
  bool isBuffered() const { return _buffered; }
  void setBuffered(bool buffered) { _buffered = buffered; }
  int getFlushLevel() const { return _flushLevel; }
  void setFlushLevel(int level)  { _flushLevel = level; }
  //foreign virtual void configure(INP(RString) propnameprefix, INP(RProperties) props);
};

} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_FileConsumer_h
