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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/Win32DbgConsumer.h,v 1.7 2005/04/09 19:26:59 kommer Exp $

#ifndef acdk_util_logging_Win32DbgConsumer_h
#define acdk_util_logging_Win32DbgConsumer_h

#include "WriterConsumer.h"
#include <acdk/io/PrintWriter.h>
#include <acdk/io/OutputDebugStringWriter.h>

namespace acdk {
namespace util {
namespace logging {

ACDK_DECL_CLASS(Win32DbgFormatter);

/**
  This format for a log entry int Microsoft Studio compativle format.
  Double clicking on a logentry shown in the outputwindow of Visual Studio
  jump to the source location in the editor.
  @see Win32DbgConsumer
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC Win32DbgFormatter
: extends acdk::lang::Object
, implements Formatter
{
  ACDK_WITH_METAINFO(Win32DbgFormatter)
public:
  Win32DbgFormatter()
  {
  }
  RString format(INP(RLogRecord) rec)
  {
    return Formatter::format(rec);
  }
  void format(INP(RLogRecord) rec, INP(RCharWriter) out);
#if !defined(ACDK_WS)
  foreign virtual void configure(INP(RString) propnameprefix, INP(::acdk::util::RProperties) props)
  {
    // has no options
  }
#endif
};


ACDK_DECL_CLASS(Win32DbgConsumer);
/**
  different to the name this consumer can also be used
  in other than win32 platforms.
  On Windows platforms this consumer writes to DebugOut, which is helpfull
  if debugging in an IDE like Visual Studio.
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC Win32DbgConsumer
: extends WriterConsumer
{
  ACDK_WITH_METAINFO(Win32DbgConsumer)
public:
  /**
    if formatter is Nil uses the Win32DbgFormatter
  */
  Win32DbgConsumer(INP(RFormatter) formatter = Nil, int minll = AllSys, int maxll = None)
    : WriterConsumer(new acdk::io::OutputDebugStringWriter(), formatter, minll, maxll)
  {
    if (formatter == Nil)
      setFormatter(new Win32DbgFormatter());
  }
  //foreign virtual void configure(INP(RString) propnameprefix, INP(RProperties) props) {}
};

} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_Win32DbgConsumer_h
