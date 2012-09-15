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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/WriterConsumer.h,v 1.5 2005/04/09 19:26:59 kommer Exp $

#ifndef acdk_util_logging_WriterConsumer_h
#define acdk_util_logging_WriterConsumer_h

#include "AbstractLogConsumer.h"


#include <acdk/io/Writer.h>

namespace acdk {
namespace util {
namespace logging {


ACDK_DECL_CLASS(WriterConsumer);
/**
  pass logs to a given writer in the standard encoding
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC WriterConsumer
: extends AbstractLogConsumer
{
  ACDK_WITH_METAINFO(WriterConsumer)
protected:
  ::acdk::io::RPrintWriter _writer;
public:
  WriterConsumer(INP(acdk::io::RWriter) out, INP(RFormatter) formatter = Nil, int minll = AllSys, int maxll = None)
  : AbstractLogConsumer(formatter, minll, maxll)
  , _writer(new acdk::io::PrintWriter(out))
  {
  }
  foreign void publish(INP(RLogRecord) rec) 
  {
    if (wantPublish(rec) == false) 
      return;
    SYNCTHIS();
    getFormatter()->format(rec, &_writer);
  }
  //foreign virtual void configure(INP(RString) propnameprefix, INP(RProperties) props);
};

} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_WriterConsumer_h
