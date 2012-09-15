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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/MsgBoxConsumer.h,v 1.8 2005/03/11 14:25:43 kommer Exp $

#ifndef acdk_util_logging_MsgBoxConsumer_h
#define acdk_util_logging_MsgBoxConsumer_h

#include "AbstractLogConsumer.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace util {
namespace logging {


ACDK_DECL_CLASS(MsgBoxConsumer);

/**
  The message will displayed in a Message Dialog Box

  @author Roger Rene Kommer
*/
class ACDK_CORE_PUBLIC MsgBoxConsumer
: extends AbstractLogConsumer
{
  ACDK_WITH_METAINFO(MsgBoxConsumer)
public:
  MsgBoxConsumer(INP(RFormatter) formatter = Nil, int minll = AllSys, int maxll = None)
    : AbstractLogConsumer(formatter, minll, maxll)
  {
  }
  foreign void publish(INP(RLogRecord) rec) 
  {
    if (wantPublish(rec) == false) 
      return;
    acdk::io::StringWriter sout;
    acdk::io::PrintWriter pout((acdk::io::RCharWriter)&sout);
    getFormatter()->format(rec, &pout);
    acdk::lang::RString category = rec->catName;
#if defined(ACDK_OS_WIN32)
    MessageBox(NULL, ACDK_API_CONSTCHARPTR(sout.getStringBuffer()->toString()->convertToNative()->native_c_str()), 
                     ACDK_API_CONSTCHARPTR(category->convertToNative()->native_c_str()), MB_OK);
#endif
  }
};

} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_MsgBoxConsumer_h
