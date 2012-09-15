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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/StdFormatter.cpp,v 1.12 2005/02/05 10:45:07 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Integer.h>

#include "StdFormatter.h"

namespace acdk {
namespace util {
namespace logging {

void 
StdFormatter::format(IN(RLogRecord) rec, IN(RCharWriter) out)
{
  out->writeString(SysDate(rec->timeStamp).toString());
  out->writeString("|");
  byte lc = (byte)Level::toChar(rec->level);
  out->writeChar((char)lc);
  out->writeString("|");
  out->writeString(acdk::lang::Integer::toString(rec->processId));
  out->writeString("|");
  out->writeString(acdk::lang::Integer::toString(rec->threadId));
  out->writeString("|");
  
  if (rec->file != Nil)
  {
    out->writeString(rec->file);
    out->writeString(":");
    out->writeString(acdk::lang::Integer::toString(rec->line));
  } 

  if (rec->message != Nil)
  {
    out->writeString("|");
    out->writeString(rec->message);
  }
  if (rec->namedParameters != Nil) 
  {
    out->writeString(": ");
    for (int i = 0; i < rec->namedParameters->length(); ++i)
    {
      out->writeString(rec->namedParameters[i]->name);
      out->writeString("=[");
      RString str = rec->namedParameters[i]->value.toString();
      out->writeString(str);
      out->writeString("]; ");
    }
  }
  out->writeString("\n");
}


} // namespace logging
} // namespace util
} // namespace acdk


