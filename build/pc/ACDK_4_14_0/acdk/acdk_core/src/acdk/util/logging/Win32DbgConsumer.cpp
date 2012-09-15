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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/Win32DbgConsumer.cpp,v 1.5 2005/02/05 10:45:08 kommer Exp $


#include "Win32DbgConsumer.h"

namespace acdk {
namespace util {
namespace logging {


void 
Win32DbgFormatter::format(INP(RLogRecord) rec, INP(acdk::io::RCharWriter) out)
{
  char b[2]; b[1] = 0;
  out->writeString(SBSTR(rec->file << "(" << rec->line << ") : "));
  b[0] = Level::toChar(rec->level);
  out->writeString(b);
  out->writeString(": ");
  out->writeString(rec->message);
  if (rec->namedParameters != Nil) 
  {
    out->writeString(": ");
    for (int i = 0; i < rec->namedParameters->length(); ++i)
    {
      acdk::util::logging::RNamedParameter np = rec->namedParameters[i];
      out->writeString(np->name);
      out->writeString("=[");
      RString str = np->value.toCode();
      out->writeString(str);
      out->writeString("]; ");
    }
  }
  out->writeString("\n");
}

} // namespace logging
} // namespace util
} // namespace acdk


