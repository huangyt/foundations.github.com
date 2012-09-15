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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/SimpleFormatter.cpp,v 1.10 2005/02/05 10:45:07 kommer Exp $


#include "SimpleFormatter.h"

namespace acdk {
namespace util {
namespace logging {

void 
SimpleFormatter::format(IN(RLogRecord) rec, IN(RCharWriter) out)
{
  char b[2]; b[1] = 0;
  b[0] = Level::toChar(rec->level);
  out->writeString(b);
  out->writeString(": ");
  out->writeString(rec->message);
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


