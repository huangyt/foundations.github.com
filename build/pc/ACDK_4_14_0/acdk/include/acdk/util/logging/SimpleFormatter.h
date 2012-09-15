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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/SimpleFormatter.h,v 1.8 2005/04/09 19:26:59 kommer Exp $

#ifndef acdk_util_logging_SimpleFormatter_h
#define acdk_util_logging_SimpleFormatter_h

#include "Formatter.h"

namespace acdk {
namespace util {
namespace logging {

ACDK_DECL_CLASS(SimpleFormatter);

/**
  Very simple format with only "Category: Message" output
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC SimpleFormatter
: extends acdk::lang::Object
, implements Formatter
{
  ACDK_WITH_METAINFO(SimpleFormatter)
public:
  SimpleFormatter()
  {
  }
  RString format(IN(RLogRecord) rec)
  {
    return Formatter::format(rec);
  }
  void format(IN(RLogRecord) rec, IN(RCharWriter) out);
  foreign virtual void configure(IN(RString) propnameprefix, IN(::acdk::util::RProperties) props)
  {
    // has no options
  }
};


} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_SimpleFormatter_h
