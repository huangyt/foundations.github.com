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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/Formatter.h,v 1.12 2005/02/05 10:45:07 kommer Exp $

#ifndef acdk_util_logging_Formatter_h
#define acdk_util_logging_Formatter_h

#include <acdk.h>
#include <acdk/io/StringWriter.h>
#include <acdk/util/Properties.h>
#include "LogRecord.h"

namespace acdk {
namespace util {
namespace logging {

ACDK_DECL_INTERFACE(Formatter);

/**
  same role as java.util.logging.Handler
  or org.apache.log4j.Appender

  You have to implement at least one of the format(...)
  methods.
*/
class ACDK_CORE_PUBLIC Formatter
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Formatter)
public:
  /**
    convert the LogRecord into a readable string
  */
  virtual RString format(IN(RLogRecord) rec) 
  {
    ::acdk::io::StringWriter cout; 
     format(rec, &cout);
     return cout.getString();
  }
  /**
    writes the LogRecord to the writer
  */
  virtual void format(IN(RLogRecord) rec, IN(::acdk::io::RCharWriter) out) 
  {
    RString str = format(rec);
    out->writeString(str);
  }
  virtual RString getHeader()
  {
    return "";
  }
  virtual RString getFooter()
  {
    return "";
  }
  
  /**
    configure with given properties using the prefix for key names
    To access a custom configure value 'foo' just read it with the 
    the prefix propnameprefix + "foo"
  */
  virtual void configure(IN(RString) propnameprefix, IN(::acdk::util::RProperties) props) = 0;
};


} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_Formatter_h
