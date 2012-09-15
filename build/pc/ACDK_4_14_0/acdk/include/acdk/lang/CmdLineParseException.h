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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/CmdLineParseException.h,v 1.9 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_CmdLineParseException_h
#define acdk_lang_CmdLineParseException_h

#include <acdk/lang/RuntimeException.h>
#include <acdk/lang/CmdLineParser.h>

namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(CmdLineParseException, RuntimeException);

/**
  Thrown by the CmdLineParser if a command line cannot be parsed.
  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:48 $
  @see CmdLineParser
*/
class ACDK_CORE_PUBLIC CmdLineParseException
: extends ::acdk::lang::RuntimeException
{
  ACDK_WITH_METAINFO(CmdLineParseException)
private:
  RStringArray _cmdline;
  RCmdLineParser _parser;
public:
  CmdLineParseException(IN(RString) msg, IN(RStringArray) cmdlines, IN(RCmdLineParser) cparser)
  : RuntimeException(msg)
  , _cmdline(cmdlines)
  , _parser(cparser)
  {
  }
  foreign RString getMessage();
};

} // lang
} // acdk

#endif //acdk_lang_CmdLineParseException_h


