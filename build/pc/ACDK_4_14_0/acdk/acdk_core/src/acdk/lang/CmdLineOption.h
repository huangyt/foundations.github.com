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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/CmdLineOption.h,v 1.9 2005/02/05 10:44:55 kommer Exp $

#ifndef acdk_lang_CmdLineOption_h
#define acdk_lang_CmdLineOption_h

#include <acdk.h>

namespace acdk {
namespace lang {


ACDK_DECL_CLASS(CmdLineOption);

  
/**
  CmdLineOption represent a option in a command line
  and returns the options.

  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:44:55 $
  @see CmdLineParser
*/
class ACDK_CORE_PUBLIC CmdLineOption
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(CmdLineOption)
public:
  /**
    the option as string like:
    -v
    -value
  */
  RString _option;
  /**
    like -v
  */
  RString _shortOption;
  /**
    the alias. if this is not Nil, this
    value will be used as key in the Property
  */
  RString _alias;
  /**
    if set, option what to have an value
    -acdk-opt=value
    or
    -acdk-opt value

    if not set, value is "1" if 
    option is set
  */
  bool _expectArg;
  /**
    documents the option
  */
  RString _docText;
  /**
    This options has to be specified
  */
  bool _required;
  /**
    This options is detected
  */
  bool _parsed;
  CmdLineOption(IN(RString) option, IN(RString) alias, bool expectArg, IN(RString) help, bool required = false)
  : Object()
  , _option(option)
  , _alias(alias)
  , _expectArg(expectArg)
  , _docText(help)
  , _required(required)
  , _parsed(false)
  {
  }
  CmdLineOption(IN(RString) option, IN(RString) shortoption, IN(RString) alias, bool expectArg, IN(RString) help, bool required = false)
  : Object()
  , _option(option)
  , _shortOption(shortoption)
  , _alias(alias)
  , _expectArg(expectArg)
  , _docText(help)
  , _required(required)
  , _parsed(false)
  {
  }
  void printOn(IN(::acdk::io::RPrintWriter) out);
};



} // lang
} // acdk

#endif //acdk_lang_CmdLineOption_h

