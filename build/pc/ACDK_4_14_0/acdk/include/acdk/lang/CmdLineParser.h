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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/CmdLineParser.h,v 1.15 2005/02/11 10:12:00 kommer Exp $

#ifndef acdk_lang_CmdLineParser_h
#define acdk_lang_CmdLineParser_h

#include <acdk/lang/CmdLineOption.h>
#include <acdk/util/HashMap.h>
#include <acdk/util/Properties.h>

namespace acdk {
namespace lang {


ACDK_DECL_CLASS(CmdLineParser);

/**
  CmdLineParser parses a Commandline (RStringArray)
  and returns the options.

  @author Roger Rene Kommer
  @version $Revision: 1.15 $
  @date $Date: 2005/02/11 10:12:00 $
*/
class ACDK_CORE_PUBLIC CmdLineParser
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(CmdLineParser)
public:
  ::acdk::util::RHashMap _map;
  RString _helpHeader;
  RString _helpDetail;
  CmdLineParser();
  /**
    Add an Option to this parser
  */
  void addOption(IN(RCmdLineOption) opt);
  /**
    Add an Option to this parser
    @param name of command line option 
    @param alias used as key to store in Properties
    @param expectArg next command line string will used as value in the Property
    @param help print on screen if parsing fails
  */
  void addOption(IN(RString) option, IN(RString) alias, bool expectArg, IN(RString) help, bool required = false)
  {
    addOption(new CmdLineOption(option, alias, expectArg, help, required));
  }
  void addOption(IN(RString) option, IN(RString) shortoption, IN(RString) alias, bool expectArg, IN(RString) help, bool required = false)
  {
    addOption(new CmdLineOption(option, shortoption, alias, expectArg, help, required));
  }
  /**
    dumps out the current configuration set 
  */
  void printHelp(IN(::acdk::io::RPrintWriter) out);
  /**
    parsed the commandline
    @param args the arguments. The first argumnet (at index 0) will be skipped, because 
           there is normally the name of the executable.
    @param ignoreUnknown ignore unkown option, otherwise throw CmdLineParseException
    @param stripDetected strip away known and parsed options 
          Only evaluated if ignoreUnknown is also been set.
    @return the parsed option
  */
  ::acdk::util::RProperties parse(IN(RStringArray) args, 
                                      bool ignoreUnknown = false,
                                      bool stripDetected = false);
  /**
    stores the properties in the given props
  */
  ::acdk::util::RProperties parse(IN(::acdk::util::RProperties) props,
                                      IN(RStringArray) args, 
                                      bool ignoreUnknown = false,
                                      bool stripDetected = false);
};



} // lang
} // acdk

#endif //acdk_lang_CmdLineParser_h


