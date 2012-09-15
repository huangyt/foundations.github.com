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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/CmdLineParser.cpp,v 1.18 2005/02/11 10:12:00 kommer Exp $


#include <acdk/lang/CmdLineParser.h>
#include <acdk/lang/CmdLineParseException.h>

#include <acdk/io/PrintWriter.h>
#include <acdk/io/CharArrayWriter.h>
#include <acdk/util/HashMap.h>
#include <acdk/util/Properties.h>
#include <acdk/lang/System.h>

namespace acdk {
namespace lang {

CmdLineParser::CmdLineParser()
: Object()
, _map(new ::acdk::util::HashMap())
{
}

void 
CmdLineParser::addOption(IN(RCmdLineOption) opt)
{
  _map->put((RObject)opt->_option, (RObject)opt);
  if (opt->_shortOption != Nil && opt->_shortOption->length() > 0)
    _map->put((RObject)opt->_shortOption, (RObject)opt);
}

void 
CmdLineParser::printHelp(IN(::acdk::io::RPrintWriter) out)
{
  if (_helpHeader != Nil)
    out->print(_helpHeader);

  acdk::util::RIterator it = _map->iterator();
  while (it->hasNext() == true) {

    RCmdLineOption opt = RCmdLineOption(::acdk::util::RBucketNode(it->next())->getValue());
    out->print("\t");
    opt->printOn(out);
    out->print("\n");
  }
  if (_helpDetail != Nil)
    out->print(_helpDetail);
  out->flush();
}


::acdk::util::RProperties 
CmdLineParser::parse(IN(RStringArray) args, 
                         bool ignoreUnknown/* = false*/,
                         bool stripDetected/* = false*/)
{
  return parse(new ::acdk::util::Properties(), args, ignoreUnknown, stripDetected);
}

::acdk::util::RProperties 
CmdLineParser::parse(IN(::acdk::util::RProperties) props,
                         IN(RStringArray) args, 
                         bool ignoreUnknown/* = false*/,
                         bool stripDetected/* = false*/)
{
  for (int i = 1; i < args.length(); i++) 
  {
    RString key = args[i];
    int pos = key->indexOf('=');
    RString value;
    if (pos != -1) 
    {
      value = key->substr(pos  + 1);
      key = key->substr(0, pos);
    }
    
    RCmdLineOption opt = (RCmdLineOption)_map->get((RObject)key);

    if (opt == Nil) 
    {
      if (ignoreUnknown == true || (key->startsWith("-") == false && key->startsWith("/") == false)) 
        continue;
      THROW3(CmdLineParseException, "unknown option", args, this);
    }
    
    if (opt->_expectArg == true) 
    {
      if (value != Nil) 
      {
        props->setProperty((opt->_alias != Nil ? opt->_alias : key), value);
      } 
      else 
      {
        if (i + 1 >= args.length()) 
          THROW3(CmdLineParseException, RString("option expexts value: ") + opt->toString(), args, this);
        value = args[i + 1];
        if (stripDetected == true)
          args->remove(i + 1);
        else
          ++i;
        props->setProperty((opt->_alias != Nil ? opt->_alias : key), value);
      }
    } 
    else
      props->setProperty((opt->_alias != Nil ? opt->_alias : key), String::emptyString());

    opt->_parsed = true;
    if (stripDetected == true) 
    {
      args->remove(i);
      i--;
    }
  }
  acdk::util::RIterator it = _map->entrySet()->iterator();
  while (it->hasNext() == true)
  {
    RCmdLineOption opt(acdk::util::RBasicMapEntry(it->next())->getValue());
    if (opt->_required == true && opt->_parsed == false)
    {
      THROW3(CmdLineParseException, RString("option is required: ") + opt->toString(), args, this);
    }
  }
  return props;
}


//virtual 
RString 
CmdLineParseException::getMessage()
{
  acdk::io::RCharArrayWriter cout = new acdk::io::CharArrayWriter();
  acdk::io::RPrintWriter pout = new acdk::io::PrintWriter((acdk::io::RCharWriter)&cout);
  _parser->printHelp(pout);
  return _what + ": command line '" + _cmdline->toString() + "'\n"
            + cout->toString() + "\n" + System::getSystemCmdLineOps();
}

} // lang
} // acdk





