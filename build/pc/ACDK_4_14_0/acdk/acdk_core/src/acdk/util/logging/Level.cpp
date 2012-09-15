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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/Level.cpp,v 1.14 2005/02/05 10:45:07 kommer Exp $


#include "Level.h"
#include <acdk/lang/Integer.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/NumberFormatException.h>
#include <acdk/lang/IllegalArgumentException.h>

namespace acdk {
namespace util {
namespace logging {

const char* 
Level::toCString(int level)
{
  switch (level)
  {
  case All: return "All";
  case TransBegin : return "TransBegin";
  case TransCommit: return "TransCommit";
  case TransRollback: return "TransRollback";
  case Debug: return "Debug";
  case Trace: return "Trace";
  case Info: return "Info";
  case Warn: return "Warn";
  case Note: return "Note";
  case Error: return "Error";
  case Fatal: return "Fatal";
  case None: return "None";
  case SysDebug: return "SysDebug";
  default:
    if (level < Debug)
      return "SysDebug";
    if (level < Trace)
      return "Debug";
    if (level < Info)
      return "Trace";
    if (level < Note)
      return "Info";
    if (level < Warn)
      return "Note";

    if (level < Error)
      return "Warn";
    if (level < Fatal)
      return "Error";
    if (level < None)
      return "Fatal";
    return "None";
  }
}

char 
Level::toChar(int level)
{
  switch (level)
  {
  case SysDebug: return 's';
  case All: return 'A';
  case TransBegin: return '[';
  case TransCommit: return ']';
  case TransRollback: return '~';
  case Debug: return 'D';
  case Trace: return 'T';
  case Info: return 'I';
  case Note: return 'N';
  case Warn: return 'W';
  case Error: return 'E';
  case Fatal: return 'F';
  case None: return 'X';
  default:
    if (level < Debug)
      return 'A';
    if (level < Trace)
      return 'D';
    if (level < Info)
      return 'T';
    if (level < Note)
      return 'I';
    if (level < Warn)
      return 'N';
    if (level < Error)
      return 'W';
    if (level < Fatal)
      return 'E';
    if (level < None)
      return 'F';
    return 'N';
  }
}

//static 
int 
Level::parseLevel(IN(RString) str, bool throwExOnUnknown)
{
  if (Character::isDigit(str->charAt(0)) == true)
  {
    try {
      return ::acdk::lang::Integer::parseInt(str);
    } catch (RNumberFormatException ex) {
    }
  }
  if (str->equalsIgnoreCase("All") == true)
    return All;
  else if (str->equalsIgnoreCase("SysDebug") == true)
    return SysDebug;
  else if (str->equalsIgnoreCase("AllSys") == true)
    return AllSys;
  else if (str->equalsIgnoreCase("Debug") == true)
    return Debug;
  else if (str->equalsIgnoreCase("Trace") == true)
    return Trace;
  else if (str->equalsIgnoreCase("Info") == true)
    return Info;
  else if (str->equalsIgnoreCase("Warn") == true)
    return Warn;
  else if (str->equalsIgnoreCase("Note") == true)
    return Note;
  else if (str->equalsIgnoreCase("Error") == true)
    return Error;
  else if (str->equalsIgnoreCase("Fatal") == true)
    return Fatal;
  else if (str->equalsIgnoreCase("None") == true)
    return None;
  if (throwExOnUnknown)
    THROW1(IllegalArgumentException, "Invalid Level: " + str);
  return None;
}

} // namespace logging
} // namespace util
} // namespace acdk

