// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ide/inspector/CfgScriptConsole.cpp,v 1.4 2005/04/13 15:38:10 kommer Exp $


#include "CfgScriptConsole.h"
#include <acdk/lang/System.h>
#include <acdk/util/Arrays.h>
#include <acdk/util/logging/Log.h>
#include <acdk/wx/TextCtrlCharWriter.h>

namespace acdk {
namespace wx {
namespace ide {
namespace inspector {


enum ConsoleStates
{
  EvalCommand,
  EvalScript,
  PrintOutput,
};


RString getHelp()
{
  StringBuffer sb;
  sb <<   "\ncommands:\n"
      "  h        print this help\n"
      "  e|eval   evaluate CfgScript until single line with '.'\n"
      "  c        clear console\n"
      "  gc       start garbage collector\n"
      "  tl <LL>  Set the Threshold LogLevel (LL: SysDebug, Debug, Trace,\n"
      "           Info, Note, Warn, Error, Fatal, None or number)\n"
      ;
  return sb.toString();
}

CfgScriptConsole::CfgScriptConsole(IN(RWindow) parent, int id, IN(RPoint) point, IN(RSize) size)
: TextCtrl(parent, id, "", point, size, TeLeft | TeMultiline /* | TeProcessEnter | TeProcessTab */)
, _consoleState(EvalCommand)
, _scriptBeginPos(-1)
{
  ACDK_SAFE_CONSTRUCTOR();
  connect(CommandEvent::EvtCommandTextUpdated, id, (ObjectEventFunction)&CfgScriptConsole::onTextUpdated);
  try {
    _script = (RObject)New("acdk.cfgscript.Script", inOf(RString("<CfgScriptConsole>")));
    _props = (RObject)New("acdk.cfgscript.Props");
    //_script->invoke("getGlobals")
    _props->invoke("setObjectVal", inOf(RString("out")), inOf(new acdk::io::PrintWriter(new TextCtrlCharWriter(this))));
  } catch (RThrowable ex) {
    printText("Cannot load CfgScript interpreter: " + ex->getMessage());
  }
  printText(getHelp());
  _prompt();
}

void 
CfgScriptConsole::printText(IN(RString) text)
{
  int lastState = _consoleState;
  _consoleState = PrintOutput;
  appendText(text);
  _consoleState = lastState;
}


void 
CfgScriptConsole::_processCommand(IN(RString) text, IN(RString) line)
{
  if (line->equals("eval") == true || line->equals("e") == true)
  {
    _consoleState = EvalScript;
    _scriptBeginPos = text->length();
  }
  else if (line->equals("gc") == true)
  {
    callGc(true);
  }
  else if (line->equals("h") == true)
  {
    printText(getHelp());
  }
  else if (line->equals("c") == true)
  {
    setValue("");
    printText(getHelp());
    //_prompt();
  }
  else if (line->startsWith("tl ") == true)
  {
    RString level = line->substr(4);
    try {
      acdk::util::logging::LogManager::Threshold = acdk::util::logging::Level::parseLevel(level, true);
    } catch(RIllegalArgumentException ex) {
      printText(ex->getMessage() + "\n");
    }
  }
  else
  {
    printText("Unknown command: " + line + 
              getHelp()
              );
  }
}

void 
CfgScriptConsole::onTextUpdated(IN(RCommandEvent) event)
{
  if (_consoleState == PrintOutput)
    return;
  RString text = event->getString();
  if (text->length() == 0)
    return;
  //acdk::lang::System::out->println("CMD: " + text);
  char lchar = text->charAt(text->length() - 1);
  if (lchar == '\n')
  {
    int lines = getNumberOfLines();
    RString line = getLineText(lines - 2);
    //acdk::lang::System::out->println("LINE: " + line);
    if (_consoleState == EvalCommand)
    {
      if (line->startsWith("> ") == true)
        line = line->substr(2);
      if (line->length() > 0)
        _processCommand(text, line);
      if (_consoleState == EvalCommand)
        _prompt();
    }
    else if (_consoleState == EvalScript)
    {
      if (line->equals(".") == true)
      {
        _eval(text->substr(_scriptBeginPos, text->length() - 2));
        _consoleState = EvalCommand;
        _prompt();
      }
    }
  }
}

void 
CfgScriptConsole::_eval(IN(RString) text)
{
  //acdk::lang::System::out->println("EVAL: " + text);
  if (_script == Nil)
  {
    writeText("No script interpreter available");
    return;
  }
  try {
    _script->invoke("eval", inOf(text), inOf(_props));
  } catch (RThrowable ex) {
    printText("Exception: " + ex->getMessage() + "\n");
  }
}

void 
CfgScriptConsole::_prompt()
{
  printText("> ");

}

void 
CfgScriptConsole::callGc(bool all)
{
  acdk::lang::System::gc();
}

} // inspector
} // ide
} // wx
} // acdk

