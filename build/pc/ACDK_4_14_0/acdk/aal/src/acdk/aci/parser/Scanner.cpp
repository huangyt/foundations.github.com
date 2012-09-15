// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 



#include "Scanner.h"
#include <acdk/io/MemWriter.h>
#include <acdk/lang/Character.h>
#include <acdk/util/logging/Log.h>
#include "../parser/TerminalParseNode.h"
#include "../ast/Terminal.h"
#include "../ast/EofTerminal.h"

namespace acdk {
namespace aci {
namespace parser {

int dout_indent = 0;

const int Scanner::TT_EOF =                     -2;
const int Scanner::TT_EOL =                     -3;
const int Scanner::TT_WS =                      -7;
const int Scanner::TT_MINRESERVED =           -100; // ### @todo not used
const int Scanner::TT_MAXRESERVED =           +1000; // ### @todo not used

/*
bool 
ScannerTerminal::isWs() 
{ 
  return tokenIndex == Scanner::TT_WS; 
}
*/

/*
const int Scanner::TT_NUMBER =                  -2;
const int Scanner::TT_WORD =                    -3;
const int Scanner::TT_STRING =                  -4;
const int Scanner::TT_QCHAR =                   -8;
const int Scanner::TT_CCOMMENT =                -5;
const int Scanner::TT_CXXCOMMENT =              -6;

*/

Scanner::Scanner(IN(acdk::io::RReader) in, int flags)
: _flags(flags)
, _top(-1)
, _bufferTop(0)
, _stack(0)
, _parseEnv(new ParseEnv())
//, _sourceName(in->getReaderStorage()->getDeviceName())
{
  ::acdk::io::MemWriter memwriter;
  in->trans(&memwriter);
  _inBuffer = new String(memwriter.getBuffer());
  _init();
}



Scanner::Scanner(IN(RString) text, int flags)
: _flags(flags)
,  _top(-1)
, _bufferTop(0)
, _inBuffer(text)
//, _terminals(new TerminalSetType())
, _stack(0)
, _parseEnv(new ParseEnv())
{
  //_codeLocation.sourceName = "<anon buffer>";
  _init();
}

void 
Scanner::_init()
{
  
}


int 
Scanner::registerTerminal(IN(RTerminalParseNode) st)
{
  //addTerminal->add(st->getNodeName(), st);
  if (st->getScannerTokenId() == -1)
    st->setScannerTokenId(Scanner::TT_MINRESERVED - 1 - _parseEnv->_currentParseFrame->_terminalSet->size());
  
  ACDK_NLOGP("acdk.aci.Scanner", Debug300, 
            "Scanner::registerTerminal:",
               LOG_NPV("TokenIndex", st->getScannerTokenId()) <<
               LOG_NPV("TokenName", st->toString()) <<
               LOG_NPV("prio", st->getScannerPrio())
            );
  return st->getScannerTokenId();
}


#define MKSTKSTR(it, end) RString(String(it, end))


acdk::aci::ast::RTerminal 
Scanner::peek(int pos)
{
  int notreaded = (_stack.length() - _top) + pos;
  int rpos = _top;
  int sicpos = _top;
  for (int i = 0; i <= notreaded; ++i)
    rpos = _readNext();
  _top = sicpos;
  return &_stack[rpos];
}



int
Scanner::_readNext()
{
  bool loop = false;
  int ctop = _top;
  do {
scan_next:
    ++ctop;
    if (_stack.length() - 1 < ctop)
      _scanNext();
    RTerminal t = _stack[ctop];
    int tk = t->getScannerTokenId();
    if (tk == TT_EOF)
      return  _top = ctop;
    if (_parseEnv->isIgnoreToken(tk) == true)
      goto scan_next;
    if ((_flags & ScanWs) && ((_flags & WantWs) == false) && _stack[ctop]->isWhiteSpace())
      goto scan_next;

  } while (false);
  //setCodeLocation(*_stack[ctop]->getCodeLocation());
  
  return _top = ctop;
}

acdk::aci::util::RCodeLocation 
Scanner::getClonedCodeLocation()
{
  //return new acdk::aci::util::CodeLocation(*getCodeLocation());
  return new acdk::aci::util::CodeLocation(_codeLocation);
}

acdk::aci::util::RCodeLocation 
Scanner::getCodeLocation()
{
  if (_top == -1)
    return new acdk::aci::util::CodeLocation();
  return _stack[_top]->getCodeLocation();
}

int 
Scanner::_scanNext()
{
  // ### @todo implement
  TerminalSetType::RIteratorType it = _parseEnv->_currentParseFrame->_terminalSet->iterator();
  RString ctext = _inBuffer->substr(_bufferTop);
  if (ctext->length() == 0)
  {
    _bufferTop = getCodeLocation()->getEndCharPos();
    _stack.append(new EofTerminal(getClonedCodeLocation(), TT_EOF));
    return _stack.length() - 1;
  }
  RTerminal lastterminal;

  while (it->hasNext() == true)
  {
    RTerminalParseNode st = it->next();
    RTerminal ct = st->scanNextFromSource(ctext, getClonedCodeLocation());
    
    if (ct != Nil)
    {
      if (lastterminal != Nil)
      {
        if ((st->getScannerPrio() > lastterminal->getScannerPrio() || 
                                 (st->getScannerPrio() == lastterminal->getScannerPrio() && 
                                  lastterminal->getCodeLocation()->getEndCharPos() <= ct->getCodeLocation()->getEndCharPos())))
        {
          lastterminal = ct;
          continue;
        }
      }
      else
        lastterminal = ct;
    }
  }
  if (lastterminal == Nil)
  {
    StringBuffer sb;
    sb << "Cannot find matching terminal: '" << RString(new String(_inBuffer->begin() + _bufferTop, _inBuffer->end())) << "'";
    THROW1(Exception, sb.toString());
  }
  RString erg = new String(_inBuffer->begin() + _bufferTop, _inBuffer->begin() + _bufferTop + lastterminal->getCodeLocation()->getEndCharPos());
  int nlcount = erg->elementCount('\n');
  if (nlcount > 0)
  {
    _codeLocation.setLinePos(_codeLocation.getLinePos() + nlcount);
    RString lline = erg->substr(erg->lastIndexOf('\n') + 1);
    _codeLocation.setColumnPos(lline->length());
  }
  else
  {
    _codeLocation.setColumnPos(_codeLocation.getColumnPos() + erg->length());
  }
  _bufferTop = lastterminal->getCodeLocation()->getEndCharPos();
  _stack.append(lastterminal);
  //int ret = _safeToken(lastterminal);
  _codeLocation.setCharPos(_bufferTop);
  return _top = _stack.length() - 1;
}

void 
Scanner::resetTokenIdx(int tkidx)
{
  _top = tkidx;
  if (_top == -1)
    return;
  /*ACDK_NLOGP("acdk.aci.Scanner", Debug, "Scanner::resetTokenIdx", 
    LOG_NPV("top", SBSTR("t:" << _top << ";cl: " << _stack[tkidx]->getCodeLocation()->toString())));
    */
}




acdk::aci::ast::RTerminal 
Scanner::getNext()
{
  _readNext();
  acdk::aci::ast::RTerminal term = _stack[_top];
  ACDK_NLOGP("acdk.aci.Scanner", Debug, "Scanner::getNext", 
    LOG_NPV("top", SBSTR("t:" << _top << ";cl: " << term->getCodeLocation()->toString())) << LOG_NPV("Value", term->toString()));
  return _stack[_top];
}

acdk::aci::ast::RTerminal 
ScannerTokenStack::peek(int pos)
{
  acdk::aci::ast::RTerminal st = _scanner->peek(pos);
  /**
  if (ignoreTk(st->getScannerTokenId()) == false)
    return st;
    */
  ++pos;
  do {
    st = _scanner->peek(pos);
    ++pos;
  } while (ignoreTk(st->getScannerTokenId()) == true && st->getScannerTokenId() != Scanner::TT_EOF);
  return st;
}

acdk::aci::ast::RTerminal 
ScannerTokenStack::top()
{
  return peek(0);
}


void 
Scanner::resetPreparsedTerminals()
{
  while (_stack.length() > 0)
  {
    if (_stack[_stack.length() - 1]->getCodeLocation()->getCharPos() < _bufferTop - 1)
      break;
    _stack.remove(_stack.length() - 1);
  }
  _top = _stack.length() - 1;
  // #### @todo set _codeLocation
}


RScanner 
Scanner::createSubTextScanner(IN(RString) text)
{
  RScanner scanner = new Scanner(text);
  scanner->_parseEnv = _parseEnv;
  scanner->_flags = _flags;
  return scanner;
}



} // parser
} // aci
} // acdk



