// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 


#include "StringTerminalParseNode.h"
#include "../ast/Literal.h"

namespace acdk {
namespace aci {
namespace parser {

RString
StringTerminalParseNode::scanStringText(String::iterator& it, String::iterator end)
{
  StringBuffer sb;
  if (*it == '\"')
    ++it;
  else
    ; // ### @todo throw unknown encoding

  while (it < end)
  {
    if (*it == '\\')
    {
      ++it;
      if (!(it < end))
        ; // ### @todo throw
      switch (*it)
      {
      case '\\': sb.append("\\"); break;
      case 'r': sb.append("\r"); break;
      case 'n': sb.append("\n"); break;
      case 't': sb.append("\t"); break;
      case 'b': sb.append("\b"); break;
      case '"': sb.append("\""); break;
      default: sb.append(*it); break;
      }
    }
    else
    {
      if (*it == '"')
      {
        ++it;
        break;
      }
      sb.append(*it);
    }
    ++it;
  }
  return sb.toString();
}

RTerminal 
StringTerminalParseNode::scanNextFromSource(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cl)
{
  if (input->length() < 2)
    return Nil;

  if (input->charAt(0) != '\"') // handle also encoding prefixes
    return Nil;
  String::iterator it = input->begin();
  String::iterator end = input->end();
  RString s = scanStringText(it, end);
  cl->setEndCharPos(cl->getCharPos() + (it - input->begin()));
  return new acdk::aci::ast::Literal(this, cl, inOf(s));
}

} // parser
} // aci
} // acdk



