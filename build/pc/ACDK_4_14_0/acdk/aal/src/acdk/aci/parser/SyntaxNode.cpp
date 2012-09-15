// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#include "SyntaxParseNode.h"
#include "../Compiler.h"
#include "../ast/AstNode.h"
#include "../ast/Keyword.h"

#include "KeywordParseNode.h"
#include "StringTerminalParseNode.h"

#include <acdk/lang/Character.h>
#include <acdk/lang/System.h>
#include <acdk/io/StreamTokenizer.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace aci {
namespace parser {


namespace {

enum Token
{
  Eof           = -1,
  Identifier    = -2,
  Constant      = -3,
  RegExpToken   = -4,
  EvalBlock     = -5,
  StringLit     = -6,
  
  Or          = '|',
  OptStart    = '[',
  OptEnd      = ']',
  GroupStart  = '(',
  GroupEnd    = ')',
  ZeroOrMore  = '*',
  OneOrMore   = '+',
  GrammarCommit = '!',
  SaveRule      = '$',
  HideSubRule   = '&',
  SaveRuleSub   = '%',
  EoStatement   = ';', // ';'
  Colon         = ':', // ':'
  Dot           = '.'
  
};

struct SyntaxScanner
{
  RString _syntax;
  String::iterator* it;
  String::iterator end;
  String::iterator sit;
  String::iterator send;
  RString strLit;
  int curToken;
  SyntaxScanner(IN(RString) syntax, String::iterator* it_, String::iterator end_)
  : _syntax(syntax)
  , it(it_)
  , end(end_)
  , sit(end_)
  , send(end_)
  , curToken(Token(-1))
  {
  }
  RString curTokStr() 
  { 
    if (curToken == StringLit || curToken == Constant)
      return strLit;
    return new String(sit, send); 
  }
  int nextToken();
  RString scanFqIdentifier();
};

int 
SyntaxScanner::nextToken()
{
  /*
  enum State
  {
    ScanStart      = 0x0,
    ScanIdentifier = 0x1,
    ScanConstant   = 0x2,
    ScanRegExp     = 0x3,
    ScanEvalText   = 0x4
  };*/
  //State state = ScanStart;
  //String::iterator begin = *it;
  char tcht;
  while (*it < end)
  {
    tcht = (char)**it;
    switch (**it)
    {
    case '/': // ### @todo handle regexp
    {
      tcht = *((*it) + 1);
      if (tcht == '*')
      {
        ++(*it);++(*it);
        while (*it < end)
        {
          if (**it == '*' && *((*it) + 1) == '/')
          {
            ++(*it);
            goto nextLoop;
          }
          ++(*it);
        }
        // ### @todo ex
      }
      else if (tcht == '/')
      {
        ++(*it);++(*it);
        while (*it < end)
        {
          if (**it == '\n')
          {
            goto nextLoop;
          }
          ++(*it);
        }
      }
      else
        ; //no break
    }
    case ' ':
    case '\n':
    case '\t':
    case '\r':
      break;
    case '|':
    case '[':
    case ']':
    case '(':
    case ')':
    case '*':
    case '+':
    case '!':
    case '$':
    case '&':
    case '%':
    case ';':
    case ':':
    case '.':
      sit = *it;
      curToken = **it;
      ++(*it);
      send = *it;
      return curToken; 
    case '{':
    {
      ++(*it);
      String::iterator ssit = *it;
      int tk = 0;
      int tkc = 0;
      while ((tk = nextToken()) != Eof)
      {
        if (tk == '{')
          ++tkc;

        if (tk == '}')
        {
          if (--tkc > 0)
            continue;
          sit = ssit;
          send = *it;
          --send;
          return curToken = EvalBlock;
        }
      }
      ; // ### @todo throw ex
      return curToken = EvalBlock;
      break;
    }
    
      
    case '"':
    case '\'':
    {
      int termtk = **it;
      StringBuffer sb;
      ++(*it);
      while (*it < end)
      {
        if (**it == '\\')
        {
          ++(*it);
          sb.append((char)acdk::io::StreamTokenizer::mapCEscapedChar(**it));
        }
        if (**it == termtk)
        {
          ++(*it);
          strLit = sb.toString();
          if (termtk == '\'')
            return curToken = Constant;
          else
            return curToken = StringLit;
        }
        sb.append(**it);
        ++(*it);
      }
      // ### @todo throw ex
      break;
    }
    default:
      if (acdk::lang::Character::isWhitespace(**it) == true)
        break;
      if (acdk::lang::Character::isUnicodeIdentifierStart(**it) == true)
      {
        sit = *it;
        while (*it < end)
        {
          if (acdk::lang::Character::isUnicodeIdentifierPart(**it) == false)
            break;
          ++(*it);
        }
        send = *it;
        return curToken = Identifier;
      }
      curToken = **it;
      sit = *it;
      ++(*it);
      send = *it;
      return curToken;
    }
    /*
    if (state == ScanStart)
    {
      if (**it == '\'')
      {
        state = ScanConstant;
        ++(*it);
        sit = *it;
      }
      else if (**it == '/') 
      {
        state = ScanRegExp;
        ++(*it);
        sit = *it;
      }
      else if (acdk::lang::Character::isWhitespace(**it))
      {
        // noting
      } 
      else if (**it == '|')
      {
        ++(*it);
        return curToken = Or;
      }
      else if (**it == '[')
      {
        ++(*it);
        return curToken = OptStart;
      }
      else if (**it == ']')
      {
        ++(*it);
        return curToken = OptEnd;
      }
      else if (**it == '(')
      {
        ++(*it);
        return curToken = GroupStart;
      }
      else if (**it == ')')
      {
        ++(*it);
        return curToken = GroupEnd;
      }
      else if (**it == '*')
      {
        ++(*it);
        return curToken = ZeroOrMore;
      }
      else if (**it == '+')
      {
        ++(*it);
        return curToken = OneOrMore;
      }
      
      else if (**it == '$')
      {
        ++(*it);
        return curToken = SaveRule;
      }
      else if (**it == '%')
      {
        ++(*it);
        return curToken = SaveRuleSub;
      }
      else if (**it == ';')
      {
        ++(*it);
        return curToken = EoStatement;
      }
      else if (**it == ':')
      {
        ++(*it);
        return curToken = Colon;
      }
      
      else if (**it == '&')
      {
        ++(*it);
        return curToken = HideSubRule;
      }
      else if (**it == '.')
      {
        ++(*it);
        return curToken = Dot;
      }
      else if (**it == '"')
      {
        strLit = StringTerminalParseNode::scanStringText(*it, end);
        return curToken = StringLit;
      }
      else if (**it == '!' && *(*it + 1) == '{')
      {
        state = ScanEvalText;
        //sit = (*it + 2);
        sit = *it;
      }
      else if (**it == '!')
      {
        ++(*it);
        return curToken = GrammarCommit;
      }
      else
      {
        sit = *it;
        state = ScanIdentifier;
      }
    }
    else if (state == ScanIdentifier)
    {
      if (**it == '[' ||
          **it == ']' ||
          **it == '|' || 
          **it == '(' || 
          **it == ')' || 
          **it == '*' || 
          **it == '+' || 
          **it == '!' || 
          **it == '$' || 
          **it == '%' || 
          **it == '&' || 
          **it == ':' || 
          **it == ';' || 
          **it == '.' || 
          acdk::lang::Character::isWhitespace(**it))
      {
        send = *it;
        return curToken = Identifier;
      }
    }
    else if (state == ScanConstant)
    {
      if (**it == '\'')
      {
        send = *it;
        ++(*it);
        return curToken = Constant;
      }
    }
    else if (state == ScanRegExp)
    {
      if (**it == '\\')
      {
        ++(*it);
      }
      else if (**it == '/')
      {
        send = *it;
        ++(*it);
        return curToken = RegExpToken;
      }
    }
    else if (state == ScanEvalText)
    {
      if (**it == '}' && *(*it + 1) == '!')
      {
        (*it) += 2;
        send = *it;
        return curToken = EvalBlock;
      }

    }*/
nextLoop:
    ++(*it);
  }
  /*
  if (state == ScanIdentifier)
  {
    send = end;
    return curToken = Identifier;
  }
  */
  return curToken = Eof;
}

RString 
SyntaxScanner::scanFqIdentifier()
{
  Token expect = Identifier;
  int tk;
  StringBuffer sb;
  String::iterator lit = *it;
  while ((tk = nextToken()) != Eof)
  {
    if (tk != expect)
    {
      *it = lit;
      return sb.toString();
    }
    sb.append(curTokStr());
    if (tk == Identifier)
      expect = Dot;
    else
      expect = Identifier;
    lit = *it;
  }
  return sb.toString();
}

} // anon namespace



//static 
RString 
SyntaxNode::toName(SyntaxType tp)
  {
    switch(tp)
    {
    case ST_Or : return "or";
    case ST_Follow : return "follow";
    case ST_Optional : return "optional";
    case ST_Rule : return "rule";
    case ST_Keyword: return "keyword";
    case ST_ZeroOrMore: return "zero or more";
    case ST_OneOrMore: return "one or more";
    case ST_Eval: return "eval";
    case ST_SaveRule: return "save";
    case ST_SaveIfOptRule: return "save if opt sub";
    default: return "<unknown syntax element>";
    }
  }


void 
SyntaxNode::renderToTree(StringBuffer& sb, int ident)
{
  int i;
  for (i = 0; i < ident; ++i)
    sb << " ";
  sb << toString() << ":\n";
  for (i = 0; i < childs->length(); ++i)
    childs[i]->renderToTree(sb, ident + 1);
}




#define MKSTR(it, end) new String(it, end)




RSyntaxNode 
SyntaxParseNode_parseToTree(String::iterator* it, String::iterator end)
{
  int tk;
  SyntaxScanner scanner(MKSTR(*it, end), it, end);
  RSyntaxNode curNode = new SyntaxFollow();

  while ((tk = scanner.nextToken()) != Eof)
  {
    switch(tk)
    {
    case EoStatement:
      return curNode;
    case Identifier:
    {
      RString id = scanner.curTokStr();
      if (id->equals("error") == true)
      {
        tk = scanner.nextToken();
        if (tk != GroupStart)
          ; // throw
        tk = scanner.nextToken();
        if (tk != StringLit)
          ; // throw
        RString arg = scanner.curTokStr();
        tk = scanner.nextToken();
        if (tk != GroupStart)
          ; // throw
        curNode->push_back(new SyntaxError(arg));
        break;
      }
      curNode->push_back(new SyntaxRule(scanner.curTokStr()));
      break;
    }
    case Constant:
      curNode->push_back(new SyntaxKeyword(scanner.curTokStr()));
      break;
    case Or:
    {

      RSyntaxNode sor = new SyntaxOr();
      sor->push_back(curNode);
      sor->push_back(SyntaxParseNode_parseToTree(it, end));
      //curNode = sor;
      return sor;
      break;
    }
    case OptStart:
    {
      RSyntaxNode opt = new SyntaxOptional();
      opt->push_back(SyntaxParseNode_parseToTree(it, end));
      curNode->push_back(opt);
      break;
    }
    case OptEnd:
      return curNode;
    case GroupStart:
    {
      curNode->push_back(SyntaxParseNode_parseToTree(it, end));
      break;
    }
    case GroupEnd:
      return curNode;
    case ZeroOrMore:
    {
      RSyntaxNode n = new SyntaxZeroOrMore();
      n->push_back(curNode->pop_back());
      curNode->push_back(n);
      break;
    }
    case OneOrMore:
    {
      RSyntaxNode n = new SyntaxOneOrMore();
      n->push_back(curNode->pop_back());
      curNode->push_back(n);
      break;
    }
    case EvalBlock:
      curNode->push_back(new SyntaxEval(scanner.curTokStr()));
      break;
    
    case SaveRule:
      curNode->push_back(new SyntaxSave());
      break;
    case SaveRuleSub:
      curNode->push_back(new SyntaxSaveIfSub());
      break;
    case HideSubRule:
      curNode->push_back(new SyntaxHideSubRule());
    case GrammarCommit:
    {
      curNode->push_back(new SyntaxCommit());
      break;
    }
    case RegExpToken:
    default:
      THROW1(Error, "unknown token in syntax grammar");
      break;
    }
  }
  return curNode;
}

bool reduce(INOUT(RSyntaxNode) n)
{
  bool bret = false;
  if (instanceof(n, SyntaxFollow) == true && n->childs->length() == 1)
  {
    n = n->childs[0];
    bret = true;
  }
  if (instanceof(n, SyntaxOr) && n->childs->length() >= 2 && instanceof(n->childs[n->childs->length() - 1], SyntaxOr) == true)
  {
    RSyntaxNode c = n->childs[n->childs->length() - 1];
    n->childs->remove(n->childs->length() - 1);
    for (int i = 0; i < c->childs->length(); ++i)
      n->childs->append(c->childs[i]);
    bret = true;
  }
  for (int i = 0; i < n->childs->length(); ++i)
    bret |= reduce(n->childs[i]);
  return bret;
}

void
SyntaxParseNode::parseSyntaxText(IN(RCompiler) comp, IN(RString) sf, IN(RString) inputFn)
{
  String::iterator it = sf->begin();
  String::iterator end = sf->end();
  SyntaxScanner scanner(MKSTR(it, end), &it, end);
  enum Expect 
  {
    SyntaxLabel
    
  };
  Expect expect = SyntaxLabel;
  RString curRuleName;
  int tk;
  while ((tk = scanner.nextToken()) != Eof)
  {
    switch (tk)
    {
    case EvalBlock:
    {
      RString evalSource = scanner.curTokStr();
      acdk::cfgscript::RProps props = comp->getSyntaxParserEnv(); 
      props->execScript(evalSource, inputFn);
      break;
    }
    case Identifier:
    {
      curRuleName = scanner.curTokStr();
      RString clsName;
      if (curRuleName->equals("new") == true)
      {
        clsName = scanner.scanFqIdentifier();
        tk = scanner.nextToken();
        if (tk != '(')
          ; // ### todo throw
        tk = scanner.nextToken();
        if (tk != Identifier)
          ; // ### todo throw
        curRuleName = scanner.curTokStr();
        tk = scanner.nextToken();
        if (tk != ')')
          ; // ### todo throw
        
      }
      tk = scanner.nextToken();
      if (tk != Colon)
        ; // ### todo throw
      String::iterator sit = it;
      RSyntaxNode sn = SyntaxParseNode_parseToTree(&it, end);
      while (reduce(sn) == true)
        ;
      RString syntax = MKSTR(sit, it);
      if (clsName != Nil)
      {
        comp->registerSyntaxRule((RSyntaxParseNode)Object::New(clsName, inOf(curRuleName), inOf(sn), inOf(syntax)));
      }
      else
        comp->registerSyntaxRule(new SyntaxParseNode(curRuleName, sn, syntax));
      break;
    }
    default:
      THROW1(Exception, "unexpected token in Syntax File: " + scanner.curTokStr());
      break;
    }

  }
}

void 
SyntaxNode::checkRules(IN(RCompiler) comp)
{
  RSyntaxNode tn(this);
  if (instanceof(tn, SyntaxRule) == true)
  {
    RString ruleName = RSyntaxRule(this)->ruleName;
    RParseNode pn = comp->getParseNode(ruleName);
    if (pn == Nil)
      THROW1(Exception, "Rule doesn't exists: " + ruleName);
  }
  for (int i = 0; i < childs->length(); ++i)
  {
    childs[i]->checkRules(comp);
  }
}

RAstNodeArray 
SyntaxFollow::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  RAstNodeArray erg = new AstNodeArray(0);
  RAstNodeArray sibs = new AstNodeArray(0);
  sibs->concat(siblings);
  ScannerTokenStack ss(comp->scanner);
  bool hasLastNodes = false;
  for (int i = 0; i < childs->length(); ++i)
  {
    if (instanceof(childs[i], SyntaxSaveIfSub) == true)
    {
      if (hasLastNodes == true)
        parent->setSaveNodeAfterBuild(true);
      continue;
    }
    if (instanceof(childs[i], SyntaxHideSubRule) == true)
    {
      erg->pop_back();
      sibs->pop_back();
      continue;
    }
    RAstNodeArray ch = childs[i]->scan(comp, parent, sibs, pn);
    if (ch == Nil)
      return Nil;
    if (ch->length() > 0)
      hasLastNodes = true;
    else
      hasLastNodes = false;
    for (int j = 0; j < ch->length(); ++j)
    {
      RAstNode can = ch[j];
      if (can->getSaveNodeAfterBuild() == true)
      {
        erg->append(can);
        sibs->append(can);
      }
      else
      {
        for (int k = 0; k < can->getChildCount(); ++k)
        {
          //parent->addChild(can->getChild(k), true);
          erg->append(can->getChild(k));
          sibs->append(can->getChild(k));
        }
      }
    }
  }
  ss.commit();
  return erg;
}


RAstNodeArray 
SyntaxRule::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  RAstNode erg = comp->parseRule(ruleName, pn); // ### @todo problem, a subrule may parses, but not produces AstNodes
  if (erg == Nil)
    return Nil;
  if (erg->getSaveNodeAfterBuild() == true)
  {
    RAstNodeArray erga = new AstNodeArray(1);
    erga[0] = erg;
    return erga;
  }
  else
  {
    RAstNodeArray erga = new AstNodeArray(0);
    for (int i = 0; i < erg->getChildCount(); ++i)
    {
      erga->append(erg->getChild(i));
    }
    return erga;
  }
}

RAstNodeArray 
SyntaxKeyword::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  RTerminalParseNode n = comp->getTerminal(keyWord);
  //RParseNode n = new KeywordParseNode("Keyword", keyWord);
  RAstNode erg = n->parse(comp);
  if (erg == Nil)
    return Nil;
  RAstNodeArray erga = new AstNodeArray(1);
  erga[0] = erg;
  return erga;
}

RAstNodeArray 
SyntaxOr::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  for (int i = 0; i < childs->length(); ++i)
  {
    ScannerTokenStack ss(comp->scanner);
    RAstNodeArray ch = childs[i]->scan(comp, parent, siblings, pn);
    if (ch != Nil)
    {
      ss.commit();
      return ch;
    }
  }
  return Nil;
}

RAstNodeArray 
SyntaxOptional::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  RAstNodeArray erg = new AstNodeArray(0);
  RAstNodeArray sibs = new AstNodeArray(0);
  sibs->concat(siblings);
    
  for (int i = 0; i < childs->length(); ++i)
  {
    RAstNodeArray ch = childs[i]->scan(comp, parent, sibs, pn);
    ScannerTokenStack ss(comp->scanner);
    if (ch != Nil)
    {
      for (int j = 0; j < ch->length(); ++j)
      {
        erg->append(ch[j]);
        sibs->append(ch[j]);
      }
      ss.commit();
    }
  }
  return erg;
}

RAstNodeArray 
SyntaxZeroOrMore::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  RAstNodeArray erg = new AstNodeArray(0);
  RAstNodeArray sibs = new AstNodeArray(0);
  sibs->concat(siblings);
  do {
    ScannerTokenStack ss(comp->scanner);
    RAstNodeArray ch = childs[0]->scan(comp, parent, sibs, pn);
    if (ch != Nil)
    {
      for (int j = 0; j < ch->length(); ++j)
      {
        erg->append(ch[j]);
        sibs->append(ch[j]);
      }
      ss.commit();
    }
    else
      return erg;
  } while (true);
  return Nil;
}

RAstNodeArray 
SyntaxOneOrMore::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  RAstNodeArray erg;
  RAstNodeArray sibs = new AstNodeArray(0);
  sibs->concat(siblings);

  do {
    ScannerTokenStack ss(comp->scanner);
    RAstNodeArray ch = childs[0]->scan(comp, parent, sibs, pn);
    if (ch != Nil)
    {
      ss.commit();
      if (erg == Nil)
         erg = new AstNodeArray(0);
      for (int j = 0; j < ch->length(); ++j)
      {
        erg->append(ch[j]);
        sibs->append(ch[j]);
      }
    }
    else
      return erg;
  } while (true);
  return Nil;
}

acdk::aci::ast::RAstNodeArray 
SyntaxCommit::scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  // ### @todo output all errors if any
  return new acdk::aci::ast::AstNodeArray(0);
}

RAstNodeArray 
SyntaxEval::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  acdk::cfgscript::RProps props = comp->getSyntaxParserEnv(); 
  props->setObjectVal("PN", &parent);
  props->setObjectVal("PARSENODE", &pn);
  for (int i = 0; i < siblings->length(); ++i)
  {
    props->setObjectVal(SBSTR("AN" << (i + 1)), &siblings[i]);
  }
  props->setObjectVal("RET", Nil);
  props->execScript(evalSource);
  props->dump();
  if (props->hasValue("RET") == true && props->getObjectVal("RET") != Nil)
  {
    RAstNodeArray na = new AstNodeArray(1);
    na[0] = (RAstNode)props->getObjectVal("RET");
    return na;
  }
  // read result
  return new AstNodeArray(0);
}

RAstNodeArray 
SyntaxSave::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  parent->setSaveNodeAfterBuild(true);
  return new AstNodeArray(0);
}

RAstNodeArray 
SyntaxSaveIfSub::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  // will never be called
  return new AstNodeArray(0);
}

RAstNodeArray 
SyntaxHideSubRule::scan(IN(RCompiler) comp, IN(RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  // will never be called
  return new AstNodeArray(0);
}



RSyntaxNode 
SyntaxParseNode::_parseToTree()
{
  RString s = _syntax;
  String::iterator begin = s->begin();
  String::iterator end = s->end();
  RSyntaxNode n = SyntaxParseNode_parseToTree(&begin, end);
  while (reduce(n) == true)
    ;
  StringBuffer sb;
  n->renderToTree(sb, 0);
  return n;
}

acdk::aci::ast::RAstNodeArray 
SyntaxError::scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn)
{
  // ### @todo Save current leaf of ast and pn, register as __parse_error
  return Nil;
}


} // namespace parser
} // aci
} // acdk

