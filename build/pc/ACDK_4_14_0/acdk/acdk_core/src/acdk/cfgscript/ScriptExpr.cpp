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




#include <acdk/io/StreamTokenizer.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/StringReader.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/locale/Encoding.h>
#include <acdk/lang/System.h>
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>
#include <acdk/lang/sys/core_vector.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/dmi/DmiDelegate.h>
#include <acdk/lang/dmi/MetaInfoChildsArray.h>
#include <acdk/lang/reflect/Method.h>

#include "ScriptObject.h"
#include "Script.h"
#include "ScriptException.h"
#include "ScriptEval.h"
#include "ScriptExpr.h"
#include "ScriptGlobals.h"

namespace acdk {
namespace cfgscript {

using acdk::io::StreamTokenizer;

PEVal::PEVal(Token t, IN(PEVal) left, IN(PEVal) right)
: tk(t)
, args(new LeftRightArgs(left, right))
{
}

//static 
RString 
PEVal::tokenToString(int tk)
  {
    switch (tk)
    {
    case TT_VALUE: return "VALUE";
    case TT_REFVALUE: return "REFVALUE";
    case TT_IDENT: return "IDENT";
    case TT_ARGLABEL: return "ARGLABEL";
    case TT_OPERATOR: return "OPERATOR";
    case TT_METHOD: return "METHOD";
    case TT_FIELD: return "FIELD";
    case TT_DOTOP: return "DOTOP";
    default: return RString("UnknownTk=[") + tk + "]";
    }
  }

RString 
PEVal::toCode()
  {
    switch (tk)
    {
    case TT_DOTOP: 
      if (args != Nil)
        return args->left.toCode() + "." + args->right.toCode();
      return ".";
    case TT_REFVALUE: 
    case TT_VALUE: return val.toString(); 
    case TT_METHOD:
    case TT_UNIT:
    case TT_CLASS:
    case TT_OPERATOR: 
    case TT_IDENT: return val.getStringVar();
    case TT_ARGLABEL: return val.getStringVar() + ":";
    default: return RString("UnknownTk=[") + tk + "]";
    }
  }

RString 
PEVal::toString()
{
  if (tk == TT_DOTOP && args != Nil)
    return tokenToString(tk) + ": " + args->left.toString() + "." + args->right.toString();
  return tokenToString(tk) + ": " + val.getClazzInfo()->name + "=" + val.toCode();
}
/*
OpDescription OpPriorities[] =
{
  { "operator_popc", 1, OpSuffix | OpOutfix }, // ()
  { "operator_co", 1, OpSuffix | OpOutfix }, // {
  { "operator_po", 1, OpSuffix | OpOutfix }, // ()
  { "operator_bobc", 1, OpSuffix | OpOutfix }, // []
  { "operator_bo", 1, OpSuffix | OpOutfix }, // []
  { "operator_dt", 1, OpPrefix | OpInfix }, // .
  { "operator_migt", 1, OpInfix  }, // ->
  { "operator_plpl", 2, OpPrefix | OpSuffix | OpRightAssoc }, // ++
  { "operator_mimi", 2, OpPrefix | OpSuffix | OpRightAssoc }, // --

  { "operator_new", 2, OpPrefix }, // new
  { "operator_instanceof", 2, OpInfix }, // *
  { "operator_as", 2, OpInfix }, // *
  { "operator_sl", 2, OpInfix  }, // /
  { "operator_pl", 3, OpPrefix | OpInfix  }, // +
  { "operator_mi", 3, OpPrefix | OpInfix  }, // -
  { "operator_ltlt", 4, OpInfix }, // <<
  { "operator_gtgt", 4, OpInfix }, // >>
  { "operator_gt", 5, OpInfix },
  { "operator_lt", 5, OpInfix },
  { "operator_gteq", 5, OpInfix },
  { "operator_lteq", 5, OpInfix },
  { "operator_eqeq", 6, OpInfix },
  { "operator_nteq", 6, OpInfix },
  { "operator_la", 7, OpInfix }, // &
  { "operator_rf", 8, OpInfix }, // ^
  { "operator_vb", 9, OpInfix }, // |
  { "operator_lala", 10, OpInfix }, // &&
  { "operator_vbvb", 11, OpInfix }, // ||
  { "operator_eq", 12, OpInfix | OpRightAssoc },  // =
  { 0, 0, 0 }
};
*/

OpDescription UnEncodedOpPriorities[] =
{
  { "{", 1, OpOutfix }, // .
  { ".", 1, OpPrefix | OpInfix }, // .
  { "->", 1, OpInfix  }, // ->
  { "`", 2, OpSuffix | OpOutfix }, // `<string-literal>`
  { "(", 2, OpSuffix | OpOutfix }, // ()
  { "[", 2, OpSuffix | OpOutfix }, // []
  { "!", 2, OpPrefix }, 
  { "++", 2, OpPrefix | OpSuffix | OpRightAssoc }, // ++
  { "--", 2, OpPrefix | OpSuffix | OpRightAssoc }, // --

  { "new", 2, OpPrefix }, // *
  { "instanceof", 2, OpInfix }, // 
  { "*", 2, OpInfix }, // *
  { "/", 2, OpInfix  }, // /
  { "%", 2, OpInfix  }, // /
  { "+", 3, OpPrefix | OpInfix  }, // +
  { "-", 3, OpPrefix | OpInfix  }, // -

  { "<<", 4, OpInfix }, // <<
  { ">>", 4, OpInfix }, // >>
  { ">", 5, OpInfix },
  { "<", 5, OpInfix },
  { ">=", 5, OpInfix },
  { "<=", 5, OpInfix },
  { "==", 6, OpInfix },
  { "!=", 6, OpInfix },
  { "&", 7, OpInfix }, // &
  { "^", 8, OpInfix }, // ^
  { "|", 9, OpInfix }, // |
  { "&&", 10, OpInfix }, // &&
  { "||", 11, OpInfix }, // ||
  { "=", 12, OpInfix | OpRightAssoc },  // =
  { 0, 0, 0 }
};

OpDescription varOpDescription = { "", 1, OpVarOrIdent };

const OpDescription& 
getOperatorDesc(INP(RString) op)
{
  int i;
  for (i = 0; UnEncodedOpPriorities[i].op != 0; ++i)
    if (op->equals(UnEncodedOpPriorities[i].op) == true)
      return UnEncodedOpPriorities[i];
  return UnEncodedOpPriorities[i];
}



int getOperatorPredence(INP(RString) op)
{
  for (int i = 0; UnEncodedOpPriorities[i].op != 0; ++i)
    if (op->equals(UnEncodedOpPriorities[i].op) == true)
      return UnEncodedOpPriorities[i].prio;
  return -1;
}

int getOperatorFlags(INP(RString) op)
{
  //RString opstr = acdk::lang::reflect::Method::encodeOperatorToFuncName(op);
  for (int i = 0; UnEncodedOpPriorities[i].op != 0; ++i)
    if (op->equals(UnEncodedOpPriorities[i].op) == true)
      return UnEncodedOpPriorities[i].flags;
  return 0;
}


int compareOperators(INP(RString) left, INP(RString) right)
{
  return getOperatorPredence(left) - 
         getOperatorPredence(right);
}

bool isOperator(const SourceToken& stk)
{
  if (stk.tk == StreamTokenizer::TT_OPERATOR  || 
         stk.tk == '(' ||
         stk.tk == '[' ||
         stk.tk == '%' ||
         stk.tk == '{' ||
         stk.tk == '`' )
    return true;
  if (stk.tk == StreamTokenizer::TT_WORD)
  {

    ASCLITERAL(new);
    ASCLITERAL(instanceof);

    RString sval = stk.value.getStringVar();
    if (sval->equals(lit_new) == true ||
        sval->equals(lit_instanceof) == true)
        return true;

  }
  return false;
}


bool valOrIdent(const SourceToken& stk)
{
  return stk.tk == StreamTokenizer::TT_NUMBER || stk.tk == StreamTokenizer::TT_STRING || 
         stk.tk == StreamTokenizer::TT_QCHAR ||
    (stk.tk == StreamTokenizer::TT_WORD && isOperator(stk) == false);
}


bool isEndExpr(const SourceToken& stk)
{
  return stk.tk != StreamTokenizer::TT_NUMBER && 
         stk.tk != StreamTokenizer::TT_STRING && 
         stk.tk != StreamTokenizer::TT_WORD &&
         stk.tk != StreamTokenizer::TT_QCHAR &&
         isOperator(stk) == false 
         ;
}


bool parseExpression(PEStack& stack, OUTP(RScriptExpr) expr);

bool
parseLeftOfNonExpr(PEStack& stack, OUTP(RScriptExpr) expr)
{
  int mparsePos = stack.getCurTokenIndex();
  int mtk = stack.nextToken();
  RString ctstr = stack.curTokenAsCode();
  if (isEndExpr(stack.curSourceToken()) == true)
  {
    expr = new ScriptExpr(varOpDescription, mparsePos);
    return true;
  }
  if (isOperator(stack.curSourceToken()) == true) // x +
  {
    RString leftstr = stack.curTokenAsString();
    const OpDescription& opdesc = getOperatorDesc(leftstr);
    if ((opdesc.flags & OpSuffix) == OpSuffix) // x++ 
    {
      RScriptExpr leftExpr;
      if (parseLeftOfNonExpr(stack, leftExpr) == false)
        return false;
      expr = new ScriptExpr(opdesc, mparsePos, Nil, leftExpr);
      return true;
    }
    else if ((opdesc.flags & OpOutfix) == OpSuffix) // x(
    {
      if (parseExpression(stack, expr) == false)
        return false;
      expr = new ScriptExpr(opdesc, mparsePos, expr);
      return true;
    }
    else if ((opdesc.flags & OpInfix) == OpInfix) // x + ?
    {
      if (parseExpression(stack, expr) == false)
        return false;
      if (opdesc.prio <= expr->opDescription.prio)
      {
        // x * y + 3
        RScriptExpr texpr = new ScriptExpr(opdesc, mparsePos);
        texpr->rightExpr = expr->leftExpr;
        expr->leftExpr = texpr;
      }
      else
      {
        // x + y * 3
        expr = new ScriptExpr(opdesc, mparsePos, expr);
      }
      return true;
    }
    else
    {
      ELOG("Unknown Token Type: " + leftstr);
    }
  }
  return false;
}

bool
parseExpression(PEStack& stack, OUTP(RScriptExpr) expr)
{
  int cparsePos = stack.getCurTokenIndex();
  int otk = stack.nextToken();
  if (otk == -1)
  {
    return false;
  }
  
  RString leftstr = stack.curTokenAsString();
  
  if (isEndExpr(stack.curSourceToken()) == true)
  {
    expr = new EmptyExpr();
    return true;
  }
  
  if (isOperator(stack.curSourceToken()) == true)
  {
    const OpDescription& opdesc = getOperatorDesc(leftstr);
    
    if ((opdesc.flags & OpPrefix) == OpPrefix)
    {
      RScriptExpr rightExpr;
      if (parseExpression(stack, rightExpr) == false)
        return false;
      
      if (opdesc.prio > rightExpr->opDescription.prio)
      {
        // ++i.left =>
        //      ++  
        // Nil     i . left
        expr = new ScriptExpr(opdesc, cparsePos, Nil, rightExpr); 
      }
      else
      {
        // ++i * 3 =>
        //        *
        //  ++
        //     i      3
        expr = new ScriptExpr(opdesc, cparsePos, Nil, rightExpr->leftExpr);
        rightExpr->leftExpr = expr;
        expr = rightExpr;
      }
      return true;
    }
    else if ((opdesc.flags & OpOutfix) == OpOutfix)
    {
      RScriptExpr rightExpr;
      if (parseExpression(stack, rightExpr) == false)
        return false;
      expr = new ScriptExpr(opdesc, cparsePos, Nil, rightExpr);
      int otk2 = stack.nextToken(); // closing expr
      return true;
    }
    else
    {
      ELOG("Operator is not prefix or outfix: " + leftstr);
    }
  }
  else // left is no operator
  {
    // x
    RScriptExpr rightExpr;
    if (parseLeftOfNonExpr(stack, rightExpr) == false)
      return false;
    if (rightExpr->opDescription.flags == OpEmpty)
    {
      expr = new VarOrValExpr(cparsePos);
      return true;
    }
    /*
    if (opdesc.prio <= rightExpr->opDescription.prio)
    {
      expr = new VarOrValExpr(cparsePos);
      return true;
    }
    
    if (opdesc.prio > rightExpr->opDescription.prio)
    {
      rightExpr
    } 
    else
    {
    }*/
    return true;
  }

  return false;
}

//static 
acdk::lang::dmi::RDmiObject 
ScriptGlobals::castTo(IN(RDmiObject) val, IN(acdk::lang::RClass) cls) 
{
  if (val->ScriptVar::getClazzInfo() == cls->objectClazzInfo())
    return val;
  using namespace acdk::lang::dmi;
  return new acdk::lang::dmi::DmiObject(val->_castScriptVar(cls->objectClazzInfo(), 
        SVCastSVCastChar2Int | SVCastInt2Float | SVCastNum2Bool | SVCastBool2Number | SVCastObject2Bool | SVCastAutobox));
}

} // namespace cfgscript
} // namespace acdk 
  

