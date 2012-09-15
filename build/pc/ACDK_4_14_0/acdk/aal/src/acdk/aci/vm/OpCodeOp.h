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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/vm/OpCodeOp.h,v 1.5 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_vm_OpCodeOp_h
#define acdk_aci_vm_OpCodeOp_h

#include <acdk.h>

namespace acdk {
namespace aci {
namespace vm {

enum OpCodeOp
{
  /**
    Invalid operation, never occours in compilation
  */
  OCO_INVALID_OP = -1,
  /* does nothing
     Stack: nothing
  */
  OCO_NOP     = 0x00000000,
  /**
    push a constant onto stack
  */
  OCO_PUSH    = 0x00000001,
  OCO_POP     = 0x00000002,
  OCO_ADD     = 0x00000003,
  OCO_SUB     = 0x00000004,
  OCO_LT      = 0x00000005,
  OCO_GT      = 0x00000006,
  OCO_LTEQ    = 0x00000007,
  OCO_GTEQ    = 0x00000008,
  OCO_EQ      = 0x00000009,
  OCO_NE      = 0x0000000A,
  OCO_INC     = 0x0000000B,
  OCO_DEC     = 0x0000000C,
  OCO_NOT     = 0x0000000D,

  OCO_LOAD     = 0x00000010,
  OCO_STORE    = 0x00000011,
  /// create local var
  OCO_CLVR     = 0x00000012, 
  OCO_LOADREF  = 0x00000013, 
  OCO_DUP      = 0x00000014, 
  OCO_LDAREL   = 0x00000015, 
  OCO_LDARELREF= 0x00000016,

  OCO_RET      = 0x00000020,
  OCO_BR       = 0x00000021,
  OCO_BRFALSE  = 0x00000022,
  OCO_BRTRUE   = 0x00000023,
  OCO_ASSIGN   = 0x00000024,
  OCO_MUL      = 0x00000025,
  OCO_DIV      = 0x00000026,
  OCO_MOD      = 0x00000027,
  OCO_BIN_AND  = 0x00000030,
  OCO_BIN_OR   = 0x00000031,
  OCO_BIN_NOT  = 0x00000032,
  OCO_BIN_XOR  = 0x00000033,
  OCO_BIN_SHR  = 0x00000034,
  OCO_BIN_SHRUS  = 0x00000035,
  OCO_BIN_SHL  = 0x00000036,
  OCO_LOADGLOB = 0x00000037,
  OCO_LOADGLOBREF = 0x00000038,
  OCO_STOREGLOB = 0x00000039,
  OCO_CREATEGLOB = 0x0000003A,
  
  /**
    copy the value, not the reference if orginal is referece
  */
  OCO_DUP_VAL  = 0x00000040,
  /**
    copy the reference, not the value if original is value
  */
  OCO_DUP_REF  = 0x00000041,

  OCO_NEW             = 0x00000100,
  OCO_INVOKE          = 0x00000101,
  OCO_INVOKE_STATIC   = 0x00000102,
  OCO_PEEK            = 0x00000103,
  OCO_PEEK_STATIC     = 0x00000104,
  OCO_POKE            = 0x00000105,
  OCO_POKE_STATIC     = 0x00000106,
  OCO_CLASSCAST       = 0x00000107,
  OCO_INSTANCEOF      = 0x00000108,
  OCO_NEWARR          = 0x00000109,
  OCO_HASH_INVOKE     = 0x0000010A,
  OCO_HASH_INVOKE_STATIC = 0x0000010B,
  OCO_INIT_CLAZZ      = 0x00000200,
  OCO_TRY             = 0x00000201,
  OCO_CATCHEND        = 0x00000202,
  OCO_FINALLYEND      = 0x00000203,
  OCO_THROW           = 0x00000204,
  /**
    check if topmost stack value is true
    otherwise throw exception
    Stack: top[0] = String msg
    Stack: top[-1] = boolean condition
    Stack: removes 2 elements from stack
  */
  OCO_ASSERT          = 0x00001000,
  /**
    print stack content
    Stack: nothing
  */
  OCO_DUMPSTACK       = 0x00002000,
  OCO_USER            = 0x0000F000,
  OCO_MAXOPCODE       = 0x0000FFFF
};
ACDK_DEF_LIB_ENUM(ACDK_ACI_PUBLIC, OpCodeOp);

ACDK_DECL_CLASS(OpCodeStm);
ACDK_DECL_CLASS(EvalEnv);

struct ACDK_ACI_PUBLIC OpCodeOpDescription;
typedef ROpCodeStm (*OpCodeCreatorFnc)(OpCodeOp opCodeOp, IN(RString) name, IN(RString) label, const acdk::lang::dmi::ScriptVar& val);

typedef void (*ExecuteFunc)(IN(REvalEnv) env);

struct ACDK_ACI_PUBLIC OpCodeOpDescription
{
  OpCodeOp opCodeOp;
  const char* name;
  int argCount;
  OpCodeCreatorFnc creator;
};

} // vm
} // aci
} // acdk



#endif //acdk_aci_vm_OpCodeOp_h
