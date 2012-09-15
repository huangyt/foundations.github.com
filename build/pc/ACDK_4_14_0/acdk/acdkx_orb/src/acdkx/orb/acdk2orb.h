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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/acdk2orb.h,v 1.8 2005/02/05 10:45:39 kommer Exp $

#include "CorObject.h"

#include <org/omg/CORBA/portable/InputStream.h>
#include <org/omg/CORBA/portable/OutputStream.h>
#include <org/omg/CORBA/TypeCode.h>

namespace acdkx {
namespace orb {

using acdk::lang::dmi::ScriptVar;
using acdk::lang::dmi::ScriptVarArray;
using acdk::lang::dmi::ClazzMethodArgInfo;
using acdk::lang::dmi::ClazzMethodInfo;
using acdk::lang::dmi::ClazzInfo;
using ::acdk::lang::reflect::Modifier;
using ::org::omg::CORBA::TCKind;

void readScriptVarValue(::org::omg::CORBA::portable::InputStream& input, const ClazzMethodArgInfo* cmai, ScriptVar& arg);

void readArgValues(::org::omg::CORBA::portable::InputStream& input, const ScriptVarArray& args, const ClazzMethodInfo* cmi);

void writeValue(::org::omg::CORBA::portable::OutputStream& out, ScriptVar& erg);

//void readAny(::org::omg::CORBA::portable::InputStream& in, ScriptVar& arg);

ScriptVar readAny(::org::omg::CORBA::portable::InputStream& in);
void readAnys(::org::omg::CORBA::portable::InputStream& input, ScriptVarArray& args);
void writeScriptVar(::org::omg::CORBA::portable::OutputStream& out, const ScriptVar& arg);

ScriptVar readAnyParam(::org::omg::CORBA::portable::InputStream& in, const ClazzMethodArgInfo* ai, int flags);
void readAnyParams(::org::omg::CORBA::portable::InputStream& in, const ClazzMethodInfo* cmi, ScriptVarArray& args, int flags);

void writeAnyParam(::org::omg::CORBA::portable::OutputStream& out, const ScriptVar& arg, const ClazzMethodArgInfo* ai, int flags);
void writeAnyParams(::org::omg::CORBA::portable::OutputStream& out,  const ClazzMethodInfo* cmi, const ScriptVarArray& args, int flags);
void writeAnyParam(::org::omg::CORBA::portable::OutputStream& out, const ScriptVar& sv);
void writeAnyParam(::org::omg::CORBA::portable::OutputStream& out, IN(RDmiObject) obj);
void writeAnyParams(::org::omg::CORBA::portable::OutputStream& out, IN(RDmiObjectArray) inp);


inline ScriptVar readAnyParam(::org::omg::CORBA::portable::InputStream& in)
{
  return readAny(in);
}
void readAnyParam(::org::omg::CORBA::portable::InputStream& in, OUT(RDmiObject) dmiobj);
void readAnyParams(::org::omg::CORBA::portable::InputStream& in, ScriptVarArray& args);

void readAnyParams(::org::omg::CORBA::portable::InputStream& in, OUT(RDmiObjectArray) outp);

void writeValueReturn(::org::omg::CORBA::portable::OutputStream& out, const ClazzMethodInfo* cmi, ScriptVar& arg);

bool readValueParam(::org::omg::CORBA::portable::InputStream& input, ScriptVar& arg, 
                    const acdk::lang::dmi::ClazzInfo* type, int isflags, int wantflags);
void writeValueParam(::org::omg::CORBA::portable::OutputStream& out, ScriptVar& arg, 
                      const acdk::lang::dmi::ClazzInfo* type, int isflags, int wantflags);
} // namespace acdkx
} // namespace orb



