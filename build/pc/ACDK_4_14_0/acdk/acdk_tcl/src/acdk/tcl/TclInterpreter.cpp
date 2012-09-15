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
// $Header: /cvsroot/acdk/acdk/acdk_tcl/src/acdk/tcl/TclInterpreter.cpp,v 1.33 2005/04/19 14:42:00 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/ClassNotFoundException.h>
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>
#include <acdk/lang/sys/core_string.h>

#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/io/File.h>
#include "TclInterpreter.h"

#include <tcl.h>


namespace acdk {
namespace tcl {


using namespace acdk::lang;
using namespace acdk::lang::dmi;

extern "C" void acdk_FreeInternalRepProc(Tcl_Obj *objPtr);
extern "C" void acdk_DupInternalRepPro(Tcl_Obj *srcPtr, Tcl_Obj *dupPtr);
extern "C" void acdk_UpdateStringProc(Tcl_Obj *objPtr);
extern "C" int acdk_SetFromAnyProc(Tcl_Interp *interp, Tcl_Obj *objPtr);

static Tcl_ObjType acdk_ObjType = 
{
  "acdk", //name
  &acdk_FreeInternalRepProc,
  &acdk_DupInternalRepPro,
  &acdk_UpdateStringProc,
  &acdk_SetFromAnyProc
};

extern "C" void acdk_FreeInternalRepProc(Tcl_Obj *objPtr)
{
  if (objPtr->typePtr != &acdk_ObjType)
    return;
  Object* obj = (Object*)objPtr->internalRep.otherValuePtr;
  if (obj != 0)
    obj->releaseRef();
}

extern "C" void acdk_DupInternalRepPro(Tcl_Obj *srcPtr, Tcl_Obj *dupPtr)
{
  System::out->println("acdk_DupInternalRepPro ");
}

extern "C" void acdk_UpdateStringProc(Tcl_Obj *objPtr)
{
  System::out->println("acdk_UpdateStringProc ");
}

extern "C" int acdk_SetFromAnyProc(Tcl_Interp *interp, Tcl_Obj *objPtr)
{
  System::out->println("acdk_SetFromAnyProc ");
  return TCL_ERROR;
}


/*
extern "C" int tcl_acdk_invoke(ClientData clientData, Tcl_Interp* interp, int argc, char* argv[])
{
  return ((TclInterpreter*)clientData)->acdk_invoke(argc, argv);
}

extern "C" int tcl_acdk_invoke_static(ClientData clientData, Tcl_Interp* interp, int argc, char* argv[])
{
  return ((TclInterpreter*)clientData)->acdk_invoke_static(argc, argv);
}

extern "C" int tcl_acdk_peek(ClientData clientData, Tcl_Interp* interp, int argc, char* argv[])
{
  return ((TclInterpreter*)clientData)->acdk_peek(argc, argv);
}

extern "C" int tcl_acdk_peek_static(ClientData clientData, Tcl_Interp* interp, int argc, char* argv[])
{
  return ((TclInterpreter*)clientData)->acdk_peek_static(argc, argv);
}

extern "C" int tcl_acdk_poke(ClientData clientData, Tcl_Interp* interp, int argc, char* argv[])
{
  return ((TclInterpreter*)clientData)->acdk_poke(argc, argv);
}

extern "C" int tcl_acdk_poke_static(ClientData clientData, Tcl_Interp* interp, int argc, char* argv[])
{
  return ((TclInterpreter*)clientData)->acdk_poke_static(argc, argv);
}
*/

bool isObject(const char* tcltoken)
{
  if (strncmp(tcltoken, "mo:", 3) == 0)
    return true;
  return false;
}

RObject decodeObject(const char* tcltoken)
{
  tcltoken += 3;
  if (strcmp(tcltoken, "0") == 0 || strcmp(tcltoken, "Nil") == 0)
    return Nil;
  return (Object*)(void*)Integer::parseInt(tcltoken);
}

bool tcl2acdk(const char* tclarg, ScriptVar& var)
{
  if (isObject(tclarg) == true) {
    var = decodeObject(tclarg);
    return true;
  }
  return false;
}

bool acdk2tcl(RString& buffer, ScriptVar& var)
{
  return false;
}

bool 
tcl2acdk(int argc, char* argv[], ScriptVarArray& sa)
{
  for (int i = 0; i < argc; i++) 
    if (tcl2acdk(argv[i], sa[i]) == false)
      return false;
  return true;
}


RString tcl2string(Tcl_Obj* tobj)
{
  char* ptr =  Tcl_GetString(tobj);
  return SCS(ptr);
}

RObject tcl2object(Tcl_Interp* interp, Tcl_Obj* tobj)
{
  if (tobj->typePtr == &acdk_ObjType) {
    return (Object*)tobj->internalRep.otherValuePtr;
  }
  if (tobj->typePtr && strcmp(tobj->typePtr->name, "bool") == 0) {
    int erg = 0;
    if (Tcl_GetBooleanFromObj(interp, tobj, &erg) == TCL_OK)
      return new Boolean(erg != 0);
  }
  if (tobj->typePtr && strcmp(tobj->typePtr->name, "int") == 0) {
    int erg = 0;
    if (Tcl_GetIntFromObj(interp, tobj, &erg) == TCL_OK)
      return new Integer(erg);
  }
  if (tobj->typePtr && strcmp(tobj->typePtr->name, "double") == 0) {
    double erg = 0;
    if (Tcl_GetDoubleFromObj(interp, tobj, &erg) == TCL_OK)
      return new Double(erg);
  }
  return (RObject)SCS(Tcl_GetString(tobj));
}

bool tcl2acdk(Tcl_Interp* interp, Tcl_Obj* tobj, ScriptVar& var)
{
  if (tobj->typePtr == &acdk_ObjType) {
    var = (RObject)(Object*)tobj->internalRep.otherValuePtr;
    return true;
  }
  if (tobj->typePtr && strcmp(tobj->typePtr->name, "bool") == 0) {
    int erg = 0;
    if (Tcl_GetBooleanFromObj(interp, tobj, &erg) == TCL_ERROR)
      return false;
    var = (erg != 0);
    return true;
  }
  if (tobj->typePtr && strcmp(tobj->typePtr->name, "int") == 0) {
    int erg = 0;
    if (Tcl_GetIntFromObj(interp, tobj, &erg) == TCL_ERROR)
      return false;
    var = erg;
    return true;
  }
  if (tobj->typePtr && strcmp(tobj->typePtr->name, "double") == 0) {
    double erg = 0;
    if (Tcl_GetDoubleFromObj(interp, tobj, &erg) == TCL_ERROR)
      return false;
    var = erg;
    return true;
  }
  var = (RObject)SCS(Tcl_GetString(tobj));
  return true;
}

bool tcl2acdk(Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[], ScriptVarArray& sa)
{
  for (int i = 0; i < argc; i++) 
    if (tcl2acdk(interp, argv[i], sa[i]) == false)
      return false;
  return true;
}

bool acdk2tcl(Tcl_Interp* interp, ScriptVar& var, Tcl_Obj*& tobj )
{
  switch (var.type) {
  case ScriptVar::BoolType :
  case ScriptVar::BoolRefType :
    tobj = Tcl_NewBooleanObj(var.getBoolVar());
    return true;
  case ScriptVar::CharType :
  case ScriptVar::CharRefType :
  case ScriptVar::UcCharType :
  case ScriptVar::UcCharRefType :
  case ScriptVar::ByteType :
  case ScriptVar::ByteRefType :
  case ScriptVar::ShortType :
  case ScriptVar::ShortRefType :
  case ScriptVar::IntType :
  case ScriptVar::IntRefType :
  case ScriptVar::LongType :
  case ScriptVar::LongRefType :
    tobj = Tcl_NewIntObj(var.getIntVar());
    return true;
  case ScriptVar::FloatType :
  case ScriptVar::FloatRefType :
  case ScriptVar::DoubleType :
  case ScriptVar::DoubleRefType :
    tobj = Tcl_NewDoubleObj(var.getDoubleVar());
    return true;
  case ScriptVar::ObjectType : 
  case ScriptVar::ObjectRefType : 
  {
    if (instanceof(var.getObjectVar(), String) == true) {
      RString tstr = (RString)var.getObjectVar();
      tobj = Tcl_NewStringObj(tstr->c_str(), tstr->length());
      return true;
    }
    tobj = Tcl_NewObj();
    tobj->typePtr = &acdk_ObjType;
    Object* o = var.getObjectVar().iptr();
    tobj->internalRep.otherValuePtr = o;
    if (o != 0)
      o->addRef();
    return true;
  }
  case ScriptVar::UnknownType: // void
    tobj = Tcl_NewBooleanObj(true);
    return true;
  }
  return false;
}

/* set static string */
void setResult(Tcl_Interp* interp, char* ptr)
{
  Tcl_SetResult(interp, ptr, TCL_STATIC);
}
void setResult(Tcl_Interp* interp, RString str)
{
  Tcl_SetObjResult(interp, Tcl_NewStringObj(str->c_str(), str->length()));
}

const int StandardDmiCastFlags = SVCastStdFlags | SVCastEncodeString | SVCastDecodeString;

int 
TclInterpreter_acdk_new(TclInterpreter* _this, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  if (argc < 2) {
    setResult(interp, "acdk_new needs at least 1 argument");
    return TCL_ERROR;
  }
  
  RString clsname = tcl2string(argv[1]);
  RClass cls;
  try {
    cls = Class::forName(clsname);
  } catch (RClassNotFoundException ex) {
    setResult(interp, "acdk_new failed: Class not found: " + ex->getMessage());
    return TCL_ERROR; 
  }
  RString constructorname = clsname;
  if (constructorname->indexOf('/') != -1)
    constructorname = constructorname->substr(constructorname->lastIndexOf('/') + 1);

  ScriptVarArray sa(argc - 2);
  if (tcl2acdk(interp, argc - 2, &argv[1] + 1, sa) == false)
    return TCL_ERROR; // ### better

  ScriptVar ret;
  AcdkStdWeakTypeDmiClient dmiclient(StandardDmiCastFlags);
  if (cls->objectClazzInfo()->static_dispatch( constructorname, ret, sa, dmiclient, Nil
                                              , MiPublic | MiMiConstructor, cls->objectClazzInfo(), 0) == 0)
    return TCL_ERROR; // ### better
  Tcl_Obj *tobj;
  if (acdk2tcl(interp, ret, tobj) == false)
    return TCL_ERROR;
  Tcl_SetObjResult(interp, tobj);
  return TCL_OK;
}

TclInterpreter* __currentInterpreter;

extern "C" int tcl_acdk_new(ClientData clientData, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  return TclInterpreter_acdk_new((TclInterpreter*)clientData, interp, argc, argv);
}


int 
TclInterpreter_acdk_invoke(TclInterpreter* _this, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  if (argc < 3) {
    interp->result = "acdk_new needs at least 2 argument";
    return TCL_ERROR;
  }
  
  RObject obj = tcl2object(interp, argv[1]);
  RString funcname = tcl2string(argv[2]);
  int argoffset = 3;
  ScriptVarArray sa(argc - argoffset);
  if (tcl2acdk(interp, argc - argoffset, &argv[0] + argoffset, sa) == false)
    return TCL_ERROR; // ### better
  ScriptVar ret;
  AcdkStdWeakTypeDmiClient dmiclient(StandardDmiCastFlags);
  if (obj->standardDispatch(funcname, ret, sa, dmiclient, Nil, MiPublic, 0) == 0)
    return TCL_ERROR; // ### better
  Tcl_Obj *tobj;
  if (acdk2tcl(interp, ret, tobj) == false)
    return TCL_ERROR;
  Tcl_SetObjResult(interp, tobj);
  return TCL_OK;
}

extern "C" int tcl_acdk_invoke(ClientData clientData, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  return TclInterpreter_acdk_invoke((TclInterpreter*)clientData, interp, argc, argv);
}

int 
TclInterpreter_acdk_invoke_static(TclInterpreter* _this, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  if (argc < 3) {
    interp->result = "acdk_new needs at least 2 argument";
    return TCL_ERROR;
  }
  
  RString clsname = tcl2string(argv[1]);
  RString funcname = tcl2string(argv[2]);
  RClass cls;
  try {
    cls = Class::forName(clsname);
  } catch (RClassNotFoundException ex) {
    setResult(interp, "acdk_new failed: Class not found: " + ex->getMessage());
    return TCL_ERROR; 
  }
  int argoffset = 3;
  ScriptVarArray sa(argc - argoffset);
  if (tcl2acdk(interp, argc - argoffset, &argv[0] + argoffset, sa) == false)
    return TCL_ERROR; // ### better
  ScriptVar ret;
  AcdkStdWeakTypeDmiClient dmiclient(StandardDmiCastFlags);
  if (cls->objectClazzInfo()->static_dispatch(funcname, ret, sa, dmiclient, Nil, MiPublic | MiStatic, cls->objectClazzInfo(), 0) == 0) 
  {
    interp->result = "acdk_invoke_static failed";
    return TCL_ERROR; // ### better
  }
  Tcl_Obj *tobj;
  if (acdk2tcl(interp, ret, tobj) == false)
    return TCL_ERROR;
  Tcl_SetObjResult(interp, tobj);
  return TCL_OK;
}

extern "C" int tcl_acdk_invoke_static(ClientData clientData, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  return TclInterpreter_acdk_invoke_static((TclInterpreter*)clientData, interp, argc, argv);
}



int 
TclInterpreter_acdk_peek(TclInterpreter* _this, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  if (argc != 3) {
    interp->result = "acdk_peek needs 2 argument";
    return TCL_ERROR;
  }
  
  RObject obj = tcl2object(interp, argv[1]);
  RString membername = tcl2string(argv[2]);
    
  ScriptVar ret = obj->peek(membername);
  Tcl_Obj *tobj;
  if (acdk2tcl(interp, ret, tobj) == false)
    return TCL_ERROR;
  Tcl_SetObjResult(interp, tobj);
  return TCL_OK;
}


extern "C" int tcl_acdk_peek(ClientData clientData, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  return TclInterpreter_acdk_peek((TclInterpreter*)clientData, interp, argc, argv);
}


int 
TclInterpreter_acdk_peek_static(TclInterpreter* _this, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  if (argc != 3) {
    interp->result = "acdk_peek needs 2 argument";
    return TCL_ERROR;
  }
  
  RString clsname = tcl2string(argv[1]);
  RString membername = tcl2string(argv[2]);
  /*
  RClass cls;
  try {
    cls = Class::forName(clsname);
  } catch (RClassNotFoundException ex) {
    setResult(interp, "acdk_new failed: Class not found: " + ex->getMessage());
    return TCL_ERROR; 
  }
  */
  ScriptVar ret = StdDispatch::peek_static(clsname, membername);
  //ScriptVar ret = cls->getStaticMember(membername);

  Tcl_Obj *tobj;
  if (acdk2tcl(interp, ret, tobj) == false)
    return TCL_ERROR;
  Tcl_SetObjResult(interp, tobj);
  return TCL_OK;
}

extern "C" int tcl_acdk_peek_static(ClientData clientData, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  return TclInterpreter_acdk_peek_static((TclInterpreter*)clientData, interp, argc, argv);
}


int 
TclInterpreter_acdk_poke(TclInterpreter* _this, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  if (argc != 4) {
    interp->result = "acdk_poke needs 3 argument";
    return TCL_ERROR;
  }
  RObject obj = tcl2object(interp, argv[1]);
  RString membername = tcl2string(argv[2]);
  ScriptVar arg;
  if (tcl2acdk(interp, argv[3], arg) == false)
    return TCL_ERROR;
  obj->poke(membername, arg);
  return TCL_OK;
}


extern "C" int tcl_acdk_poke(ClientData clientData, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  return TclInterpreter_acdk_poke((TclInterpreter*)clientData, interp, argc, argv);
}

int 
TclInterpreter_acdk_poke_static(TclInterpreter* _this, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  if (argc != 4) {
    interp->result = "acdk_poke_static needs 3 argument";
    return TCL_ERROR;
  }
  
  RString clsname = tcl2string(argv[1]);
  RString membername = tcl2string(argv[2]);
  /*
  RClass cls;
  try {
    cls = Class::forName(clsname);
  } catch (RClassNotFoundException ex) {
    setResult(interp, "acdk_new failed: Class not found: " + ex->getMessage());
    return TCL_ERROR; 
  }
  */
  ScriptVar arg;
  if (tcl2acdk(interp, argv[3], arg) == false)
    return TCL_ERROR;
    
  StdDispatch::poke_static(clsname, membername, arg);
  //cls->setStaticMember(membername, arg);
  return TCL_OK;
}

extern "C" int tcl_acdk_poke_static(ClientData clientData, Tcl_Interp* interp, int argc, Tcl_Obj *CONST argv[])
{
  return TclInterpreter_acdk_poke_static((TclInterpreter*)clientData, interp, argc, argv);
}

extern "C" 


extern "C"
int ACDK_Tcl_Init(Tcl_Interp* interp)
{
  if (Tcl_Init(interp) == TCL_ERROR)
  {
//#if !defined(ACDK_OS_WIN32) // always return err
    sys::coreout << "Tcl_Init return TCL_ERROR: " << interp->result << sys::eofl;
    //return TCL_ERROR;
//#endif //!defined(ACDK_OS_WIN32) 
  }
  Tcl_InitStubs(interp, "8", 0);
  Tcl_RegisterObjType(&acdk_ObjType);
  Tcl_ObjType* acdktcl = Tcl_GetObjType("acdk");
  if (acdktcl == 0)
  {
    sys::coreout << "Cannot register Object Type" << sys::eofl;
    return TCL_ERROR;
  }
  //Tcl_Command cmd = Tcl_CreateCommand(interp, "acdk_new", tcl_acdk_new, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  Tcl_Command cmd = Tcl_CreateObjCommand(interp, "acdk_new", tcl_acdk_new, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);

  Tcl_CmdInfo cmdinfo;
  if (Tcl_GetCommandInfo(interp, "acdk_new", &cmdinfo) == 0)
  {
      sys::coreout << "was not able to register command acdk_new" << sys::eofl;
      return TCL_ERROR;
  }
  cmd = Tcl_CreateObjCommand(interp, "acdk_invoke", tcl_acdk_invoke, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  cmd = Tcl_CreateObjCommand(interp, "acdk_invoke_static", tcl_acdk_invoke_static, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  cmd = Tcl_CreateObjCommand(interp, "acdk_peek", tcl_acdk_peek, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  cmd = Tcl_CreateObjCommand(interp, "acdk_peek_static", tcl_acdk_peek_static, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  cmd = Tcl_CreateObjCommand(interp, "acdk_poke", tcl_acdk_poke, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  cmd = Tcl_CreateObjCommand(interp, "acdk_poke_static", tcl_acdk_poke_static, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  /*
  Tcl_CreateCommand(interp, "acdk_peek", tcl_acdk_peek, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  Tcl_CreateCommand(interp, "acdk_peek_static", tcl_acdk_peek_static, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  Tcl_CreateCommand(interp, "acdk_poke", tcl_acdk_poke, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  Tcl_CreateCommand(interp, "acdk_poke_static", tcl_acdk_poke_static, (ClientData)__currentInterpreter, (Tcl_CmdDeleteProc*)0);
  */
  return TCL_OK;
}
//extern TclStubs tclStubs;
//TclStubs *tclStubsPtr;

TclInterpreter::TclInterpreter(void* interp)
{
  //tclStubsPtr = &tclStubs;
  if (interp == 0)
    _interpreter = Tcl_CreateInterp();
  else
    _interpreter = interp;
  __currentInterpreter = this;
  
  ACDK_Tcl_Init((Tcl_Interp*)_interpreter);
  //Tcl_Main(System::getArgc(), System::getArgv(), ACDK_Tcl_Init);

  /*
  int argc = 1;
  char* argv[1];
  argv[0] = System::getArgv()[0];
  
  Tcl_Main(argc, argv, ACDK_Tcl_Init);
  */
}

//virtual 
TclInterpreter::~TclInterpreter()
{
  if (_interpreter != 0)
    Tcl_DeleteInterp(((Tcl_Interp*)_interpreter));
}

//virtual 
void 
TclInterpreter::parse(IN(acdk::io::RFile) file)
{
  int ret = Tcl_EvalFile(((Tcl_Interp*)_interpreter), (char*)file->getCanonicalPath()->c_str());
  Tcl_Obj* obj = Tcl_GetObjResult((Tcl_Interp*)_interpreter);
  if (ret != TCL_OK) {
    RString strmsg = tcl2string(obj);
    strmsg = file->toString() + ":" + ((Tcl_Interp*)_interpreter)->errorLine +  ": failed: " + strmsg;
    System::out->println(strmsg);
  }
}

//virtual 
void 
TclInterpreter::parse(IN(RString) script)
{
  
  int ret = Tcl_EvalFile(((Tcl_Interp*)_interpreter), (char*)script->c_str());
  Tcl_Obj* obj = Tcl_GetObjResult((Tcl_Interp*)_interpreter);
  if (ret != TCL_OK) {
    RString strmsg = tcl2string(obj);
    strmsg = script + ":" + ((Tcl_Interp*)_interpreter)->errorLine +  ": failed: " + strmsg;
    System::out->println(strmsg);
  }
}

//virtual 
RObject 
TclInterpreter::eval(IN(RString) code)
{
  ::acdk::lang::sys::core_string cstr(code->c_str()); //  needed because Tcl_Evl modify buffer
  Tcl_Eval(((Tcl_Interp*)_interpreter), (char*)cstr.c_str());
  return Nil;
}

//virtual 
acdk::lang::dmi::ScriptVar 
TclInterpreter::call(IN(RString) func, acdk::lang::dmi::ScriptVarArray& args)
{
  // ### not implemented
  return ScriptVar();
}

//virtual 
acdk::lang::dmi::ScriptVar 
TclInterpreter::invoke(IN(RObject) obj, IN(RString) func, acdk::lang::dmi::ScriptVarArray& args)
{
  // ### not implemented
  return ScriptVar();
}


extern "C" int tcl_interactive_init(Tcl_Interp* interp)
{
  return TCL_OK;
}


extern "C" void exit_callback()
{
  throw "unexpected exit() called";
}

//virtual 
void 
TclInterpreter::interactive(IN(acdk::io::RCharReader) in, IN(acdk::io::RCharWriter) out, IN(acdk::io::RCharWriter) err)
{
  char* argv[2];
  argv[0] = System::getArgv()[0];
  argv[1] = 0;
  atexit(exit_callback); // this is hack, because Tcl_Main calles exit
  try {
    //Tcl_Main(1, argv, tcl_interactive_init);
  } catch (char*) {
    _interpreter = 0;
  }
}

//static 
RTclInterpreter 
TclInterpreter ::getInstance()
{
  static RTclInterpreter ip = new TclInterpreter();
  return ip;
}


} // namespace acdk 
} // namespace tcl



