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
// $Header: /cvsroot/acdk/acdk/acdk_perl/src/acdk/perl/PerlInterpreter.cpp,v 1.27 2005/05/01 12:56:30 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/SystemIntern.h>
#include <acdk/io/InputReader.h>

#include "PerlInterpreter.h"

#define HAS_PERL_DYNLOADER

//#define PERL_CAPI
#include "EXTERN.h"
#include <perl.h>
#include <XSUB.h>

#if defined(PERL_XS_APIVERSION) && defined(WIN32)
#	define ACTIVE_PERL
#endif




void registerACDKEntries();

bool perl2acdk(SV* cv, acdk::lang::dmi::ScriptVar& erg);
bool acdk2perl(acdk::lang::dmi::ScriptVar& var, SV*& cv, bool mortal = true, bool higherObjects = true);

#ifdef ACTIVE_PERL
# define PERL5INTERPRETER ::interpreter*
#else
# define PERL5INTERPRETER ::PerlInterpreter*
#endif

#ifdef HAS_PERL_DYNLOADER
#if defined(ACTIVE_PERL) || PERL_API_VERSION >= 8
extern "C" void boot_DynaLoader(PERL5INTERPRETER pi, CV* cv);
#else
extern "C" void boot_DynaLoader(CV* cv);
#endif
#endif

namespace acdk {
namespace perl {
  
using namespace acdk::lang;
using namespace acdk::io;






APerlInterpreter::APerlInterpreter()
: Object(),
  _pi(0)
{
  _pi = ::perl_alloc();
  PERL5INTERPRETER pi = (PERL5INTERPRETER)_pi;
  ::perl_construct(pi);
}


//virtual 
APerlInterpreter::~APerlInterpreter()
{
  if (_pi == 0)
    return;
  ::perl_destruct((PERL5INTERPRETER)_pi);
  ::perl_free((PERL5INTERPRETER)_pi);
}


#if defined(ACTIVE_PERL) || PERL_API_VERSION >= 8
void xs_init(PERL5INTERPRETER pi)
#else
void xs_init()
#endif
{
  dXSUB_SYS;
  char *file = __FILE__;
  dXSUB_SYS;
#ifdef HAS_PERL_DYNLOADER
  newXS("DynaLoader::boot_DynaLoader", (XSUBADDR_t)boot_DynaLoader, file);
#endif
  registerACDKEntries();
}

void 
APerlInterpreter::parse(IN(RStringArray) args)
{
  if (args == Nil) {
    char* dummy_argv[] = { "", "-e", "0" } ;
    ::perl_parse((PERL5INTERPRETER)_pi, (XSINIT_t)xs_init, 3, dummy_argv, 0);
  } else {
    ArgumentHolder cargs(args);
    ::perl_parse((PERL5INTERPRETER)_pi, (XSINIT_t)xs_init, cargs.getArgc(), cargs.getArgv(), 0);
  }
}

RObject
APerlInterpreter::eval(IN(RString) code)
{
  SV* sverg = perl_eval_pv((char*)code->c_str(), TRUE);
  acdk::lang::dmi::ScriptVar erg;
  perl2acdk(sverg, erg);
  return erg.getObjectVar();
}


acdk::lang::dmi::ScriptVar 
APerlInterpreter::call(IN(RString) func, acdk::lang::dmi::ScriptVarArray& args)
{
  
  //THROW0(UnsupportedOperationException);
  dSP;
  ENTER;
  SAVETMPS;
  PUSHMARK(sp);

  for (int i = 0; i < args.size(); i++) {
    SV* lv = 0;
    if (acdk2perl(args[i], lv, true) == false) {
      
    }
    XPUSHs(lv);
  }
  PUTBACK;
  int result = perl_call_pv((char*)func->c_str(), G_SCALAR);
  SPAGAIN;
  acdk::lang::dmi::ScriptVar sret;
  if (result > 0) {
    perl2acdk(*sp--, sret);
  }
  FREETMPS;
  LEAVE;
  return sret;
}

int
APerlInterpreter::run()
{
    return ::perl_run((PERL5INTERPRETER)_pi);
    return 0;
}

#ifdef write
# undef write
#endif

void
APerlInterpreter::interactive(IN(RCharReader) rin, IN(RCharWriter) rout, IN(RCharWriter) rerr)
{ 
  //init_debugger(); // symbol not found
  RPrintWriter out;
  RPrintWriter err;
  if (instanceof(rout, PrintWriter))
    out = (RPrintWriter)rout;
  else
    out = new PrintWriter(rout);
  RInputReader in;
  if (instanceof(rin, InputReader))
    in = (RInputReader)rin;
  else
    in = new InputReader(rin);
  err = new PrintWriter(rerr);
  while (true) 
  {
    out->print("> ");
    out->flush();
    try {
      RString line = in->readLine();
      if (line->equals("exit") == true)
        break;
      eval(line);
    } catch (RThrowable lex) {
      StringBuffer sb("Catched Execption: ");
      sb.append(lex->getMessage());
      err->println(sb.toString());
    }
  }
}

static void foo()
{
  RAPerlInterpreter pi = new APerlInterpreter();
  pi->run();
}


//virtual 
void 
APerlInterpreter::parse(IN(RFile) file)
{
  parse(file->loadAscii());
}

//virtual 
void 
APerlInterpreter::parse(IN(RString) script)
{
  char* text = (char*)script->c_str();
  
  RString ath = "-I" + System::getAcdkToolsHome() + "/cfg/acdk/perl";
  char* dummy_argv[] = { "", (char*)ath->c_str(),"-e", text, 0 } ;
  ::perl_parse((PERL5INTERPRETER)_pi, (XSINIT_t)xs_init, 4, dummy_argv, 0);
}

//virtual 
acdk::lang::dmi::ScriptVar 
APerlInterpreter::invoke(IN(RObject) obj, IN(RString) func, acdk::lang::dmi::ScriptVarArray& args)
{
  _throwNotImplementedYet("APerlInterpreter::invoke");
  return acdk::lang::dmi::ScriptVar(Nil);
}
} //namespace perl 
} // namespace acdk

