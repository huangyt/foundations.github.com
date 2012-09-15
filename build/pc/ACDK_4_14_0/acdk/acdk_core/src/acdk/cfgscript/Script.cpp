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


#include "Script.h"
#include "ScriptException.h"
#include "ChDir.h"
#include "ScriptEval.h"
#include "ScriptObject.h"
#include "ScriptDebug.h"

#include <acdk/io/StreamTokenizer.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/StringReader.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/locale/Encoding.h>
#include <acdk/lang/System.h>
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>
#include <acdk/lang/sys/core_vector.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/reflect/Enumeration.h>

namespace acdk {
namespace cfgscript {

USING_CLASS(::acdk::io::, StreamTokenizer);
using acdk::lang::dmi::ScriptVar;
using acdk::lang::dmi::ScriptVarArray;



RString 
Script::getCfgScriptCommandLineHelp()
{
  return "  -csfinclude directory     insert include search directory\n"
         "  -csfpath    directory     insert path for the CfgScript class loader\n"
         "  -csfdebug                 start interpreter in debug mode\n"
         "  -csfdebugonfail           branch to debugger if exception is thrown\n"
         //"                            this will overwrite the CSFPATH env variable\n"
         ;
} 

//static 
RStringArray 
Script::parseCfgScriptOptions(IN(RStringArray) args, IN(RProps) envProps)
{
  for (int i = 0; i < args->length(); ++i)
  {
    RString a = args[i];
    if (a->equals("-csfinclude") == true)
    {
      RString incl = args[i + 1];
      RString s = System::getProperty("CSFINCLUDES");
      if (s != Nil && s->length() > 0)
        System::setProperty("CSFINCLUDES", incl + acdk::io::File::pathSeparator() + s);
      else
        System::setProperty("CSFINCLUDES", incl);
      args->remove(i);
      args->remove(i);
      i = i - 1;
    }
    else if (a->equals("-csfpath") == true)
    {
      RString path = args[i + 1];
      RString s = System::getProperty("CSFPATH");
      if (s != Nil && s->length() > 0)
        System::setProperty("CSFPATH", path + acdk::io::File::pathSeparator() + s);
      else
        System::setProperty("CSFPATH", path);
      //System::setProperty("CSFPATH", path);
      args->remove(i);
      args->remove(i);
      i = i - 1;
    }
    else if (a->equals("-csfdebug") == true)
    {
      Script::breakToDebug();
      args->remove(i);
    }
    else if (a->equals("-csfdebugonfail") == true)
    {
      DebugBreakPoints::get()->addBreakPoint(new ThrowExceptionDebugPoint(""));
      args->remove(i);
    }
  }
  return args;
}

Script::~Script()
{

}


int
Script::readEval(IN(RProps) props, int flags)
{
  if (_tokenized == Nil)
  {
    _tokenized = new TokenizedSource(_filename);
    _tokenized->parseAll();
  }
  return _readEval(props, flags);
}

int
Script::eval(IN(RString) text, IN(RProps) props, int flags)
{
  _tokenized = new TokenizedSource("<text>", text);
  _tokenized->parseAll();
  return _readEval(props, flags);
}

int 
Script::evalTemplate(IN(RString) text, IN(RProps) props, int flag)
{
  if (_tokenized == Nil)
  {
    _tokenized = new TokenizedSource("<text>", text, STParseTemplate);
    _tokenized->parseAll();
  }
  return _readEval(props, flag);
}
int 
Script::evalTemplate(IN(acdk::io::RFile) sourceFile, IN(RProps) props, int flag)
{
  if (_tokenized == Nil)
  {
    _tokenized = new TokenizedSource(sourceFile->getPath(), sourceFile->getReader(), STParseTemplate);
    _tokenized->parseAll();
  }
  return _readEval(props, flag);
}

RDmiObject 
Script::evalExpr(IN(RString) text, IN(RProps) props, int flags)
{
  _tokenized = new TokenizedSource("<text>", text);
  _tokenized->parseAll();
  props->setBoolVal("__evalScriptAsExpr", true);
  int ret = _readEval(props, flags);
  if (ret != 0)
    return Nil;
  return props->get("__evalScriptResult");
}

RString
Script::getScriptPath()
{
  return acdk::io::File(getFileName()).getParentFile()->getCanonicalPath();
}


bool
Script::include(IN(RString) fname, bool nodups, bool changeDir)
{
  RString fqfilename;
  acdk::io::File orgf(_filename);
  acdk::io::File f(orgf.getParent(), fname);
  if (f.exists() == true)
  {
    fqfilename = f.getCanonicalPath();
  }
  else if (fname->startsWith(".") == false)
  {
    acdk::io::File f(fname);
    if (f.exists() == true)
    {
      fqfilename = f.getCanonicalPath();
    }
    else
    {
      if (currentProps->hasValue("CSFINCLUDES") == true)
      {
        RStringArray sa = currentProps->getStringArrayVal("CSFINCLUDES");
        for (int i = 0; i < sa->length(); ++i)
        {
          acdk::io::File f(sa[i], fname);
          if (f.exists() == true)
          {
            fqfilename = f.getCanonicalPath();
            break;
          }
        }
      }
      if (fqfilename == Nil)
      {
        RString acdkhome = currentProps->getAcdkToolsHome(false);
        if (acdkhome != Nil)
        {
          acdk::io::File f(acdkhome + "/cfg/csf/include", fname);
          if (f.exists() == true)
          {
            fqfilename = f.getCanonicalPath();
          }
        }
      }
    }
  }
  else
  {
    acdk::io::File f(fname);
    if (f.exists() == true)
      fqfilename = f.getCanonicalPath();
  }
found:
  if (fqfilename == Nil)
  {

    return false;
  }
  if (nodups == true)
  {
    if (acdk::util::Arrays::sequenceSearch(_alreadyIncluded, fqfilename) != -1)
      return true;
  }
  {
    RString incfiledir;
    if (changeDir == true)
    {
      incfiledir = acdk::io::File(fqfilename).getParent();
    }
    ChDir cdir(incfiledir);
    RScript cscript = new Script(fqfilename, this);
    return cscript->readEval(currentProps, ScriptReadWriteParent | ScriptNoDefaultProps) == 0;
  }
  return true;
}

struct _SetResetCfgVal
{
  RProps _props;
  RString _key;
  int _flags;
  _SetResetCfgVal(IN(RProps) p, IN(RString) k, IN(RDmiObject) v, int flags = 0)
  : _props(p)
  , _key(k)
  , _flags(flags)
  {
    _props->set(k, v, _flags);
  }
  ~_SetResetCfgVal()
  {
    _props->unset(_key, _flags);
  }
};

namespace {
struct CurrentPropsResetter
{
  Script& _cs;
  CurrentPropsResetter(Script& cs, IN(RProps) props) : _cs(cs) { _cs.currentProps = props; }
  ~CurrentPropsResetter() { _cs.currentProps = Nil; }
};

}

//static 
void
Script::initAsEnvProps(IN(RProps) scriptenv)
{
  RDmiObject nilObject = new DmiObject(Nil);
  scriptenv->set("Nil", nilObject, 0);
  scriptenv->set("nil", nilObject, 0);
  scriptenv->set("null", nilObject, 0);
  scriptenv->set("false", new DmiObject(false));
  scriptenv->set("true", new DmiObject(true));
  if (scriptenv->hasValue("out") == false)
    scriptenv->set("out", new DmiObject(inOf(System::out)));
  if (scriptenv->hasValue("err") == false)
    scriptenv->set("err", new DmiObject(inOf(System::err)));
  if (scriptenv->hasValue("in") == false)
    scriptenv->set("in", new DmiObject(inOf(System::in)));
}



int
Script::_readEval(/*IN(::acdk::io::RCharReader) in, */IN(RProps) props, int flags)
{
#ifdef LOCAL_DEBUG
  traceOn();
#endif
  if (props == Nil)
  {
    flags |= ScriptNoDefaultProps;
  }
  ExecutionStack::get()->startTransMetaInfo(flags);
  
  int stackIndex = ExecutionStack::get()->push(new ExecutionStackFrame(this, 0, 0));
  int ret = 0;
  bool inplaceProps = false;
  RProps scriptenv;
  if ((flags & ScriptReadParent) == ScriptReadParent && (flags & ScriptWriteParent) == ScriptWriteParent && props != Nil)
  {
    scriptenv = props;
    inplaceProps = true;
  }
  else if ((flags & ScriptReadParent) == ScriptReadParent && (flags & ScriptWriteParent) == 0)
    scriptenv = new Props("ScriptEnv", PropsParentRead | PropsNoParentWrite, props);
  else if ((flags & ScriptReadParent) == 0 && (flags & ScriptWriteParent) == 0)
    scriptenv = new Props("ScriptEnv", 0);
  else
    scriptenv = new Props("ScriptEnv", PropsParentRead | PropsNoParentWrite, props);
  scriptenv->setCastFlags(_castFlags);
  CurrentPropsResetter prs(*this, scriptenv);
  
  if ((flags & ScriptNoDefaultProps) == 0)
  {
    initAsEnvProps(scriptenv);
    
    _SetResetCfgVal _localProps(scriptenv, "__props", new DmiObject(inOf(scriptenv)));
    RObject thisObj = this;
    _SetResetCfgVal _script(scriptenv, "__script", new DmiObject(thisObj), PropsNoParentWrite);
    ret = _readEval2(scriptenv, inplaceProps);
  } else
    ret = _readEval2(scriptenv, inplaceProps);
  
  ExecutionStack::get()->pop(stackIndex);
  //_currentStack.set(Nil);
  return ret;
}

RString 
Script::_getCurrentAssertLine()
{
  return ExecutionStack::get()->top()->getScriptBackTrace(true, false);
}

void 
Script::assert(bool test)
{
  if (test == true)
    return;
  StringBuffer sb;
  sb << "Assertion: failed at: " << _getCurrentAssertLine();

  ACDK_NLOG("acdk.cfgscript.Script.assert", Error, sb.toString());
  THROW1(ScriptException, sb.toString());
}

void 
Script::assertTest(bool test)
{
  
  if (test == true)
  {
    StringBuffer sb;
    sb << "Assertion: OK at: " << _getCurrentAssertLine();
    ACDK_NLOG("acdk.cfgscript.Script.assert", Info, sb.toString());
    return;
  }
  StringBuffer sb;
  sb << "Assertion: failed at: " << _getCurrentAssertLine();

  ACDK_NLOG("acdk.cfgscript.Script.assert", Error, sb.toString());
  THROW1(ScriptException, sb.toString());
}

void
Script::assertExists(IN(RProps) props, IN(RString) variable)
{
  if (props->hasValue(variable) == true)
    return;
  StringBuffer sb;
  sb << "Assertion: variable " << variable << " not found at: " << _getCurrentAssertLine();
  ACDK_NLOG("acdk.cfgscript.Script.assert", Error, sb.toString());
  THROW1(ScriptException, sb.toString());
}

void
Script::assertTrue(bool test, IN(RString) msg)
{
  if (test == true)
    return;
   StringBuffer sb;
   sb << "Assertion: failed: " << msg << " at: " << _getCurrentAssertLine();

   
  THROW1(ScriptException, sb.toString());
}

void 
Script::testAssert(bool test)
{
  if (test == true)
  {
    ACDK_NLOG("acdk.tools.aunit", Trace, SBSTR("[AUNIT:SUCC][" << getName() << "] " << _getCurrentAssertLine()));
    return;
  }
  RString msg = SBSTR("[AUNIT:FAIL][" << getName() << "] " << _getCurrentAssertLine());
  ACDK_NLOG("acdk.tools.aunit", Error, msg);
  THROW1(ScriptException, msg);
}
void 
Script::testAssertComment(bool test, IN(RString) msg)
{
  if (test == true)
  {
    ACDK_NLOG("acdk.tools.aunit", Trace, SBSTR("[AUNIT:SUCC][" << getName() << "] " << msg << ": " << _getCurrentAssertLine()));
    return;
  }
  RString cmdmsg = SBSTR("[AUNIT:FAIL][" << getName() << "] " <<  msg << ": " << _getCurrentAssertLine());
  ACDK_NLOG("acdk.tools.aunit", Error, cmdmsg);
  THROW1(ScriptException, msg);
}


} // namespace cfgscript
} // namespace acdk


