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


#include "PlattformSelectTask.h"
#include "LookupFileTask.h"
#include "ShellExecuteTask.h"

#include <acdk/lang/System.h>
#include <acdk/io/NullWriter.h>
#include <acdk/io/StringWriter.h>
#include <acdk/cfgscript/Script.h>


namespace acdk {
namespace make {

using namespace acdk::cfgscript;



RStringArray 
PlattformSelectTask::getAvailableTargets(IN(RProps) globals)
{
  initTargetProps(globals);
  return _targetProps->getKeys();
}


RProps 
PlattformSelectTask::getTargetSpecs(IN(RProps) globals, IN(RString) targetname)
{
  initTargetProps(globals);
  return _targetProps->getProps(targetname);
}

void _initTargetProps(IN(RProps) target, IN(RProps) source, IN(RProps) master)
{
  RStringArray keys = source->getKeys();
  
  if (source->hasValue("_parent") == true)
  {
    RStringArray p = source->getStringArrayVal("_parent");
    for (int j = 0; j < p->length(); ++j)
    {
      _initTargetProps(target, master->getProps(p[j]), master);
    }
  }
  for (int i = 0; i < keys->length(); ++i)
  {
    RString k = keys[i];
    if (k->equals("_parent") == false)
    {
      if (k->equals("AMAKE_TARGET") == true)
      {
        //master->appendStringArrayVal("AMAKE_TARGET_TAGS", source->getStringVal(k), PropsNoStringDups | PropsNoParentWrite);
        target->appendStringArrayVal("AMAKE_TARGET_TAGS", source->getStringVal(k), PropsNoStringDups | PropsNoParentWrite | PropsNoWarnRead);
      }
      //if (source != target && target->hasValue(k) == false)
      /*if (k->equals("AMAKE_TARGET_TAGS") == true)
      {
        source->dump();
      } 
      else*/
        target->set(k, source->get(k), PropsNoParentWrite);
    }
  }
  
}

void
PlattformSelectTask::initTargetProps(IN(RProps) props)
{
  if (_targetProps != Nil)
    return;
  RProps platform = props->getProps("AMAKE_TARGET_PROPS", PropsParentRead | PropsNoWarnRead);
  RString cfgfile = "compile_specs.csf";
  Script script(cfgfile);
  
  script.currentProps = new Props("AMAKE_TARGET_PROPS", PropsParentRead | PropsNoParentWrite, props);

  script.include(cfgfile);
  RProps sourcespecs = props->getProps("AMAKE_TARGET_PROPS");
  RProps targetspecs = new Props("AMAKE_TARGET_PROPS", PropsParentRead | PropsNoParentWrite);
  RStringArray targets = sourcespecs->getKeys(PropsNoParentRead);
  for (int i = 0; i < targets->length(); ++i)
  {
    RString t = targets[i];
    RProps source = sourcespecs->getProps(t);
    RProps target = new Props(t);
    //source->dump();
    _initTargetProps(target, source, sourcespecs);
    //System::out->println("\n\n\n" + t + ":\n");
    //target->dump();
    targetspecs->setProps(t, target);
  }
  props->setProps("AMAKE_TARGET_PROPS", targetspecs);
  _targetProps = targetspecs;
}




bool
determinePlatform(IN(RProps) props)
{
  RString osstr = System::getProperties()->getProperty("OSTYPE");
  if (osstr == Nil && props->hasValue("OSTYPE", PropsParentRead) == true)
    osstr = props->getStringVal("OSTYPE", PropsParentRead);

  if (osstr == Nil)
  {
#if defined(ACDK_OS_BSD)
    osstr = "freebsd";
#elif defined(ACDK_OS_LINUX)
    osstr = "linux";
#else
    osstr = Nil;
#endif
  }
  //System::out->println("OSTYPE: " + osstr);
  /*
    if (osstr == Nil)
  {
    const char* ptr = getenv("OSTYPE");
    std::cout << "OSTYPE: " << ptr << std::endl;
    osstr = new String(ptr, String::Normal);
  }
  */
  if (osstr != Nil && (osstr->equalsIgnoreCase("linux") || 
		       osstr->equalsIgnoreCase("gnu-linux") ||
		       osstr->equalsIgnoreCase("linux-gnu")))
  {
    ACDK_NLOG("acdk.make", Info, "Select platform linux");
    props->setStringVal("OS", "linux", PropsNoFlags);
    return true;
  } 
  else if (osstr != Nil && 
	   (osstr->startsWith("freebsd") == true || 
	    osstr->startsWith("FreeBSD") == true))
  {
    ACDK_NLOG("acdk.make", Info, "Select platform bsd");
    props->setStringVal("OS", "bsd", PropsNoFlags);
    return true;
  }
  else if (osstr != Nil && osstr->startsWith("solaris") == true)
  {
    ACDK_NLOG("acdk.make", Info, "Select platform solaris");
    props->setStringVal("OS", "solaris", PropsNoFlags);
    return true;
  }
  else 
  {
    osstr = System::getProperties()->getProperty("OS");
    if (osstr != Nil && osstr->equals("Windows_NT"))
    {
      props->setStringVal("OS", "nt", PropsNoFlags);
      return true;
    } else {
      ACDK_NLOG("acdk.make", Error, 
		"Cannot determine Platform. Try to set env varialbe AMAKE_TARGET");
      return false;
    }
  }
  return false;
}



bool
locateTargetElement(IN(RProps) props, IN(RString) el)
{
  if (props->hasValue(el, PropsParentRead) == true)
    return true;
  //props->dump(0);
  RString compilername = props->getStringVal(el + "_BASENAME", PropsParentRead);
  LookupFileTask lft(compilername, Executable);
  /*
  if (props->hasValue(el + "_SPATHS") == true)
  {
  }*/
  if (lft.execute("locate", props) == false)
    return false;
  props->setStringVal(el, lft.foundPath(), PropsNoFlags);
  return true;
}

struct ParentOnStack
{
  RProps _props;
  RProps _parent;
  ParentOnStack(IN(RProps) props, IN(RProps) parent)
  : _props(props)
  , _parent(parent)
  {
    props->addParentProps(parent);
  }
  ~ParentOnStack()
  {
    _props->removeParentProps(_parent);
  }
};

RProps
locateTarget(IN(RProps) props, IN(RString) tag, bool warn = false)
{
  RProps platforms = props->getProps("AMAKE_TARGET_PROPS", PropsParentRead);
  RProps pp = platforms->getProps(tag, PropsParentRead);
  ParentOnStack _pos(pp, props);
  if (pp == Nil)
  {
    if (warn) 
      ACDK_NLOG("acdk.make", Error, "No target settings for " + tag);
    return Nil;
  }
  if (locateTargetElement(pp, "CCC_COMPILER") == false)
  {
    if (warn) 
      ACDK_NLOG("acdk.make", Error, "Cannot locate Compiler: "  + pp->getStringVal("CCC_COMPILER_BASENAME", PropsParentRead));
    return Nil;
  }
  if (locateTargetElement(pp, "CCC_LINKER") == false)
  {
    if (warn) 
      ACDK_NLOG("acdk.make", Error, "Cannot locate Linker: "  + pp->getStringVal("CCC_LINKER_BASENAME", PropsParentRead));
    return Nil;
  }
  if (locateTargetElement(pp, "CCC_AR") == false)
  {
    if (warn) 
      ACDK_NLOG("acdk.make", Error, "Cannot locate Linker: "  + pp->getStringVal("CCC_AR_BASENAME", PropsParentRead));
    return Nil;
  }
  if (locateTargetElement(pp, "SHELL") == false)
  {
    if (warn) 
      ACDK_NLOG("acdk.make", Error, "Cannot locate Shell: " + pp->getStringVal("SHELL_BASENAME", PropsParentRead));
    return Nil;
  }
  return pp;
}

bool
determineCompiler(IN(RProps) props)
{
  RString os = props->getStringVal("OS", PropsParentRead);
  RString ostype = props->getStringVal("OSTYPE", PropsParentRead);
  RProps cfgprops;
  
  if (os->equals("nt") == true)
  {
    cfgprops = locateTarget(props, "vc");
    if (cfgprops != Nil)
    {
      RShellExecuteTask sh = new ShellExecuteTask("cl", "gather version info", cfgprops->getQuotedStringVal("CCC_COMPILER", PropsParentRead), 0);
      sh->setErrWriter(new acdk::io::StringWriter());
      sh->setOutWriter(new acdk::io::StringWriter());
      sh->execute("",cfgprops);
      RString str = sh->getErrString();
      if (str->indexOf("Version 13.") != -1)
      {
        cfgprops = locateTarget(props, "vc7");
      }
      else if (str->indexOf("Version 12.") != -1)
      {
        // nothing
      } 
      else
      {
        ACDK_NLOG("acdk.make", Error, "Unknown/Unsupported MSC version: " + str);
      }
      props->merge(cfgprops, PropsMergeOverWrite);
      return true;
    }
    cfgprops = locateTarget(props, "bcc");
    if (cfgprops != Nil)
    {
      props->merge(cfgprops, PropsMergeOverWrite);
      return true;
    }
  } 
  else if (os->equals("solaris") == true || ostype->startsWith("solaris") == true)
  {
    cfgprops = locateTarget(props, "sungcc");
    if (cfgprops != Nil)
    {
      props->merge(cfgprops, PropsMergeOverWrite);
      return true;
    }
  }
  else if (os->equals("linux") == true || os->equals("bsd") == true)
  {
    cfgprops = locateTarget(props, os);
    if (cfgprops != Nil)
    {
      props->merge(cfgprops, PropsMergeOverWrite);
      return true;
    }
  }
  ACDK_NLOG("acdk.make", Error, "Cannot determine Compiler");
  return false;
}

bool
determineAcdkMc(IN(RProps) props)
{
  if (props->hasValue("ACDKMC", PropsParentRead) == true)
    return true;
  if (LookupFileTask("acdkmc", AcdkExecutable).execute(Nil, props) == false)
  {
    ACDK_NLOG("acdk.make", Warn, "Cannot find acdkmc");
    return false;
  }
  props->setStringVal("ACDKMC", props->getStringVal("_located", PropsNoFlags), PropsNoFlags);
  return true;
}


//virtual 
bool 
PlattformSelectTask::execute(IN(RString) exec, IN(RProps) props)
{
  //props->dump();
  //System::in->readLine();
  initTargetProps(props);
  //props->dump();
  RString acdkhome = props->getAcdkHome();
  props->setStringVal("ACDK_HOME", acdkhome, PropsNoFlags);
  props->setStringVal("ACDK_TOOLS_HOME", props->getAcdkToolsHome());

  if (determinePlatform(props) == false)
    return false;
  if (props->hasValue("AMAKE_TARGET", PropsParentRead) == false)
  {
    ACDK_NLOG("acdk.make", Trace, "AMAKE_TARGET not defined. Try to determine compiler");
    if (determineCompiler(props) == false)
      return false;
  } else  {
    ACDK_NLOG("acdk.make", Trace, "AMAKE_TARGET defined: " + props->getStringVal("AMAKE_TARGET", PropsParentRead));
    RProps cfgprops = locateTarget(props, props->getStringVal("AMAKE_TARGET", PropsParentRead), true);
    if (cfgprops == Nil)
      return false;
    props->merge(cfgprops, PropsMergeOverWrite);
  }
  RStringArray ptags = props->getStringArrayVal("AMAKE_TARGET_TAGS", PropsParentRead);
  ACDK_NLOG("acdk.make", Info, "Select target Tags: " + ptags->toString());

  RProps cfgprops = props->getProps("AMAKE_TARGET_PROPS", PropsParentRead)->getProps(props->getStringVal("AMAKE_TARGET", PropsParentRead), PropsParentRead);
  //RStringArray platformtags = cfgprops->getAllStringVals("AMAKE_TARGET");
  //props->setStringArrayVal("AMAKE_TARGET_TAGS", platformtags);
  props->setQuotedStringVal("CCC_COMPILER", props->getStringVal("CCC_COMPILER", PropsParentRead), PropsNoFlags);
  props->setQuotedStringVal("CCC_LINKER", props->getStringVal("CCC_LINKER", PropsParentRead), PropsNoFlags);
  props->setQuotedStringVal("CCC_AR", props->getStringVal("CCC_AR", PropsParentRead), PropsNoFlags);
  props->setQuotedStringVal("SHELL", props->getStringVal("SHELL", PropsParentRead), PropsNoFlags);
  
  determineAcdkMc(props);
  return true;
}



} // namespace make
} // namespace acdk



