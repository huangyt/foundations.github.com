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


#include "ScriptExecuteTask.h"
#include <acdk/cfgscript/Script.h>


namespace acdk {
namespace make {

//virtual 
bool 
ScriptExecuteTask::execute(IN(RString) exec, IN(RProps) props)
{
  ::acdk::io::File tf(_fname);
  if (tf.exists() == false)
  {
    ACDK_NLOG("acdk.make", Error, "Script file doesn't exists: " + _fname);
    return false;
  }
  acdk::cfgscript::RScript script;
  try {
    script = new acdk::cfgscript::Script(_fname);
    return script->readEval(props, _flags) == 0;
  } catch(RThrowable ex) {

    ACDK_NLOG("acdk.make", Error, "Error in Script: " + _fname + acdk::cfgscript::Script::getScriptBackTrace());
    throw;
  }
}
  



} // namespace make
} // namespace acdk



