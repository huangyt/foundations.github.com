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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/ScriptInterpreter.h,v 1.12 2005/04/08 10:53:19 kommer Exp $

#ifndef acdk_lang_dmi_ScriptInterpreter_h
#define acdk_lang_dmi_ScriptInterpreter_h

#include <acdk.h>
#include <acdk/io/CharReader.h>
#include <acdk/io/CharWriter.h>
#include <acdk/io/File.h>


namespace acdk {
namespace lang {
namespace dmi {

using namespace acdk::lang;
USING_CLASS(acdk::io::, File);
USING_CLASS(acdk::io::, CharReader);
USING_CLASS(acdk::io::, CharWriter);
//using namespace acdk::io;

ACDK_DECL_INTERFACE(ScriptInterpreter);

/** 
  Interface to a script interpreter like Lisp, Perl, PHP, tcl, Java, etc.
  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/04/08 10:53:19 $
*/

class ACDK_CORE_PUBLIC ScriptInterpreter
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ScriptInterpreter)
public:
  /**
    Parse a file. On some interpreter also execute the script.
    @param file the file to parse
  */
  virtual void parse(IN(RFile) file) = 0;
  /**
    Parse a file. On some interpreter also execute the script.
    @param script the script to parse
  */
  virtual void parse(IN(RString) script) = 0;
  /**
    Evaluate the code . On some interpreter it is equal to parse.
    @param script the script to eval
    @return the output or result of the script fragment
  */
  virtual RObject eval(IN(RString) code) = 0;
  /**
    Calls a script function.
    Note: may not all script interpreter support this function
    @param func the name of the function
    @param args the arguments for the function
    @return result of the call
  */
  foreign virtual acdk::lang::dmi::ScriptVar call(IN(RString) func, acdk::lang::dmi::ScriptVarArray& args) 
  { 
    ObjectBase::_throwNotImplementedYet("ScriptInterpreter::call"); 
    return acdk::lang::dmi::ScriptVar();
  }
  /**
    Calls a script method of given object.
    @param obj the 'this' object of the script, which is a wrapper to the scripting
    @param func the name of the function
    @param args the arguments for the function
    @return result of the call
  */
  foreign virtual acdk::lang::dmi::ScriptVar invoke(IN(RObject) obj, IN(RString) func, acdk::lang::dmi::ScriptVarArray& args) 
  {
    ObjectBase::_throwNotImplementedYet("ScriptInterpreter::invoke"); 
    return acdk::lang::dmi::ScriptVar();
  }
  /**
    Do debug the given Script. May not work on all scripting languages
    @param in Inputstream for users input and script
    @param out Scripts output and users Echo output
    @param err Error stream
  */
  virtual void interactive(IN(RCharReader) in, IN(RCharWriter) out, IN(RCharWriter) err) = 0;
};

} // namespace dmi 
} // namespace lang
} // namespace acdk 




#endif //acdk_lang_dmi_ScriptInterpreter_h
