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
// $Header: /cvsroot/acdk/acdk/acdk_perl/src/acdk/perl/PerlInterpreter.h,v 1.14 2005/04/08 10:53:20 kommer Exp $


#ifndef acdk_perl_PerlInterpreter_h
#define acdk_perl_PerlInterpreter_h

#include <acdk.h>
#include "Config.h"
#include <acdk/lang/dmi/ScriptInterpreter.h>

namespace acdk {
namespace perl {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(APerlInterpreter);
/** 
  Representing the Perl interpreter
  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.14 $
  @date $Date: 2005/04/08 10:53:20 $
*/



class ACDK_ACDK_PERL_PUBLIC APerlInterpreter
: public acdk::lang::Object,
  implements acdk::lang::dmi::ScriptInterpreter
{
protected:
  typedef void* Perl5Interpreter;
  Perl5Interpreter _pi;
public:
  APerlInterpreter();
  virtual ~APerlInterpreter();
  
    /**
    Parse a file. On some interpreter also execute the script.
    @param file the file to parse
  */
  virtual void parse(IN(RFile) file);
  /**
    Parse a file. On some interpreter also execute the script.
    @param script the script to parse
  */
  virtual void parse(IN(RString) script);
  /**
    Evaluate the code . On some interpreter it is equal to parse.
    @param script the script to eval
    @return the output or result of the script fragment
    @bug does't seem to work
  */
  virtual RObject eval(IN(RString) code);
  /**
    Calls a script function.
    Note: may not all script interpreter support this function
    @param func the name of the function
    @param args the arguments for the function
    @return result of the call
  */
  virtual acdk::lang::dmi::ScriptVar call(IN(RString) func, acdk::lang::dmi::ScriptVarArray& args);
  /**
    Calls a script method of given object.
    @param obj the 'this' object of the script, which is a wrapper to the scripting
    @param func the name of the function
    @param args the arguments for the function
    @return result of the call
  */
  virtual acdk::lang::dmi::ScriptVar invoke(IN(RObject) obj, IN(RString) func, acdk::lang::dmi::ScriptVarArray& args);
  /**
    Do debug the given Script. May not work on all scripting languages
    @param in Inputstream for users input and script
    @param out Scripts output and users Echo output
    @param err Error stream
  */
  virtual void interactive(IN(::acdk::io::RCharReader) in, IN(::acdk::io::RCharWriter) out, IN(::acdk::io::RCharWriter) err);
  
  
  void parse(IN(RStringArray) args);
  int run();
  
  
  
};
  
} //namespace perl 
} // namespace acdk

#endif // acdk_perl_PerlInterpreter_h
