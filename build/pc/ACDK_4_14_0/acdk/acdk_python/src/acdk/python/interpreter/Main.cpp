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
// $Header: /cvsroot/acdk/acdk/acdk_python/src/acdk/python/interpreter/Main.cpp,v 1.9 2005/02/06 12:56:40 kommer Exp $



#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/CmdLineParser.h>

#include <acdk/python/PythonInterpreter.h>

namespace acdk {
namespace python {
namespace interpreter {

  
USING_CLASS(::acdk::python::, PythonInterpreter);




class Main
: public ::acdk::lang::Object
{
public:
  static int acdkmain(RStringArray args)
  {
    CmdLineParser cmdline;
    cmdline.addOption(  "-e", "PYTHON_CODE_STRING", true
                      , "evaluate the given string as Python code");
    ::acdk::util::RProperties props = cmdline.parse(args, false, true);
    RString code = props->getProperty("PYTHON_CODE_STRING");
    if (code != Nil) {
      PythonInterpreter pi;
      pi.parse(code);
      return pi.getLastReturnCode();
    }
    if (args->length() < 2) {
      PythonInterpreter pi;
      pi.interactive((acdk::io::RCharReader)System::in, (acdk::io::RCharWriter)System::out, (acdk::io::RCharWriter)System::err);    
      return pi.getLastReturnCode();
    }
    RString fname = args[1];
    PythonInterpreter pi;
    pi.parse(fname);
    return pi.getLastReturnCode();
  }
};



} // namespace interpreter 
} // namespace python 
} // namespace acdk 


int
main(int argc, char* argv[], char** envptr)
{
  return ::acdk::lang::System::main(acdk::python::interpreter::Main::acdkmain, argc, argv, envptr);
} 
