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
// $Header: /cvsroot/acdk/acdk/acdk_tcl/src/acdk/tcl/interpreter/Main.cpp,v 1.10 2005/02/05 10:45:32 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include "../TclInterpreter.h"
#include "tcl.h"

namespace acdk {
namespace tcl {
namespace interpreter {

class Main
: public ::acdk::lang::Object
{
public:
  static int acdkmain(RStringArray args)
  {
    RTclInterpreter pi = new TclInterpreter();
    if (args->length() > 1) {
      pi->parse(new acdk::io::File(args[1]));
      return 0;
      //return pi->run();
    } 
    pi->interactive((::acdk::io::RCharReader)System::in, (::acdk::io::RCharWriter)System::out, (::acdk::io::RCharWriter)System::err);    

    return 0;
  }
};



} // namespace interpreter
} // namespace tcl
} // namespace acdk 


int
main(int argc, char* argv[], char** envptr)
{
  return ::acdk::lang::System::main(acdk::tcl::interpreter::Main::acdkmain, argc, argv, envptr);
} 
