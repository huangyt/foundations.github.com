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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/interpreter/IACDKLisp.cpp,v 1.13 2005/05/09 13:47:58 kommer Exp $


#include <acdk.h>

#include <acdk/lang/System.h>
#include <acdk/lang/ClassLoader.h>
#include <acdk/lang/SharedLibrary.h>
#include <acdk/io/ConsoleWriter.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/lang/Boolean.h>

#include <acdk/lisp/lisp.h>
#include <acdk/lisp/LispEnvironment.h>

namespace acdk {
namespace lisp {

using namespace acdk::lang;
using namespace acdk::io;


/** 
  Interactive Lisp Interpreter
  API: ACDK<br>
  
  @author Roger Rene Kommer
  @version $Revision: 1.13 $
  @date $Date: 2005/05/09 13:47:58 $
  @bug Incomplete
  
*/
class IACDKLisp
: public ::acdk::lang::Object
{
public:
  static int doit(RStringArray args);
  static void help();
};

void 
IACDKLisp::help()
{
  StringBuffer sb;
  sb << "acdklisp [options] [lispfiles]\n\n"
        "  Where options are:\n"
        "  -nostdlisp              don't load standard lisp files\n"
        "  -loadcode               load standard Lisp code instead of image\n"
        "  -loadlisp <lispfile>    Load the given lispfile\n"
        "  -loadimage <imagefile>  Load the given image file\n"
        "  -dumpimage <imagefile>  Dump the the loaded env into image\n"
        "  -trace                  Trace Lisp evaluation\n"
        "  -interactive            Enter interactive mode after loading\n"
        ;
  System::out->println(sb.toString());
}


//static 
int 
IACDKLisp::doit(RStringArray args)
{
  try {
    RLispEnvironment lenv = new LispEnvironment(System::getProperties(), args, false);
    RStringArray lispfiles = new StringArray(0);
    RStringArray imagefiles = new StringArray(0);
    int arglength = args->length();
    bool nostdlisp = false;
    bool interactive = false;
    RString dumpImage = Nil;
    bool readonelispfile = false;
    bool loadstdlispcode = false;
    int i;
    for (i = 1; i < arglength; ++i)
    {
      
      if (args[i]->equals("-loadlisp") == true)
      {
        if (arglength == ++i) { help(); return 1; }
        lispfiles->append(args[i]);
      } 
      else if (args[i]->equals("-loadimage") == true) 
      {
        if (arglength == ++i) { help(); return 1; }
        imagefiles->append(args[i]);
      } 
      else if (args[i]->equals("-dumpimage") == true) 
      {
        if (arglength == ++i) { help(); return 1; }
        dumpImage = args[i];
      } 
      else if (args[i]->equals("-nostdlisp") == true) 
      {
        nostdlisp = true;
      } 
      else if (args[i]->equals("-loadcode") == true) 
      {
        loadstdlispcode = true;
      } 
      
      else if (args[i]->equals("-interactive") == true) 
      {
        interactive = true;
      } 
      else if (args[i]->equals("-trace") == true) 
      {
        lenv->trace(true);
      } 
       else if (args[i]->equals("-help") == true) 
      {
        help();
        return 0;
      } 
      else if (args[i]->startsWith("-") == true) 
      {
        /*System::out->println("Unknown option: " + args[i]);
        help();
        return 1;
        */
      } 
      else 
      {
        if (readonelispfile == false)
        {
          lispfiles->append(args[i]);
          readonelispfile = true;
        } else {
          ;// nothing
        }
      }
    }
    if (nostdlisp == false)
    {
      lenv->init(loadstdlispcode);
    }
    for (i = 0; i < imagefiles->length(); ++i)
    {
      System::out->println("load image: " + imagefiles[i]);
      lenv->loadCompiled(imagefiles[i], false);
    }
    
    for (i = 0; i < lispfiles->length(); ++i)
    {
      System::out->println("load lispfile: " + lispfiles[i]);
      lenv->load(lispfiles[i]);
    }
    if (dumpImage != Nil)
    {
      lenv->storeCompiled(dumpImage);
    }
    if (interactive == true)
    {
      lenv->interactive(&System::in, &System::out);
    }
    else if (readonelispfile == false && lispfiles->length() == 0)
    {
      help();
    }
  } catch (RThrowable ex) {
    ex->printStackTrace(System::err);
    System::err->println(ex->getMessage());
  }
  //int i;
  //std::cin >> i;
  return 0;
}

} //namespace lisp 
} //namespace acdk 





int
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(acdk::lisp::IACDKLisp::doit, argc, argv, envptr);
}
