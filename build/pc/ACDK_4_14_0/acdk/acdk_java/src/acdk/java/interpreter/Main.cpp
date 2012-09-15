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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/interpreter/Main.cpp,v 1.14 2005/04/13 15:37:24 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>

#include "../JavaInterpreter.h"
#include "../JavaObject.h"

#include <jni.h>

namespace acdk {
namespace java {
/**
  Executable to a Java VM integrated in C++. Experimental.
*/
namespace interpreter {

#define USER_CLASSPATH "."
#define PATH_SEPARATOR ';' 



class Main
: public ::acdk::lang::Object
{
public:
  static void showWindow(IN(RJavaInterpreter) ji)
  {
    ScriptVarArray args(1);
    args[0] = (RObject)RString("Hallo from Java");
    RJavaObject frame = ji->getInstance("java/awt/Frame", args);
    {
      frame->invoke("setSize", 300, 200);
      frame->invoke("show");
      int wait = 5;
      while (wait) {
        --wait;
        Thread::sleep(1000);
      }
      frame->invoke("dispose");
      // invoke??? sun/awt/windows/WToolkit.shutdown
    }
  }  
    
  static void testJava(IN(RJavaInterpreter) ji)
  {
    {

      RObject jsb = (RObject)Object::New("acdk/java/JavaObject", (const char*)"java/lang/StringBuffer", (const char*)"Hello");
      jsb->invoke("append", (const char*)" Java from ACDK");
      RObject javaout = (RObject)Object::invoke_static("acdk/java/JavaObject", "peek_static", (const char*)"java/lang/System", (const char*)"out");
      RObject obj = (RObject)jsb->invoke("toString");
      javaout->invoke("println", obj);
    }
    
    //showWindow(ji);
    RJavaObject jo;
    {
      ScriptVar erg = JavaObject::peek_static(ji, "java/lang/System", "out");
      RJavaObject javaout = (RJavaObject)erg.getObjectVar();
      ScriptVarArray args(1);
      args[0] = (RObject)RString("Hallo from Java");
      javaout->invoke("println", args);
    }
    {
      ScriptVarArray args(1);
      args[0] = (RObject)RString("Hallo");
      jo = ji->getInstance("java/lang/StringBuffer", args);
    }
    {
      ScriptVarArray args(0);
      ScriptVar erg = jo->invoke("hashCode", args);
      erg = jo->invoke("toString", args);
      System::out->println(jo->toString());
    }
  }
  static int acdkmain(RStringArray args)
  {
    RJavaInterpreter pi = new JavaInterpreter();
    testJava(pi);
    return 0;
    if (args->length() > 1) {
      RString clazz = args[1];
      /*dmi::ScriptVarArray sargs(args->length() - 2);
      for (int i = 2; i < args->length(); i++)
        args[i - 2] = args[i]; */
      pi->callMain(clazz, args);
      return 0;
    } 
    //pi->parse(Nil);
    //pi->interactive(System::in, System::out, System::err);    
    return 1;
  }
};



} // namespace interpreter
} // namespace perl 
} // namespace acdk 


int
main(int argc, char* argv[], char** envptr)
{
  return ::acdk::lang::System::main(acdk::java::interpreter::Main::acdkmain, argc, argv, envptr);
} 
