// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Id: HelloWorld.cpp,v 1.5 2005/02/05 10:44:52 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Double.h>
#include <acdk/boot/HelloWorldClass.h>


namespace acdk {


using namespace acdk::lang;
using namespace acdk::boot;
using namespace acdk::lang::reflect;

ACDK_DECL_CLASS(HelloWorld);

/** 
  HelloWorld Main-Class
  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:44:52 $
  @bug none known
*/

class HelloWorld
: public ::acdk::lang::Object
{
public:
  static RHelloWorldInterface getHello();
  static int acdkmain(RStringArray args); 
};

//static 
RHelloWorldInterface 
HelloWorld::getHello()
{
  return new HelloWorldClass("Greetings!");
}

//static 
int 
HelloWorld::acdkmain(RStringArray args)
{
  try {
    RHelloWorldInterface  h = getHello();
    h->sayHello("World");
  } catch (RThrowable ex) {
  }
  
  return 0;
}

} // namespace acdk




int
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(acdk::HelloWorld::acdkmain, argc, argv, envptr);
}


