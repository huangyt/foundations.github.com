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
// $Header: /cvsroot/acdk/acdk/acdk_java/tests/acdk/java/serialization/sample/Main.cpp,v 1.3 2005/02/05 10:45:12 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/FileWriter.h>

#include <acdk/java/serialization/JavaObjectReader.h>
#include <acdk/java/serialization/JavaObjectWriter.h>
#include "JavaSerSample.h"

namespace acdk {
namespace java {
namespace serialization {
namespace sample {

class Main
: extends acdk::lang::Object
{
public:
  /**
    write JavaSerSample and JavaSerSampleArray to be read by Java
  */
  static void writeJavaSerSample()
  {
    RString acdkhome = System::getProperties()->getProperty("ACDKHOME");
    {
      RString rfname = acdkhome + "/acdk_java/tests/acdk/java/serialization/sample/JavaSerSample_a2j.dat";
      ::acdk::io::FileWriter fout(rfname);
      ::acdk::java::serialization::JavaObjectWriter jout(&fout);
      RJavaSerSample sample = new JavaSerSample(44, 45);
      jout.writeObject(&sample);
    }
    {
      RString rfname = acdkhome + "/acdk_java/tests/acdk/java/serialization/sample/JavaSerSampleArray_a2j.dat";
      ::acdk::io::FileWriter fout(rfname);
      ::acdk::java::serialization::JavaObjectWriter jout(&fout);
      RJavaSerSampleArray sampleArray = new JavaSerSampleArray(2);
      sampleArray[0] = new JavaSerSample(50, 51);
      sampleArray[1] = new JavaSerSample(52, 53);
      jout.writeObject(&sampleArray);
    }
  }
  /**
    read JavaSerSample and JavaSerSampleArray wrote by Java
  */
  static void readJavaSerSample()
  {
    RString acdkhome = System::getProperties()->getProperty("ACDKHOME");
    {
      RString rfname = acdkhome + "/acdk_java/tests/acdk/java/serialization/sample/JavaSerSample_j2a.dat";
      ::acdk::io::FileReader fin(rfname);
      ::acdk::java::serialization::JavaObjectReader jin(&fin);
      RObject obj = jin.readObject();
      System::out->println("JavaSerSample readed: " + obj->getClass()->getName() + ": " + obj->toString());
      
      RJavaSerSample sample = (RJavaSerSample)obj;

    }
    {
      RString rfname = acdkhome + "/acdk_java/tests/acdk/java/serialization/sample/JavaSerSampleArray_j2a.dat";
      ::acdk::io::FileReader fin(rfname);
      ::acdk::java::serialization::JavaObjectReader jin(&fin);
      RObject obj = jin.readObject();
      System::out->println("JavaSerSample Array readed: " + obj->getClass()->getName() + ": " + obj->toString());
      RJavaSerSampleArray samplearray = (RJavaSerSampleArray)obj;
    }
  }
  static int acdkmain(RStringArray args)
  {
    try {
      writeJavaSerSample();
      readJavaSerSample();
    } catch (::acdk::io::RIOException ex) {
      ex->printStackTrace();
      System::out->println("IOException caught: " + ex->getMessage());
    }
    return 0;
  }

};

}
}
}
}

int
main(int argc, char* argv[], char** envptr)
{
  return ::acdk::lang::System::main(acdk::java::serialization::sample::Main::acdkmain, argc, argv, envptr);
} 
