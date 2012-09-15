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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_StringBuffer_Test.cpp,v 1.16 2005/02/05 10:45:08 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>





namespace tests {
namespace acdk {
namespace lang {
 


BEGIN_DECLARE_TEST( StringBuffer_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( equalsTest )
  DECLARE_TEST( replaceChar )
  DECLARE_TEST( replaceString )
  DECLARE_TEST( fromByteArray )
  DECLARE_TEST( streaming )
  DECLARE_TEST( appending )
END_DECLARE_TEST( StringBuffer_Test )

BEGIN_DEFINE_TEST( StringBuffer_Test )
  ADD_TEST( StringBuffer_Test, standard ) 
  ADD_TEST( StringBuffer_Test, equalsTest ) 
  ADD_TEST( StringBuffer_Test, replaceChar ) 
  ADD_TEST( StringBuffer_Test, replaceString ) 
  ADD_TEST( StringBuffer_Test, fromByteArray ) 
  ADD_TEST( StringBuffer_Test, streaming ) 
  ADD_TEST( StringBuffer_Test, appending ) 
  
END_DEFINE_TEST( StringBuffer_Test )

USING_CLASS(::acdk::lang::, Throwable);
USING_CLASS(::acdk::lang::, Exception);

void StringBuffer_Test::standard()
{
  {
    StringBuffer sb;
    sb.append("Hello");
    testAssert(sb.toString()->equals("Hello"));
    sb.append(" World");
    testAssert(sb.toString()->equals("Hello World"));
    sb.set("ACDK");
    testAssert(sb.toString()->equals("ACDK"));
    sb.append(' ');
    testAssert(sb.toString()->equals("ACDK "));

  }
  {
    RString s = SBSTR("ABC" << 42);
    testAssert(s->equals("ABC42") == true);
    char* text1 = "sadf";
    const char* text2 = "asdf";
    s = SBSTR(text1);
    s = SBSTR(text2);
    s = SBSTR(text2 << text2);
    s = SBSTR(text1 << text2);
  }
}


void
StringBuffer_Test::equalsTest() 
{
  
  
}

void
StringBuffer_Test::replaceChar()
{
}

void
StringBuffer_Test::replaceString()
{

}




void StringBuffer_Test::fromByteArray()
{
}



void
StringBuffer_Test::streaming()
{
  StringBuffer sb;
  RString sval = " [text] ";
  RInteger iobj = new Integer(42);
  RObject obj;
  sb << "This is a Text";
  sb << 12;
  sb << " ";
  sb << 3.5;
  sb << sval;
  sb << obj;
  sb << iobj;
  sb << new Integer(42);
  RString erg = sb.toString();


  RString erg2 = SBSTR("This is a Text" << 12 << " " << 3.5 << sval << obj << iobj << new Integer(42));
  testAssert(erg2->equals(erg) == true);
}

void
StringBuffer_Test::appending()
{
  RString first = "A";
  RString second = "B";
  second = second->convert(CCUcs2);
  {
    StringBuffer sb;
    RString text = "asdfasdfasdfasdfasdfasdf";
    for (int i = 0; i < 40; ++i)
    {
      sb << "[" << i << "=[" << first << text << second;
    }
    System::out->println(sb.toString());
  }
  {
    
    StringBuffer sb(first);
    testAssert(sb.length() == 1);
    sb.append(second);
    testAssert(sb.length() == 2);
    testAssert(sb.toString()->equals("AB") == true);
  }
  {
    StringBuffer sb;
    sb << first << "B" << second << "C";
    testAssert(sb.length() == 4);
  }
}

} // namespace lang 
} //namespace acdk 
} //namespace tests 


