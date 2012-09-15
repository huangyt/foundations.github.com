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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_Scanner_Test.cpp,v 1.6 2005/02/05 10:44:51 kommer Exp $


#include <acdk.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>


#include <acdk/aci/Compiler.h>
#include <acdk/io/MemReader.h>
#include <acdk/lang/System.h>
#include <acdk/text/RegExp.h>

namespace tests {
namespace acdk {
namespace aal {

using namespace ::acdk::aci;

// Declare test cases
BEGIN_DECLARE_TEST( Scanner_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( string )
  DECLARE_TEST( interactive )
  DECLARE_TEST( cComments )
END_DECLARE_TEST( Scanner_Test  )

BEGIN_DEFINE_TEST( Scanner_Test )
  ADD_TEST( Scanner_Test, standard ) 
  ADD_TEST( Scanner_Test, string ) 
  ADD_TEST( Scanner_Test, interactive ) 
  ADD_TEST( Scanner_Test, cComments ) 
  
END_DEFINE_TEST( Scanner_Test )


void fetchStream(Scanner& scanner, IN(RString) text)
{
  scanner.setInBuffer(text);
  std::cout << "Scan text: " << text->c_str() << std::endl;
  ScannerTokenStack ss(&scanner);

  ScannerToken* st;
  while ((st = ss.fetch()) && st->token != Scanner::TT_EOF)
  {
    std::cout << "(" << st->token << "): " << st->sval->c_str() << std::endl;
  }
}

void 
Scanner_Test::standard()
{
  Scanner scanner("");
  enum Token
  {
    Plus = -10,
    Minus = -11,
    Dot = -12,
    Digits = -13,
    WhiteSpace = -14,
    AA = -20
  };
  scanner.registerTerminal("a", AA);
  scanner.registerTerminal("\\-", Minus);
  scanner.registerTerminal("\\.", Dot);
  scanner.registerTerminal("\\d+", Digits);
  scanner.registerTerminal("[\n\t\r ]+", WhiteSpace);
  RString text = "aa";
  //fetchStream(scanner, text);
  
  text = "9134";
  //fetchStream(scanner, text);

  text = "-9134.123 a";
  fetchStream(scanner, text);

}

void 
Scanner_Test::string()
{
  Scanner scanner("");
  scanner.registerTerminal(new StringTerminal());
  RString text = "\"My name is\\t\\\"AAL\\\"\"";
  fetchStream(scanner, text);
}

void
Scanner_Test::interactive()
{
#ifdef TEST_INTERACTIVE
  do 
  {
    System::out->print("Input RegEx: "); System::out->flush();
    RString ins = System::in->readLine();
    if (ins->length() < 1)
      return;
    ::acdk::text::RegExp regex(ins);
    
    do 
    {
      System::out->print("Input MatchTest: ");System::out->flush();
      RString text = System::in->readLine();
      if (text->length() == 0)
        break;
      int i = regex.matchSize(text, 0);
      System::out->println(RString("Matched: ") + i);
    } while(true);
  } while(true);
#endif
}

void
Scanner_Test::cComments()
{
  enum Token
  {
    Plus = -10,
    Minus = -11,
    Dot = -12,
    Digits = -13,
    WhiteSpace = -14,
    AA = -20
  };
  Scanner scanner("");
  scanner.registerTerminal("a", AA);
  scanner.registerTerminal(new CCommentTerminal());
  scanner.registerTerminal(new CxxCommentTerminal());
  RString text = "a/* asdf */a";
  fetchStream(scanner, text);
  text = "aa//  asdfasdf a\na// asdf";
  fetchStream(scanner, text);
}

} // namespace aal
} // namespace acdk
} // namespace tests

