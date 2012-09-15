// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/TestACDKObject.cpp,v 1.3 2003/06/19 14:37:18 kommer Exp $
// $Log: TestACDKObject.cpp,v $
// Revision 1.3  2003/06/19 14:37:18  kommer
// source comment header ajusted
//
// Revision 1.2  2001/12/20 21:30:49  kommer
// panta rei
//
// Revision 1.1.1.1  2000/12/11 18:05:19  kommer
// ACDK Free edition
//
// Revision 1.3  2000/12/08 21:10:38  roger
// panta rei
//
// Revision 1.2  2000/12/08 15:52:26  roger
// panta rei
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:19  roger
// initial acdk sources
//
// Revision 1.1  2000/06/09 12:27:23  roger
// initial revision
//
// Revision 1.3  2000/02/08 16:29:54  roger
// RefHolder and Arrays changed
//
// Revision 1.2  2000/01/04 11:02:17  roger
// panta rei
//
// Revision 1.1  1999/12/22 14:51:04  roger
// initial revision
//
//


#include <acdk.h>
#include "TestACDKObject.h"
#include "TestException.h"

namespace tests {

using namespace ::acdk::lang;

static acdk::lang::ObjectCreator _curobc = 0;
//static 
int 
TestACDKObject::main(RObjectArrayImpl<RString> args)
{
  //try {
    if (_curobc == 0)
      return -1;
    RTestACDKObject tm = (RTestACDKObject )_curobc();
    tm->doTest(args);
  /*} catch (RTestException ex) {
    ex->printStackTrace(System.err);
    System::err->println("**** Test in Module fehlerValue() failed: " + ex->getMessage());
    return 1;
  } catch (RThrowable ex) {
    ex->printStackTrace(System.err);
    System::err->println("***** Unexpected Throwable in Test in Module fehlerValue(): " + ex->getMessage());
    return 2;
  }*/
  return 0;
}

int 
TestACDKObject::main(::acdk::lang::ObjectCreator obc, int argc, char* argv[], char** envptr)
{ 
  _curobc  = obc;
  return acdk::lang::System::main(tests::TestACDKObject::main, argc, argv, envptr);
}

} // namespace tests

