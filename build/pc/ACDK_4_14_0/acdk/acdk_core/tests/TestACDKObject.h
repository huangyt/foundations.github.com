// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/TestACDKObject.h,v 1.4 2005/01/14 19:59:55 kommer Exp $
// $Log: TestACDKObject.h,v $
// Revision 1.4  2005/01/14 19:59:55  kommer
// panta rei
//
// Revision 1.3  2003/06/19 14:37:18  kommer
// source comment header ajusted
//
// Revision 1.2  2001/12/20 21:30:49  kommer
// panta rei
//
// Revision 1.1.1.1  2000/12/11 18:05:19  kommer
// ACDK Free edition
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:19  roger
// initial acdk sources
//
// Revision 1.2  2000/09/28 13:55:06  niro
// *** empty log message ***
//
// Revision 1.1  2000/06/09 12:27:19  roger
// initial revision
//
// Revision 1.3  2000/02/14 22:09:55  roger
// new ACDK_DECL_CLASS
//
// Revision 1.2  2000/02/08 16:29:54  roger
// RefHolder and Arrays changed
//
// Revision 1.1  1999/12/22 14:51:07  roger
// initial revision
//
//

#ifndef TestACDKObject_h
#define TestACDKObject_h

#include <acdk.h>

namespace tests {

using namespace ::acdk::lang;
ACDK_DECL_CLASS(TestACDKObject);

/** 
  Test-Framework for ACDK-Tests
  @author kommer@artefaktur.com
  @version $Revision: 1.4 $
  @date $Date: 2005/01/14 19:59:55 $
  @bug Incomplete
  
*/
class TestACDKObject
: extends ::acdk::lang::Object
{
  DECL_ACDK_DEFAULT_METACLASS(Object)
public:
  /**
    Implement this method, which returns a new created 
    @param args the main()-args
  */
#if 0 // not implemented here
  static RTestACDKObject createInstance();
#endif
  /**
    Reimplement this method for testing
    @param args the main()-args
  */
  virtual void doTest(RObjectArrayImpl<RString> args) = 0;
  /**
    Reimplement this method for testing
    @param args the main()-args
  */
  TestACDKObject() : Object() { }
  
  static int main(::acdk::lang::ObjectCreator obc, int argc, char* argv[], char** envptr);
  static int main(RObjectArrayImpl<RString> args);
  
};

} //namespace tests

#endif //TestACDKObject_h
