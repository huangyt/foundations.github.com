// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/TestException.h,v 1.3 2005/01/14 19:59:55 kommer Exp $
//
// $Log: TestException.h,v $
// Revision 1.3  2005/01/14 19:59:55  kommer
// panta rei
//
// Revision 1.2  2003/06/19 14:37:18  kommer
// source comment header ajusted
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
// Revision 1.1  2000/06/09 10:27:10  roger
// initial revision
//
// Revision 1.4  2000/04/03 13:39:20  roger
// panta rei
//
// Revision 1.3  2000/02/10 15:50:28  roger
// ACDK_DECL_CLASS 2 ACDK_DECL_THROWABLE
//
// Revision 1.2  1999/12/08 19:40:31  roger
// dos2unix
//
// Revision 1.1  1999/12/01 11:26:46  roger
// initial revision
//

#ifndef TestException_h
#define TestException_h

#include <acdk.h>
#include <acdk/lang/Error.h>

ACDK_DECL_THROWABLE_FQ(TestException, ::acdk::lang::, Error);

class TestException
: extends ::acdk::lang::Error
{
  DECL_ACDK_DEFAULT_METACLASS(Error)
public:
  TestException() : Error() { }
  TestException(::acdk::lang::RString what) : Error(what) { }

};

#endif //TestException_h
