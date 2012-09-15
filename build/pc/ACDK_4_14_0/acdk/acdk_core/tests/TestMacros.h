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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/TestMacros.h,v 1.5 2005/03/07 17:52:08 kommer Exp $

#ifndef TestMacros_h
#define TestMacros_h


#define ACDK_DOCORETEST(tm) \
  sys::coreout << "Testing " << #tm << " ==> "; \
  if ((tm) == false) { \
    sys::coreout << "in " << __FILE__ << ":" << __LINE__ << " failed!!!!!!!********" << sys::eofl; \
    exit(1); \
  } else { \
    sys::coreout << "OK." << sys::eofl; \
  } \

#define ACDK_DOCORETEST1(tm, msg) \
  sys::coreout << "Testing " << #tm << " ==> "; \
  if ((tm) == false) { \
    sys::coreout << "in " << __FILE__ << ":" << __LINE__ << " failed!!!!!!!********: " << msg << sys::eofl; \
    exit(1); \
  } else { \
    sys::coreout << "OK." << sys::eofl; \
  } \


#define ACDK_DOTEST(tm) \
  sys::coreout << ">> Testing [" << #tm << "] in [" << __FILE__ << ":" << __LINE__ << "]:\n"; \
  if ((tm) == false) { \
    sys::coreout << " **** failed <<" << sys::eofl; \
    THROW1_FQ(::, TestException, ::acdk::lang::RString("Test failed")); \
  } else { \
    sys::coreout << " <<OK." << sys::eofl; \
  } \


#define ACDK_DOTEST1(tm, msg) \
  sys::coreout << ">> Testing [" << #tm << "] in [" << __FILE__ << ":" << __LINE__ << "]:\n"; \
  if ((tm) == false) { \
    sys::coreout << " **** failed: " << msg << "<<" << sys::eofl; \
    THROW1_FQ(::, TestException, ::acdk::lang::RString("Test failed") + msg); \
  } else { \
    sys::coreout << " <<OK." << sys::eofl; \
  } \



#endif //TestMacros_h
