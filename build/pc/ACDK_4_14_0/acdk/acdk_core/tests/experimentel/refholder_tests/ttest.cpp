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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/experimentel/refholder_tests/ttest.cpp,v 1.5 2005/03/07 17:52:10 kommer Exp $


#include "ORef1.h"
#include "Objects.h"


void doWithRNumber(RNumber rnum)
{

}


void doWithNumber(const Number& num)
{
  
}

void ObjectTest2()
{
  RObject obj = new Object();
  RNumber num = new Number();
  RObject obj2 = obj;
  RObject obj3 = &num;
  
  sys::coreout << "expect intit<T>: ";
  obj = num;
  
  sys::coreout << "expect intit<TO>: ";
  //num = obj;
  num = (RNumber)obj;
  
  sys::coreout << "expect ORef(OT*): ";
  obj = num;
  
  sys::coreout << "expect ORef(T*): ";
  //num = &obj;
  obj = new Object();
  obj = new Number();
  //num = new Object();
  RComparable c = (Comparable*)num;
  c->compare(*num);
  RComparable c2 = (RComparable)obj;
  c->compare(c2);
  //num = (RNumber)c;
  num = c;
  RStringWritable swr = &num;
  RWritable wr = &swr;
  wr = swr;
  swr = (RStringWritable)wr;
  RWritable* wrptr = wr._ref_this();

  Number nononstack;
  doWithRNumber(num);
  doWithNumber(*num);
  doWithNumber(nononstack);
  doWithRNumber(&nononstack);
  
}


int main()
{
  ObjectTest2();
  return 0;
}
