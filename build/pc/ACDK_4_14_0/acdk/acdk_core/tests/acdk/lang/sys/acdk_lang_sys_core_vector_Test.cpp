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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/sys/acdk_lang_sys_core_vector_Test.cpp,v 1.12 2005/04/25 13:19:34 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include "../../../../src/acdk/lang/sys/core_vector.h"

#include <vector>
#include <exception>


namespace tests {
namespace acdk {
namespace lang {
namespace sys {

using namespace ::acdk::lang;
using namespace ::acdk::lang::sys;

  
BEGIN_DECLARE_TEST( core_newvector_Test )
  DECLARE_TEST( intVector )
  DECLARE_TEST( structVector )
END_DECLARE_TEST( core_newvector_Test  )

BEGIN_DEFINE_TEST( core_newvector_Test )
  ADD_TEST( core_newvector_Test, intVector ) 
  ADD_TEST( core_newvector_Test, structVector ) 
  
END_DEFINE_TEST( core_newvector_Test )


//#define core_vector core_newvector

void core_newvector_Test::intVector()
{
  ::acdk::lang::sys::core_vector<int> cint(0);
  int i;
  int count = 3;
  for (i = 0; i < count; ++i)
    cint.push_back(i);
  testAssert(cint.size() == count);
  for (i = 0; i < count; ++i)
    testAssert(cint[i] == i);
  cint.addFirst(200);
  testAssert(cint.size() == count + 1);
  testAssert(cint[0] == 200);
  for (i = 0; i < count; ++i)
    testAssert(cint[i + 1] == i);
  testAssert(cint.hasElement(200) == true);
  testAssert(cint.hasElement(300) == false);
  cint.deleteElement(200);
  testAssert(cint.size() == count);
  for (i = 0; i < count; ++i)
    testAssert(cint[i] == i);

  {
    ::acdk::lang::sys::core_vector<int>::iterator it = cint.begin();
    ::acdk::lang::sys::core_vector<int>::iterator end = cint.end();
    //### cint.erase(it);
    // testAssert(cint.size() == 99);
    //### cint.erase(*it + 51, *it + 60);
    // testAssert(cint.size() == 90);
    
  }
  
}

class ContainedClass
{
public:
  int val;
  ContainedClass()
  : val(-1)
  {
  }
  explicit ContainedClass(int i)
  : val(i)
  {
    sys::coreout << "+";
  }
  ~ContainedClass()
  {
    sys::coreout << "-";
    val = -1;
  }
  bool isValid() const
  {
    return val != -1;
  }
  ContainedClass(const ContainedClass& other)
  : val(other.val)
  {
    sys::coreout << "=";
  }
};

void core_newvector_Test::structVector()
{
  ::acdk::lang::sys::core_vector<ContainedClass> vec;
  typedef ::acdk::lang::sys::core_vector<ContainedClass>::iterator iterator;
  int i;
  for (i = 0; i < 4; ++i)
  {
    vec.push_back(ContainedClass(i));
  }
  testAssert(vec.size() == 4);

  for (i = 0; i < vec.size(); ++i)
  {
    testAssert(vec[i].val == i);
  }
  iterator it = vec.begin();
  iterator end = vec.end();
  iterator lit = it;
  for (; it < end; ++it)
  {
    if (it != lit)
      testAssert(it->val == lit->val + 1);
    lit = it;
  }
  vec.erase(vec.begin());
  testAssert(vec.size() == 3);
  it = vec.begin();
  end = vec.end();
  for (; it < end; ++it)
  {
    testAssert(it->isValid() == true);
  }
  testAssert(vec.front().val == 1);
  testAssert(vec.back().val == 3);
  vec.erase(vec.begin() + 1);
  ::acdk::lang::sys::core_vector<ContainedClass> copyvec = vec;
  testAssert(vec.size() == 2);
  testAssert(vec.front().val == 1);
  testAssert(vec.back().val == 3);
  vec.erase(vec.end() - 1);
  testAssert(vec.size() == 1);
  testAssert(vec.front().val == 1);
  testAssert(vec.back().val == 1);
  

}


} // namespace sys
} // namespace lang
} // namespace acdk
} // namespace tests
