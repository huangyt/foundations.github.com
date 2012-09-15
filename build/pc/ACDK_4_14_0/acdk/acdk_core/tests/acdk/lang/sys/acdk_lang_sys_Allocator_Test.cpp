// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/sys/acdk_lang_sys_Allocator_Test.cpp,v 1.3 2003/06/19 14:37:18 kommer Exp $
//

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/util/ArrayList.h>
#include <acdk/lang/sys/BitmapPagedAllocator.h>

namespace tests {
namespace acdk {
namespace lang {
namespace sys {

  using namespace ::acdk::lang;
  using namespace ::acdk::lang::sys;
  USING_CLASS(::acdk::util::, ArrayList);

class Allocator_Test
{
 
public:
  static int acdkmain(RStringArray args)
  {
    ::acdk::lang::sys::BitmapPagedAllocator* allocator = new ::acdk::lang::sys::BitmapPagedAllocator();
    //::acdk::lang::sys::RawAllocator* allocator = new ::acdk::lang::sys::RawAllocator();
    {  
      RObject aobj = new (allocator) Object();
      {
        RObject bobj = new (allocator) Object();
      }
      //RArrayList l = new (allocator)ArrayList();
      /*
      for (int i = 0; i < 10; i++) {
        l->add(new (allocator)Object());
      }*/
    }
    return 0;
  }

};

} // namespace sys
} //namespace lang 
} //namespace acdk 
} // namespace tests 


int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(tests::acdk::lang::sys::Allocator_Test::acdkmain, argc, argv, envptr);
}
