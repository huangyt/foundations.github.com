// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/sys/acdk_lang_sys_BackTrace_Test.cpp,v 1.14 2005/03/08 14:58:52 kommer Exp $
//

#include <acdk.h>
#include <acdk/tools/aunit/core_test.h>  
#include <acdk/lang/sys/BackTrace.h>
#include <acdk/lang/System.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Integer.h>
#include <ctype.h>

namespace tests {
namespace acdk {
namespace lang {
namespace sys {

using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::lang::sys;

  
BEGIN_DECLARE_TEST( BackTrace_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( BackTrace_Test  )

BEGIN_DEFINE_TEST( BackTrace_Test )
  ADD_TEST( BackTrace_Test, standard ) 
END_DEFINE_TEST( BackTrace_Test )

typedef void (*VoidFunc)();
typedef RObject (*RObjectIntCharPtrFunc)(int, const char*);


#ifdef __GNUG__


RString readFuncName(const char*& ptr, const char* end)
{
  const char* beginptr = ptr;
  while (ptr < end) {
    if (*ptr == '_' && *(ptr + 1) == '_') {
      --ptr;
      return new String((byte*)beginptr, ptr - beginptr, (byte*)beginptr, (byte*)ptr + 1, ConstSST | CCAscii);
    }
    ++ptr;
  }
  return new String((byte*)beginptr, ptr - beginptr, (byte*)beginptr, (byte*)ptr, ConstSST | CCAscii);
}

int readNum(const char*& ptr, const char* end)
{
  if (isdigit(*ptr) == false)
    return 0;
  const char* beginptr = ptr;

  if (isdigit(*ptr) == false)
    return *ptr - '0';
  while (ptr < end && isdigit(*(ptr + 1)) == true)
    ++ptr;
  String s((byte*)beginptr, ptr - beginptr, (byte*)beginptr, (byte*)ptr, ConstSST | CCAscii);
  return Integer::parseInt(&s);
}

RString readFqName(const char*& ptr, const char* end)
{
  if (*ptr == 'Q')
    ++ptr;
  int elements = *ptr - '0';
  ++ptr;
  StringBuffer sb(512);
  for (int i = 0; i < elements && ptr < end; ++i) {
    int elength = readNum(ptr, end);
    if (elength == 0)
      return sb.toString();
    if (sb.length() != 0)
      sb.append("::");
    String s(ptr, elength);
    sb.append(s);
    ptr += elength;
  }
  return sb.toString();
}

RString demangle(RString orgname)
{
  StringBuffer sb(512);
  RString funcname = "";
  RString& curtoken = funcname;
  enum State 
  {
    Begin
    
  };
  State state = Begin;
  const char* ptr = (const char*)orgname->byte_begin();
  const char* eptr = (const char*)orgname->byte_end();
  while (ptr < eptr) 
  {
    switch(*ptr) 
    {
    case '_':
      if (*(ptr + 1) == '_') {
        ++ptr;
        if (*ptr == 'Q')
          curtoken = readFqName(ptr, eptr);
        break;
      }
      break;
      
    default:
      if (state == Begin) {
        curtoken = readFuncName(ptr, eptr);
        break;
      }
      ; //oops;
      break;
    }
    ++ptr;
  }
  return "";
}

#endif //__GNUG__

void foo()
{
 
  /*
  for (int i = 0; i < btf->length(); i++) 
  {
    printf("-> %s in (%s)\n\n", btf[i]->functionName->c_str(), btf[i]->library->c_str());  
  }*/
  System::printStackTrace();
}

RObject baz(int, const char*)
{
  foo();
  return Nil;
}

void tryThrowableEx()
{
  try {
    RcharArray ch = new charArray(1);
    char c = ch[2];
  } catch (RThrowable ex)  {
    ex->printStackTrace();
  }
 
}

int useFuncPtrs();

void BackTrace_Test::standard()
{
  baz(1, "hallo");
  tryThrowableEx();
  useFuncPtrs();
}

RObjectIntCharPtrFunc bazf = baz;
VoidFunc tryThrowFunc  = tryThrowableEx;
VoidFunc foofunc = foo;
int useFuncPtrs()
{
  if (bazf && tryThrowFunc && foofunc)
    return 0;
  return 2;
}

} // namespace sys
} //namespace lang 
} //namespace acdk 
} // namespace tests 


