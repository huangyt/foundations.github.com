// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Object_Test.cpp,v 1.28 2005/03/07 14:08:12 kommer Exp $
//
// $Log: acdk_lang_Object_Test.cpp,v $
// Revision 1.28  2005/03/07 14:08:12  kommer
// typo
//
// Revision 1.27  2004/04/24 00:59:57  kommer
// panta rei
//
// Revision 1.26  2003/12/29 13:14:46  kommer
// panta rei
//
// Revision 1.25  2003/09/14 17:09:27  kommer
// panta rei
//
// Revision 1.24  2003/06/19 14:37:18  kommer
// source comment header ajusted
//
// Revision 1.23  2003/06/19 13:17:20  kommer
// merged acdk-3-unicode into MAIN
//
// Revision 1.22.2.1  2003/02/28 01:32:16  kommer
// panta rei
//
// Revision 1.22  2002/01/14 11:44:54  kommer
// panta rei
//
// Revision 1.21  2001/12/28 20:03:46  kommer
// panta rei
//
// Revision 1.20  2001/12/27 11:45:44  kommer
// panta rei
//
// Revision 1.19  2001/12/23 13:11:13  kommer
// panta rei
//
// Revision 1.18  2001/12/20 21:30:49  kommer
// panta rei
//
// Revision 1.17  2001/12/19 22:08:52  kommer
// panta rei
//
// Revision 1.16  2001/12/15 13:03:06  kommer
// panta rei
//
// Revision 1.15  2001/12/02 13:19:58  kommer
// renamed testunit  to aunit
//
// Revision 1.14  2001/11/22 08:13:44  kommer
// panta rei
//
// Revision 1.13  2001/06/24 21:38:54  kommer
// Ajustments for GCC 3.0
//
// Revision 1.12  2001/06/09 15:06:32  kommer
// panta rei
//
// Revision 1.11  2001/06/09 01:19:44  kommer
// panta rei
//
// Revision 1.10  2001/06/01 08:56:17  kommer
// panta rei
//
// Revision 1.9  2001/05/27 15:28:13  kommer
// dos2unix
//
// Revision 1.8  2001/05/27 13:19:57  kommer
// panta rei
//
// Revision 1.7  2001/05/25 22:17:57  kommer
// panta rei
//
// Revision 1.6  2001/05/20 15:15:01  kommer
// panta rei
//
// Revision 1.5  2001/05/20 12:31:44  kommer
// panta rei
//
// Revision 1.4  2001/05/18 09:01:02  kommer
// panta rei
//
// Revision 1.3  2001/05/12 16:56:51  kommer
// *** empty log message ***
//
// Revision 1.2  2001/05/05 18:12:25  kommer
// panta rei
//
// Revision 1.1  2001/04/30 11:43:02  kommer
// initial revision
//


#include <acdk.h>

#include <acdk/lang/System.h>
#include <acdk/lang/Class.h>
#include <acdk/lang/Integer.h>
#include <acdk/io/Serializable.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>
#include <acdk/io/ObjectWriter.h>
#include <acdk/io/BinaryObjectWriter.h>
#include <acdk/io/BinaryObjectReader.h>
#include <acdk/io/CharArrayWriter.h>
#include <acdk/io/File.h>

#include "../../test.h"
#include <acdk/tools/aunit/core_test.h>
#include <acdk/lang/dmi/Marshaler.h>
#include <stdio.h>
namespace tests {
namespace acdk {
namespace lang {

using namespace ::acdk::lang;
using namespace ::acdk::lang::sys;

USING_CLASS(::acdk::io::, CharArrayWriter);



ACDK_DECL_CLASS(MyValType);

class MyValType
: extends ::acdk::lang::Object
, implements ::acdk::io::Serializable
{
public:
  MyValType()
  : sval("")
  , ival(0)
  {
  }
	RString sval;
	int ival;
  RString toString()
  {
    return sval + " " + String::valueOf(ival);
  }
};



class MyClass
: public ::acdk::lang::Object
{
public:
  RString _val;
  MyClass()
  : Object()
  , _val("")
  {

  }

	void sendString(BYVALIN(RString) val1)
  {
    RString str = val1;
    System::out->println(str);
    testAssert(str->equals("sendstring") == true);
    //val1 = (RString)new String("nonsense");
  }
  void receiveString(BYVALOUT(RString) val2)
  {
    val2 = _val;
  }
  void sendreceiveString(BYVALINOUT(RString) val)
  {
    RString str = val;
    System::out->println(str);
    str = str + " ja";
    val = str;
  }
  void sendObject(BYVALIN(RObject) val1)
  {
    RObject lobj;
    lobj =   val1;

  }
  void sendStringBuffer(BYVALIN(RStringBuffer) sb)
  {
    System::out->println(sb->toString());
    sb->append(" appended");
  }
  
};

template <class FromT>
class RefCast
{
public:
  Object* _impl;
  FromT* _iptr;
  RefCast(FromT* ptr)
  {
  }
};

template <class T>
class RefH
{
public:
  Object* _impl;
  T* _iptr;
  RefH(const RefCast<T>& arg)
  {
    RefH<T>::_impl = RefH<T>::_arg;

  }
};


// RefHolder

template <class ToCast, class FromCast>
ToCast iface_cast(const FromCast& from)
{
  return ToCast(from.iptr(), from.impl());
}
  /* ### gcc parse error??
template <class ToCast, class FromCast>
ToCast& iface_assign(ToCast& lval, const FromCast& rval)
{

    static_cast< ToCast::Type * >(rval.iptr());
  return lval._assign(static_cast< ToCast::Type* >(rval.iptr()), rval.impl());

}

template <class ToCast, class FromCast>
ToCast obj_cast(const FromCast& from)
{
  return ToCast(static_cast<ToCast::Type*>(from.iptr()));
}
*/

/*
class IA { };
class A { }; class B : public A, public virtual IA { }; class C : public B { }; class X { };
  */  
namespace {

void fooObject(RObject obj)
{
}

} // anon namespace

class Object_Test
{
public:
  static void foo()
  {
    
    Object o; String s;
    if (is_base_of<Object>(s)) {
      System::out->println(" A is baseof C ");
    } 
    if (is_base_of<String>(o)) {
      System::out->println(" B is baseof A ");
    } else {
      System::out->println(" A are not related B ");
    }
    
    
    RInteger int1 = new Integer(42);
    fooObject(&int1);
    RObject obj = &int1;
    obj = (RObject)int1;
    //str = obj;
    RInteger integer  = (RInteger)obj;
    /*RClass cls1 = (RClass)obj;
    cls1 = (RClass)obj;
    cls1->getClassName();
    */

  }
  static void throwTest ()
  {
    try {
      THROW0(RuntimeException);
    } catch (RException ex) {

    }
  }
  static void sendtest()
  {
  }

  static int acdkmain(RStringArray args)
  {
    printf("output should be OK\n");
    ::acdk::lang::Object* optr = System::out.impl();
    ::acdk::io::PrintWriter* pptr = System::out.iptr();
    pptr->println("OK");
    System::out->println("OK");
    printf("not crashed yet\n");
    return 0;
    
    /*
     throwTest();
    foo();
    RInteger integer = new Integer(42);
    RNumber number = obj_cast<RNumber>(integer);

    RComparable comp = iface_cast<RComparable>(integer);
    comp = iface_cast<RComparable>(integer);
    //comp = integer.castTo<RComparable>();
    iface_assign(comp, integer);
      */
    MyClass mcl;
    //RMyValType v1 = new MyValType();
    //RMyValType v2;
    //mcl.foo(v1, v2);
    
    RString s1 = new String("sendstring");
    RString s2;
    mcl.sendString(s1);
    //mcl.receiveString(s2); // ws6 compiler error??
    System::out->println(s2);
    mcl.sendreceiveString(s1);
    System::out->println(s1);
    
    RObjectArray oa = new ObjectArray(2);
    oa[0] = new String("asdf");
    oa[1] = (RObject)oa;
    //mcl.sendObject((RObject)oa);
    RStringBuffer sb = new StringBuffer("org");
    mcl.sendStringBuffer(sb);
    System::out->println(sb->toString());
    return 0;
  }
};


} // namespace tests
} //namespace acdk 
} //namespace lang 


void testSharedLibAccess()
{
  ::acdk::lang::Object* obj = new ::acdk::lang::Object();
  obj->hashCode();
  
  int imax = ::acdk::lang::Integer::MAX_VALUE;
  ::acdk::lang::RString str = ::acdk::lang::Integer::toString(imax);
  printf("MAXINT is %i, %s\n", imax, str->c_str());
}




using namespace ::acdk::lang;
void ObjectTest1()
{
  {
    String str("Test");
    RString str2 = new String("Test");
    str2->equals(str);
  }
  {
    RString fname = "G:\\artefaktur\\acdk\\bin\\acdk_lang_Object_Test.exe";
    ::acdk::io::RFile f = new ::acdk::io::File(fname);
    RString p = f->getParent();
  }
}



int 
main(int argc, char* argv[], char** envptr)
{
  ObjectTest1();
  testSharedLibAccess();
 
  return ::acdk::lang::System::main(::tests::acdk::lang::Object_Test::acdkmain, argc, argv, envptr);
}
