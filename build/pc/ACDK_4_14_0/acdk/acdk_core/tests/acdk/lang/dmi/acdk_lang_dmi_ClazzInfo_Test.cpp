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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/dmi/acdk_lang_dmi_ClazzInfo_Test.cpp,v 1.15 2005/03/07 17:52:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/System.h>

namespace tests {
namespace acdk {
namespace lang {
namespace dmi {

BEGIN_DECLARE_TEST( ClazzInfo_Test )
  DECLARE_TEST( findClasses )
  DECLARE_TEST( findNamespaces )
  DECLARE_TEST( assignable )
  DECLARE_TEST( toTypeName )
  DECLARE_TEST( enumeration )
  DECLARE_TEST( experimental )
END_DECLARE_TEST( ClazzInfo_Test  )

BEGIN_DEFINE_TEST( ClazzInfo_Test )
  ADD_TEST( ClazzInfo_Test, findClasses ) 
  ADD_TEST( ClazzInfo_Test, findNamespaces ) 
  ADD_TEST( ClazzInfo_Test, assignable )
  ADD_TEST( ClazzInfo_Test, toTypeName )
  ADD_TEST( ClazzInfo_Test, enumeration )
  ADD_TEST( ClazzInfo_Test, experimental )
END_DEFINE_TEST( ClazzInfo_Test )

using namespace ::acdk::lang::dmi;

void
ClazzInfo_Test::findClasses()
{
#if !defined(__BORLANDC__) // ### crashes
  ::acdk::lang::sys::core_vector<const ClazzInfo*> ret;
  ClazzInfo::findClasses("acdk/lang", ret, false);
  testAssert(ret.size() != 0);

  ::acdk::lang::sys::core_vector<const ClazzInfo*> ret2;
  ClazzInfo::findClasses("acdk/lang", ret2, true);
  testAssert(ret.size() < ret2.size());

  ::acdk::lang::sys::core_vector<const ClazzInfo*> ret3;
  ClazzInfo::findClasses("", ret3, true);
  testAssert(ret2.size() < ret3.size());
#endif //!defined(__BORLANDC__) // ### crashes
}


void
ClazzInfo_Test::findNamespaces()
{
#if !defined(__BORLANDC__) // ### crashes
  ::acdk::lang::sys::core_vector<const char*> ret1;
  ClazzInfo::findNamespaces("acdk", ret1, false);
  testAssert(ret1.size() > 0);

  ::acdk::lang::sys::core_vector<const char*> ret2;
  ClazzInfo::findNamespaces("acdk", ret2, true);
  testAssert(ret2.size() > ret1.size());
#endif //!defined(__BORLANDC__) // ### crashes
  
}

void
ClazzInfo_Test::assignable()
{
  const ClazzInfo* integerclazz = Integer::clazzInfo();
  const ClazzInfo* numberclazz = Number::clazzInfo();
  testAssert(integerclazz->assignableFrom(integerclazz) == true);
  testAssert(integerclazz->assignableFrom(Object::clazzInfo()) == false);
  testAssert(Object::clazzInfo()->assignableFrom(integerclazz) == true);
  testAssert(numberclazz->assignableFrom(integerclazz) == true);
  testAssert(integerclazz->assignableFrom(numberclazz) == false);

}

using namespace ::acdk::lang::dmi;

void
ClazzInfo_Test::toTypeName()
{
  {
    System::out->println("\nOnlye names:\nDefault Class: ");
    System::out->println(StringBuffer::GetClass()->objectClazzInfo()->toTypeString());
    System::out->println("TpFtJavaClass: ");
    System::out->println(StringBuffer::GetClass()->objectClazzInfo()->toTypeString(TpFtJavaType));
    System::out->println("TpFtLoadableClass: ");
    System::out->println(StringBuffer::GetClass()->objectClazzInfo()->toTypeString(TpFtLoadableClass));
    System::out->println("TpFtJavaSignature: ");
    System::out->println(StringBuffer::GetClass()->objectClazzInfo()->toTypeString(TpFtJavaSignature));
    System::out->println("TpFtACDKSignature: ");
    System::out->println(StringBuffer::GetClass()->objectClazzInfo()->toTypeString(TpFtACDKSignature));
  }
  {
    System::out->println("\nOnlye names:\nDefault Class: ");
    System::out->println(StringBufferArray::GetClass()->objectClazzInfo()->toTypeString());
    System::out->println("TpFtJavaClass: ");
    System::out->println(StringBufferArray::GetClass()->objectClazzInfo()->toTypeString(TpFtJavaType));
    System::out->println("TpFtLoadableClass: ");
    System::out->println(StringBufferArray::GetClass()->objectClazzInfo()->toTypeString(TpFtLoadableClass));
    System::out->println("TpFtJavaSignature: ");
    System::out->println(StringBufferArray::GetClass()->objectClazzInfo()->toTypeString(TpFtJavaSignature));
    System::out->println("TpFtACDKSignature: ");
    System::out->println(StringBufferArray::GetClass()->objectClazzInfo()->toTypeString(TpFtACDKSignature));
  }
  {
    System::out->println("\nDeclarations:\nACDK Class: ");
    System::out->println(StringBuffer::GetClass()->objectClazzInfo()->toTypeString(TpFtAcdkType | TpFtTypeDecl));
    System::out->println("TpFtJavaClass: ");
    System::out->println(StringBuffer::GetClass()->objectClazzInfo()->toTypeString(TpFtJavaType | TpFtTypeDecl));
  }
  {
    System::out->println("\nDefinition:\nACDK Class: ");
    System::out->println(StringBuffer::GetClass()->objectClazzInfo()->toTypeString(TpFtAcdkType | TpFtTypeDef));
    System::out->println("TpFtJavaClass: ");
    System::out->println(StringBuffer::GetClass()->objectClazzInfo()->toTypeString(TpFtJavaType | TpFtTypeDef));
  }
  //System::in->readLine();
}

using namespace ::acdk::lang::sys;
typedef core_vector< const ClazzMethodInfo* > MethodsHashes;

bool equalSignature(const ClazzMethodInfo* first, const ClazzMethodInfo* second)
{
  if (strcmp(first->name, second->name) != 0)
    return false;
  if (first->methodArgs == 0 && second->methodArgs == 0)
    return true;
  if (first->methodArgs == 0 || second->methodArgs == 0)
    return false;
  int i;
  for (i = 0; first->methodArgs[i] != 0; ++i)
  {
    if (second->methodArgs[i] == 0)
      return false;
    if (MetaInfo::calcHashValue(first->methodArgs[i]->flags) != MetaInfo::calcHashValue(second->methodArgs[i]->flags))
      return false;
    if (first->methodArgs[i]->type != second->methodArgs[i]->type)
      return false;
  }
  if (second->methodArgs[i] != 0)
    return false;
  return true;
}

void checkMethod(MethodsHashes& methods, const ClazzMethodInfo* cm)
{
  MethodsHashes::iterator it = methods.begin();
  for (; it < methods.end(); ++it)
  {
    if ((*it)->getMethodSignatureHashValue() == cm->getMethodSignatureHashValue())
    {
      if (equalSignature(*it, cm) == false)
      {
        StringBuffer sb("Equal Signature: ");
        (*it)->toTypeString(sb, 0, TpFtJavaType | TpFtTypeDef | TpFtFqName);
        sb << " and: ";
        cm->toTypeString(sb, 0, TpFtJavaType | TpFtTypeDef | TpFtFqName);
        System::out->println(sb.toString());
        equalSignature(*it, cm);
      }
    }
  }
  methods.push_back(cm);
}

void
ClazzInfo_Test::enumeration()
{
  const ClazzEnumInfo* ei = ClazzEnumInfo::findEnum("VarType", "acdk/lang/dmi");
  testAssert(ei != 0);
  ei = ClazzEnumInfo::findEnum("VarType", Nil);
  testAssert(ei != 0);
  const ClazzEnumValueInfo* evi = ClazzEnumInfo::findEnumValue("LongVT", "acdk/lang/dmi", &ei);
  sys::coreout << "evi: " << (void*)evi << sys::eofl;
  testAssert(ei != 0 && evi != 0 && evi->value == ::acdk::lang::dmi::LongVT);
}

void
ClazzInfo_Test::experimental()
{
  const ClazzInfo* ci = ClazzInfo::getClazzInfoRoot();
  MethodsHashes methods;
  while (ci != 0)
  {
    if (ci->methods != 0)
      for (int i = 0; ci->methods[i] != 0; ++i)
      {
        checkMethod(methods, ci->methods[i]);
      }
    ci = ci->_next;
  }
}

} // namespace lang 
} // namespace dmi 
} //namespace acdk 
} //namespace tests 




