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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/a2jser_java_util.cpp,v 1.6 2005/02/05 10:45:12 kommer Exp $


#include <acdk/java/serialization/ClassDescription.h>
#include <acdk/java/serialization/JavaObjectReader.h>
#include <acdk/java/serialization/JavaObjectWriter.h>
#include <acdk/util/ArrayList.h>

using namespace ::acdk::util;

::acdk::java::serialization::MemberTypeMapping a2jser_java_util_ArrayList_field_size = 
{
  "size", // acdk_name
  "int", // acdk_type
  "size", // java_name
  "I" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_java_util_ArrayList_fields[] = 
{
  &a2jser_java_util_ArrayList_field_size,
  0
};


RObject 
a2jser_read_acdk_util_ArrayList(IN(::acdk::java::serialization::RJavaObjectReader) in, 
                                const ::acdk::java::serialization::ClassTypeMapping* ctm)
{
  int count = in->readInt();
  RArrayList al = new ArrayList();
  in->registerNewObject(&al);
  for (int i = 0; i < count; ++i)
  {
    al->add(in->readObject());
  }
  return &al;
}

void 
a2jser_write_acdk_util_ArrayList(IN(::acdk::java::serialization::RJavaObjectWriter) out, 
                                    const ::acdk::java::serialization::ClassTypeMapping* ctm, 
                                    IN(::acdk::lang::RObject) obj)
{
  RArrayList a = (RArrayList)obj;
  out->writeInt(a->size());
  RIterator it = a->iterator();
  while (it->hasNext() == true)
  {
    out->writeObject(it->next());
  }

}


::acdk::java::serialization::ClassTypeMapping a2jser_java_util_ArrayList = 
{
  "acdk/util/ArrayList", // acdk_name
  "java/util/ArrayList", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x7881d21d99c7619d), // UID
  JLONG_CONSTANT(0x788cfdda08df1046), //[] UID
  a2jser_java_util_ArrayList_fields, // Fields
  a2jser_read_acdk_util_ArrayList, // read_func
  a2jser_write_acdk_util_ArrayList, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_util_ArrayList(&a2jser_java_util_ArrayList);


