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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/a2jser_java_io.cpp,v 1.7 2005/02/05 10:45:12 kommer Exp $

#include <acdk.h>
#include <acdk/java/serialization/ClassDescription.h>
#include <acdk/java/serialization/JavaObjectReader.h>
#include <acdk/java/serialization/JavaObjectWriter.h>
#include <acdk/io/File.h>

//ACDK_USE_CLASS(::acdk::io::, File);
using namespace acdk::io;

RObject 
a2jser_read_acdk_io_File(IN(::acdk::java::serialization::RJavaObjectReader) in, 
                         const ::acdk::java::serialization::ClassTypeMapping* ctm)
{
  RString fname = in->readString();
  RObject file = new File(fname);
  in->registerNewObject(file);
  return file;
}

void 
a2jser_write_acdk_io_File(IN(::acdk::java::serialization::RJavaObjectWriter) out, 
                                    const ::acdk::java::serialization::ClassTypeMapping* ctm, 
                                    IN(::acdk::lang::RObject) obj)
{
  RFile file = (RFile)obj;
  RString str = file->getCanonicalPath();
  out->writeString(str);
}





::acdk::java::serialization::MemberTypeMapping a2jser_java_io_File_field_path = 
{
  "path", // acdk_name
  "acdk/lang/String;", // acdk_type
  "path", // java_name
  "Ljava/lang/String;" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_java_io_File_fields[] = 
{
  &a2jser_java_io_File_field_path,
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_java_io_File = 
{
  "acdk/io/File", // acdk_name
  "java/io/File", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x42da4450e0de4ff), // UID
  JLONG_CONSTANT(0xa8b95e19fc300502), //[] UID
  a2jser_java_io_File_fields, // Fields
  a2jser_read_acdk_io_File, // read_func
  a2jser_write_acdk_io_File, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_io_File(&a2jser_java_io_File);


