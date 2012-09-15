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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/ClassTypeMapping.cpp,v 1.7 2005/02/05 10:45:11 kommer Exp $


#include "ClassDescription.h"
#include "JavaObjectReader.h"

namespace acdk {
namespace java {
namespace serialization {


char 
MemberTypeMapping::getJavaTypeCode() const
{
  return java_type[0];
  /*
  if (strcmp(java_type, "i") == 0 || strcmp(java_type, "int") == 0)
    return 'I';
  if (strcmp(java_type, "j") == 0 || strcmp(java_type, "long") == 0)
    return 'J';
  
  return 'L';
  */
}


const ClassTypeMapping* ClassTypeMapping::root = 0;

//static 
void 
ClassTypeMapping::registerMapping(const ClassTypeMapping* ctm)
{
  const ClassTypeMapping* next = ClassTypeMapping::root;
  const_cast<ClassTypeMapping*>(ctm)->next = next;
  ClassTypeMapping::root = ctm;
}
  
//static 
void 
ClassTypeMapping::unregisterMapping(const ClassTypeMapping* ctm)
{
}


//static 
const ClassTypeMapping* 
ClassTypeMapping::findJavaClass(RString name)
{
  name = name->replace('.', '/');
  for (const ClassTypeMapping* ctm = ClassTypeMapping::root;
       ctm != 0;
       ctm = ctm->next)
  {
    if (name->equals(ctm->java_name) == true)
      return ctm;
  }
  return 0;
}

//static 
const ClassTypeMapping* 
ClassTypeMapping::findAcdkClass(const char* name)
{
  for (const ClassTypeMapping* ctm = ClassTypeMapping::root;
       ctm != 0;
       ctm = ctm->next)
  {
    if (strcmp(ctm->acdk_name, name) == 0)
      return ctm;
  }
  return 0;
}

const MemberTypeMapping* 
ClassTypeMapping::findAcdkMember(const char* name) const
{
  for (int i = 0; fields[i] != 0; ++i)
  {
    if (strcmp(fields[i]->acdk_field, name) == 0)
      return fields[i];
  }
  return 0;
}
  
const MemberTypeMapping* 
ClassTypeMapping::findJavaMember(const char* name) const
{
  for (int i = 0; fields[i] != 0; ++i)
  {
    if (strcmp(fields[i]->java_field, name) == 0)
      return fields[i];
  }
  return 0;
}


} // namespace serialization
} // namespace java 
} // namespace acdk 
