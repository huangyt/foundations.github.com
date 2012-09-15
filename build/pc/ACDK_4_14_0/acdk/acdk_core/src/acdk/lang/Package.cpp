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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Package.cpp,v 1.16 2005/03/08 12:45:37 kommer Exp $


#include <acdk.h>

#include <stdio.h>
#include "Package.h"
#include "ObjectArrayImpl.h"
#include "sys/core_system.h"

namespace acdk {
namespace lang {


ACDK_DECL_PACKAGE_VERSION(acdk, 1.4.1);
static PackageDefinition* __firstPackageNode = 0;

static bool checkPackageCompatibility(const char* source, const char* desired)
{
  if (source == 0 || desired == 0)
    return true;
  return true; //### FIXIT
  /*
    because scanf cannot call before main
  int sv, sr, sb;
  int tv, tr, tb;
  sscanf(source, "%i.%i.%i", &sv, &sr, &sb);
  sscanf(desired, "%i.%i.%i", &tv, &tr, &tb);
  if (sv != tv)
    return false;
  if (tr < sr)
    return false;
  return true;
  */
}

RegisterPackage::RegisterPackage(PackageDefinition* pdef)
: _pdef(pdef)
{
  // avoid multiple registration on linux (.so loads .so multiple times)
  if (pdef->flags & acdk::lang::dmi::MiRegistered)
    return;
  pdef->flags |= acdk::lang::dmi::MiRegistered;
  pdef->next = __firstPackageNode;
#if !defined(ACDK_OS_LINUX) // will crash, if called vor int main()
  if (pdef != 0 && checkPackageCompatibility(pdef->spec.version, acdk_version) == false) {
    sys::coreout << "Incompatible package version: name=[" << pdef->name << "]; version=[" << pdef->spec.version << "];" << sys::eofl;
    //exit(1);
  }
#endif //!defined(ACDK_OS_LINUX)
  __firstPackageNode = pdef;
}

RegisterPackage::~RegisterPackage()
{
  if (_pdef == __firstPackageNode) 
  {
    __firstPackageNode = __firstPackageNode->next;
    return;
  }
  PackageDefinition* pdef = __firstPackageNode;
  PackageDefinition* prev = __firstPackageNode;
  while (pdef != 0) 
  {
    if (pdef == _pdef) 
    {
      prev->next = pdef->next;
      return;
    }
    
    prev = pdef;
    pdef = pdef->next;
    if (prev == pdef)
    {
      sys::coreout << "multiple  registered packages!: " << pdef->name 
		<< sys::eofl;
      return;
    }
  }
}

//virtual 
Package::~Package()
{
  if (_dummyPackage == true && _packageDefinition != 0) {
    if (_packageDefinition->name != 0)
      delete[] (char*)_packageDefinition->name;
    delete _packageDefinition;
    _packageDefinition = 0;
  }
}

//virtual 
RString 
Package::getImplementationTitle()
{
  return new String(_packageDefinition->impl.title);
}

//virtual 
RString 
Package::getImplementationVendor()
{
  return new String(_packageDefinition->impl.vendor);
}

//virtual 
RString 
Package::getImplementationVersion() 
{
  return new String(_packageDefinition->impl.version);
}

//virtual 
RString 
Package::getName()
{
  return new String(_packageDefinition->name);
}

VersionInformation __standardVersionInfo = 
{
  "ACDK",
  "artefaktur",
  "0"
};


//static 
RPackage 
Package::getPackage(IN(RString) n) 
{
  PackageDefinition* pdef = __firstPackageNode;
  RString name = n;
  name = name->replace("::", ".");
  name = name->replace('/', '.');
  while (pdef != 0) 
  {
    if (strcmp(pdef->name, name->c_str()) == 0)
      return new Package(pdef);
    pdef = pdef->next;
  }
  PackageDefinition *pd = new PackageDefinition();
  pd->name = name->c_strdup();
  pd->spec = __standardVersionInfo;
  pd->impl = __standardVersionInfo;
  pd->next = 0;
  return new Package(pd, true);
}

//static 
RPackageArray
Package::getPackages()
{
  PackageDefinition* pdef = __firstPackageNode;
  int count = 0;
  while (pdef != 0) 
  {
    count++;
    pdef = pdef->next;
  }
  RObjectArrayImpl<RPackage> pa = new ObjectArrayImpl<RPackage>(count);
  pdef = __firstPackageNode;
  for (int i = 0; i < count; i++) {
    pa[i] = new Package(pdef);
    pdef = pdef->next;
  }
  return pa;
}

//virtual 
RString Package::getSpecificationTitle()
{
  return new String(_packageDefinition->spec.title);
}
  
//virtual 
RString 
Package::getSpecificationVendor()
{
  return new String(_packageDefinition->spec.vendor);
}

//virtual 
RString 
Package::getSpecificationVersion()
{
  return new String(_packageDefinition->spec.version);
}

//virtual 
int 
Package::hashCode()
{
  return toString()->hashCode();
}

//virtual 
bool 
Package::isCompatibleWith(IN(RString) desired)THROWS1(RNumberFormatException)
{
  return checkPackageCompatibility(_packageDefinition->spec.version, desired->c_str());
}

//virtual 
bool 
Package::isSealed()
{
  //THROW1(UnsupportedOperationException, "Not implemented yet");
  return false;
}
  
//virtual bool Package::isSealed(URL url);
//virtual 
RString 
Package::toString()
{
  return RString("package ") + _packageDefinition->name + ", " +  _packageDefinition->spec.title + ", " + _packageDefinition->spec.version;
}



} // namespace lang 
} // namespace acdk 


