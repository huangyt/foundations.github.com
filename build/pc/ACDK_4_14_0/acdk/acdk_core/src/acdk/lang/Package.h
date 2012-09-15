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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Package.h,v 1.18 2005/04/08 10:53:19 kommer Exp $

#ifndef acdk_lang_Package_h
#define acdk_lang_Package_h


#include "NumberFormatException.h"

namespace acdk {
namespace lang {



/** 
  static information about the package
  @author Roger Rene Kommer
  @version $Revision: 1.18 $
  @date $Date: 2005/04/08 10:53:19 $
*/
foreign class VersionInformation
{
public:
  const char* title;
  const char* vendor;
  const char* version;
};

/** 
  static information about the package
  @author Roger Rene Kommer
  @version $Revision: 1.18 $
  @date $Date: 2005/04/08 10:53:19 $
*/

foreign class PackageDefinition 
{
public:
  /** @see acdk::lang::dmi::MetaInfoFlags */
  int flags;
  /** the name of the package. "acdk.lang" */
  const char* name;
  /** the Version information of the underlying specification */
  VersionInformation spec;
  /** the Version information of the underlying implementation */
  VersionInformation impl;
  /** internal, for create link, initialize this with 0 */
  PackageDefinition* next;
};

/** 
  Register helper
  @author Roger Rene Kommer
  @version $Revision: 1.18 $
  @date $Date: 2005/04/08 10:53:19 $
*/
foreign
class ACDK_CORE_PUBLIC RegisterPackage
{
private:
  PackageDefinition* _pdef;
public:
  RegisterPackage(PackageDefinition* pdef);
  ~RegisterPackage();
};


ACDK_DECL_CLASS(Package);
/** 
  just an nonfunction wrapper.
  API: Java Modified<br>
  <p>Note:<br>
  Will be used with SharedLibrary and/or the common type library.

  @author Roger Rene Kommer
  @version $Revision: 1.18 $
  @date $Date: 2005/04/08 10:53:19 $
  @see ResourceBundle
  @see ClassLoader
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy)) 
class ACDK_CORE_PUBLIC Package 
: extends Object
{
  ACDK_WITH_METAINFO(Package)
private:
  foreign PackageDefinition* _packageDefinition;
  bool _dummyPackage;
public:
  foreign Package(PackageDefinition* pdef, bool isdummyPackage = false)
  : Object(),
    _packageDefinition(pdef),
    _dummyPackage(isdummyPackage)
  {
  }
  foreign virtual ~Package();
  virtual RString getImplementationTitle();
  virtual RString getImplementationVendor();
  virtual RString getImplementationVersion();
  virtual RString getName();
  static RPackage getPackage(IN(RString) name);
  static RPackageArray getPackages();
  virtual RString getSpecificationTitle();
  virtual RString getSpecificationVendor();
  virtual RString getSpecificationVersion();
  /// reimplemented from Object
  foreign int hashCode();

  virtual bool isCompatibleWith(IN(RString) desired) THROWS1(RNumberFormatException);
  virtual bool isSealed();
  //virtual bool isSealed(URL url);
  
  /// reimplemented from Object
  foreign RString toString();
};

/** use to declare version number 
  in package_name_Package.cpp
  ACDK_DECL_PACKAGE_VERSION(package_name, 1.0.1);
*/
#define ACDK_DECL_PACKAGE_VERSION(packagesuffix, version) static const char* packagesuffix##_version = #version

} // namespace lang 
} // namespace acdk 

#endif //acdk_lang_Package_h

