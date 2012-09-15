
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


#ifndef acdk_tools_mc_McConfigAttribute_h
#define acdk_tools_mc_McConfigAttribute_h

#include "mc.h"
#include "CodeAttribute.h"

namespace acdk {
namespace tools {
namespace mc {

/**
  Collection of flags to control acdkmc.
  used in constructor for McConfigAttribute attribute
*/
enum McConfigAttributes
{
  /**
    don't generate MetaInfo for this element
  */
  McConfNoMetaInfo            = 0x00000001,
  /**
    dont't create fields metainfo
  */
  McConfNoFields                = 0x00000002,
  /**
    create fields metainfo
  */
  McConfWithFields              = 0x00000004,
  /**
    dont't create methods metainfo
  */
  McConfNoMethods                = 0x00000008,
  /**
    dont't create methods metainfo
  */
  McConfWithMethods                = 0x00000008,
  /**
    this class the method
    void getCollectableFields(FieldReferences& fields);
    will be implemented manually
  */
  McConfOwnCollectableFields = 0x00000100,
  /**
    Used to tag a method or class not to 
    generate a corresponding DMI proxy
  */
  McConfNoDmiProxy           = 0x00000200,
};
ACDK_DEF_LIB_ENUM(ACDK_TOOLS_MC_PUBLIC, McConfigAttributes);

ACDK_DECL_CLASS(McConfigAttribute);
/**
  This class add DmiProxy interface to an
  ACDK interface. A DMIProxy enables an
  Script language to implement an ACDK interface.
*/
class ACDK_TOOLS_MC_PUBLIC McConfigAttribute
: extends CodeAttribute
{
  ACDK_WITH_METAINFO(McConfigAttribute)
public:
  int attributes;
  McConfigAttribute(int configAttributes) 
  : attributes(configAttributes)
  {
  }
  static void initAttribute(IN(RMetaCompiler) mc); // will be called via DMI
  foreign virtual bool apply(IN(RCodeInfo) cm) { return true; }
  foreign bool genMethods(bool defaultValue)
  {
    if (attributes & McConfNoMetaInfo)
      return false;
    if (attributes & McConfNoMethods)
      return false;
    if (attributes & McConfWithMethods)
      return true;
    return defaultValue;
  }
  foreign bool genFields(bool defaultValue)
  {
    if (attributes & McConfNoMetaInfo)
      return false;
    
    if (attributes & McConfNoFields)
      return false;
    if (attributes & McConfWithFields)
      return true;
    return defaultValue;
  }
  foreign bool genMetaInfo(bool defaultValue = true)
  {
    if ((McConfNoMetaInfo & attributes) == McConfNoMetaInfo)
      return false;
    return defaultValue;
  }
  
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_McConfigAttribute_h
