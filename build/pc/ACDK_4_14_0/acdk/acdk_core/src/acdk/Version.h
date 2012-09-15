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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/Version.h,v 1.7 2005/04/09 19:26:42 kommer Exp $


#ifndef acdk_Version_h
#define acdk_Version_h

/** 
  defines the major ACDK version 
  @ingroup acdkplatformmacros
*/
#define ACDK_MAJOR_VERSION   4
/** 
  defines the minor ACDK version 
  @ingroup acdkplatformmacros
*/
#define ACDK_MINOR_VERSION   14
/**
  defines the release number
  @ingroup acdkplatformmacros
*/
#define ACDK_RELEASE_VERSION 0

#define ACDK_VERSION_NUMBER (ACDK_MAJOR_VERSION * 10000) + (ACDK_MINOR_VERSION * 100) + ACDK_RELEASE_VERSION

/**
  acdk version as string 
  @ingroup acdkplatformmacros
*/
#define ACDK_VERSION_STRING "ACDK 4.14.0"

/**
  Macro to check for version compatibility
  @ingroup acdkplatformmacros
*/
#define ACDK_CHECK_VERSION(Major, Minor, Release) \
    (ACDK_MAJOR_VERSION > (Major) || \
    (ACDK_MAJOR_VERSION == (Major) && ACDK_MINOR_VERSION > (Minor)) || \
    (ACDK_MAJOR_VERSION == (Major) && ACDK_MINOR_VERSION == (Minor) && ACDK_RELEASE_VERSION >= (Release)))

#endif //acdk_Version_h
