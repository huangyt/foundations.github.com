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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FilenameFilter.h,v 1.10 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_FilenameFilter_h
#define acdk_io_FilenameFilter_h

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(File);

ACDK_DECL_INTERFACE(FilenameFilter);

/** 
  Filter used in connection the acdk::io::File class.
  API: Java<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC FilenameFilter 
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(FilenameFilter)
public:
  virtual bool accept(IN(RFile) dir, IN(RString) name) = 0;
};


} // io
} // lang

#endif //acdk_io_FilenameFilter_h

