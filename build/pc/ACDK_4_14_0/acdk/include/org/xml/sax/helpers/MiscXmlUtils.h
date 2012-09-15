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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/helpers/MiscXmlUtils.h,v 1.4 2005/02/05 10:45:38 kommer Exp $

#ifndef org_xml_sax_helpers_MiscXmlUtils_h
#define org_xml_sax_helpers_MiscXmlUtils_h

#include <acdk.h>
#include "../Config.h"

namespace org {
namespace xml {
namespace sax {
namespace helpers {

ACDK_DECL_CLASS(MiscXmlUtils);

/**
  This class contains misc helper function
*/
class ACDK_ORG_XML_PUBLIC MiscXmlUtils
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(MiscXmlUtils)
protected:
  /// don't create instance
  MiscXmlUtils()
  {
  }

public: 
  /**
    convert a XML encoding name to a name corresponding ACDK names
    of encodings
    @return If encoding is unknown, returns the original parameter
  */
  static RString xmlEncodingNameToAcdkEncodingName(IN(RString) encname);
};



} // namespace org
} // namespace xml
} // namespace sax
} // namespace helpers

#endif //org_xml_sax_helpers_MiscXmlUtils_h
