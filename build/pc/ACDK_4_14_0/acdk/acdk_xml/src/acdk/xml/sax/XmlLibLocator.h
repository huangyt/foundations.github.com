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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/sax/XmlLibLocator.h,v 1.4 2005/02/05 10:45:37 kommer Exp $

#ifndef acdk_xml_sax_XmlLibLocator_h
#define acdk_xml_sax_XmlLibLocator_h

#include "LibXMLInternals.h"
#include <org/xml/sax/Locator.h>

namespace acdk {
namespace xml {
namespace sax {



ACDK_DECL_CLASS(XmlLibLocator);

/** 
  API: org.xml.sax<br>
  @author Roger Rene Kommer
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:37 $
*/
class ACDK_XML_PUBLIC XmlLibLocator
: extends ::acdk::lang::Object
, implements ::org::xml::sax::Locator
{
  xmlParserCtxtPtr _ctx;
  xmlSAXLocatorPtr _loc;
public: 
  XmlLibLocator(xmlParserCtxtPtr pcp, xmlSAXLocatorPtr loc) 
    : _ctx(pcp)
    , _loc(loc)
  {}

  RString getPublicId()
  {
    SaxParseContext* sax = (SaxParseContext*)_ctx->_private;
    return sax->publicId;
  }
  RString getSystemId()
  {
    SaxParseContext* sax = (SaxParseContext*)_ctx->_private;
    return sax->systemId;
  }
  int getLineNumber()
  {
    return _ctx->input->line;
  }
  int getColumnNumber()
  {
    return _ctx->input->col;
  }

};

} // namespace sax
} // namespace xml
} // namespace acdk

#endif //acdk_xml_sax_XmlLibLocator_h
