// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright(C) 2000-2003 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLDOMInternals.h,v 1.2 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLDOMInternals_h
#define acdk_xml_libxmldom_LibXMLDOMInternals_h

#include <org/w3c/dom/Node.h>

#include "../sax/LibXMLInternals.h"
#include <libxml/parser.h>
#include <libxml/xpath.h>


namespace acdk {
namespace xml {
namespace libxmldom {

bool _match(const xmlChar* name, xmlNodePtr node);
bool _matchNS(const xmlChar* uri, const xmlChar* localName, xmlNodePtr node);
xmlNodePtr _getNodePtr(IN(org::w3c::dom::RNode) node);

const xmlChar* _getPrefix(const xmlChar* fqName);
const xmlChar* _getLocalName(const xmlChar* fqName);

org::w3c::dom::RNodeList _getElementsByTagName(xmlNodePtr _nodePtr, IN(RString) name);
org::w3c::dom::RNodeList _getElementsByTagNameNS(xmlNodePtr _nodePtr, IN(RString) namespaceURI, IN(RString) localName);


} // namespace libxmldom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_libxmldom_LibXMLDOMInternals_h
