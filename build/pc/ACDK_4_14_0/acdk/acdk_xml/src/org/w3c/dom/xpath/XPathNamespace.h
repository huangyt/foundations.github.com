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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/xpath/XPathNamespace.h,v 1.2 2005/02/05 10:45:37 kommer Exp $

#ifndef org_w3c_dom_xpath_XPathNamespace_h
#define org_w3c_dom_xpath_XPathNamespace_h

#include "XPathExpression.h"
#include "../Element.h"

namespace org {
namespace w3c {
namespace dom {
namespace xpath {



ACDK_DECL_INTERFACE(XPathNamespace);

/**
  The <code>XPathNamespace</code> interface is returned by 
  <code>XPathResult</code> interfaces to represent the XPath namespace node 
  type that DOM lacks. There is no public constructor for this node type. 
  Attempts to place it into a hierarchy or a NamedNodeMap result in a 
  <code>DOMException</code> with the code <code>HIERARCHY_REQUEST_ERR</code>
  . This node is read only, so methods or setting of attributes that would 
  mutate the node result in a DOMException with the code 
  <code>NO_MODIFICATION_ALLOWED_ERR</code>.
  <p>The core specification describes attributes of the <code>Node</code> 
  interface that are different for different node types but does not 
  describe <code>XPATH_NAMESPACE_NODE</code>, so here is a description of 
  those attributes for this node type. All attributes of <code>Node</code> 
  not described in this section have a <code>null</code> or 
  <code>false</code> value.
  <p><code>ownerDocument</code> matches the <code>ownerDocument</code> of the 
  <code>ownerElement</code> even if the element is later adopted.
  <p><code>nodeName</code> is always the string "<code>#namespace</code>".
  <p><code>prefix</code> is the prefix of the namespace represented by the 
  node.
  <p><code>localName</code> is the same as <code>prefix</code>.
  <p><code>nodeType</code> is equal to <code>XPATH_NAMESPACE_NODE</code>.
  <p><code>namespaceURI</code> is the namespace URI of the namespace 
  represented by the node.
  <p><code>nodeValue</code> is the same as <code>namespaceURI</code>.
  <p><code>adoptNode</code>, <code>cloneNode</code>, and 
  <code>importNode</code> fail on this node type by raising a 
  <code>DOMException</code> with the code <code>NOT_SUPPORTED_ERR</code>.
  <p ><b>Note:</b> In future versions of the XPath specification, the 
  definition of a namespace node may be changed incomatibly, in which case 
  incompatible changes to field values may be required to implement 
  versions beyond XPath 1.0.
  <p>See also the <a href='http://www.w3.org/TR/2004/NOTE-DOM-Level-3-XPath-20040226'>Document Object Model (DOM) Level 3 XPath Specification</a>.
*/
ACDK_INTERFACE 
class ACDK_ORG_XML_PUBLIC XPathNamespace
: implements Node
{
  ACDK_WITH_METAINFO(XPathNamespace)
public: 
  /**
    The <code>Element</code> on which the namespace was in scope when it 
    was requested. This does not change on a returned namespace node even 
    if the document changes such that the namespace goes out of scope on 
    that element and this node is no longer found there by XPath.
  */
  virtual RElement getOwnerElement() = 0;
};


} // namespace xpath
} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_xpath_XPathNamespace_h
