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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/xpath/XPathNSResolver.h,v 1.2 2005/02/05 10:45:37 kommer Exp $

#ifndef org_w3c_dom_xpath_XPathNSResolver_h
#define org_w3c_dom_xpath_XPathNSResolver_h

#include "XPathException.h"

namespace org {
namespace w3c {
namespace dom {
namespace xpath {



ACDK_DECL_INTERFACE(XPathNSResolver);

/**
  The <code>XPathNSResolver</code> interface permit <code>prefix</code> 
  strings in the expression to be properly bound to 
  <code>namespaceURI</code> strings. <code>XPathEvaluator</code> can 
  construct an implementation of <code>XPathNSResolver</code> from a node, 
  or the interface may be implemented by any application.
  <p>See also the <a href='http://www.w3.org/TR/2004/NOTE-DOM-Level-3-XPath-20040226'>Document Object Model (DOM) Level 3 XPath Specification</a>.
 */
class ACDK_ORG_XML_PUBLIC XPathNSResolver
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(XPathNSResolver)
public: 
  /**
    Look up the namespace URI associated to the given namespace prefix. The 
    XPath evaluator must never call this with a <code>null</code> or 
    empty argument, because the result of doing this is undefined.
    @param prefix The prefix to look for.
    @return Returns the associated namespace URI or <code>null</code> if 
      none is found.
  */
  virtual RString lookupNamespaceURI(IN(RString) prefix) = 0;
};


} // namespace xpath
} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_xpath_XPathNSResolver_h
