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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/xpath/XPathExpression.h,v 1.2 2005/02/05 10:45:37 kommer Exp $

#ifndef org_w3c_dom_xpath_XPathExpression_h
#define org_w3c_dom_xpath_XPathExpression_h

#include "XPathException.h"
#include "../DOMException.h"
#include "../Node.h"

namespace org {
namespace w3c {
namespace dom {
namespace xpath {



ACDK_DECL_INTERFACE(XPathExpression);

/* 
  The <code>XPathExpression</code> interface represents a parsed and resolved 
  XPath expression.
  <p>See also the <a href='http://www.w3.org/TR/2004/NOTE-DOM-Level-3-XPath-20040226'>Document Object Model (DOM) Level 3 XPath Specification</a>.
*/
class ACDK_ORG_XML_PUBLIC XPathExpression
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(XPathExpression)
public: 
  /**
     Evaluates this XPath expression and returns a result.
     @param contextNode The <code>context</code> is context node for the 
       evaluation of this XPath expression.If the XPathEvaluator was 
       obtained by casting the <code>Document</code> then this must be 
       owned by the same document and must be a <code>Document</code>, 
       <code>Element</code>, <code>Attribute</code>, <code>Text</code>, 
       <code>CDATASection</code>, <code>Comment</code>, 
       <code>ProcessingInstruction</code>, or <code>XPathNamespace</code> 
       node.If the context node is a <code>Text</code> or a 
       <code>CDATASection</code>, then the context is interpreted as the 
       whole logical text node as seen by XPath, unless the node is empty 
       in which case it may not serve as the XPath context.
     @param type If a specific <code>type</code> is specified, then the 
       result will be coerced to return the specified type relying on 
       XPath conversions and fail if the desired coercion is not possible. 
       This must be one of the type codes of <code>XPathResult</code>.
     @param result The <code>result</code> specifies a specific result 
       object which may be reused and returned by this method. If this is 
       specified as <code>null</code>or the implementation does not reuse 
       the specified result, a new result object will be constructed and 
       returned.For XPath 1.0 results, this object will be of type 
       <code>XPathResult</code>.
     @return The result of the evaluation of the XPath expression.For XPath 
       1.0 results, this object will be of type <code>XPathResult</code>.
     @exception XPathException
       TYPE_ERR: Raised if the result cannot be converted to return the 
       specified type.
     @exception DOMException
       WRONG_DOCUMENT_ERR: The Node is from a document that is not supported 
       by the XPathEvaluator that created this <code>XPathExpression</code>
       .
       <br>NOT_SUPPORTED_ERR: The Node is not a type permitted as an XPath 
       context node or the request type is not permitted by this 
       <code>XPathExpression</code>.
   */
  virtual RObject evaluate(IN(RNode) contextNode, short type,  IN(RObject) result) THROWS2(RXPathException, RDOMException) = 0;
};


} // namespace xpath
} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_xpath_XPathExpression_h
