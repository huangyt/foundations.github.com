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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/DOMImplementation.h,v 1.13 2005/02/05 10:45:37 kommer Exp $

/*
  Documentation:
  Copyright (c) 2004 World Wide Web Consortium,
 
  (Massachusetts Institute of Technology, European Research Consortium for
  Informatics and Mathematics, Keio University). All Rights Reserved. This
  work is distributed under the W3C(r) Software License [1] in the hope that
  it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 
  [1] http://www.w3.org/Consortium/Legal/2002/copyright-software-20021231
*/

#ifndef org_w3c_dom_DOMImplementation_h
#define org_w3c_dom_DOMImplementation_h

#include "Node.h"
#include "DocumentType.h"

namespace org {
namespace w3c {
namespace dom {

ACDK_DECL_INTERFACE(Document);
ACDK_DECL_INTERFACE(DocumentType);

ACDK_DECL_INTERFACE(DOMImplementation);

/**
 * The <code>DOMImplementation</code> interface provides a number of methods 
 * for performing operations that are independent of any particular instance 
 * of the document object model.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 
  @author Roger Rene Kommer
  @version $Revision: 1.13 $
  @date $Date: 2005/02/05 10:45:37 $
*/
class ACDK_ORG_XML_PUBLIC DOMImplementation
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(DOMImplementation)
public: 
  /**
     * Test if the DOM implementation implements a specific feature and 
     * version, as specified in .
     * @param feature  The name of the feature to test. 
     * @param version  This is the version number of the feature to test. 
     * @return <code>true</code> if the feature is implemented in the 
     *   specified version, <code>false</code> otherwise.
     */
  virtual bool hasFeature(IN(RString) feature, IN(RString) version) = 0;
  /**
     * Creates a DOM Document object of the specified type with its document 
     * element.
     * <br>Note that based on the <code>DocumentType</code> given to create 
     * the document, the implementation may instantiate specialized 
     * <code>Document</code> objects that support additional features than 
     * the "Core", such as "HTML" [<a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>DOM Level 2 HTML</a>]
     * . On the other hand, setting the <code>DocumentType</code> after the 
     * document was created makes this very unlikely to happen. 
     * Alternatively, specialized <code>Document</code> creation methods, 
     * such as <code>createHTMLDocument</code> [<a href='http://www.w3.org/TR/2003/REC-DOM-Level-2-HTML-20030109'>DOM Level 2 HTML</a>]
     * , can be used to obtain specific types of <code>Document</code> 
     * objects.
     * @param namespaceURI The namespace URI of the document element to 
     *   create or <code>null</code>.
     * @param qualifiedName The qualified name of the document element to be 
     *   created or <code>null</code>.
     * @param doctype The type of document to be created or <code>null</code>.
     *   When <code>doctype</code> is not <code>null</code>, its 
     *   <code>Node.ownerDocument</code> attribute is set to the document 
     *   being created.
     * @return A new <code>Document</code> object with its document element. 
     *   If the <code>NamespaceURI</code>, <code>qualifiedName</code>, and 
     *   <code>doctype</code> are <code>null</code>, the returned 
     *   <code>Document</code> is empty with no document element.
     * @exception DOMException
     *   INVALID_CHARACTER_ERR: Raised if the specified qualified name is not 
     *   an XML name according to [<a href='http://www.w3.org/TR/2004/REC-xml-20040204'>XML 1.0</a>].
     *   <br>NAMESPACE_ERR: Raised if the <code>qualifiedName</code> is 
     *   malformed, if the <code>qualifiedName</code> has a prefix and the 
     *   <code>namespaceURI</code> is <code>null</code>, or if the 
     *   <code>qualifiedName</code> is <code>null</code> and the 
     *   <code>namespaceURI</code> is different from <code>null</code>, or 
     *   if the <code>qualifiedName</code> has a prefix that is "xml" and 
     *   the <code>namespaceURI</code> is different from "<a href='http://www.w3.org/XML/1998/namespace'>
     *   http://www.w3.org/XML/1998/namespace</a>" [<a href='http://www.w3.org/TR/1999/REC-xml-names-19990114/'>XML Namespaces</a>]
     *   , or if the DOM implementation does not support the 
     *   <code>"XML"</code> feature but a non-null namespace URI was 
     *   provided, since namespaces were defined by XML.
     *   <br>WRONG_DOCUMENT_ERR: Raised if <code>doctype</code> has already 
     *   been used with a different document or was created from a different 
     *   implementation.
     *   <br>NOT_SUPPORTED_ERR: May be raised if the implementation does not 
     *   support the feature "XML" and the language exposed through the 
     *   Document does not support XML Namespaces (such as [<a href='http://www.w3.org/TR/1999/REC-html401-19991224/'>HTML 4.01</a>]). 
     * @since DOM Level 2
     */
  virtual RDocument createDocument(IN(RString) namespaceURI, IN(RString) qualifiedName, IN(RDocumentType) doctype) = 0;
  
  /*
     * Creates an empty <code>DocumentType</code> node. Entity declarations 
     * and notations are not made available. Entity reference expansions and 
     * default attribute additions do not occur..
     * @param qualifiedName The qualified name of the document type to be 
     *   created.
     * @param publicId The external subset public identifier.
     * @param systemId The external subset system identifier.
     * @return A new <code>DocumentType</code> node with 
     *   <code>Node.ownerDocument</code> set to <code>null</code>.
     * @exception DOMException
     *   INVALID_CHARACTER_ERR: Raised if the specified qualified name is not 
     *   an XML name according to [<a href='http://www.w3.org/TR/2004/REC-xml-20040204'>XML 1.0</a>].
     *   <br>NAMESPACE_ERR: Raised if the <code>qualifiedName</code> is 
     *   malformed.
     *   <br>NOT_SUPPORTED_ERR: May be raised if the implementation does not 
     *   support the feature "XML" and the language exposed through the 
     *   Document does not support XML Namespaces (such as [<a href='http://www.w3.org/TR/1999/REC-html401-19991224/'>HTML 4.01</a>]). 
     * @since DOM Level 2
     */
    // ### TODO implement me: public DocumentType createDocumentType(String qualifiedName,  String publicId,  String systemId) throws DOMException;

  
   /*
     *  This method returns a specialized object which implements the 
     * specialized APIs of the specified feature and version, as specified 
     * in . The specialized object may also be obtained by using 
     * binding-specific casting methods but is not necessarily expected to, 
     * as discussed in . This method also allow the implementation to 
     * provide specialized objects which do not support the 
     * <code>DOMImplementation</code> interface. 
     * @param feature  The name of the feature requested. Note that any plus 
     *   sign "+" prepended to the name of the feature will be ignored since 
     *   it is not significant in the context of this method. 
     * @param version  This is the version number of the feature to test. 
     * @return  Returns an object which implements the specialized APIs of 
     *   the specified feature and version, if any, or <code>null</code> if 
     *   there is no object which implements interfaces associated with that 
     *   feature. If the <code>DOMObject</code> returned by this method 
     *   implements the <code>DOMImplementation</code> interface, it must 
     *   delegate to the primary core <code>DOMImplementation</code> and not 
     *   return results inconsistent with the primary core 
     *   <code>DOMImplementation</code> such as <code>hasFeature</code>, 
     *   <code>getFeature</code>, etc. 
     * @since DOM Level 3
     */
    // ### TODO implement me: public Object getFeature(String feature,  String version);

};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_DOMImplementation_h
