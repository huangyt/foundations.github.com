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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/Text.h,v 1.11 2005/02/05 10:45:37 kommer Exp $

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

#ifndef org_w3c_dom_Text_h
#define org_w3c_dom_Text_h

#include "CharacterData.h"

namespace org {
namespace w3c {
namespace dom {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Text);

/**
 * The <code>Text</code> interface inherits from <code>CharacterData</code> 
 * and represents the textual content (termed <a href='http://www.w3.org/TR/2004/REC-xml-20040204#syntax'>character data</a> in XML) of an <code>Element</code> or <code>Attr</code>. If there is no 
 * markup inside an element's content, the text is contained in a single 
 * object implementing the <code>Text</code> interface that is the only 
 * child of the element. If there is markup, it is parsed into the 
 * information items (elements, comments, etc.) and <code>Text</code> nodes 
 * that form the list of children of the element.
 * <p>When a document is first made available via the DOM, there is only one 
 * <code>Text</code> node for each block of text. Users may create adjacent 
 * <code>Text</code> nodes that represent the contents of a given element 
 * without any intervening markup, but should be aware that there is no way 
 * to represent the separations between these nodes in XML or HTML, so they 
 * will not (in general) persist between DOM editing sessions. The 
 * <code>Node.normalize()</code> method merges any such adjacent 
 * <code>Text</code> objects into a single node for each block of text.
 * <p> No lexical check is done on the content of a <code>Text</code> node 
 * and, depending on its position in the document, some characters must be 
 * escaped during serialization using character references; e.g. the 
 * characters "&lt;&amp;" if the textual content is part of an element or of 
 * an attribute, the character sequence "]]&gt;" when part of an element, 
 * the quotation mark character " or the apostrophe character ' when part of 
 * an attribute. 
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 
   @author Roger Rene Kommer
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:45:37 $
*/
ACDK_INTERFACE class ACDK_ORG_XML_PUBLIC Text
: implements CharacterData
{
  ACDK_WITH_METAINFO(Text)
public: 
  /**
     * Breaks this node into two nodes at the specified <code>offset</code>, 
     * keeping both in the tree as siblings. After being split, this node 
     * will contain all the content up to the <code>offset</code> point. A 
     * new node of the same type, which contains all the content at and 
     * after the <code>offset</code> point, is returned. If the original 
     * node had a parent node, the new node is inserted as the next sibling 
     * of the original node. When the <code>offset</code> is equal to the 
     * length of this node, the new node has no data.
     * @param offset The 16-bit unit offset at which to split, starting from 
     *   <code>0</code>.
     * @return The new node, of the same type as this node.
     * @exception DOMException
     *   INDEX_SIZE_ERR: Raised if the specified offset is negative or greater 
     *   than the number of 16-bit units in <code>data</code>.
     *   <br>NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
     */
  virtual RText splitText(int offset) THROWS1(RDOMException) = 0;

  /**
     * Returns whether this text node contains <a href='http://www.w3.org/TR/2004/REC-xml-infoset-20040204#infoitem.character'>
     * element content whitespace</a>, often abusively called "ignorable whitespace". The text node is 
     * determined to contain whitespace in element content during the load 
     * of the document or if validation occurs while using 
     * <code>Document.normalizeDocument()</code>.
     * @since DOM Level 3
     */
  // ### TODO implement me: public boolean isElementContentWhitespace();

    /**
     * Returns all text of <code>Text</code> nodes logically-adjacent text 
     * nodes to this node, concatenated in document order.
     * <br>For instance, in the example below <code>wholeText</code> on the 
     * <code>Text</code> node that contains "bar" returns "barfoo", while on 
     * the <code>Text</code> node that contains "foo" it returns "barfoo". 
     * @since DOM Level 3
     */
  // ### TODO implement me: public String getWholeText();

    /**
     * Replaces the text of the current node and all logically-adjacent text 
     * nodes with the specified text. All logically-adjacent text nodes are 
     * removed including the current node unless it was the recipient of the 
     * replacement text.
     * <br>This method returns the node which received the replacement text. 
     * The returned node is: 
     * <ul>
     * <li><code>null</code>, when the replacement text is 
     * the empty string;
     * </li>
     * <li>the current node, except when the current node is 
     * read-only;
     * </li>
     * <li> a new <code>Text</code> node of the same type (
     * <code>Text</code> or <code>CDATASection</code>) as the current node 
     * inserted at the location of the replacement.
     * </li>
     * </ul>
     * <br>For instance, in the above example calling 
     * <code>replaceWholeText</code> on the <code>Text</code> node that 
     * contains "bar" with "yo" in argument results in the following: 
     * <br>Where the nodes to be removed are read-only descendants of an 
     * <code>EntityReference</code>, the <code>EntityReference</code> must 
     * be removed instead of the read-only nodes. If any 
     * <code>EntityReference</code> to be removed has descendants that are 
     * not <code>EntityReference</code>, <code>Text</code>, or 
     * <code>CDATASection</code> nodes, the <code>replaceWholeText</code> 
     * method must fail before performing any modification of the document, 
     * raising a <code>DOMException</code> with the code 
     * <code>NO_MODIFICATION_ALLOWED_ERR</code>.
     * <br>For instance, in the example below calling 
     * <code>replaceWholeText</code> on the <code>Text</code> node that 
     * contains "bar" fails, because the <code>EntityReference</code> node 
     * "ent" contains an <code>Element</code> node which cannot be removed.
     * @param content The content of the replacing <code>Text</code> node.
     * @return The <code>Text</code> node created with the specified content.
     * @exception DOMException
     *   NO_MODIFICATION_ALLOWED_ERR: Raised if one of the <code>Text</code> 
     *   nodes being replaced is readonly.
     * @since DOM Level 3
     */
  // ### TODO implement me: public Text replaceWholeText(String content) throws DOMException;
  
};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_Text_h
