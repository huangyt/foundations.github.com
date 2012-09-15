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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/traversal/DocumentTraversal.h,v 1.3 2005/02/05 10:45:37 kommer Exp $
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


#ifndef org_w3c_dom_traversal_DocumentTraversal_h
#define org_w3c_dom_traversal_DocumentTraversal_h

#include "NodeIterator.h"
#include "TreeWalker.h"

namespace org {
namespace w3c {
namespace dom {
namespace traversal {

ACDK_DECL_INTERFACE(DocumentTraversal);

/**
  <code>DocumentTraversal</code> contains methods that create 
  <code>NodeIterators</code> and <code>TreeWalkers</code> to traverse a 
  node and its children in document order (depth first, pre-order 
  traversal, which is equivalent to the order in which the start tags occur 
  in the text representation of the document). In DOMs which support the 
  Traversal feature, <code>DocumentTraversal</code> will be implemented by 
  the same objects that implement the Document interface.
  <p>See also the <a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Traversal-Range-20001113'>Document Object Model (DOM) Level 2 Traversal and Range Specification</a>.
  @since DOM Level 2
 */
class ACDK_ORG_XML_PUBLIC DocumentTraversal
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(DocumentTraversal)
public: 
  /**
     * Create a new <code>NodeIterator</code> over the subtree rooted at the 
     * specified node.
     * @param root The node which will be iterated together with its 
     *   children. The <code>NodeIterator</code> is initially positioned 
     *   just before this node. The <code>whatToShow</code> flags and the 
     *   filter, if any, are not considered when setting this position. The 
     *   root must not be <code>null</code>.
     * @param whatToShow This flag specifies which node types may appear in 
     *   the logical view of the tree presented by the 
     *   <code>NodeIterator</code>. See the description of 
     *   <code>NodeFilter</code> for the set of possible <code>SHOW_</code> 
     *   values.These flags can be combined using <code>OR</code>.
     * @param filter The <code>NodeFilter</code> to be used with this 
     *   <code>TreeWalker</code>, or <code>null</code> to indicate no filter.
     * @param entityReferenceExpansion The value of this flag determines 
     *   whether entity reference nodes are expanded.
     * @return The newly created <code>NodeIterator</code>.
     * @exception DOMException
     *   NOT_SUPPORTED_ERR: Raised if the specified <code>root</code> is 
     *   <code>null</code>.
     */
  virtual RNodeIterator createNodeIterator(IN(RNode) root,  int whatToShow, IN(RNodeFilter) filter,  bool entityReferenceExpansion) THROWS1(RDOMException) = 0;

    /**
     * Create a new <code>TreeWalker</code> over the subtree rooted at the 
     * specified node.
     * @param root The node which will serve as the <code>root</code> for the 
     *   <code>TreeWalker</code>. The <code>whatToShow</code> flags and the 
     *   <code>NodeFilter</code> are not considered when setting this value; 
     *   any node type will be accepted as the <code>root</code>. The 
     *   <code>currentNode</code> of the <code>TreeWalker</code> is 
     *   initialized to this node, whether or not it is visible. The 
     *   <code>root</code> functions as a stopping point for traversal 
     *   methods that look upward in the document structure, such as 
     *   <code>parentNode</code> and nextNode. The <code>root</code> must 
     *   not be <code>null</code>.
     * @param whatToShow This flag specifies which node types may appear in 
     *   the logical view of the tree presented by the 
     *   <code>TreeWalker</code>. See the description of 
     *   <code>NodeFilter</code> for the set of possible <code>SHOW_</code> 
     *   values.These flags can be combined using <code>OR</code>.
     * @param filter The <code>NodeFilter</code> to be used with this 
     *   <code>TreeWalker</code>, or <code>null</code> to indicate no filter.
     * @param entityReferenceExpansion If this flag is false, the contents of 
     *   <code>EntityReference</code> nodes are not presented in the logical 
     *   view.
     * @return The newly created <code>TreeWalker</code>.
     * @exception DOMException
     *    NOT_SUPPORTED_ERR: Raised if the specified <code>root</code> is 
     *   <code>null</code>.
     */
  virtual RTreeWalker createTreeWalker(IN(RNode) root,  int whatToShow, IN(RNodeFilter) filter, bool entityReferenceExpansion) THROWS1(RDOMException) = 0;
};

} // namespace traversal
} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_traversal_DocumentTraversal_h
