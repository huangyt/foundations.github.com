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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/traversal/NodeFilter.h,v 1.5 2005/02/05 10:45:37 kommer Exp $
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


#ifndef org_w3c_dom_traversal_NodeFilter_h
#define org_w3c_dom_traversal_NodeFilter_h

#include "traversal.h"
#include "../Node.h"

namespace org {
namespace w3c {
namespace dom {
namespace traversal {


enum NodeFilterAcceptFlags
{
    /**
     Accept the node. Navigation methods defined for 
     <code>NodeIterator</code> or <code>TreeWalker</code> will return this 
     node.
     */
  FILTER_ACCEPT             = 1,
    /**
     Reject the node. Navigation methods defined for 
     <code>NodeIterator</code> or <code>TreeWalker</code> will not return 
     this node. For <code>TreeWalker</code>, the children of this node 
     will also be rejected. <code>NodeIterators</code> treat this as a 
     synonym for <code>FILTER_SKIP</code>.
     */
  FILTER_REJECT             = 2,
    /**
     Skip this single node. Navigation methods defined for 
     <code>NodeIterator</code> or <code>TreeWalker</code> will not return 
     this node. For both <code>NodeIterator</code> and 
     <code>TreeWalker</code>, the children of this node will still be 
     considered. 
     */
  FILTER_SKIP               = 3
};
ACDK_DEF_LIB_ENUM(ACDK_ORG_XML_PUBLIC, NodeFilterAcceptFlags);

enum NodeFilterShowFlags
{

    // Constants for whatToShow
    /**
     Show all <code>Nodes</code>.
     */
  SHOW_ALL                  = 0xFFFFFFFF,
    /**
     Show <code>Element</code> nodes.
     */
  SHOW_ELEMENT              = 0x00000001,
    /**
     Show <code>Attr</code> nodes. This is meaningful only when creating an 
     <code>NodeIterator</code> or <code>TreeWalker</code> with an 
     attribute node as its <code>root</code>; in this case, it means that 
     the attribute node will appear in the first position of the iteration 
     or traversal. Since attributes are never children of other nodes, 
     they do not appear when traversing over the document tree.
     */
  SHOW_ATTRIBUTE            = 0x00000002,
    /**
     Show <code>Text</code> nodes.
     */
  SHOW_TEXT                 = 0x00000004,
    /**
     Show <code>CDATASection</code> nodes.
     */
  SHOW_CDATA_SECTION        = 0x00000008,
    /**
     Show <code>EntityReference</code> nodes.
     */
  SHOW_ENTITY_REFERENCE     = 0x00000010,
    /**
     Show <code>Entity</code> nodes. This is meaningful only when creating 
     an <code>NodeIterator</code> or <code>TreeWalker</code> with an 
     <code>Entity</code> node as its <code>root</code>; in this case, it 
     means that the <code>Entity</code> node will appear in the first 
     position of the traversal. Since entities are not part of the 
     document tree, they do not appear when traversing over the document 
     tree.
     */
  SHOW_ENTITY               = 0x00000020,
    /**
     Show <code>ProcessingInstruction</code> nodes.
     */
  SHOW_PROCESSING_INSTRUCTION = 0x00000040,
    /**
     Show <code>Comment</code> nodes.
     */
  SHOW_COMMENT              = 0x00000080,
    /**
     Show <code>Document</code> nodes.
     */
  SHOW_DOCUMENT             = 0x00000100,
    /**
     Show <code>DocumentType</code> nodes.
     */
  SHOW_DOCUMENT_TYPE        = 0x00000200,
    /**
     Show <code>DocumentFragment</code> nodes.
     */
  SHOW_DOCUMENT_FRAGMENT    = 0x00000400,
    /**
     Show <code>Notation</code> nodes. This is meaningful only when creating 
     an <code>NodeIterator</code> or <code>TreeWalker</code> with a 
     <code>Notation</code> node as its <code>root</code>; in this case, it 
     means that the <code>Notation</code> node will appear in the first 
     position of the traversal. Since notations are not part of the 
     document tree, they do not appear when traversing over the document 
     tree.
     */
  SHOW_NOTATION             = 0x00000800
};
ACDK_DEF_LIB_ENUM(ACDK_ORG_XML_PUBLIC, NodeFilterShowFlags);

ACDK_DECL_INTERFACE(NodeFilter);

/**
  Filters are objects that know how to "filter out" nodes. If a 
  <code>NodeIterator</code> or <code>TreeWalker</code> is given a 
  <code>NodeFilter</code>, it applies the filter before it returns the next 
  node. If the filter says to accept the node, the traversal logic returns 
  it; otherwise, traversal looks for the next node and pretends that the 
  node that was rejected was not there.
  <p>The DOM does not provide any filters. <code>NodeFilter</code> is just an 
  interface that users can implement to provide their own filters. 
  <p><code>NodeFilters</code> do not need to know how to traverse from node 
  to node, nor do they need to know anything about the data structure that 
  is being traversed. This makes it very easy to write filters, since the 
  only thing they have to know how to do is evaluate a single node. One 
  filter may be used with a number of different kinds of traversals, 
  encouraging code reuse.
  <p>See also the <a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Traversal-Range-20001113'>Document Object Model (DOM) Level 2 Traversal and Range Specification</a>.
  @since DOM Level 2
 */
class ACDK_ORG_XML_PUBLIC NodeFilter
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(NodeFilter)
public: 
  
    /**
     Test whether a specified node is visible in the logical view of a 
     <code>TreeWalker</code> or <code>NodeIterator</code>. This function 
     will be called by the implementation of <code>TreeWalker</code> and 
     <code>NodeIterator</code>; it is not normally called directly from 
     user code. (Though you could do so if you wanted to use the same 
     filter to guide your own application logic.)
     @param n The node to check to see if it passes the filter or not.
     @return A constant to determine whether the node is accepted, 
       rejected, or skipped, as defined above.
     */
  virtual short acceptNode(IN(RNode) n) = 0;
};

} // namespace traversal
} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_traversal_NodeFilter_h
