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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/traversal/NodeIterator.h,v 1.4 2005/02/20 13:56:33 kommer Exp $
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


#ifndef org_w3c_dom_traversal_NodeIterator_h
#define org_w3c_dom_traversal_NodeIterator_h

#include "traversal.h"
#include "../Node.h"
#include "NodeFilter.h"

namespace org {
namespace w3c {
namespace dom {
namespace traversal {

ACDK_DECL_INTERFACE(NodeIterator);

/**
  <code>NodeIterators</code> are used to step through a set of nodes, e.g. 
  the set of nodes in a <code>NodeList</code>, the document subtree 
  governed by a particular <code>Node</code>, the results of a query, or 
  any other set of nodes. The set of nodes to be iterated is determined by 
  the implementation of the <code>NodeIterator</code>. DOM Level 2 
  specifies a single <code>NodeIterator</code> implementation for 
  document-order traversal of a document subtree. Instances of these 
  <code>NodeIterators</code> are created by calling 
  <code>DocumentTraversal</code><code>.createNodeIterator()</code>.
  <p>See also the <a href='http://www.w3.org/TR/2000/REC-DOM-Level-2-Traversal-Range-20001113'>Document Object Model (DOM) Level 2 Traversal and Range Specification</a>.
  @since DOM Level 2
 */
class ACDK_ORG_XML_PUBLIC NodeIterator
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(NodeIterator)
public: 
  /**
     The root node of the <code>NodeIterator</code>, as specified when it 
     was created.
     */
  virtual RNode getRoot() = 0;

    /**
     This attribute determines which node types are presented via the 
     <code>NodeIterator</code>. The available set of constants is defined 
     in the <code>NodeFilter</code> interface.  Nodes not accepted by 
     <code>whatToShow</code> will be skipped, but their children may still 
     be considered. Note that this skip takes precedence over the filter, 
     if any. 
     */
  virtual int getWhatToShow() = 0;

    /**
     The <code>NodeFilter</code> used to screen nodes.
     */
  virtual RNodeFilter getFilter() = 0;

    /**
      The value of this flag determines whether the children of entity 
     reference nodes are visible to the <code>NodeIterator</code>. If 
     false, these children  and their descendants will be rejected. Note 
     that this rejection takes precedence over <code>whatToShow</code> and 
     the filter. Also note that this is currently the only situation where 
     <code>NodeIterators</code> may reject a complete subtree rather than 
     skipping individual nodes. 
     <br>
     <br> To produce a view of the document that has entity references 
     expanded and does not expose the entity reference node itself, use 
     the <code>whatToShow</code> flags to hide the entity reference node 
     and set <code>expandEntityReferences</code> to true when creating the 
     <code>NodeIterator</code>. To produce a view of the document that has 
     entity reference nodes but no entity expansion, use the 
     <code>whatToShow</code> flags to show the entity reference node and 
     set <code>expandEntityReferences</code> to false.
     */
  virtual bool getExpandEntityReferences() = 0;

    /**
     Returns the next node in the set and advances the position of the 
     <code>NodeIterator</code> in the set. After a 
     <code>NodeIterator</code> is created, the first call to 
     <code>nextNode()</code> returns the first node in the set.
     @return The next <code>Node</code> in the set being iterated over, or 
       <code>null</code> if there are no more members in that set.
     @exception DOMException
       INVALID_STATE_ERR: Raised if this method is called after the 
       <code>detach</code> method was invoked.
     */
  virtual RNode nextNode() THROWS1(RDOMException) = 0;

    /**
     Returns the previous node in the set and moves the position of the 
     <code>NodeIterator</code> backwards in the set.
     @return The previous <code>Node</code> in the set being iterated over, 
       or <code>null</code> if there are no more members in that set. 
     @exception DOMException
       INVALID_STATE_ERR: Raised if this method is called after the 
       <code>detach</code> method was invoked.
     */
  virtual RNode previousNode() THROWS1(RDOMException) = 0;

    /**
     Detaches the <code>NodeIterator</code> from the set which it iterated 
     over, releasing any computational resources and placing the 
     <code>NodeIterator</code> in the INVALID state. After 
     <code>detach</code> has been invoked, calls to <code>nextNode</code> 
     or <code>previousNode</code> will raise the exception 
     INVALID_STATE_ERR.
     */
  virtual void detach() = 0;
};

} // namespace traversal
} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_traversal_NodeIterator_h
