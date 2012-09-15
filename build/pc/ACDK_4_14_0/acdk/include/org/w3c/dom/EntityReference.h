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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/EntityReference.h,v 1.8 2005/02/05 10:45:37 kommer Exp $

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

#ifndef org_w3c_dom_EntityReference_h
#define org_w3c_dom_EntityReference_h

namespace org {
namespace w3c {
namespace dom {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(EntityReference);

/**
 * <code>EntityReference</code> nodes may be used to represent an entity 
 * reference in the tree. Note that character references and references to 
 * predefined entities are considered to be expanded by the HTML or XML 
 * processor so that characters are represented by their Unicode equivalent 
 * rather than by an entity reference. Moreover, the XML processor may 
 * completely expand references to entities while building the 
 * <code>Document</code>, instead of providing <code>EntityReference</code> 
 * nodes. If it does provide such nodes, then for an 
 * <code>EntityReference</code> node that represents a reference to a known 
 * entity an <code>Entity</code> exists, and the subtree of the 
 * <code>EntityReference</code> node is a copy of the <code>Entity</code> 
 * node subtree. However, the latter may not be true when an entity contains 
 * an unbound namespace prefix. In such a case, because the namespace prefix 
 * resolution depends on where the entity reference is, the descendants of 
 * the <code>EntityReference</code> node may be bound to different namespace 
 * URIs. When an <code>EntityReference</code> node represents a reference to 
 * an unknown entity, the node has no children and its replacement value, 
 * when used by <code>Attr.value</code> for example, is empty.
 * <p>As for <code>Entity</code> nodes, <code>EntityReference</code> nodes and 
 * all their descendants are readonly.
 * <p ><b>Note:</b> <code>EntityReference</code> nodes may cause element 
 * content and attribute value normalization problems when, such as in XML 
 * 1.0 and XML Schema, the normalization is performed after entity reference 
 * are expanded.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:37 $
*/
ACDK_INTERFACE class ACDK_ORG_XML_PUBLIC EntityReference
: implements Node
{
  ACDK_WITH_METAINFO(EntityReference)
public: 

};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_EntityReference_h
