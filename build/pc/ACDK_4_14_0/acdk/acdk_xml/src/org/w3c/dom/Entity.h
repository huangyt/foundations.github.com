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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/Entity.h,v 1.8 2005/02/05 10:45:37 kommer Exp $

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

#ifndef org_w3c_dom_Entity_h
#define org_w3c_dom_Entity_h

namespace org {
namespace w3c {
namespace dom {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Entity);

/**
 * This interface represents a known entity, either parsed or unparsed, in an 
 * XML document. Note that this models the entity itself <em>not</em> the entity declaration.
 * <p>The <code>nodeName</code> attribute that is inherited from 
 * <code>Node</code> contains the name of the entity.
 * <p>An XML processor may choose to completely expand entities before the 
 * structure model is passed to the DOM; in this case there will be no 
 * <code>EntityReference</code> nodes in the document tree.
 * <p>XML does not mandate that a non-validating XML processor read and 
 * process entity declarations made in the external subset or declared in 
 * parameter entities. This means that parsed entities declared in the 
 * external subset need not be expanded by some classes of applications, and 
 * that the replacement text of the entity may not be available. When the <a href='http://www.w3.org/TR/2004/REC-xml-20040204#intern-replacement'>
 * replacement text</a> is available, the corresponding <code>Entity</code> node's child list 
 * represents the structure of that replacement value. Otherwise, the child 
 * list is empty.
 * <p>DOM Level 3 does not support editing <code>Entity</code> nodes; if a 
 * user wants to make changes to the contents of an <code>Entity</code>, 
 * every related <code>EntityReference</code> node has to be replaced in the 
 * structure model by a clone of the <code>Entity</code>'s contents, and 
 * then the desired changes must be made to each of those clones instead. 
 * <code>Entity</code> nodes and all their descendants are readonly.
 * <p>An <code>Entity</code> node does not have any parent.
 * <p ><b>Note:</b> If the entity contains an unbound namespace prefix, the 
 * <code>namespaceURI</code> of the corresponding node in the 
 * <code>Entity</code> node subtree is <code>null</code>. The same is true 
 * for <code>EntityReference</code> nodes that refer to this entity, when 
 * they are created using the <code>createEntityReference</code> method of 
 * the <code>Document</code> interface.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:37 $
*/
ACDK_INTERFACE class ACDK_ORG_XML_PUBLIC Entity
: implements Node
{
  ACDK_WITH_METAINFO(Entity)
public: 
   /**
     * The public identifier associated with the entity if specified, and 
     * <code>null</code> otherwise.
     */
  virtual RString getPublicId() = 0;
  /**
     * The system identifier associated with the entity if specified, and 
     * <code>null</code> otherwise. This may be an absolute URI or not.
     */
  virtual RString getSystemId() = 0;
  /**
     * For unparsed entities, the name of the notation for the entity. For 
     * parsed entities, this is <code>null</code>.
     */
  virtual RString getNotationName() = 0;

  /**
     * An attribute specifying the encoding used for this entity at the time 
     * of parsing, when it is an external parsed entity. This is 
     * <code>null</code> if it an entity from the internal subset or if it 
     * is not known.
     * @since DOM Level 3
     */
  // ### TODO implement me: public String getInputEncoding();

    /**
     * An attribute specifying, as part of the text declaration, the encoding 
     * of this entity, when it is an external parsed entity. This is 
     * <code>null</code> otherwise.
     * @since DOM Level 3
     */
  // ### TODO implement me: public String getXmlEncoding();

    /**
     * An attribute specifying, as part of the text declaration, the version 
     * number of this entity, when it is an external parsed entity. This is 
     * <code>null</code> otherwise.
     * @since DOM Level 3
     */
  // ### TODO implement me: public String getXmlVersion();


};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_Entity_h
