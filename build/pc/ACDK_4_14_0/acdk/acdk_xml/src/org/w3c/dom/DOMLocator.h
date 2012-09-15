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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/DOMLocator.h,v 1.3 2005/02/05 10:45:37 kommer Exp $
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

#ifndef org_w3c_dom_DOMLocator_h
#define org_w3c_dom_DOMLocator_h

#include "Node.h"

namespace org {
namespace w3c {
namespace dom {



ACDK_DECL_INTERFACE(DOMLocator);

/**
 <code>DOMLocator</code> is an interface that describes a location (e.g. 
 where an error occurred).
 <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 @since DOM Level 3
*/
class ACDK_ORG_XML_PUBLIC DOMLocator
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(DOMLocator)
public: 
  /**
    The line number this locator is pointing to, or <code>-1</code> if 
    there is no column number available.
  */
  virtual int getLineNumber() = 0;

    /**
    The column number this locator is pointing to, or <code>-1</code> if 
    there is no column number available.
     */
  virtual int getColumnNumber() = 0;

    /**
    The byte offset into the input source this locator is pointing to or 
    <code>-1</code> if there is no byte offset available.
     */
  virtual int getByteOffset() = 0;

    /**
    The UTF-16, as defined in [Unicode] and Amendment 1 of [ISO/IEC 10646], offset into the input source this locator is pointing to or 
    <code>-1</code> if there is no UTF-16 offset available.
     */
  virtual int getUtf16Offset() = 0;

    /**
    The node this locator is pointing to, or <code>null</code> if no node 
    is available.
     */
  virtual RNode getRelatedNode() = 0;

    /**
    The URI this locator is pointing to, or <code>null</code> if no URI is 
    available.
     */
  virtual RString getUri() = 0;
};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_DOMLocator_h
