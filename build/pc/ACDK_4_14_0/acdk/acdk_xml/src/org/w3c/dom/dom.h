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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/dom.h,v 1.10 2005/02/05 10:45:37 kommer Exp $

#ifndef org_w3c_dom_dom_h
#define org_w3c_dom_dom_h

#ifndef acdk_h
# include <acdk.h>
#endif

#include "../../xml/sax/Config.h"

// defines the unit org/w3c/dom
ACDK_DECL_UNIT(org_w3c_dom)


/**
   Interfaces and implementation provided by organisations
*/
namespace org {
  /**
    Interfaces and implementation provided by the w3c organisation
    @see http://www.w3c.org
  */
namespace w3c {
  /**
     Interfaces Document Object Model for XML files
   */
namespace dom {
  
using namespace acdk::lang;
/*
  ACDK_DECL_INTERFACE(Node);
  ACDK_DECL_INTERFACE(NodeList);
  ACDK_DECL_INTERFACE(Document);
  ACDK_DECL_INTERFACE(DocumentType);
  ACDK_DECL_INTERFACE(Element);
  ACDK_DECL_INTERFACE(DocumentFragment);
  ACDK_DECL_INTERFACE(ProcessingInstruction);
  ACDK_DECL_INTERFACE(EntityReference);
  */

} // namespace dom
} // namespace w3c
} // namespace org
/*
#include "DOMException.h"

#include "NodeList.h"
#include "NamedNodeMap.h"

#include "Node.h"
#include "Attr.h"
#include "DOMImplementation.h"

#include "CharacterData.h"

#include "Comment.h"
#include "Text.h"
#include "CDATASection.h"


#include "Document.h"
#include "DocumentFragment.h"
#include "DocumentType.h"
#include "Element.h"

#include "Entity.h"
#include "EntityReference.h"


#include "Notation.h"
#include "ProcessingInstruction.h"

#include "Element.h"
*/



#endif //org_w3c_dom_dom_h
