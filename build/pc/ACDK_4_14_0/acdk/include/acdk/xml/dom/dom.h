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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/dom.h,v 1.11 2005/02/12 17:33:46 kommer Exp $

#ifndef acdk_xml_dom_dom_h
#define acdk_xml_dom_dom_h

#ifndef acdk_h
# include <acdk.h>
#endif

#include "../../xml/Config.h"
// defines the unit acdk/xml/dom
ACDK_DECL_UNIT(acdk_xml_dom)

namespace acdk {
namespace xml {
/**
   Deprecated XML implementation following the DOM interface.
   Please use the implementation in acdk::xml::libxmldom.
   @see org::w3c::dom
*/
namespace dom {
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
} // namespace acdk



#endif //org_xml_dom_dom_h
