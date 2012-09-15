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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/Branch.h,v 1.3 2005/02/05 10:45:37 kommer Exp $

#ifndef org_w3c_dom_Branch_h
#define org_w3c_dom_Branch_h

#include "Node.h"


namespace org {
namespace w3c {
namespace dom {

ACDK_DECL_INTERFACE(Element);
ACDK_DECL_INTERFACE(Branch);

/** 
  Extension inspired by dom4j
  @author Roger Rene Kommer
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:37 $
*/
class ACDK_ORG_XML_PUBLIC Branch
: implements Node
{
  ACDK_WITH_METAINFO(Branch)
public: 
  /**
    dom4j like extension
    @return the new added element
  */
  virtual RElement addElement(IN(RString) name);
  
};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_Branch_h
