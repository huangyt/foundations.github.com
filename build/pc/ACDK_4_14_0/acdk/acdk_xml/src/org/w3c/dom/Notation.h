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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/Notation.h,v 1.9 2005/02/05 10:45:37 kommer Exp $

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

#ifndef org_w3c_dom_Notation_h
#define org_w3c_dom_Notation_h

namespace org {
namespace w3c {
namespace dom {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Notation);

/**
 * This interface represents a notation declared in the DTD. A notation either 
 * declares, by name, the format of an unparsed entity (see <a href='http://www.w3.org/TR/2004/REC-xml-20040204#Notations'>section 4.7</a> of the XML 1.0 specification [<a href='http://www.w3.org/TR/2004/REC-xml-20040204'>XML 1.0</a>]), or is 
 * used for formal declaration of processing instruction targets (see <a href='http://www.w3.org/TR/2004/REC-xml-20040204#sec-pi'>section 2.6</a> of the XML 1.0 specification [<a href='http://www.w3.org/TR/2004/REC-xml-20040204'>XML 1.0</a>]). The 
 * <code>nodeName</code> attribute inherited from <code>Node</code> is set 
 * to the declared name of the notation.
 * <p>The DOM Core does not support editing <code>Notation</code> nodes; they 
 * are therefore readonly.
 * <p>A <code>Notation</code> node does not have any parent.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 
  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:37 $
*/
ACDK_INTERFACE class ACDK_ORG_XML_PUBLIC Notation
: implements Node
{
  ACDK_WITH_METAINFO(Notation)
public: 
  /**
     * The public identifier of this notation. If the public identifier was 
     * not specified, this is <code>null</code>.
     */
  virtual RString getPublicID() = 0;
  /**
     * The system identifier of this notation. If the system identifier was 
     * not specified, this is <code>null</code>. This may be an absolute URI 
     * or not.
     */
  virtual RString getSystemID() = 0;
};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_Notation_h
