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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/ProcessingInstruction.h,v 1.10 2005/02/05 10:45:37 kommer Exp $

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

#ifndef org_w3c_dom_ProcessingInstruction_h
#define org_w3c_dom_ProcessingInstruction_h

namespace org {
namespace w3c {
namespace dom {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(ProcessingInstruction);

/**
 * The <code>ProcessingInstruction</code> interface represents a "processing 
 * instruction", used in XML as a way to keep processor-specific information 
 * in the text of the document.
 * <p> No lexical check is done on the content of a processing instruction and 
 * it is therefore possible to have the character sequence 
 * <code>"?&gt;"</code> in the content, which is illegal a processing 
 * instruction per section 2.6 of [<a href='http://www.w3.org/TR/2004/REC-xml-20040204'>XML 1.0</a>]. The 
 * presence of this character sequence must generate a fatal error during 
 * serialization. 
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:37 $
*/
ACDK_INTERFACE class ACDK_ORG_XML_PUBLIC ProcessingInstruction
: implements Node
{
  ACDK_WITH_METAINFO(ProcessingInstruction)
public: 
  /**
     * The target of this processing instruction. XML defines this as being 
     * the first token following the markup that begins the processing 
     * instruction.
     */
  virtual RString getTarget() = 0;
  /**
     * The content of this processing instruction. This is from the first non 
     * white space character after the target to the character immediately 
     * preceding the <code>?&gt;</code>.
     */
  virtual RString getData() = 0;
  /**
     * The content of this processing instruction. This is from the first non 
     * white space character after the target to the character immediately 
     * preceding the <code>?&gt;</code>.
     * @exception DOMException
     *   NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
     */
  virtual void setData(IN(RString) data) THROWS1(RDOMException) = 0;
};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_ProcessingInstruction_h
