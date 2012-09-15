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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/CDATASection.h,v 1.10 2005/02/05 10:45:37 kommer Exp $

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

#ifndef org_w3c_dom_CDATASection_h
#define org_w3c_dom_CDATASection_h

#include "Text.h"

namespace org {
namespace w3c {
namespace dom {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(CDATASection);

/** 
 * CDATA sections are used to escape blocks of text containing characters that 
 * would otherwise be regarded as markup. The only delimiter that is 
 * recognized in a CDATA section is the "]]&gt;" string that ends the CDATA 
 * section. CDATA sections cannot be nested. Their primary purpose is for 
 * including material such as XML fragments, without needing to escape all 
 * the delimiters.
 * <p>The <code>CharacterData.data</code> attribute holds the text that is 
 * contained by the CDATA section. Note that this <em>may</em> contain characters that need to be escaped outside of CDATA sections and 
 * that, depending on the character encoding ("charset") chosen for 
 * serialization, it may be impossible to write out some characters as part 
 * of a CDATA section.
 * <p>The <code>CDATASection</code> interface inherits from the 
 * <code>CharacterData</code> interface through the <code>Text</code> 
 * interface. Adjacent <code>CDATASection</code> nodes are not merged by use 
 * of the <code>normalize</code> method of the <code>Node</code> interface.
 * <p> No lexical check is done on the content of a CDATA section and it is 
 * therefore possible to have the character sequence <code>"]]&gt;"</code> 
 * in the content, which is illegal in a CDATA section per section 2.7 of [<a href='http://www.w3.org/TR/2004/REC-xml-20040204'>XML 1.0</a>]. The 
 * presence of this character sequence must generate a fatal error during 
 * serialization or the cdata section must be splitted before the 
 * serialization (see also the parameter <code>"split-cdata-sections"</code> 
 * in the <code>DOMConfiguration</code> interface). 
 * <p ><b>Note:</b> Because no markup is recognized within a 
 * <code>CDATASection</code>, character numeric references cannot be used as 
 * an escape mechanism when serializing. Therefore, action needs to be taken 
 * when serializing a <code>CDATASection</code> with a character encoding 
 * where some of the contained characters cannot be represented. Failure to 
 * do so would not produce well-formed XML.
 * <p ><b>Note:</b> One potential solution in the serialization process is to 
 * end the CDATA section before the character, output the character using a 
 * character reference or entity reference, and open a new CDATA section for 
 * any further characters in the text node. Note, however, that some code 
 * conversion libraries at the time of writing do not return an error or 
 * exception when a character is missing from the encoding, making the task 
 * of ensuring that data is not corrupted on serialization more difficult.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:37 $
*/
ACDK_INTERFACE class ACDK_ORG_XML_PUBLIC CDATASection
: implements Text
{
  ACDK_WITH_METAINFO(CDATASection)
public: 
  // no additionally interfaces
};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_CDATASection_h
