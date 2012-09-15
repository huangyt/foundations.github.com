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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/DOMErrorHandler.h,v 1.4 2005/02/05 10:45:37 kommer Exp $
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

#ifndef org_w3c_dom_DOMErrorHandler_h
#define org_w3c_dom_DOMErrorHandler_h

#include "DOMError.h"

namespace org {
namespace w3c {
namespace dom {


ACDK_DECL_INTERFACE(DOMErrorHandler);

/**
   <code>DOMErrorHandler</code> is a callback interface that the DOM 
  implementation can call when reporting errors that happens while 
  processing XML data, or when doing some other processing (e.g. validating 
  a document). A <code>DOMErrorHandler</code> object can be attached to a 
  <code>Document</code> using the "error-handler" on the 
  <code>DOMConfiguration</code> interface. If more than one error needs to 
  be reported during an operation, the sequence and numbers of the errors 
  passed to the error handler are implementation dependent. 
  <p> The application that is using the DOM implementation is expected to 
  implement this interface. 
  <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
  @since DOM Level 3
 */
class ACDK_ORG_XML_PUBLIC DOMErrorHandler
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(DOMErrorHandler)
public: 
  /**
     This method is called on the error handler when an error occurs.
     <br> If an exception is thrown from this method, it is considered to be 
     equivalent of returning <code>true</code>. 
     @param error  The error object that describes the error. This object 
       may be reused by the DOM implementation across multiple calls to 
       the <code>handleError</code> method. 
     @return  If the <code>handleError</code> method returns 
       <code>false</code>, the DOM implementation should stop the current 
       processing when possible. If the method returns <code>true</code>, 
       the processing may continue depending on 
       <code>DOMError.severity</code>. 
     */
  virtual bool handleError(IN(RDOMError) error) = 0;
};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_DOMErrorHandler_h
