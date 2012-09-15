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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/DOMError.h,v 1.5 2005/02/05 10:45:37 kommer Exp $
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

#ifndef org_w3c_dom_DOMError_h
#define org_w3c_dom_DOMError_h

#include "DOMLocator.h"

namespace org {
namespace w3c {
namespace dom {


enum ErrorSeverity
{
    /**
     The severity of the error described by the <code>DOMError</code> is 
     warning. A <code>SEVERITY_WARNING</code> will not cause the 
     processing to stop, unless <code>DOMErrorHandler.handleError()</code> 
     returns <code>false</code>.
     */
  SevertyWarning = 1,
    /**
     The severity of the error described by the <code>DOMError</code> is 
     error. A <code>SEVERITY_ERROR</code> may not cause the processing to 
     stop if the error can be recovered, unless 
     <code>DOMErrorHandler.handleError()</code> returns <code>false</code>.
     */
  SevertyError = 2,
    /**
     The severity of the error described by the <code>DOMError</code> is 
     fatal error. A <code>SEVERITY_FATAL_ERROR</code> will cause the 
     normal processing to stop. The return value of 
     <code>DOMErrorHandler.handleError()</code> is ignored unless the 
     implementation chooses to continue, in which case the behavior 
     becomes undefined.
     */
  SevertyFatalError      = 3
};
ACDK_DEF_LIB_ENUM(ACDK_ORG_XML_PUBLIC, ErrorSeverity);

ACDK_DECL_INTERFACE(DOMError);

/**
  <code>DOMError</code> is an interface that describes an error.
  <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
  @since DOM Level 3
 */
class ACDK_ORG_XML_PUBLIC DOMError
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(DOMError)
public: 
  /**
     The severity of the error, either <code>SEVERITY_WARNING</code>, 
     <code>SEVERITY_ERROR</code>, or <code>SEVERITY_FATAL_ERROR</code>.
     */
  virtual short getSeverity() = 0;

    /**
     An implementation specific string describing the error that occurred.
     */
  virtual RString getMessage() = 0;

    /**
      A <code>DOMString</code> indicating which related data is expected in 
     <code>relatedData</code>. Users should refer to the specification of 
     the error in order to find its <code>DOMString</code> type and 
     <code>relatedData</code> definitions if any. 
     <p ><b>Note:</b>  As an example, 
     <code>Document.normalizeDocument()</code> does generate warnings when 
     the "split-cdata-sections" parameter is in use. Therefore, the method 
     generates a <code>SEVERITY_WARNING</code> with <code>type</code> 
     <code>"cdata-sections-splitted"</code> and the first 
     <code>CDATASection</code> node in document order resulting from the 
     split is returned by the <code>relatedData</code> attribute. 
     */
  virtual RString getType() = 0;

    /**
     The related platform dependent exception if any.
     */
  virtual RObject getRelatedException() = 0;

    /**
      The related <code>DOMError.type</code> dependent data if any. 
     */
  virtual RObject getRelatedData() = 0;

    /**
     The location of the error.
     */
  virtual RDOMLocator getLocation() = 0;
};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_DOMError_h
