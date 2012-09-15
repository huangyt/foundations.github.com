// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright(C) 2000-2003 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/xpath/XPathException.h,v 1.4 2005/02/05 10:45:37 kommer Exp $

#ifndef org_w3c_dom_xpath_XPathException_h
#define org_w3c_dom_xpath_XPathException_h

#include "xpath.h"
#include <acdk/lang/RuntimeException.h>

namespace org {
namespace w3c {
namespace dom {
namespace xpath {

enum XPathExceptionCode
{
  XPATH_NUMBER_ERROR  = 1201,
  XPATH_UNFINISHED_LITERAL_ERROR, /* 1202 */
  XPATH_START_LITERAL_ERROR, /* 1203 */
  XPATH_VARIABLE_REF_ERROR, /* 1204 */
  XPATH_UNDEF_VARIABLE_ERROR, /* 1205 */
  XPATH_INVALID_PREDICATE_ERROR, /* 1206 */
  XPATH_EXPR_ERROR,  /* 1207 */
  XPATH_UNCLOSED_ERROR, /* 1208 */
  XPATH_UNKNOWN_FUNC_ERROR, /* 1209 */
  XPATH_INVALID_OPERAND, /* 1210 */
  XPATH_INVALID_TYPE, /* 1211 */
  XPATH_INVALID_ARITY, /* 1212 */
  XPATH_INVALID_CTXT_SIZE, /* 1213 */
  XPATH_INVALID_CTXT_POSITION, /* 1214 */
  XPATH_MEMORY_ERROR /* 1215 */
};
ACDK_DEF_LIB_ENUM(ACDK_ORG_XML_PUBLIC, XPathExceptionCode);

ACDK_DECL_THROWABLE(XPathException, RuntimeException);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:37 $
*/
class ACDK_ORG_XML_PUBLIC XPathException
: extends acdk::lang::RuntimeException
{
  ACDK_WITH_METAINFO(XPathException)

public: 
  short _code;
  XPathException(short code, IN(RString) message) 
  : RuntimeException(message)
  , _code(code)
  {
  }  
};


} // namespace xpath
} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_xpath_XPathException_h
