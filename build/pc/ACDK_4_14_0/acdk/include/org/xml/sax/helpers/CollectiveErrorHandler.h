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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/helpers/CollectiveErrorHandler.h,v 1.4 2005/02/05 10:45:38 kommer Exp $


#ifndef org_xml_sax_helpers_CollectiveErrorHandler_h
#define org_xml_sax_helpers_CollectiveErrorHandler_h

#include "../ErrorHandler.h"

namespace org {
namespace xml {
namespace sax {
namespace helpers {

ACDK_DECL_CLASS(CollectiveErrorHandler);

/**
  This ErrorHandler just collect the warnings and errors
*/
class ACDK_ORG_XML_PUBLIC CollectiveErrorHandler
: extends ::acdk::lang::Object
, implements ErrorHandler
{
  ACDK_WITH_METAINFO(CollectiveErrorHandler)
public: 
  RSAXParseExceptionArray _warnings;
  RSAXParseExceptionArray _errors;
  RSAXParseExceptionArray _fatalErrors;
  CollectiveErrorHandler()
  : _warnings(new SAXParseExceptionArray(0))
  , _errors(new SAXParseExceptionArray(0))
  , _fatalErrors(new SAXParseExceptionArray(0))
  {
  }
  virtual void warning(IN(RSAXParseException) saxparseexception) THROWS1(RSAXException)
  {
    _warnings->append(saxparseexception);
  }
  virtual void error(IN(RSAXParseException) saxparseexception) THROWS1(RSAXException)
  {
    _errors->append(saxparseexception);
  }
  virtual void fatalError(IN(RSAXParseException) saxparseexception) THROWS1(RSAXException)
  {
    _errors->append(saxparseexception);
  }
  bool hasErrors() { return _errors->length() > 0; }
  bool hasFatalErrors() { return _fatalErrors->length() > 0; }
  bool hasWarnings() { return _warnings->length() > 0; }
  bool hasAnyErrors() { return hasErrors() == true || hasFatalErrors() == true; }
};



} // namespace org
} // namespace xml
} // namespace sax
} // namespace helpers

#endif //org_xml_sax_helpers_CollectiveErrorHandler_h
