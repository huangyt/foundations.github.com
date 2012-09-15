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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/helpers/StdErrorHandler.h,v 1.5 2005/02/05 10:45:38 kommer Exp $

#ifndef org_xml_sax_helpers_StdErrorHandler_h
#define org_xml_sax_helpers_StdErrorHandler_h

#include "../ErrorHandler.h"
#include <acdk/util/logging/Log.h>

namespace org {
namespace xml {
namespace sax {
namespace helpers {

ACDK_DECL_CLASS(StdErrorHandler);

/**
  This ErrorHandler do logging and throws exception in case of error or fatalError
*/
class ACDK_ORG_XML_PUBLIC StdErrorHandler
: extends ::acdk::lang::Object
, implements ErrorHandler
{
  ACDK_WITH_METAINFO(StdErrorHandler)
public: 
  StdErrorHandler()
  {
  }
  /**
    logs Warn to "org.xml.sax"
  */

  virtual void warning(IN(RSAXParseException) saxparseexception) THROWS1(RSAXException)
  {
    ACDK_NLOG("org.xml.sax", Warn, saxparseexception->getMessage());
  }
  /**
    logs error to "org.xml.sax" and throws the saxparseexception
  */
  virtual void error(IN(RSAXParseException) saxparseexception) THROWS1(RSAXException)
  {
    ACDK_NLOG("org.xml.sax", Error, saxparseexception->getMessage());
    THROW_INSTANCE(saxparseexception);
  }
  /**
    logs error to "org.xml.sax" and throws the saxparseexception
  */

  virtual void fatalError(IN(RSAXParseException) saxparseexception) THROWS1(RSAXException)
  {
    ACDK_NLOG("org.xml.sax", Error, saxparseexception->getMessage());
    THROW_INSTANCE(saxparseexception);
  }
};



} // namespace org
} // namespace xml
} // namespace sax
} // namespace helpers

#endif //org_xml_sax_helpers_StdErrorHandler_h
