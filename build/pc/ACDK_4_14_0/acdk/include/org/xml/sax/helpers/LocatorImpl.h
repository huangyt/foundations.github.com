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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/helpers/LocatorImpl.h,v 1.5 2005/02/05 10:45:38 kommer Exp $

#ifndef org_xml_sax_helpers_LocatorImpl_h
#define org_xml_sax_helpers_LocatorImpl_h

#include <acdk.h>
#include <acdk/util/Hashtable.h>
#include <acdk/util/EmptyCollectionIterator.h>

#include "../Locator.h"

namespace org {
namespace xml {
namespace sax {
namespace helpers {

ACDK_DECL_CLASS(LocatorImpl);

/**
  default implementaiton of org::xml::sax::Locator interface
*/
class ACDK_ORG_XML_PUBLIC LocatorImpl
: extends acdk::lang::Object
, implements org::xml::sax::Locator
{
  ACDK_WITH_METAINFO(LocatorImpl)
private:
  int _lineNumber;
  int _columnNumber;
  RString _publicId;
  RString _systemId;
public:
  LocatorImpl(int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId)
  : _lineNumber(lineNumber)
  , _columnNumber(columnNumber)
  , _publicId(publicId)
  , _systemId(systemId)
  {
  }
  RString getPublicId() { return _publicId; }
  RString getSystemId() { return _systemId; }
  int getLineNumber() { return _lineNumber; }
  int getColumnNumber() { return _columnNumber; }
};

} // namespace helpers
} // namespace sax
} // namespace xml
} // namespace org

#endif //org_xml_sax_helpers_LocatorImpl_h
