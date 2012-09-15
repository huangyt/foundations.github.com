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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLXPathResult.h,v 1.4 2005/03/03 13:49:49 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLXPathResult_h
#define acdk_xml_libxmldom_LibXMLXPathResult_h

#include "libxmldom.h"

#include <org/w3c/dom/xpath/XPathResult.h>
#include <org/w3c/dom/xpath/XPathNSResolver.h>
#include "LibXMLNode.h"

struct _xmlXPathObject;
typedef struct _xmlXPathObject xmlXPathObject;
typedef xmlXPathObject *xmlXPathObjectPtr;

namespace acdk {
namespace xml {
namespace libxmldom {


ACDK_DECL_CLASS(LibXMLXPathResult);

class ACDK_XML_PUBLIC LibXMLXPathResult
: extends acdk::lang::Object
, implements org::w3c::dom::xpath::XPathResult
{
  ACDK_WITH_METAINFO(LibXMLXPathResult)
protected:
  foreign XmlNodePtrHolder _nodePtr;
  foreign xmlXPathObjectPtr _xpathPtr;
  int _curIdx;
public:
  foreign LibXMLXPathResult(xmlXPathObjectPtr xpathPtr, xmlNodePtr docNode)
  : _nodePtr(docNode)
  , _xpathPtr(xpathPtr)
  , _curIdx(0)
  {
  }
  foreign ~LibXMLXPathResult();
  virtual short getResultType();

  virtual double getNumberValue() THROWS1(org::w3c::dom::xpath::RXPathException);
  virtual RString getStringValue() THROWS1(org::w3c::dom::xpath::RXPathException);
  virtual bool getBooleanValue() THROWS1(org::w3c::dom::xpath::RXPathException);
  virtual org::w3c::dom::RNode getSingleNodeValue() THROWS1(org::w3c::dom::xpath::RXPathException);
  /**
    returns always false
  */
  virtual bool getInvalidIteratorState();
  virtual int getSnapshotLength() THROWS1(org::w3c::dom::xpath::RXPathException);
  virtual org::w3c::dom::RNode iterateNext() THROWS2(org::w3c::dom::xpath::RXPathException, org::w3c::dom::RDOMException);
  virtual org::w3c::dom::RNode snapshotItem(int index) THROWS1(org::w3c::dom::xpath::RXPathException);
  RObject getObjectValue() THROWS1(org::w3c::dom::xpath::RXPathException);
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_libxmldom_LibXMLXPathResult_h
