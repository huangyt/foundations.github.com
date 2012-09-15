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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLXPathResult.cpp,v 1.7 2005/02/05 10:45:36 kommer Exp $


#include "LibXMLXPathResult.h"
#include "LibXMLDOMInternals.h"
#include "LibXMLNode.h"
#include <acdk/lang/Double.h>

namespace acdk {
namespace xml {
namespace libxmldom {

using namespace org::w3c::dom::xpath;

LibXMLXPathResult::~LibXMLXPathResult()
{
  xmlXPathFreeObject(_xpathPtr);
}

short
LibXMLXPathResult::getResultType()
{
  switch (_xpathPtr->type)
  {
  case XPATH_UNDEFINED:
    return ANY_TYPE;
  case XPATH_NUMBER:
    return NUMBER_TYPE;
  case XPATH_STRING:
    return STRING_TYPE;
  case XPATH_BOOLEAN:
    return BOOLEAN_TYPE;
  case XPATH_NODESET:
    return UNORDERED_NODE_SNAPSHOT_TYPE;
  case XPATH_POINT:
  case XPATH_RANGE:
  case XPATH_LOCATIONSET:
  case XPATH_USERS:
  case XPATH_XSLT_TREE:
  default:
    return ANY_TYPE;
  }
}

double 
LibXMLXPathResult::getNumberValue() THROWS1(org::w3c::dom::xpath::RXPathException)
{
  if (getResultType() != NUMBER_TYPE)
    THROW2(XPathException, org::w3c::dom::xpath::XPATH_INVALID_TYPE, "not a number");
  return _xpathPtr->floatval;
}

RString 
LibXMLXPathResult::getStringValue() THROWS1(org::w3c::dom::xpath::RXPathException)
{
  short resType = getResultType();
  if (resType != STRING_TYPE && resType != NUMBER_TYPE && resType != BOOLEAN_TYPE)
    THROW2(XPathException, org::w3c::dom::xpath::XPATH_INVALID_TYPE, "not a string");
  return XML2STR(_xpathPtr->stringval);
}

bool 
LibXMLXPathResult::getBooleanValue() THROWS1(org::w3c::dom::xpath::RXPathException)
{
  if (getResultType() != BOOLEAN_TYPE)
    THROW2(XPathException, org::w3c::dom::xpath::XPATH_INVALID_TYPE, "not a boolean");
  return _xpathPtr->boolval != 0;
}

org::w3c::dom::RNode 
LibXMLXPathResult::getSingleNodeValue() THROWS1(org::w3c::dom::xpath::RXPathException)
{
  short restype = getResultType();
  if (restype != UNORDERED_NODE_SNAPSHOT_TYPE && restype != ANY_TYPE && restype != ORDERED_NODE_SNAPSHOT_TYPE)
    THROW2(XPathException, org::w3c::dom::xpath::XPATH_INVALID_TYPE, "");
      
  if (_xpathPtr->nodesetval == 0)
    return Nil;
    
  if (_xpathPtr->nodesetval->nodeNr <= 0)
    return Nil;
  return &LibXMLNode::newInstance(_xpathPtr->nodesetval->nodeTab[0]);
}

RObject 
LibXMLXPathResult::getObjectValue() THROWS1(org::w3c::dom::xpath::RXPathException)
{
  short restype = getResultType();
  switch (restype)
  {
  case NUMBER_TYPE:
    return new Double(getNumberValue());
  case STRING_TYPE:
    return (RObject)getStringValue();
  case BOOLEAN_TYPE:
    return (RObject)Boolean::valueOf(getBooleanValue());
  case UNORDERED_NODE_SNAPSHOT_TYPE:
  case ANY_TYPE:
  case ORDERED_NODE_SNAPSHOT_TYPE:
    if (getSnapshotLength() == 0)
      return Nil;
    if (getSnapshotLength() == 1)
      return (RObject)getSingleNodeValue();
    return (RObject)getSnapshotNodeList();
  default:
    return Nil;
  }
}

bool 
LibXMLXPathResult::getInvalidIteratorState()
{
  short restype = getResultType();
  if (restype != UNORDERED_NODE_SNAPSHOT_TYPE && restype != ANY_TYPE && restype != ORDERED_NODE_SNAPSHOT_TYPE)
    return true;
  if (_xpathPtr->nodesetval == 0)
    return true;
  if (_xpathPtr->nodesetval->nodeNr <= _curIdx)
    return true;
  return false;
}

int 
LibXMLXPathResult::getSnapshotLength() THROWS1(org::w3c::dom::xpath::RXPathException)
{
  short restype = getResultType();
  if (restype != UNORDERED_NODE_SNAPSHOT_TYPE && restype != ANY_TYPE && restype != ORDERED_NODE_SNAPSHOT_TYPE)
    THROW2_FQ(org::w3c::dom::xpath::, XPathException, org::w3c::dom::xpath::XPATH_INVALID_TYPE, "");
  
  if (_xpathPtr->nodesetval == 0)
    return 0;

  return _xpathPtr->nodesetval->nodeNr;
}

org::w3c::dom::RNode 
LibXMLXPathResult::iterateNext() THROWS2(org::w3c::dom::xpath::RXPathException, org::w3c::dom::RDOMException)
{
  org::w3c::dom::RNode ret = snapshotItem(_curIdx);
  ++_curIdx;
  return ret;
}

org::w3c::dom::RNode 
LibXMLXPathResult::snapshotItem(int index) THROWS1(org::w3c::dom::xpath::RXPathException)
{
  short restype = getResultType();

  if (restype != UNORDERED_NODE_SNAPSHOT_TYPE && restype != ANY_TYPE && restype != ORDERED_NODE_SNAPSHOT_TYPE)
    THROW2_FQ(org::w3c::dom::xpath::, XPathException, org::w3c::dom::xpath::XPATH_INVALID_TYPE, "");
  
  if (_xpathPtr->nodesetval == 0)
    return Nil;
  if (_xpathPtr->nodesetval->nodeNr <= index)
    return Nil;
  return &LibXMLNode::newInstance(_xpathPtr->nodesetval->nodeTab[index]);

}



} // namespace libxmldom
} // namespace xml
} // namespace acdk

