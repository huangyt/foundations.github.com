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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/xpath/XPathResult.h,v 1.5 2005/02/05 10:45:37 kommer Exp $

#ifndef org_w3c_dom_xpath_XPathResult_h
#define org_w3c_dom_xpath_XPathResult_h

#include "XPathException.h"
#include "../Node.h"
#include "../NodeList.h"

namespace org {
namespace w3c {
namespace dom {
namespace xpath {

/**
*/
enum XPathResultType
{
    /**
       This code does not represent a specific type. An evaluation of an XPath 
       expression will never produce this type. If this type is requested, 
       then the evaluation returns whatever type naturally results from 
       evaluation of the expression. 
       <br>If the natural result is a node set when <code>ANY_TYPE</code> was 
       requested, then <code>UNORDERED_NODE_ITERATOR_TYPE</code> is always 
       the resulting type. Any other representation of a node set must be 
       explicitly requested.
     */
  ANY_TYPE                  = 0,
    /**
       The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#numbers'>number</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>]. 
       Document modification does not invalidate the number, but may mean 
       that reevaluation would not yield the same number.
     */
  NUMBER_TYPE               = 1,
    /**
       The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#strings'>string</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>]. 
       Document modification does not invalidate the string, but may mean 
       that the string no longer corresponds to the current document.
     */
  STRING_TYPE               = 2,
    /**
       The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#booleans'>boolean</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>]. 
       Document modification does not invalidate the boolean, but may mean 
       that reevaluation would not yield the same boolean.
     */
  BOOLEAN_TYPE              = 3,
    /**
       The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#node-sets'>node set</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] that 
       will be accessed iteratively, which may not produce nodes in a 
       particular order. Document modification invalidates the iteration.
       <br>This is the default type returned if the result is a node set and 
       <code>ANY_TYPE</code> is requested.
     */
  UNORDERED_NODE_ITERATOR_TYPE = 4,
    /**
       The result is a node set as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] that 
       will be accessed iteratively, which will produce document-ordered 
       nodes. Document modification invalidates the iteration.
     */
  ORDERED_NODE_ITERATOR_TYPE = 5,
    /**
       The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#node-sets'>node set</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] that 
       will be accessed as a snapshot list of nodes that may not be in a 
       particular order. Document modification does not invalidate the 
       snapshot but may mean that reevaluation would not yield the same 
       snapshot and nodes in the snapshot may have been altered, moved, or 
       removed from the document.
     */
  UNORDERED_NODE_SNAPSHOT_TYPE = 6,
    /**
       The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#node-sets'>node set</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] that 
       will be accessed as a snapshot list of nodes that will be in original 
       document order. Document modification does not invalidate the 
       snapshot but may mean that reevaluation would not yield the same 
       snapshot and nodes in the snapshot may have been altered, moved, or 
       removed from the document.
     */
  ORDERED_NODE_SNAPSHOT_TYPE = 7,
    /**
       The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#node-sets'>node set</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] and 
       will be accessed as a single node, which may be <code>null</code>if 
       the node set is empty. Document modification does not invalidate the 
       node, but may mean that the result node no longer corresponds to the 
       current document. This is a convenience that permits optimization 
       since the implementation can stop once any node in the resulting set 
       has been found.
       <br>If there is more than one node in the actual result, the single 
       node returned might not be the first in document order.
     */
  ANY_UNORDERED_NODE_TYPE   = 8,
    /**
       The result is a <a href='http://www.w3.org/TR/1999/REC-xpath-19991116#node-sets'>node set</a> as defined by [<a href='http://www.w3.org/TR/1999/REC-xpath-19991116'>XPath 1.0</a>] and 
       will be accessed as a single node, which may be <code>null</code> if 
       the node set is empty. Document modification does not invalidate the 
       node, but may mean that the result node no longer corresponds to the 
       current document. This is a convenience that permits optimization 
       since the implementation can stop once the first node in document 
       order of the resulting set has been found.
       <br>If there are more than one node in the actual result, the single 
       node returned will be the first in document order.
     */
  FIRST_ORDERED_NODE_TYPE   = 9,
  /**
    The node is a <code>Namespace</code>.
  */
  XPATH_NAMESPACE_NODE  = 13
};
ACDK_DEF_LIB_ENUM(ACDK_ORG_XML_PUBLIC, XPathResultType);

ACDK_DECL_INTERFACE(XPathResult);

/*  
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:37 $
*/
class ACDK_ORG_XML_PUBLIC XPathResult
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(XPathResult)
public: 
/**
  A code representing the type of this result, as defined by the type 
  constants.
  */
  virtual short getResultType() = 0;
  
  /**
    The value of this number result. If the native double type of the DOM 
    binding does not directly support the exact IEEE 754 result of the 
    XPath expression, then it is up to the definition of the binding to 
    specify how the XPath number is converted to the native binding 
    number.
    @exception XPathException
      TYPE_ERR: raised if <code>resultType</code> is not 
      <code>NUMBER_TYPE</code>.
  */
  virtual double getNumberValue() THROWS1(RXPathException) = 0;
  
  /**
    The value of this string result.
    @exception XPathException
      TYPE_ERR: raised if <code>resultType</code> is not 
      <code>STRING_TYPE</code>.
  */
  virtual RString getStringValue() THROWS1(RXPathException) = 0;
  
  /**
    The value of this boolean result.
    @exception XPathException
      TYPE_ERR: raised if <code>resultType</code> is not 
      <code>BOOLEAN_TYPE</code>.
  */
  virtual bool getBooleanValue() THROWS1(RXPathException) = 0;
  
  /**
    The value of this single node result, which may be <code>null</code>.
    @exception XPathException
      TYPE_ERR: raised if <code>resultType</code> is not 
      <code>ANY_UNORDERED_NODE_TYPE</code> or 
      <code>FIRST_ORDERED_NODE_TYPE</code>.
  */
  virtual org::w3c::dom::RNode getSingleNodeValue() THROWS1(RXPathException) = 0;
  
  /**
    Signifies that the iterator has become invalid. True if 
    <code>resultType</code> is <code>UNORDERED_NODE_ITERATOR_TYPE</code> 
    or <code>ORDERED_NODE_ITERATOR_TYPE</code> and the document has been 
    modified since this result was returned.
  */
  virtual bool getInvalidIteratorState() = 0;
  
  /**
    The number of nodes in the result snapshot. Valid values for 
    snapshotItem indices are <code>0</code> to 
    <code>snapshotLength-1</code> inclusive.
    @exception XPathException
      TYPE_ERR: raised if <code>resultType</code> is not 
      <code>UNORDERED_NODE_SNAPSHOT_TYPE</code> or 
      <code>ORDERED_NODE_SNAPSHOT_TYPE</code>.
  */
  virtual int getSnapshotLength() THROWS1(RXPathException) = 0;
  
  /**
    Iterates and returns the next node from the node set or 
    <code>null</code>if there are no more nodes.
    @return Returns the next node.
    @exception XPathException
      TYPE_ERR: raised if <code>resultType</code> is not 
      <code>UNORDERED_NODE_ITERATOR_TYPE</code> or 
      <code>ORDERED_NODE_ITERATOR_TYPE</code>.
    @exception DOMException
      INVALID_STATE_ERR: The document has been mutated since the result was 
      returned.
  */
  virtual org::w3c::dom::RNode iterateNext() THROWS2(RXPathException, RDOMException) = 0;
  
  /**
    Returns the <code>index</code>th item in the snapshot collection. If 
    <code>index</code> is greater than or equal to the number of nodes in 
    the list, this method returns <code>null</code>. Unlike the iterator 
    result, the snapshot does not become invalid, but may not correspond 
    to the current document if it is mutated.
    @param index Index into the snapshot collection.
    @return The node at the <code>index</code>th position in the 
      <code>NodeList</code>, or <code>null</code> if that is not a valid 
      index.
    @exception XPathException
      TYPE_ERR: raised if <code>resultType</code> is not 
      <code>UNORDERED_NODE_SNAPSHOT_TYPE</code> or 
      <code>ORDERED_NODE_SNAPSHOT_TYPE</code>.
  */
  virtual org::w3c::dom::RNode snapshotItem(int index) THROWS1(RXPathException) = 0;
  /**
    Extended API, that return a node list for given snapshot
  */
  virtual org::w3c::dom::RNodeList getSnapshotNodeList();
  /**
    extended API return a NodeListIterator of the getSnapshotNodeList
  */
  acdk::util::RIterator iterator() 
  {
    return getSnapshotNodeList()->iterator();
  }
};

ACDK_DECL_CLASS(XPathResultNodeList);
/**
  Helper class to wrapp a XPathResult with a NodeList
*/
class ACDK_ORG_XML_PUBLIC XPathResultNodeList
: extends acdk::lang::Object
, implements org::w3c::dom::NodeList
{
  ACDK_WITH_METAINFO(XPathResultNodeList)
protected:
  RXPathResult _result;  
public:
  XPathResultNodeList(IN(RXPathResult) result)
  : _result(result)
  {
  }
  virtual RNode item(int index)
  {
    return _result->snapshotItem(index);
  }
  virtual int getLength() { return _result->getSnapshotLength(); }
};

inline
org::w3c::dom::RNodeList 
XPathResult::getSnapshotNodeList()
{
  return new XPathResultNodeList(this);
}

} // namespace xpath
} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_xpath_XPathResult_h
