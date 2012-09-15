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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/NodeIteratorWalker.cpp,v 1.2 2005/02/05 10:45:36 kommer Exp $


#include "NodeIteratorWalker.h"

namespace acdk {
namespace xml {
namespace dom {

using namespace org::w3c::dom;
using namespace org::w3c::dom::traversal;

NodeIteratorWalker::NodeIteratorWalker(IN(org::w3c::dom::RNode) rootNode, IN(org::w3c::dom::traversal::RNodeFilter) nodeFilter, int showFlags,  bool expandEntity, bool walkMode)
: _rootNode(rootNode)
, _curNode(rootNode)
, _showFlags(showFlags)
, _nodeFilter(nodeFilter)
, _expandEntityReference(expandEntity)
, _walkMode(walkMode)
{
  if (_rootNode == Nil)
    THROW2(DOMException, NOT_SUPPORTED_ERR, "");
}

org::w3c::dom::RNode 
NodeIteratorWalker::nextNode() THROWS1(org::w3c::dom::RDOMException)
{
  if (_rootNode == Nil)
    THROW2(DOMException, NOT_SUPPORTED_ERR, "");

  org::w3c::dom::RNode ret;
  do {
    if (RObject(_curNode)->equals((RObject)_rootNode))
    {
      ret = _rootNode->getFirstChild();
    }
    else if (_walkMode == true)
    {
      ret = _curNode->getFirstChild();
      if (ret == Nil)
      {
        ret = _curNode->getNextSibling();
      }
      if (ret == Nil)
      {
        org::w3c::dom::RNode tmp = _curNode;
        ret = tmp->getParentNode();
        while (RObject(ret)->equals((RObject)_rootNode) == false && RObject(tmp)->equals((RObject)ret->getLastChild()) == true)
        {
          tmp = ret;
          ret = tmp->getParentNode();
        }
        if (RObject(ret)->equals((RObject)_rootNode) == true)
        {
          ret = Nil;
        }
        else
        {
          ret = ret->getNextSibling();
        }
      }
    }
    else
    {
      ret = _curNode->getNextSibling();
    }
  } while (_noSkip(ret) == false);
  _curNode = (ret == Nil) ? _curNode : ret;
  return ret;
}

org::w3c::dom::RNode 
NodeIteratorWalker::previousNode() THROWS1(org::w3c::dom::RDOMException)
{
  if (_rootNode == Nil)
    THROW2(DOMException, NOT_SUPPORTED_ERR, "");
  org::w3c::dom::RNode ret;
  do {
    if (RObject(_curNode)->equals((RObject)_rootNode) == true)
    {
      ret = _curNode->getLastChild();
    }
    else if (_walkMode == true)
    {
      ret = _curNode->getLastChild();
      if (ret == Nil)
      {
        ret = _curNode->getPreviousSibling();
      }
      if (ret == Nil)
      {
        org::w3c::dom::RNode tmp = _curNode;
        ret = tmp->getParentNode();
        while (RObject(ret)->equals((RObject)_rootNode) == false && RObject(tmp)->equals((RObject)ret->getFirstChild()) == true)
        {
          tmp = ret;
          ret = tmp->getParentNode();
        }
        if (RObject(ret)->equals((RObject)_rootNode) == true)
        {
          ret = Nil;
        }
        else
        {
          ret = ret->getPreviousSibling();
        }
      }
    }
    else
    {
      ret = _curNode->getPreviousSibling();
    }
  } while(_noSkip(ret) == false);
  _curNode = (ret == Nil) ? _curNode : ret;
  return ret;
}

void 
NodeIteratorWalker::setCurrentNode(IN(org::w3c::dom::RNode) curNode) THROWS1(org::w3c::dom::RDOMException)
{
  if (_rootNode == Nil)
    THROW2(DOMException, NOT_SUPPORTED_ERR, "");
  if (_curNode == Nil)
    THROW2(DOMException, NOT_SUPPORTED_ERR, "");
  _curNode = curNode;
}

org::w3c::dom::RNode 
NodeIteratorWalker::parentNode() 
{
  org::w3c::dom::RNode ret = _curNode->getParentNode();
  if (_noSkip(ret) == false)
    ret = Nil;
  _curNode = (ret == Nil) ? _curNode : ret;
  return ret;
}

org::w3c::dom::RNode 
NodeIteratorWalker::firstChild()
{
  org::w3c::dom::RNode ret = _curNode->getFirstChild();
  while (_noSkip(ret) == false)
  {
    ret = ret->getNextSibling();
  }
  _curNode = (ret == Nil) ? _curNode : ret;
  return ret;
}

org::w3c::dom::RNode 
NodeIteratorWalker::lastChild()
{
  org::w3c::dom::RNode ret = _curNode->getLastChild();
  while (_noSkip(ret) == false)
  {
    ret = ret->getPreviousSibling();
  }
  _curNode = (ret == Nil) ? _curNode : ret;
  return ret;
}

org::w3c::dom::RNode 
NodeIteratorWalker::previousSibling()
{
  org::w3c::dom::RNode ret = _curNode->getPreviousSibling();
  while (_noSkip(ret) == false)
  {
    ret = ret->getPreviousSibling();
  }
  _curNode = (ret == Nil) ? _curNode : ret;
  return ret;
}

org::w3c::dom::RNode 
NodeIteratorWalker::nextSibling()
{
  org::w3c::dom::RNode ret = _curNode->getNextSibling();
  while (_noSkip(ret) == false)
  {
    ret = ret->getNextSibling();
  }
  _curNode = (ret == Nil) ? _curNode : ret;
  return ret;
}
  
bool 
NodeIteratorWalker::_noSkip(IN(org::w3c::dom::RNode) node)
{
  if (node == Nil)
  {
    return true;
  }
  bool ret;
  switch (node->getNodeType())
  {
  case ATTRIBUTE_NODE:
    ret = (_showFlags & SHOW_ATTRIBUTE) != 0;
    break;
  case CDATA_SECTION_NODE:
    ret = (_showFlags & SHOW_CDATA_SECTION) != 0;
    break;
  case COMMENT_NODE:
    ret = (_showFlags & SHOW_COMMENT) != 0;
    break;
  case DOCUMENT_NODE:
    ret = (_showFlags & SHOW_DOCUMENT) != 0;
    break;
  case DOCUMENT_FRAGMENT_NODE:
    ret = (_showFlags & SHOW_DOCUMENT_FRAGMENT) != 0;
    break;
  case DOCUMENT_TYPE_NODE:
    ret = (_showFlags & SHOW_DOCUMENT_TYPE) != 0;
    break;
  case ELEMENT_NODE:
    ret = (_showFlags & SHOW_ELEMENT) != 0;
    break;
  case ENTITY_NODE:
    ret = (_showFlags & SHOW_ENTITY) != 0;
    break;
  case ENTITY_REFERENCE_NODE:
    ret = (_showFlags & SHOW_ENTITY_REFERENCE) != 0;
    ret = ret && _expandEntityReference == true;
    break;
  case NOTATION_NODE:
    ret = (_showFlags & SHOW_NOTATION) != 0;
    break;
  case PROCESSING_INSTRUCTION_NODE:
    ret = (_showFlags & SHOW_PROCESSING_INSTRUCTION) != 0;
    break;
  case TEXT_NODE:
    ret = (_showFlags & SHOW_TEXT) != 0;
    break;
  default:
    ret = true;
  }
  if (ret && _nodeFilter != Nil)
  {
    ret = (_nodeFilter->acceptNode(node) == FILTER_ACCEPT);
  }
  return ret;
}
  


} // namespace dom
} // namespace xml
} // namespace acdk

