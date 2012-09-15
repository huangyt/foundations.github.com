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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/NodeUtil.cpp,v 1.5 2005/02/05 10:45:36 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Character.h>
#include <org/w3c/dom/Document.h>

#include "NodeUtil.h"
#include "NodeArrayList.h"
#include "XMLElement.h"
#include "XMLText.h"

namespace acdk {
namespace xml {
namespace dom {

using namespace org::w3c::dom;

String::iterator nextXPathToken(String::iterator it, String::iterator end)
{
  if (*it == '/')
    return it;

  while (it < end && Character::isLetterOrDigit(*it) == true)
  {
    ++it;
  }
  //if (it != end)
  //  --it;
  return it;
}


RNodeList 
NodeUtil::selectNodes(IN(RNode) node, IN(RString) xpath)
{
  String::iterator it = xpath->begin();
  String::iterator end = xpath->end();
  String::iterator newend = nextXPathToken(it, end);
  if (*it == '/')
  {
    RNode r = &node->getOwnerDocument();//->getDocumentElement();
    return selectNodes(r, xpath->substr(1));
  }
  else 
  {
    RString s = xpath->substr(0, newend - it);
    
    if (node->hasChildNodes() == false)
      return Nil;
    RNodeList nl = node->getChildNodes();
    int nll = nl->getLength();
    RNodeArrayList nodeList;
    if (newend == end)
      nodeList = new NodeArrayList();
    
    if (s->equals("text") && newend != end && *newend == '(' && *(newend + 1) == ')')
    {
      nodeList = new NodeArrayList();
      for (int i = 0; i < nll; ++i)
      {
        RNode sn = nl->item(i);
        if (instanceof(sn, Text) == true)
        {
          RXMLText t = (RXMLText)sn;
          nodeList->append(sn);
        }
      }
      return &nodeList;
    }
    else
    {
      for (int i = 0; i < nll; ++i)
      {
        RNode sn = nl->item(i);
        RString snn = sn->getNodeName();
        if (snn->equals(s) == true)
        {
          if (newend == end)
            nodeList->append(sn);
          else
            return selectNodes(sn, xpath->substr(newend - it + 1));
        }
      }
    }
    return &nodeList;
  }
}

org::w3c::dom::RNode 
NodeUtil::selectNode(IN(org::w3c::dom::RNode) node, IN(RString) xpath)
{
  RNodeList nl = selectNodes(node, xpath);
  if (nl == Nil || nl->getLength() == 0)
    return Nil;
  return nl->item(0);
}

void 
printNode(StringBuffer& sb, IN(RNode) node, IN(RString) ident);

void printSubNodes(StringBuffer& sb, IN(RNode) node, IN(RString) ident)
{
   RNodeList nl = node->getChildNodes();
  for (int i = 0; i < nl->getLength(); ++i)
  {
    printNode(sb, nl->item(i), ident + " ");
  }
}

void 
printNode(StringBuffer& sb, IN(RNode) node, IN(RString) ident)
{
  if (instanceof(node, Document) == true)
  {
     sb << "<?xml version=\"1.0\"?>\n";
     printSubNodes(sb, node, ident);
     return;
  }
  else if (instanceof(node, Element) == true)
  {
    RElement el = (RElement)node;
    RString tagname = el->getTagName();
    sb << ident << "<" << tagname;
    //RAttrArray attrs = el->getAttribues();
    for (int i = 0; i < el->attributeCount(); ++i)
    {
      RAttr att = el->attribute(i);
      sb << " " << att->getName() << "=\"" << att->getValue() << "\""; // ### @todo encode attr
    }
    if (el->hasChildNodes() == false)
    {
      RString val = el->getNodeValue();
      if (val == Nil || val->length() == 0)
      {
        sb << "/>\n";
        return;
      }
      sb << ">" << val << "</" << tagname << ">\n";
      return;
    }
    bool withIdent = true;
    if (instanceof(node->getChildNodes()->item(0), Text) == true)
      withIdent = false;
    if (withIdent == false)
      sb << ">";
    else
      sb << ">\n";
    
    printSubNodes(sb, node, ident);
    if (withIdent == true)
      sb << ident;
    sb << "</" << tagname << ">\n";
    return;
  }
  else if (instanceof(node, Text) == true)
  {
    RText t = (RText)node;
    sb << t->getData();
    return;
  }
}

//static 
RString 
NodeUtil::toXml(IN(org::w3c::dom::RNode) node)
{
  StringBuffer sb;
  printNode(sb, node, "");
  return sb.toString();
}

} // namespace dom
} // namespace xml
} // namespace acdk
