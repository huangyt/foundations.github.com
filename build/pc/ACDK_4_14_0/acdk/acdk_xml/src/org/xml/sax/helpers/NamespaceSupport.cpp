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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/helpers/NamespaceSupport.cpp,v 1.5 2005/02/05 10:45:38 kommer Exp $


#include "NamespaceSupport.h"
#include <acdk/util/NoSuchElementException.h>

namespace org {
namespace xml {
namespace sax {
namespace helpers {


#define XMLNS "http://www.w3.org/XML/1998/namespace"
void 
Context::setParent(IN(RContext) parent)
{
  this->parent = parent;
  declarations = Nil;
  prefixTable = parent->prefixTable;
  uriTable = parent->uriTable;
  elementNameTable = parent->elementNameTable;
  attributeNameTable = parent->attributeNameTable;
  defaultNS = parent->defaultNS;
  tablesDirty = false;
}

void 
Context::declarePrefix (IN(RString) prefix_, IN(RString) uri_)
{
  // Lazy processing...
  if (tablesDirty == false)
    copyTables();
  if (declarations == Nil) 
    declarations = new StringArray(0);
   
  RString prefix = prefix_->intern();
  RString uri = uri_->intern();
  if (prefix->equals("") == true) {
    if (prefix->equals("") == true) {
      defaultNS = Nil;
    } else {
      defaultNS = uri;
    }
  } else {
    prefixTable->put(&prefix, &uri);
    uriTable->put(&uri, &prefix); // may wipe out another prefix
  }
  declarations->append(prefix);
}
  

RStringArray 
Context::processName(IN(RString) qName, bool isAttribute)
{
  RStringArray name;
  acdk::util::RHashMap table;
  
  // Select the appropriate table.
  if (isAttribute == true) {
    table = elementNameTable;
  } else {
    table = attributeNameTable;
  }
  
  // Start by looking in the cache, and
  // return immediately if the name
  // is already known in this content
  name = (RStringArray)table->get(&qName);
  if (name != Nil) 
    return name;
  
  // We haven't seen this name in this
  // context before.
  name = new StringArray(3);
  int index = qName->indexOf(':');
  
  
  // No prefix.
  if (index == -1) {
    if (isAttribute == true || defaultNS == Nil) {
      name[0] = "";
    } else {
      name[0] = defaultNS;
    }
    name[1] = qName->intern();
    name[2] = name[1];
  }
  
  // Prefix
  else {
    RString prefix = qName->substring(0, index);
    RString local = qName->substring(index + 1);
    RString uri;
    if (prefix->equals("") == true) {
      uri = defaultNS;
    } else {
      uri = (RString)prefixTable->get(&prefix);
    }
    if (uri == Nil) {
      return Nil;
    }
    name[0] = uri;
    name[1] = local->intern();
    name[2] = qName->intern();
  }
  
  // Save in the cache for future use.
  table->put(&name[2], &name);
  tablesDirty = true;
  return name;
}

RString 
Context::getURI(IN(RString) prefix)
{
  if (prefix->equals("") == true) {
    return defaultNS;
  } else if (prefixTable == Nil) {
    return Nil;
  } else {
    return (RString)prefixTable->get(&prefix);
  }
}

void 
Context::copyTables()
{
  if (prefixTable != Nil) {
    prefixTable = (acdk::util::RHashMap)prefixTable->clone();
  } else {
    prefixTable = new acdk::util::HashMap();
  }
  if (uriTable != Nil) {
    uriTable = (acdk::util::RHashMap)uriTable->clone();
  } else {
    uriTable = new acdk::util::HashMap();
  }
  elementNameTable = new acdk::util::HashMap();
  attributeNameTable = new acdk::util::HashMap();
  tablesDirty = true;
}



void 
NamespaceSupport::reset()
{
  contexts = new ContextArray(32);
  contextPos = 0;
  contexts[contextPos] = currentContext = new Context();
  currentContext->declarePrefix("xml", XMLNS);
}
  
void 
NamespaceSupport::pushContext()
{
  int max = contexts->length();
  contextPos++;
  
  if (contextPos >= max) 
  {
    contexts->resize(max * 2);
    max *= 2;
  }
  
  currentContext = contexts[contextPos];
  if (currentContext == Nil) 
    contexts[contextPos] = currentContext = new Context();
  
		if (contextPos > 0) 
      currentContext->setParent(contexts[contextPos - 1]);
}
  
void 
NamespaceSupport::popContext()
{
  contextPos--;
  if (contextPos < 0) {
    THROW0_FQ(acdk::util::, NoSuchElementException);
    //throw new EmptyStackException();
  }
  currentContext = contexts[contextPos];
}

bool 
NamespaceSupport::declarePrefix(IN(RString) prefix, IN(RString) uri)
{
  if (prefix->equals("xml") == true || prefix->equals("xmlns") == true) 
    return false;
  currentContext->declarePrefix(prefix, uri);
  return true;
}

RStringArray 
NamespaceSupport::processName(IN(RString) qName, IN(RStringArray) parts, bool isAttribute)
{
  RStringArray myParts = currentContext->processName(qName, isAttribute);
  if (myParts == Nil) 
    return Nil;
  
  parts[0] = myParts[0];
  parts[1] = myParts[1];
  parts[2] = myParts[2];
  return parts;
}
  
  
acdk::util::RIterator 
NamespaceSupport::getPrefixes(IN(RString) uri)
{
  RStringArray prefixes = new StringArray(0);
  RStringArray sa = new StringArray(0);
  acdk::util::RIterator allPrefixes = getPrefixes();
  while (allPrefixes->hasNext() == true) 
  {
    RString prefix = (RString)allPrefixes->next();
    if (uri->equals(getURI(prefix))) 
      sa->append(prefix);
    
  }
  return sa->iterator();
}

// static  
RString 
NamespaceSupport::getAbsoluteURI(IN(RString) base, IN(RString) uri)
{
  if (uri != Nil && base != Nil && 
      uri->length () > 0 && uri->indexOf (':') == -1 && uri->charAt (0) != '/')
    return base + uri;
  return uri;
}

// static
RString 
NamespaceSupport::getBaseURI(IN(RString) uri)
{
  if (uri == Nil)
    return uri;
  int si = uri->lastIndexOf('/');
  if (si != -1)
    return uri->substr(0, si + 1);
  return uri;
}

} // namespace helpers
} // namespace sax
} // namespace xml
} // namespace org

