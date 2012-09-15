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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/parsers/DocumentBuilderFactory.h,v 1.2 2005/02/05 10:45:37 kommer Exp $

#ifndef acdk_xml_parsers_DocumentBuilderFactory_h
#define acdk_xml_parsers_DocumentBuilderFactory_h

#include "DocumentBuilder.h"
#include "ParserConfigurationException.h"

#include <acdk/lang/IllegalArgumentException.h>


namespace acdk {
namespace xml {
namespace parsers {


ACDK_DECL_CLASS(DocumentBuilderFactory);

class ACDK_XML_PUBLIC DocumentBuilderFactory
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(DocumentBuilderFactory)
private:
  bool _validating;
	bool _namespaceAware;
	bool _whitespace;
	bool _expandEntityRef;
	bool _ignoreComments;
	bool _coalescing;
protected:
  DocumentBuilderFactory()
  : _validating(false)
	, _namespaceAware(false)
	, _whitespace(false)
	, _expandEntityRef(false)
	, _ignoreComments(false)
	, _coalescing(false)
  {
  }
public:
  virtual RObject getAttribute(IN(RString) name) THROWS1(RIllegalArgumentException) = 0;
  virtual void setAttribute(IN(RString) name, IN(RObject) value) THROWS1(RIllegalArgumentException) = 0;

  virtual bool isCoalescing() { return _coalescing; }
  virtual bool isExpandEntityReferences() { return _expandEntityRef; }
  virtual bool isIgnoringComments() { return _ignoreComments; }
  virtual bool isIgnoringElementContentWhitespace() { return _whitespace; }
  virtual bool isNamespaceAware() { return _namespaceAware; }
  virtual bool isValidating()  { return _validating; }
  virtual RDocumentBuilder newDocumentBuilder() THROWS1(RParserConfigurationException) = 0;

  /*
  static DocumentBuilderFactory newInstance() 
  {
		try {
		    return (DocumentBuilderFactory)
			ClassStuff.createFactory (
				defaultPropName,  "gnu.xml.dom.JAXPFactory");
		} catch (ClassCastException e) {
			throw new FactoryConfigurationError (e, "Factory class is the wrong type");
		}
	}
  */
  
	virtual void setCoalescing(bool value) 
  {
		_coalescing = value;
	}
	virtual void setExpandEntityReferences(bool value) 
  {
		_expandEntityRef = value;
	}
	virtual void setIgnoringComments(bool value) 
  {
		_ignoreComments = value;
	}
	virtual void setIgnoringElementContentWhitespace(bool value) 
  {
		_whitespace = value;
	}
	virtual void setNamespaceAware(bool value) 
  {
		_namespaceAware = value;
	}

	virtual void setValidating(bool value) 
  {
		_validating = value;
	}
};

} // namespace parsers
} // namespace xml
} // namespace acdk

#endif //acdk_xml_parsers_DocumentBuilderFactory_h
