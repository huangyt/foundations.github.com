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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/helpers/AttributesImpl.h,v 1.4 2005/02/05 10:45:38 kommer Exp $



#ifndef org_xml_sax_helpers_AttributesImpl_h
#define org_xml_sax_helpers_AttributesImpl_h

#include <acdk.h>
#include "../Attributes.h"

namespace org {
namespace xml {
namespace sax {
namespace helpers {

ACDK_DECL_CLASS(AttributesImpl);

/**
  provides a default implementation for a list of attributes
*/
class ACDK_ORG_XML_PUBLIC AttributesImpl
: extends Object
, implements Attributes
{
  ACDK_WITH_METAINFO(AttributesImpl)
private:
  int _length;
  RStringArray _data;
public:
  AttributesImpl()
  : _length(0)
  {
  }
  AttributesImpl(IN(RAttributes) atts)
  : _length(0)
  {
    setAttributes(atts);
  }
  virtual int getLength() { return _length; }
  virtual RString getURI(int index)
  {
    if (index >= 0 && index < _length) 
	    return _data[index * 5];
	  return Nil;
	}
  virtual RString getLocalName(int index)
  {
    if (index >= 0 && index < _length) 
	    return _data[index * 5 + 1];
	  return Nil;
	}

  virtual RString getQName(int index)
  {
    if (index >= 0 && index < _length) 
	    return _data[index * 5 + 2];
	  return Nil;
	}
  virtual RString getType(int index)
  {
    if (index >= 0 && index < _length) 
	    return _data[index * 5 + 3];
	  return Nil;
	}
  virtual RString getValue(int index)
  {
    if (index >= 0 && index < _length)
	    return _data[index * 5 + 4];
	  return Nil;
  }
  virtual int getIndex(IN(RString) uri, IN(RString) localName);
  virtual int getIndex(IN(RString) qName);
  virtual RString getType(IN(RString) uri, IN(RString) localName);
  virtual RString getType(IN(RString) qName);
  virtual RString getValue(IN(RString) uri, IN(RString) localName);
  virtual RString getValue(IN(RString) qName);
  virtual void clear()
  {
    _length = 0;
  }
  virtual void setAttributes(IN(RAttributes) atts);

  virtual void addAttribute(IN(RString) uri, IN(RString) localName, IN(RString) qName, IN(RString) type, IN(RString) value);
  virtual void setAttribute(int index, IN(RString) uri, IN(RString) localName, IN(RString) qName, IN(RString) type, IN(RString) value);
  virtual void removeAttribute(int index);
  virtual void setURI(int index, IN(RString) uri)
  {
    if (index < 0 || index >= _length) 
      badIndex(index);
    _data[index * 5] = uri;
	}
  virtual void setLocalName(int index, IN(RString) localName)
  {
    if (index < 0 || index >= _length) 
      badIndex(index);
    _data[index * 5 + 1] = localName;
  }
  virtual void setQName(int index, IN(RString) qName)
  {
    if (index < 0 || index >= _length) 
      badIndex(index);
    _data[index * 5 + 2] = qName;
  }
  virtual void setType(int index, IN(RString) type)
  {
    if (index < 0 || index >= _length) 
      badIndex(index);
    _data[index * 5 + 3] = type;
  }
  virtual void setValue(int index, IN(RString) value)
  {
    if (index < 0 || index >= _length) 
      badIndex(index);
    _data[index * 5 + 4] = value;
  }
protected:
private:
  void _ensureCapacity(int n);
  void badIndex(int index);
};
} // namespace org
} // namespace xml
} // namespace sax
} // namespace helpers

#endif //org_xml_sax_helpers_AttributesImpl_h
