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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/sax/AttributeListImpl.h,v 1.10 2005/02/05 10:45:37 kommer Exp $

#ifndef acdk_xml_sax_AttributeListImpl_h
#define acdk_xml_sax_AttributeListImpl_h

#include <acdk.h>
#include <acdk/util/ArrayList.h>
#include <org/xml/sax/AttributeList.h>
#include "sax.h"

namespace acdk {
namespace xml {
/**
   XML implementation following the SAX interface
   @see org::xml::sax
*/
namespace sax {



ACDK_DECL_CLASS(AttributeListImpl);

/** 
  API: org.xml.sax<br>
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:37 $
*/
class ACDK_XML_PUBLIC AttributeListImpl
: extends ::acdk::lang::Object
, implements ::org::xml::sax::AttributeList
{
  //ACDK_WITH_METAINFO(AttributeListImpl)
  ::acdk::util::ArrayList _names;
  ::acdk::util::ArrayList _types;
  ::acdk::util::ArrayList _values;
public: 
  AttributeListImpl() { }
  ~AttributeListImpl() {}
  virtual int getLength()
  {
    return _names.size();
  }
  virtual ::acdk::lang::RString getName(int i)
  {
    if (i > _names.size() - 1)
      return "";
    return (RString)_names.get(i);
  }
  virtual ::acdk::lang::RString getType(int i)
  {
    if (i > _types.size() - 1)
      return "";
    return (RString)_types.get(i);
  }
  virtual ::acdk::lang::RString getValue(int i)
  {
    if (i > _values.size() - 1)
      return "";
    return (RString)_values.get(i);
  }
  int findElement(IN(::acdk::lang::RString) name)
  {
    for (int i = 0; i < _names.size(); ++i) 
    {
      if (RString(_names.get(i))->equals(name) == true)
        return i;
    }
    return -1;
  }
  virtual ::acdk::lang::RString getType(IN(::acdk::lang::RString) s)
  {
    int idx = findElement(s);
    if (idx == -1)
      return "";
    return getType(idx);
  }
  virtual ::acdk::lang::RString getValue(IN(::acdk::lang::RString) s)
  {
    int idx = findElement(s);
    if (idx == -1)
      return "";
    return getValue(idx);
  }

  void addAttribute(IN(RString) name, IN(RString) type, IN(RString) value)
  {
    _names.add(&name);
    _types.add(&type);
    _values.add(&value);
  }
};

} // namespace sax
} // namespace xml
} // namespace acdk

#endif //acdk_xml_sax_AttributeListImpl_h
