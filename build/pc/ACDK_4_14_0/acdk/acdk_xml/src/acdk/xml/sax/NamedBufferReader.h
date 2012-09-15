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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/sax/NamedBufferReader.h,v 1.5 2005/02/05 10:45:37 kommer Exp $

#ifndef acdk_xml_sax_NamedBufferReader_h
#define acdk_xml_sax_NamedBufferReader_h

#include <acdk.h>
#include <acdk/io/BufferedReader.h>
#include "sax.h"

namespace acdk {
namespace xml {
namespace sax {

USING_CLASS(acdk::io::, BufferedReader);

ACDK_DECL_CLASS(NamedBufferReader);

/** 
  API: org.xml.sax<br>
  @author Roger Rene Kommer
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:37 $
*/
class ACDK_XML_PUBLIC NamedBufferReader
: extends ::acdk::io::BufferedReader
{
private:
  RString _name;
public: 
  NamedBufferReader(IN(RString) name, IN(acdk::io::RReader) in, int bufferSize = 258) 
  : BufferedReader(in, bufferSize)
  , _name(name)
  { 
  }
  RString getName() { return _name; }
};

} // namespace sax
} // namespace xml
} // namespace acdk

#endif //acdk_xml_sax_NamedBufferReader_h
