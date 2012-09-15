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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/parsers/DocumentBuilder.cpp,v 1.3 2005/02/05 10:45:36 kommer Exp $


#include "DocumentBuilder.h"
#include <acdk/io/FileReader.h>

namespace acdk {
namespace xml {
namespace parsers {


//static 
RString 
DocumentBuilder::fileToURL(IN(acdk::io::RFile) f) THROWS1(acdk::io::RIOException) // ### @todo file can also be already an url
{
  RString temp = f->getCanonicalPath();
  if (acdk::io::File::separatorChar() != '/')
    temp = temp->replace(acdk::io::File::separatorChar(), '/');
	if (temp->startsWith("/") == false)
		temp = "/" + temp;
	if (temp->endsWith("/") == false && f->isDirectory() == true)
		temp = temp + "/";
	return "file:" + temp;
}

org::w3c::dom::RDocument 
DocumentBuilder::parse(IN(acdk::io::RFile) file)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException)
{
  if (file == Nil)
    THROW1(IllegalArgumentException, "Argument file is Nil");
  
  
  org::xml::sax::RInputSource	source = new org::xml::sax::InputSource(fileToURL(file));
  source->setByteStream(new acdk::io::FileReader(file));
  return parse(source);
}

org::w3c::dom::RDocument 
DocumentBuilder::parse(IN(acdk::io::RReader) reader)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException) 
{
  if (reader == Nil)
    THROW1(IllegalArgumentException, "Argument reader is Nil");
  return parse(new org::xml::sax::InputSource(reader));
} 

org::w3c::dom::RDocument 
DocumentBuilder::parse(IN(acdk::io::RReader) reader, IN(RString) systemID)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException) 
{
  if (reader == Nil)
    THROW1(IllegalArgumentException, "Argument reader is Nil");

 org::xml::sax::RInputSource	source = new org::xml::sax::InputSource(reader);
 source->setSystemId(systemID);
 return parse(source);
    
}

org::w3c::dom::RDocument 
DocumentBuilder::parse(IN(RString) uri)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException) 
{
  if (uri == Nil)
    THROW1(IllegalArgumentException, "Argument uri is Nil");
  return parse(new org::xml::sax::InputSource(uri));
}

} // namespace parsers
} // namespace xml
} // namespace acdk

