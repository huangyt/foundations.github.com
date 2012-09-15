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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/SAXNotRecognizedException.h,v 1.5 2005/02/05 10:45:38 kommer Exp $

// Documentation written by David Megginson
// NO WARRANTY!  This class documentation is in the Public Domain.

#ifndef org_xml_sax_SAXNotRecognizedException_h
#define org_xml_sax_SAXNotRecognizedException_h

#include <acdk.h>
#include "Config.h"

namespace org {
namespace xml {
namespace sax {

ACDK_DECL_CLASS(SAXNotRecognizedException);

/**
 * Exception class for an unrecognized identifier.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * See <a href='http://www.saxproject.org'>http://www.saxproject.org</a>
 * for further information.
 * </blockquote>
 *
 * <p>An XMLReader will throw this exception when it finds an
 * unrecognized feature or property identifier; SAX applications and
 * extensions may use this class for other, similar purposes.</p>
 *
 * @since SAX 2.0
 * @see org.xml.sax.SAXNotSupportedException
 */
class ACDK_ORG_XML_PUBLIC SAXNotRecognizedException
: extends SAXException
{
  ACDK_WITH_METAINFO(SAXNotRecognizedException)
public:
  SAXNotRecognizedException() {}
  SAXNotRecognizedException(IN(RString) message) : SAXException(message) {}
};

} // namespace org
} // namespace xml
} // namespace sax

#endif //org_xml_sax_SAXNotRecognizedException_h
