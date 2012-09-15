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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/SAXException.h,v 1.12 2005/02/05 10:45:38 kommer Exp $

// Documentation written by David Megginson
// NO WARRANTY!  This class documentation is in the Public Domain.

#ifndef org_xml_sax_SAXException_h
#define org_xml_sax_SAXException_h

#include "Config.h"

namespace org {
namespace xml {
namespace sax {


ACDK_DECL_THROWABLE(SAXException, Exception);

/**
 * Encapsulate a general SAX error or warning.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * See <a href='http://www.saxproject.org'>http://www.saxproject.org</a>
 * for further information.
 * </blockquote>
 *
 * <p>This class can contain basic error or warning information from
 * either the XML parser or the application: a parser writer or
 * application writer can subclass it to provide additional
 * functionality.  SAX handlers may throw this exception or
 * any exception subclassed from it.</p>
 *
 * <p>If the application needs to pass through other types of
 * exceptions, it must wrap those exceptions in a SAXException
 * or an exception derived from a SAXException.</p>
 *
 * <p>If the parser or application needs to include information about a
 * specific location in an XML document, it should use the
 * {@link org.xml.sax.SAXParseException SAXParseException} subclass.</p>
 *
 * @since SAX 1.0
 * @see org.xml.sax.SAXParseException
 */
class ACDK_ORG_XML_PUBLIC SAXException
: extends Exception
{
  ACDK_WITH_METAINFO(SAXException)
private:
  RString _message;
  RException _exception;
public: 
  SAXException(IN(RString) s = "")
  : Exception(),
    _message(s),
    _exception(Nil)
  {
  }
  /**
     * Create a new SAXException wrapping an existing exception.
     *
     * <p>The existing exception will be embedded in the new
     * one, and its message will become the default message for
     * the SAXException.</p>
     *
     * @param e The exception to be wrapped in a SAXException.
     */
  SAXException(IN(RException) exception)
  : Exception(),
    _message(Nil),
    _exception(exception)
  {
  }
  
    /**
     * Create a new SAXException from an existing exception.
     *
     * <p>The existing exception will be embedded in the new
     * one, but the new exception will have its own message.</p>
     *
     * @param message The detail message.
     * @param e The exception to be wrapped in a SAXException.
     */
  SAXException(IN(RString) msg, IN(RException) exception)
  : Exception(),
    _message(msg),
    _exception(exception)
  {
  }
  /**
     * Return a detail message for this exception.
     *
     * <p>If there is an embedded exception, and if the SAXException
     * has no detail message of its own, this method will return
     * the detail message from the embedded exception.</p>
     *
     * @return The error or warning message.
     */
  virtual RString getMessage()
  {
    if (_message == Nil && _exception != Nil)
      return _exception->getMessage();
    else
      return _message;
  }
  RException getException() { return _exception; }
  RString toString() { return getMessage(); }

};

} // namespace sax
} // namespace xml
} // namespace org

#endif //org_xml_sax_SAXException_h
