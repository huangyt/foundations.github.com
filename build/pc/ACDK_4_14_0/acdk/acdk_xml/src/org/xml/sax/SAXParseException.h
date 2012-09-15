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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/SAXParseException.h,v 1.14 2005/02/05 10:45:38 kommer Exp $

// Documentation written by David Megginson
// NO WARRANTY!  This class documentation is in the Public Domain.

#ifndef org_xml_sax_SAXParseException_h
#define org_xml_sax_SAXParseException_h

#include "SAXException.h"
#include "Locator.h"

namespace org {
namespace xml {
namespace sax {


ACDK_DECL_THROWABLE(SAXParseException, SAXException);

/**
 * Encapsulate an XML parse error or warning.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * See <a href='http://www.saxproject.org'>http://www.saxproject.org</a>
 * for further information.
 * </blockquote>
 *
 * <p>This exception may include information for locating the error
 * in the original XML document, as if it came from a {@link Locator}
 * object.  Note that although the application
 * will receive a SAXParseException as the argument to the handlers
 * in the {@link org.xml.sax.ErrorHandler ErrorHandler} interface, 
 * the application is not actually required to throw the exception; 
 * instead, it can simply read the information in it and take a 
 * different action.</p>
 *
 * <p>Since this exception is a subclass of {@link org.xml.sax.SAXException 
 * SAXException}, it inherits the ability to wrap another exception.</p>
 *
 * @since SAX 1.0
 * @see org.xml.sax.SAXException
 * @see org.xml.sax.Locator
 * @see org.xml.sax.ErrorHandler
 */
class ACDK_ORG_XML_PUBLIC SAXParseException
: extends SAXException
{
  ACDK_WITH_METAINFO(SAXParseException)
private:
  RLocator _locator;
  RString _publicId;
  RString _systemId;
  int _lineNo;
  int _columnNo;
public: 
  /**
     * Create a new SAXParseException from a message and a Locator.
     *
     * <p>This constructor is especially useful when an application is
     * creating its own exception from within a {@link org.xml.sax.ContentHandler
     * ContentHandler} callback.</p>
     *
     * @param message The error or warning message.
     * @param locator The locator object for the error or warning (may be
     *        null).
     * @see org.xml.sax.Locator
     */
  SAXParseException(IN(RString) s, IN(RLocator) locator)
  : SAXException(s)
  , _locator(locator)
  , _lineNo(-1)
  , _columnNo(-1)
  {
  }
  /**
     * Wrap an existing exception in a SAXParseException.
     *
     * <p>This constructor is especially useful when an application is
     * creating its own exception from within a {@link org.xml.sax.ContentHandler
     * ContentHandler} callback, and needs to wrap an existing exception that is not a
     * subclass of {@link org.xml.sax.SAXException SAXException}.</p>
     *
     * @param message The error or warning message, or null to
     *                use the message from the embedded exception.
     * @param locator The locator object for the error or warning (may be
     *        null).
     * @param e Any exception.
     * @see org.xml.sax.Locator
     */
  SAXParseException(IN(RString) s, IN(RLocator) locator, IN(RException) exception)
  : SAXException(s, exception)
  , _locator(locator)
  , _lineNo(-1)
  , _columnNo(-1)
  {
  }
  /**
     * Create a new SAXParseException with an embedded exception.
     *
     * <p>This constructor is most useful for parser writers who
     * need to wrap an exception that is not a subclass of
     * {@link org.xml.sax.SAXException SAXException}.</p>
     *
     * <p>All parameters except the message and exception are as if
     * they were provided by a {@link Locator}.  For example, if the
     * system identifier is a URL (including relative filename), the
     * caller must resolve it fully before creating the exception.</p>
     *
     * @param message The error or warning message, or null to use
     *                the message from the embedded exception.
     * @param publicId The public identifer of the entity that generated
     *                 the error or warning.
     * @param systemId The system identifer of the entity that generated
     *                 the error or warning.
     * @param lineNumber The line number of the end of the text that
     *                   caused the error or warning.
     * @param columnNumber The column number of the end of the text that
     *                     cause the error or warning.
     * @param e Another exception to embed in this one.
     */
  SAXParseException( IN(RString) msg, IN(RString) publicId, IN(RString) systemId
                    , int lineno, int colno, IN(RException) ex = Nil) 
  : SAXException(msg, ex)
  , _locator(Nil)
  , _publicId(publicId)
  , _systemId(systemId)
  , _lineNo(lineno)
  , _columnNo(colno)
  {
  }
  
  virtual RLocator locator() { return _locator; }
  RString getPublicId() { return _publicId; }
  RString getSystemId() { return _systemId; }
  int getLineNumber() { return _lineNo; }
  int getColumnNumber() { return _columnNo; }
  RString getMessage() { return toString(); }
  RString toString()
  {
    StringBuffer sb;
    if (getPublicId() != Nil || getSystemId() != Nil)
      sb.append("[");

    if (getPublicId() != Nil) 
      sb.append(getPublicId());

    if (getPublicId() != Nil && getSystemId() != Nil)
      sb.append("|");
    if (getSystemId() != Nil) 
      sb.append(getSystemId());
    if (getPublicId() != Nil || getSystemId() != Nil)
      sb.append("]");
    sb.append("]");
    if (getLineNumber() != -1 || getColumnNumber() != -1) {
      sb.append(" at ");
      if (getLineNumber() != -1)
        sb.append(getLineNumber());
      sb.append(",");
      if (getColumnNumber() != -1)
        sb.append(getColumnNumber());
    } 
    sb.append(": ");
    sb.append(SAXException::getMessage());
    return sb.toString();
  }
};

} // namespace sax
} // namespace xml
} // namespace org

#endif //org_xml_sax_SAXParseException_h
