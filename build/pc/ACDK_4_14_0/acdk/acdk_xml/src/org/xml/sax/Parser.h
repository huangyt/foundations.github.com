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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/Parser.h,v 1.15 2005/02/05 10:45:38 kommer Exp $

// Documentation written by David Megginson
// NO WARRANTY!  This class documentation is in the Public Domain.

#ifndef org_xml_sax_Parser_h
#define org_xml_sax_Parser_h

#include <acdk.h>
#include "Config.h"
#include "DocumentHandler.h"
#include "SAXParseException.h"
#include "EntityResolver.h"
#include "DTDHandler.h"

#include "ErrorHandler.h"
#include "InputSource.h"

#include <acdk/util/Locale.h>
#include <acdk/io/IOException.h>

namespace org {
namespace xml {
namespace sax {


ACDK_DECL_INTERFACE(Parser);


/**
 * Basic interface for SAX (Simple API for XML) parsers.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * See <a href='http://www.saxproject.org'>http://www.saxproject.org</a>
 * for further information.
 * </blockquote>
 *
 * <p>This was the main event supplier interface for SAX1; it has
 * been replaced in SAX2 by {@link org.xml.sax.XMLReader XMLReader},
 * which includes Namespace support and sophisticated configurability
 * and extensibility.</p>
 *
 * <p>All SAX1 parsers must implement this basic interface: it allows
 * applications to register handlers for different types of events
 * and to initiate a parse from a URI, or a character stream.</p>
 *
 * <p>All SAX1 parsers must also implement a zero-argument constructor
 * (though other constructors are also allowed).</p>
 *
 * <p>SAX1 parsers are reusable but not re-entrant: the application
 * may reuse a parser object (possibly with a different input source)
 * once the first parse has completed successfully, but it may not
 * invoke the parse() methods recursively within a parse.</p>
 *
 * @deprecated This interface has been replaced by the SAX2
 *             {@link org.xml.sax.XMLReader XMLReader}
 *             interface, which includes Namespace support.
 * @since SAX 1.0
 * @see org.xml.sax.EntityResolver
 * @see org.xml.sax.DTDHandler
 * @see org.xml.sax.DocumentHandler
 * @see org.xml.sax.ErrorHandler
 * @see org.xml.sax.HandlerBase
 * @see org.xml.sax.InputSource
 */
class ACDK_ORG_XML_PUBLIC Parser
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Parser)
public: 
  /**
     * Allow an application to request a locale for errors and warnings.
     *
     * <p>SAX parsers are not required to provide localisation for errors
     * and warnings; if they cannot support the requested locale,
     * however, they must throw a SAX exception.  Applications may
     * not request a locale change in the middle of a parse.</p>
     *
     * @param locale A Java Locale object.
     * @exception org.xml.sax.SAXException Throws an exception
     *            (using the previous or default locale) if the 
     *            requested locale is not supported.
     * @see org.xml.sax.SAXException
     * @see org.xml.sax.SAXParseException
     */
  virtual void setLocale(IN(acdk::util::RLocale) locale) THROWS1(RSAXException) = 0;
   /**
     * Allow an application to register a custom entity resolver.
     *
     * <p>If the application does not register an entity resolver, the
     * SAX parser will resolve system identifiers and open connections
     * to entities itself (this is the default behaviour implemented in
     * HandlerBase).</p>
     *
     * <p>Applications may register a new or different entity resolver
     * in the middle of a parse, and the SAX parser must begin using
     * the new resolver immediately.</p>
     *
     * @param resolver The object for resolving entities.
     * @see EntityResolver
     * @see HandlerBase
     */
  virtual void setEntityResolver(IN(REntityResolver) entityresolver) = 0;
  /**
     * Allow an application to register a DTD event handler.
     *
     * <p>If the application does not register a DTD handler, all DTD
     * events reported by the SAX parser will be silently
     * ignored (this is the default behaviour implemented by
     * HandlerBase).</p>
     *
     * <p>Applications may register a new or different
     * handler in the middle of a parse, and the SAX parser must
     * begin using the new handler immediately.</p>
     *
     * @param handler The DTD handler.
     * @see DTDHandler
     * @see HandlerBase
     */
  virtual void setDTDHandler(IN(RDTDHandler) dtdhandler) = 0;
  /**
     * Allow an application to register a document event handler.
     *
     * <p>If the application does not register a document handler, all
     * document events reported by the SAX parser will be silently
     * ignored (this is the default behaviour implemented by
     * HandlerBase).</p>
     *
     * <p>Applications may register a new or different handler in the
     * middle of a parse, and the SAX parser must begin using the new
     * handler immediately.</p>
     *
     * @param handler The document handler.
     * @see DocumentHandler
     * @see HandlerBase
     */
  virtual void setDocumentHandler(IN(RDocumentHandler) documenthandler) = 0;
  /**
     * Allow an application to register an error event handler.
     *
     * <p>If the application does not register an error event handler,
     * all error events reported by the SAX parser will be silently
     * ignored, except for fatalError, which will throw a SAXException
     * (this is the default behaviour implemented by HandlerBase).</p>
     *
     * <p>Applications may register a new or different handler in the
     * middle of a parse, and the SAX parser must begin using the new
     * handler immediately.</p>
     *
     * @param handler The error handler.
     * @see ErrorHandler
     * @see SAXException
     * @see HandlerBase
     */
  virtual void setErrorHandler(IN(RErrorHandler) errorhandler) = 0;
  /**
     * Parse an XML document.
     *
     * <p>The application can use this method to instruct the SAX parser
     * to begin parsing an XML document from any valid input
     * source (a character stream, a byte stream, or a URI).</p>
     *
     * <p>Applications may not invoke this method while a parse is in
     * progress (they should create a new Parser instead for each
     * additional XML document).  Once a parse is complete, an
     * application may reuse the same Parser object, possibly with a
     * different input source.</p>
     *
     * @param source The input source for the top-level of the
     *        XML document.
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @exception java.io.IOException An IO exception from the parser,
     *            possibly from a byte stream or character stream
     *            supplied by the application.
     * @see org.xml.sax.InputSource
     * @see #parse(java.lang.String)
     * @see #setEntityResolver
     * @see #setDTDHandler
     * @see #setDocumentHandler
     * @see #setErrorHandler
     */
  virtual void parse(IN(RInputSource) inputsource) THROWS2(RSAXException, acdk::io::RIOException) = 0;
  /**
     * Parse an XML document from a system identifier (URI).
     *
     * <p>This method is a shortcut for the common case of reading a
     * document from a system identifier.  It is the exact
     * equivalent of the following:</p>
     *
     * <pre>
     * parse(new InputSource(systemId));
     * </pre>
     *
     * <p>If the system identifier is a URL, it must be fully resolved
     * by the application before it is passed to the parser.</p>
     *
     * @param systemId The system identifier (URI).
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @exception java.io.IOException An IO exception from the parser,
     *            possibly from a byte stream or character stream
     *            supplied by the application.
     * @see #parse(org.xml.sax.InputSource)
     */
  virtual void parse(IN(::acdk::lang::RString) systemId) THROWS2(RSAXException, acdk::io::RIOException) = 0;
};

} // namespace sax
} // namespace xml
} // namespace org

#endif //org_xml_sax_Parser_h