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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/HandlerBase.h,v 1.18 2005/02/05 10:45:38 kommer Exp $


// Documentation written by David Megginson
// NO WARRANTY!  This class documentation is in the Public Domain.

#ifndef org_xml_sax_HandlerBase_h
#define org_xml_sax_HandlerBase_h

#include "DocumentHandler.h"
#include "ErrorHandler.h"
#include "EntityResolver.h"
#include "DTDHandler.h"


namespace org {
namespace xml {
namespace sax {


ACDK_DECL_CLASS(HandlerBase);

/**
 * Default base class for handlers.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * See <a href='http://www.saxproject.org'>http://www.saxproject.org</a>
 * for further information.
 * </blockquote>
 *
 * <p>This class implements the default behaviour for four SAX1
 * interfaces: EntityResolver, DTDHandler, DocumentHandler,
 * and ErrorHandler.  It is now obsolete, but is included in SAX2 to
 * support legacy SAX1 applications.  SAX2 applications should use
 * the {@link org.xml.sax.helpers.DefaultHandler DefaultHandler}
 * class instead.</p>
 *
 * <p>Application writers can extend this class when they need to
 * implement only part of an interface; parser writers can
 * instantiate this class to provide default handlers when the
 * application has not supplied its own.</p>
 *
 * <p>Note that the use of this class is optional.</p>
 *
 * @deprecated This class works with the deprecated
 *             {@link org.xml.sax.DocumentHandler DocumentHandler}
 *             interface.  It has been replaced by the SAX2
 *             {@link org.xml.sax.helpers.DefaultHandler DefaultHandler}
 *             class.
 * @since SAX 1.0
 * @see org.xml.sax.EntityResolver
 * @see org.xml.sax.DTDHandler
 * @see org.xml.sax.DocumentHandler
 * @see org.xml.sax.ErrorHandler
 */
class ACDK_ORG_XML_PUBLIC HandlerBase
: extends Object
, implements EntityResolver
, implements DTDHandler
, implements DocumentHandler
, implements ErrorHandler
{
  ACDK_WITH_METAINFO(HandlerBase)
public: 
  HandlerBase()
  : Object()
  {
  }
  /**
     * Resolve an external entity.
     *
     * <p>Always return null, so that the parser will use the system
     * identifier provided in the XML document.  This method implements
     * the SAX default behaviour: application writers can override it
     * in a subclass to do special translations such as catalog lookups
     * or URI redirection.</p>
     *
     * @param publicId The public identifer, or null if none is
     *                 available.
     * @param systemId The system identifier provided in the XML 
     *                 document.
     * @return The new input source, or null to require the
     *         default behaviour.
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.EntityResolver#resolveEntity
     */
  virtual RInputSource resolveEntity(IN(RString) s, IN(RString) s1) THROWS1(RSAXException) { return Nil; }
  /**
     * Receive notification of a notation declaration.
     *
     * <p>By default, do nothing.  Application writers may override this
     * method in a subclass if they wish to keep track of the notations
     * declared in a document.</p>
     *
     * @param name The notation name.
     * @param publicId The notation public identifier, or null if not
     *                 available.
     * @param systemId The notation system identifier.
     * @see org.xml.sax.DTDHandler#notationDecl
     */
  virtual void notationDecl(IN(RString) s, IN(RString) s1, IN(RString) s2)  THROWS1(RSAXException) { }
  /**
     * Receive notification of an unparsed entity declaration.
     *
     * <p>By default, do nothing.  Application writers may override this
     * method in a subclass to keep track of the unparsed entities
     * declared in a document.</p>
     *
     * @param name The entity name.
     * @param publicId The entity public identifier, or null if not
     *                 available.
     * @param systemId The entity system identifier.
     * @param notationName The name of the associated notation.
     * @see org.xml.sax.DTDHandler#unparsedEntityDecl
     */
  virtual void unparsedEntityDecl(IN(RString) s, IN(RString) s1, IN(RString) s2, IN(RString) s3)  THROWS1(RSAXException) { }
  /**
     * Receive a Locator object for document events.
     *
     * <p>By default, do nothing.  Application writers may override this
     * method in a subclass if they wish to store the locator for use
     * with other document events.</p>
     *
     * @param locator A locator for all SAX document events.
     * @see org.xml.sax.DocumentHandler#setDocumentLocator
     * @see org.xml.sax.Locator
     */
  virtual void setDocumentLocator(IN(RLocator) locator) { }
   
    /**
     * Receive notification of the beginning of the document.
     *
     * <p>By default, do nothing.  Application writers may override this
     * method in a subclass to take specific actions at the beginning
     * of a document (such as allocating the root node of a tree or
     * creating an output file).</p>
     *
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.DocumentHandler#startDocument
     */
  virtual void startDocument() THROWS1(RSAXException) { }
  /**
     * Receive notification of the end of the document.
     *
     * <p>By default, do nothing.  Application writers may override this
     * method in a subclass to take specific actions at the beginning
     * of a document (such as finalising a tree or closing an output
     * file).</p>
     *
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.DocumentHandler#endDocument
     */
  virtual void endDocument() THROWS1(RSAXException) { }
  /**
     * Receive notification of the start of an element.
     *
     * <p>By default, do nothing.  Application writers may override this
     * method in a subclass to take specific actions at the start of
     * each element (such as allocating a new tree node or writing
     * output to a file).</p>
     *
     * @param name The element type name.
     * @param attributes The specified or defaulted attributes.
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.DocumentHandler#startElement
     */
  virtual void startElement(IN(RString) s, IN(RAttributeList) attributelist) THROWS1(RSAXException) { }
  /**
     * Receive notification of the end of an element.
     *
     * <p>By default, do nothing.  Application writers may override this
     * method in a subclass to take specific actions at the end of
     * each element (such as finalising a tree node or writing
     * output to a file).</p>
     *
     * @param name The element type name.
     * @param attributes The specified or defaulted attributes.
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.DocumentHandler#endElement
     */
  virtual void endElement(IN(RString) s) THROWS1(RSAXException) { }
  /**
     * Receive notification of character data inside an element.
     *
     * <p>By default, do nothing.  Application writers may override this
     * method to take specific actions for each chunk of character data
     * (such as adding the data to a node or buffer, or printing it to
     * a file).</p>
     *
     * @param ch The characters.
     * @param start The start position in the character array.
     * @param length The number of characters to use from the
     *               character array.
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.DocumentHandler#characters
     */
  virtual void characters(IN(RString) ac, int i, int j) THROWS1(RSAXException) { }
  /**
     * Receive notification of ignorable whitespace in element content.
     *
     * <p>By default, do nothing.  Application writers may override this
     * method to take specific actions for each chunk of ignorable
     * whitespace (such as adding data to a node or buffer, or printing
     * it to a file).</p>
     *
     * @param ch The whitespace characters.
     * @param start The start position in the character array.
     * @param length The number of characters to use from the
     *               character array.
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.DocumentHandler#ignorableWhitespace
     */
  virtual void ignorableWhitespace(IN(RString) ac, int i, int j) THROWS1(RSAXException) { }
  /**
     * Receive notification of a processing instruction.
     *
     * <p>By default, do nothing.  Application writers may override this
     * method in a subclass to take specific actions for each
     * processing instruction, such as setting status variables or
     * invoking other methods.</p>
     *
     * @param target The processing instruction target.
     * @param data The processing instruction data, or null if
     *             none is supplied.
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.DocumentHandler#processingInstruction
     */
  virtual void processingInstruction(IN(RString) s, IN(RString) s1) THROWS1(RSAXException) { }
  /**
     * Receive notification of a parser warning.
     *
     * <p>The default implementation does nothing.  Application writers
     * may override this method in a subclass to take specific actions
     * for each warning, such as inserting the message in a log file or
     * printing it to the console.</p>
     *
     * @param e The warning information encoded as an exception.
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.ErrorHandler#warning
     * @see org.xml.sax.SAXParseException
     */
  virtual void warning(IN(RSAXParseException) saxparseexception) THROWS1(RSAXException) { }
  /**
     * Receive notification of a recoverable parser error.
     *
     * <p>The default implementation does nothing.  Application writers
     * may override this method in a subclass to take specific actions
     * for each error, such as inserting the message in a log file or
     * printing it to the console.</p>
     *
     * @param e The warning information encoded as an exception.
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.ErrorHandler#warning
     * @see org.xml.sax.SAXParseException
     */
  virtual void error(IN(RSAXParseException) saxparseexception) THROWS1(RSAXException) { }
  /**
     * Report a fatal XML parsing error.
     *
     * <p>The default implementation throws a SAXParseException.
     * Application writers may override this method in a subclass if
     * they need to take specific actions for each fatal error (such as
     * collecting all of the errors into a single report): in any case,
     * the application must stop all regular processing when this
     * method is invoked, since the document is no longer reliable, and
     * the parser may no longer report parsing events.</p>
     *
     * @param e The error information encoded as an exception.
     * @exception org.xml.sax.SAXException Any SAX exception, possibly
     *            wrapping another exception.
     * @see org.xml.sax.ErrorHandler#fatalError
     * @see org.xml.sax.SAXParseException
     */
  virtual void fatalError(IN(RSAXParseException) saxparseexception) THROWS1(RSAXException) 
  { 
    RSAXParseException tx = saxparseexception;
    throw tx;
  }
};

} // namespace sax
} // namespace xml
} // namespace org

#endif //org_xml_sax_HandlerBase_h
