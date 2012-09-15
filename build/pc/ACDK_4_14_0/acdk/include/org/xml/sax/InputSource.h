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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/InputSource.h,v 1.12 2005/02/05 10:45:38 kommer Exp $


// Documentation written by David Megginson
// NO WARRANTY!  This class documentation is in the Public Domain.

#ifndef org_xml_sax_InputSource_h
#define org_xml_sax_InputSource_h

#include <acdk.h>
#include <acdk/io/Reader.h>

namespace org {
namespace xml {
namespace sax {


ACDK_DECL_CLASS(InputSource);

/**
 * A single input source for an XML entity.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * See <a href='http://www.saxproject.org'>http://www.saxproject.org</a>
 * for further information.
 * </blockquote>
 *
 * <p>This class allows a SAX application to encapsulate information
 * about an input source in a single object, which may include
 * a public identifier, a system identifier, a byte stream (possibly
 * with a specified encoding), and/or a character stream.</p>
 *
 * <p>There are two places that the application can deliver an
 * input source to the parser: as the argument to the Parser.parse
 * method, or as the return value of the EntityResolver.resolveEntity
 * method.</p>
 *
 * <p>The SAX parser will use the InputSource object to determine how
 * to read XML input.  If there is a character stream available, the
 * parser will read that stream directly, disregarding any text
 * encoding declaration found in that stream.
 * If there is no character stream, but there is
 * a byte stream, the parser will use that byte stream, using the
 * encoding specified in the InputSource or else (if no encoding is
 * specified) autodetecting the character encoding using an algorithm
 * such as the one in the XML specification.  If neither a character
 * stream nor a
 * byte stream is available, the parser will attempt to open a URI
 * connection to the resource identified by the system
 * identifier.</p>
 *
 * <p>An InputSource object belongs to the application: the SAX parser
 * shall never modify it in any way (it may modify a copy if 
 * necessary).  However, standard processing of both byte and
 * character streams is to close them on as part of end-of-parse cleanup,
 * so applications should not attempt to re-use such streams after they
 * have been handed to a parser.  </p>
 *
 * @since SAX 1.0
 * @see org.xml.sax.XMLReader#parse(org.xml.sax.InputSource)
 * @see org.xml.sax.EntityResolver#resolveEntity
 * @see java.io.InputStream
 * @see java.io.Reader
 */
class ACDK_ORG_XML_PUBLIC InputSource
: public acdk::lang::Object
{
  ACDK_WITH_METAINFO(InputSource)
private:
  ::acdk::lang::RString _publicId;
  ::acdk::lang::RString _systemId;
  ::acdk::lang::RString _encoding;
  ::acdk::io::RReader _in;
public: 
  
    /**
     * Zero-argument default constructor.
     *
     * @see #setPublicId
     * @see #setSystemId
     * @see #setByteStream
     * @see #setCharacterStream
     * @see #setEncoding
     */
  InputSource()
  : Object()
  {
  }
  /**
     * Create a new input source with a system identifier.
     *
     * <p>Applications may use setPublicId to include a 
     * public identifier as well, or setEncoding to specify
     * the character encoding, if known.</p>
     *
     * <p>If the system identifier is a URL, it must be fully
     * resolved (it may not be a relative URL).</p>
     *
     * @param systemId The system identifier (URI).
     * @see #setPublicId
     * @see #setSystemId
     * @see #setByteStream
     * @see #setEncoding
     * @see #setCharacterStream
     */
  InputSource(IN(::acdk::lang::RString) sysId)
  : Object(),
    _systemId(sysId)
  {
  }
  /**
     * Create a new input source with a byte stream.
     *
     * <p>Application writers should use setSystemId() to provide a base 
     * for resolving relative URIs, may use setPublicId to include a 
     * public identifier, and may use setEncoding to specify the object's
     * character encoding.</p>
     *
     * @param reader The raw byte stream containing the document.
     * @see #setPublicId
     * @see #setSystemId
     * @see #setEncoding
     * @see #setByteStream
     * @see #setCharacterStream
     */
  InputSource(IN(::acdk::io::RReader) reader)
  : Object(),
     _in(reader)
    {
    }
  /**
     * Set the public identifier for this input source.
     *
     * <p>The public identifier is always optional: if the application
     * writer includes one, it will be provided as part of the
     * location information.</p>
     *
     * @param publicId The public identifier as a string.
     * @see #getPublicId
     * @see org.xml.sax.Locator#getPublicId
     * @see org.xml.sax.SAXParseException#getPublicId
     */
  virtual void setPublicId(IN(::acdk::lang::RString) s) { _publicId = s; } 
   /**
     * Get the public identifier for this input source.
     *
     * @return The public identifier, or null if none was supplied.
     * @see #setPublicId
     */
  virtual ::acdk::lang::RString getPublicId() { return _publicId; }
  /**
     * Set the system identifier for this input source.
     *
     * <p>The system identifier is optional if there is a byte stream
     * or a character stream, but it is still useful to provide one,
     * since the application can use it to resolve relative URIs
     * and can include it in error messages and warnings (the parser
     * will attempt to open a connection to the URI only if
     * there is no byte stream or character stream specified).</p>
     *
     * <p>If the application knows the character encoding of the
     * object pointed to by the system identifier, it can register
     * the encoding using the setEncoding method.</p>
     *
     * <p>If the system identifier is a URL, it must be fully
     * resolved (it may not be a relative URL).</p>
     *
     * @param systemId The system identifier as a string.
     * @see #setEncoding
     * @see #getSystemId
     * @see org.xml.sax.Locator#getSystemId
     * @see org.xml.sax.SAXParseException#getSystemId
     */
  virtual void setSystemId(IN(::acdk::lang::RString) s) { _systemId = s; }
  
    /**
     * Get the system identifier for this input source.
     *
     * <p>The getEncoding method will return the character encoding
     * of the object pointed to, or null if unknown.</p>
     *
     * <p>If the system ID is a URL, it will be fully resolved.</p>
     *
     * @return The system identifier, or null if none was supplied.
     * @see #setSystemId
     * @see #getEncoding
     */
  virtual ::acdk::lang::RString getSystemId() { return _systemId; }
  /** 
     * Set the character encoding, if known.
     *
     * <p>The encoding must be a string acceptable for an
     * XML encoding declaration (see section 4.3.3 of the XML 1.0
     * recommendation).</p>
     *
     * <p>This method has no effect when the application provides a
     * character stream.</p>
     *
     * @param encoding A string describing the character encoding.
     * @see #setSystemId
     * @see #setByteStream
     * @see #getEncoding
     */
  virtual void setEncoding(IN(::acdk::lang::RString) s) { _encoding = s; }
  /**
     * Get the character encoding for a byte stream or URI.
     * This value will be ignored when the application provides a
     * character stream.
     *
     * @return The encoding, or null if none was supplied.
     * @see #setByteStream
     * @see #getSystemId
     * @see #getByteStream
     */
  virtual ::acdk::lang::RString getEncoding() { return _encoding; }
  /**
     * Set the byte stream for this input source.
     *
     * <p>The SAX parser will ignore this if there is also a character
     * stream specified, but it will use a byte stream in preference
     * to opening a URI connection itself.</p>
     *
     * <p>If the application knows the character encoding of the
     * byte stream, it should set it with the setEncoding method.</p>
     *
     * @param byteStream A byte stream containing an XML document or
     *        other entity.
     * @see #setEncoding
     * @see #getByteStream
     * @see #getEncoding
     * @see java.io.InputStream
     */
  virtual void setByteStream(IN(::acdk::io::RReader) reader) { _in = reader; }
  /**
     * Get the byte stream for this input source.
     *
     * <p>The getEncoding method will return the character
     * encoding for this byte stream, or null if unknown.</p>
     *
     * @return The byte stream, or null if none was supplied.
     * @see #getEncoding
     * @see #setByteStream
     */
  virtual ::acdk::io::RReader getByteStream() { return _in; }


};

} // namespace sax
} // namespace xml
} // namespace org

#endif //org_xml_sax_InputSource_h
