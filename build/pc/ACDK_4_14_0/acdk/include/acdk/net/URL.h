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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URL.h,v 1.15 2005/03/17 12:44:14 kommer Exp $

#ifndef acdk_net_URL_h
#define acdk_net_URL_h

#include <acdk/util/HashMap.h>
#include <acdk/io/File.h>

#include "net.h"


#include "URLInterface.h"
//#include "URLConnection.h"
#include "URLStreamHandler.h"

namespace acdk {
namespace net {

using namespace acdk::lang;

USING_CLASS(::acdk::util::, HashMap);

ACDK_DECL_INTERFACE(URLStreamHandlerFactory);

ACDK_DECL_CLASS(URLStreamHandler);

ACDK_DECL_CLASS(URL);

/**
  Represents a URL.
  @todo integrate with acdk::io::FileSystem and acdk::io::FileImpl
*/
class ACDK_NET_PUBLIC URL
: extends ::acdk::lang::Object
, implements ::acdk::net::URLInterface
{
  ACDK_WITH_METAINFO(URL)
private:
 
  RString protocol;
  RString host;
  int port;
  RString file;
  /**
   The # portion of the URL
  */
  RString ref;
 
  RString user;
  RString password;
  RURLStreamHandler ph;
public:




/*************************************************************************/

/**
  * Constructs a URL and loads a protocol handler for the values passed in
  * as arugments.  Uses the default port for the protocol.
  *
  * @param protocol The protocol for this URL (", ", etc)
  * @param host The hostname or IP address for this URL
  * @param file The " portion of this URL.
  *
  * @exception MalformedURLException If a protocol handler cannot be loaded
  */

  URL(IN(RString) protocol, IN(RString) host, IN(RString) file) 
  : Object()
  , port(-1)
  {
    ACDK_SAFE_CONSTRUCTOR();
    _URLinit(protocol->toLowerCase(), host, -1, Nil, Nil, file, Nil);
  }


/*************************************************************************/

/**
  * This method initializes a new instance of <code>URL</code> with the
  * specified protocol, host, port, and file.  Additionally, this method
  * allows the caller to specify a protocol handler to use instead of 
  * the default.  If this handler is specified, the caller must have
  * the " permission (see <code>NetPermission</code>)
  * or a <code>SecurityException</code> will be thrown.
  *
  * @param protocol The protocol for this URL (", ", etc)
  * @param host The hostname or IP address to connect to
  * @param port The port number to use, or -1 to use the protocol's default port
  * @param file The " portion of the URL.
  * @param ph The protocol handler to use with this URL.
  *
  * @exception MalformedURLException If no protocol handler can be loaded
  * for the specified protocol.
  * @exception SecurityException If the <code>SecurityManager</code> exists
  * and does not allow the caller to specify its own protocol handler.
  */

  URL(IN(RString) protocol, IN(RString) host, int port, IN(RString) file, IN(RString) user = Nil, 
      IN(RString) pass = Nil, IN(RURLStreamHandler) ph = Nil) 
  : Object()
  , port(-1)
  {
    ACDK_SAFE_CONSTRUCTOR();
    _URLinit(protocol, host, port, pass, user, file, ph);
  }

private:
  void _URLinit(IN(RString) protocol, IN(RString) host, int port,
                IN(RString) user, IN(RString) pass, IN(RString) file, 
                IN(RURLStreamHandler) ph);
public:

/**
  * This method parses a String representation of a URL within the
  * context of an existing URL.  Principally this means that any fields
  * not present the URL are inheritied from the context URL.  This allows
  * relative URL's to be easily constructed (***true?***).  If the
  * context argument is null, then a complete URL must be specified in the
  * URL string.  If the protocol parsed out of the URL is different 
  * from the context URL's protocol, then then URL String is also
  * expected to be a complete URL.
  * 
  * Additionally, this method allows the caller to specify a protocol handler to 
  * use instead of  the default.  If this handler is specified, the caller must 
  * have the " permission (see <code>NetPermission</code>)
  * or a <code>SecurityException</code> will be thrown.
  *
  * @param context The context URL
  * @param url A String representing this URL
  * @param ph The protocol handler for this URL
  *
  * @exception MalformedURLException If a protocol handler cannot be found or the URL cannot be parsed
  * @exception SecurityException If the <code>SecurityManager</code> exists
  * and does not allow the caller to specify its own protocol handler.
  */

  URL(IN(RURL) context, IN(RString) url, IN(RURLStreamHandler) ph = Nil)
  : Object()
  , port(-1)
  {
    ACDK_SAFE_CONSTRUCTOR();
    _URLbyContext(context, url, ph);
  }
private:
  void _URLbyContext(IN(RURL) context, IN(RString) url, IN(RURLStreamHandler) ph = Nil);
public:

/*************************************************************************/

/**
  * Initializes a URL from a complete string specification such as
  * ".  First the protocol name is parsed
  * out of the string.  Then a handler is located for that protocol and
  * the parseURL() method of that protocol handler is used to parse the
  * remaining fields.
  *
  * @param url The complete String representation of a URL
  *
  * @exception MalformedURLException If a protocol handler cannot be found or the URL cannot be parsed
  */

  URL(IN(RString) url)
  : Object()
  , port(-1)
  {
    ACDK_SAFE_CONSTRUCTOR();
   _URLbyContext(Nil, url, Nil);
  }

/**
  * Returns the protocol name of this URL
  * @return The protocol
  */

  virtual RString getProtocol() { return protocol; }
 
/**
  * Returns the hostname or IP address for this protocol
  *
  * @return The hostname
  */
  virtual RString getHost() { return host; }

/**
  * Returns the port number of this URL or -1 if the default port number is
  * being used
  *
  * @return The port number
  */

  virtual int getPort() { return port; }
  void setPort(int portno) { port = portno; }


/*************************************************************************/

/**
  * Returns the " portion of this URL
  *
  * @return The file portion
  */

  virtual RString getFile() { return file; }
  void setFile(IN(RString) fn) { file = fn; }
  virtual RString getUser() { return user; }
  void setUser(IN(RString) u) { user = u; }
  virtual RString getPassword() { return password; }
  void setPassword(IN(RString) p) { password = p; }

/*************************************************************************/

/**
  * Returns the ref (sometimes called the ") portion of the
  * URL
  *
  * @return The ref
  */

  virtual RString getRef() { return ref; }
  void setRef(IN(RString) r) { ref = r; }

/*************************************************************************/

/**
  * Test another URL for equality with this one.  This will be true only if
  * the argument is non-null and all of the fields in the URL's match 
  * exactly (ie, protocol, host, port, file, and ref).  Overrides
  * Object.equals().
  *
  * @param url The URL to compare with
  *
  * @return true if the URL is equal, false otherwise
  */

  virtual bool equals(IN(RObject) url);


/*************************************************************************/

/**
  * Tests whether or not another URL refers to the same " as this one.
  * This will be true if and only if the passed object is not null, is a
  * URL, and matches all fields but the ref (ie, protocol, host, port,
  * and file);
  *
  * @param url The URL object to test with
  *
  * @return true if URL matches this URL's file, false otherwise
  */

  virtual bool sameFile(IN(RURLInterface) url);


/*************************************************************************/

/*
  * This is the implementation of the Comparable interface for URL's.  It
  * will return a negative int, 0, or a positive int depending on whether
  * a URL is less than, equal to, or greater than this URL respectively.
  * This is done by returning the compareTo result on this string
  * representations of these URL's.
  *
  * @param url The URL to compare against
  *
  * @return An int indicating whether a URL is less than, equal to, or greater than this URL
  */
  int compareTo(IN(RObject) url)
  {
    return (toExternalForm()->compareTo(((RURL)url)->toExternalForm()));
  }


/*************************************************************************/

/**
  * Returns a String representing this URL.  The String returned is
  * created by calling the protocol handler's toExternalForm() method.
  *
  * @return A string for this URL
  */

  virtual RString toExternalForm();

/*************************************************************************/

/**
  * Returns a String representing this URL.  Identical to toExternalForm().
  * The value returned is created by the protocol handler's 
  * toExternalForm method.  Overrides Object.toString()
  *
  * @return A string for this URL
  */

  virtual RString toString()
  {
    return(toExternalForm());
  }


/*************************************************************************/

/**
  * Returns a URLConnection for this object created by calling the
  * openConnection() method of the protocol handler
  *
  * @return A URLConnection for this URL
  *
  * @exception IOException If an error occurs
  */

  virtual RURLConnection openConnection();


/*************************************************************************/

/**
  * This method returns an InputStream for this URL by first opening the
  * connection, then calling the getInputStream() method against the
  * connection.
  *
  * @return An InputStream for this URL
  *
  * @exception IOException If an error occurs
  */

  RReader openStream();


/*************************************************************************/

/**
  * Returns the contents of this URL as an object by first opening a
  * connection, then calling the getContent() method against the connection
  *
  * @return A content object for this URL
  *
  * @exception IOException If an error occurs
  */

  RObject getContent();

/*************************************************************************/

/**
  * This method returns a hash value for this object.
  *
  * @return a hash value for this object.
  */
  virtual int hashCode()  { return toString()->hashCode(); }
  static RString fileAsUrlName(IN(RString) fileName);
  static RString fileAsUrlName(IN(acdk::io::RFile) file);
protected:


/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * This protected method is used by protocol handlers to set the values
  * of the fields in this URL.  This might be done in the parseURL method
  * of that class.
  *
  * @param protocol The protocol name for this URL
  * @param host The hostname or IP address for this URL
  * @param port The port number of this URL
  * @param file The " portion of this URL.
  * @param ref The anchor portion of this URL.
  */

  virtual void set(IN(RString) protocol, IN(RString) host, int port, IN(RString) user, IN(RString) password, IN(RString) file, IN(RString) ref);
private:
  friend class URLStreamHandler;
  friend class URL_initializer;
};
} // namespace acdk
} // namespace net

#endif //acdk_net_URL_h


