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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/helpers/NamespaceSupport.h,v 1.7 2005/02/05 10:45:38 kommer Exp $

#ifndef org_xml_sax_helpers_NamespaceSupport_h
#define org_xml_sax_helpers_NamespaceSupport_h

#include <acdk.h>
#include <acdk/util/HashMap.h>
#include <acdk/util/EmptyCollectionIterator.h>

#include "../Config.h"

namespace org {
namespace xml {
namespace sax {
namespace helpers {

ACDK_DECL_CLASS(Context);

 /**
  * Internal class for a single Namespace context.
  *
  * <p>This module caches and reuses Namespace contexts, so the number allocated
  * will be equal to the element depth of the document, not to the total
  * number of elements (i.e. 5-10 rather than tens of thousands).</p>
  */
class ACDK_ORG_XML_PUBLIC Context 
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(Context)
protected:
  acdk::util::RHashMap prefixTable;
  acdk::util::RHashMap uriTable;
  acdk::util::RHashMap elementNameTable;
  acdk::util::RHashMap attributeNameTable;
  RString defaultNS;
  
private:
  RStringArray declarations;
  bool tablesDirty;
  RContext parent;
public:   
/**
* Create the root-level Namespace context.
  */
  Context()
    : tablesDirty(false)
  {
    copyTables();
  }
  
  
  /**
  * (Re)set the parent of this Namespace context.
  *
  * @param context The parent Namespace context object.
  */
  void setParent(IN(RContext) parent);
  
  
  /**
  * Declare a Namespace prefix for this context.
  *
  * @param prefix The prefix to declare.
  * @param uri The associated Namespace URI.
  * @see org.xml.sax.helpers.NamespaceSupport#declarePrefix
  */
  void declarePrefix(IN(RString) prefix, IN(RString) uri);
  
  
  /**
  * Process a raw XML 1.0 name in this context.
  *
  * @param qName The raw XML 1.0 name.
  * @param isAttribute true if this is an attribute name.
  * @return An array of three strings containing the
  *         URI part (or empty string), the local part,
  *         and the raw name, all internalized, or null
  *         if there is an undeclared prefix.
  * @see org.xml.sax.helpers.NamespaceSupport#processName
  */
  RStringArray processName(IN(RString) qName, bool isAttribute);
  
  /**
  * Look up the URI associated with a prefix in this context.
  *
  * @param prefix The prefix to look up.
  * @return The associated Namespace URI, or null if none is
  *         declared.	
  * @see org.xml.sax.helpers.NamespaceSupport#getURI
  */
  RString getURI(IN(RString) prefix);
  
  
  /**
  * Look up one of the prefixes associated with a URI in this context.
  *
  * <p>Since many prefixes may be mapped to the same URI,
  * the return value may be unreliable.</p>
  *
  * @param uri The URI to look up.
  * @return The associated prefix, or null if none is declared.
  * @see org.xml.sax.helpers.NamespaceSupport#getPrefix
  */
  RString getPrefix(IN(RString) uri)
  {
    if (uriTable == Nil) {
      return Nil;
    } else {
      return (RString)uriTable->get(&uri);
    }
  }
  
  
  /**
  * Return an enumeration of prefixes declared in this context.
  *
  * @return An enumeration of prefixes (possibly empty).
  * @see org.xml.sax.helpers.NamespaceSupport#getDeclaredPrefixes
  */
  acdk::util::RIterator getDeclaredPrefixes()
  {
    if (declarations == Nil) {
      return new acdk::util::EmptyCollectionIterator();
    } else {
      return declarations->iterator();
    }
  }
  
  
  /**
  * Return an enumeration of all prefixes currently in force.
  *
  * <p>The default prefix, if in force, is <em>not</em>
  * returned, and will have to be checked for separately.</p>
  *
  * @return An enumeration of prefixes (never empty).
  * @see org.xml.sax.helpers.NamespaceSupport#getPrefixes
  */
  acdk::util::RIterator getPrefixes()
  {
    if (prefixTable == Nil) {
      return new acdk::util::EmptyCollectionIterator();
    } else {
      return prefixTable->keySet()->iterator();
    }
  }
  
  /**
  * Copy on write for the internal tables in this context.
  *
  * <p>This class is optimized for the normal case where most
  * elements do not contain Namespace declarations.</p>
  */	
  void copyTables();

};

ACDK_DECL_CLASS(NamespaceSupport);

/**
 * Encapsulate Namespace logic for use by SAX drivers.
 *
 * <blockquote>
 * <em>This module, both source code and documentation, is in the
 * Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
 * </blockquote>
 *
 * <p>This class encapsulates the logic of Namespace processing:
 * it tracks the declarations currently in force for each context
 * and automatically processes qualified XML 1.0 names into their
 * Namespace parts; it can also be used in reverse for generating
 * XML 1.0 from Namespaces.</p>
 *
 * <p>Namespace support objects are reusable, but the reset method
 * must be invoked between each session.</p>
 *
 * <p>Here is a simple session:</p>
 *
 * <pre>
 * String parts[] = new String[3];
 * NamespaceSupport support = new NamespaceSupport();
 *
 * support.pushContext();
 * support.declarePrefix("", "http://www.w3.org/1999/xhtml");
 * support.declarePrefix("dc", "http://www.purl.org/dc#");
 *
 * String parts[] = support.processName("p", parts, false);
 * System.out.println("Namespace URI: " + parts[0]);
 * System.out.println("Local name: " + parts[1]);
 * System.out.println("Raw name: " + parts[2]);

 * String parts[] = support.processName("dc:title", parts, false);
 * System.out.println("Namespace URI: " + parts[0]);
 * System.out.println("Local name: " + parts[1]);
 * System.out.println("Raw name: " + parts[2]);

 * support.popContext();
 * </pre>
 *
 * <p>Note that this class is optimized for the use case where most
 * elements do not contain Namespace declarations: if the same
 * prefix/URI mapping is repeated for each context (for example), this
 * class will be somewhat less efficient.</p>
 *
 * @since SAX 2.0
 * @author Roger Rene Kommer
 * @author Java Original: David Megginson, 
 *         <a href="mailto:sax@megginson.com">sax@megginson.com</a>
 * @version 2.0r2pre
 */


class ACDK_ORG_XML_PUBLIC NamespaceSupport
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(NamespaceSupport)
public:
  /**
     * Create a new Namespace support object.
     */
  NamespaceSupport()
  {
	  reset();
  }
  
  /**
  * Reset this Namespace support object for reuse.
  *
  * <p>It is necessary to invoke this method before reusing the
  * Namespace support object for a new session.</p>
  */
  void reset();
  
  
  /**
  * Start a new Namespace context.
  *
  * <p>Normally, you should push a new context at the beginning
  * of each XML element: the new context will automatically inherit
  * the declarations of its parent context, but it will also keep
  * track of which declarations were made within this context.</p>
  *
  * <p>The Namespace support object always starts with a base context
  * already in force: in this context, only the "xml" prefix is
  * declared.</p>
  *
  * @see #popContext
  */
  void pushContext();
  
  
  /**
  * Revert to the previous Namespace context.
  *
  * <p>Normally, you should pop the context at the end of each
  * XML element.  After popping the context, all Namespace prefix
  * mappings that were previously in force are restored.</p>
  *
  * <p>You must not attempt to declare additional Namespace
  * prefixes after popping a context, unless you push another
  * context first.</p>
  *
  * @see #pushContext
  */
  void popContext();
  
  /**
  * Declare a Namespace prefix.
  *
  * <p>This method declares a prefix in the current Namespace
  * context; the prefix will remain in force until this context
  * is popped, unless it is shadowed in a descendant context.</p>
  *
  * <p>To declare a default Namespace, use the empty string.  The
  * prefix must not be "xml" or "xmlns".</p>
  *
  * <p>Note that you must <em>not</em> declare a prefix after
  * you've pushed and popped another Namespace.</p>
  *
  * <p>Note that there is an asymmetry in this library: while {@link
  * #getPrefix getPrefix} will not return the default "" prefix,
  * even if you have declared one; to check for a default prefix,
  * you have to look it up explicitly using {@link #getURI getURI}.
  * This asymmetry exists to make it easier to look up prefixes
  * for attribute names, where the default prefix is not allowed.</p>
  *
  * @param prefix The prefix to declare, or null for the empty
  *        string.
  * @param uri The Namespace URI to associate with the prefix.
  * @return true if the prefix was legal, false otherwise
  * @see #processName
  * @see #getURI
  * @see #getPrefix
  */
  bool declarePrefix(IN(RString) prefix, IN(RString) uri);
  
  
  /**
  * Process a raw XML 1.0 name.
  *
  * <p>This method processes a raw XML 1.0 name in the current
  * context by removing the prefix and looking it up among the
  * prefixes currently declared.  The return value will be the
  * array supplied by the caller, filled in as follows:</p>
  *
  * <dl>
  * <dt>parts[0]</dt>
  * <dd>The Namespace URI, or an empty string if none is
  *  in use.</dd>
  * <dt>parts[1]</dt>
  * <dd>The local name (without prefix).</dd>
  * <dt>parts[2]</dt>
  * <dd>The original raw name.</dd>
  * </dl>
  *
  * <p>All of the strings in the array will be internalized.  If
  * the raw name has a prefix that has not been declared, then
  * the return value will be null.</p>
  *
  * <p>Note that attribute names are processed differently than
  * element names: an unprefixed element name will received the
  * default Namespace (if any), while an unprefixed element name
  * will not.</p>
  *
  * @param qName The raw XML 1.0 name to be processed.
  * @param parts An array supplied by the caller, capable of
  *        holding at least three members.
  * @param isAttribute A flag indicating whether this is an
  *        attribute name (true) or an element name (false).
  * @return The supplied array holding three internalized strings 
  *        representing the Namespace URI (or empty string), the
  *        local name, and the raw XML 1.0 name; or null if there
  *        is an undeclared prefix.
  * @see #declarePrefix
  * @see java.lang.String#intern 
  */

  RStringArray processName(IN(RString) qName, IN(RStringArray) parts, bool isAttribute);
  
  
  /**
  * Look up a prefix and get the currently-mapped Namespace URI.
  *
  * <p>This method looks up the prefix in the current context.
  * Use the empty string ("") for the default Namespace.</p>
  *
  * @param prefix The prefix to look up.
  * @return The associated Namespace URI, or null if the prefix
  *         is undeclared in this context.
  * @see #getPrefix
  * @see #getPrefixes
  */
  RString getURI(IN(RString) prefix)
  {
    return currentContext->getURI(prefix);
  }
  
  
  /**
  * Return an enumeration of all prefixes currently declared.
  *
  * <p><strong>Note:</strong> if there is a default prefix, it will not be
  * returned in this enumeration; check for the default prefix
  * using the {@link #getURI getURI} with an argument of "".</p>
  *
  * @return An enumeration of all prefixes declared in the
  *         current context except for the empty (default)
  *         prefix.
  * @see #getDeclaredPrefixes
  * @see #getURI
  */
  acdk::util::RIterator getPrefixes()
  {
    return currentContext->getPrefixes();
  }
  
  
  /**
  * Return one of the prefixes mapped to a Namespace URI.
  *
  * <p>If more than one prefix is currently mapped to the same
  * URI, this method will make an arbitrary selection; if you
  * want all of the prefixes, use the {@link #getPrefixes}
  * method instead.</p>
  *
  * <p><strong>Note:</strong> this will never return the empty (default) prefix;
  * to check for a default prefix, use the {@link #getURI getURI}
  * method with an argument of "".</p>
  *
  * @param uri The Namespace URI.
  * @param isAttribute true if this prefix is for an attribute
  *        (and the default Namespace is not allowed).
  * @return One of the prefixes currently mapped to the URI supplied,
  *         or null if none is mapped or if the URI is assigned to
  *         the default Namespace.
  * @see #getPrefixes(java.lang.String)
  * @see #getURI
  */
  RString getPrefix(IN(RString) uri)
  {
    return currentContext->getPrefix(uri);
  }
  
  
  /**
  * Return an enumeration of all prefixes currently declared for a URI.
  *
  * <p>This method returns prefixes mapped to a specific Namespace
  * URI.  The xml: prefix will be included.  If you want only one
  * prefix that's mapped to the Namespace URI, and you don't care 
  * which one you get, use the {@link #getPrefix getPrefix}
  *  method instead.</p>
  *
  * <p><strong>Note:</strong> the empty (default) prefix is <em>never</em> included
  * in this enumeration; to check for the presence of a default
  * Namespace, use the {@link #getURI getURI} method with an
  * argument of "".</p>
  *
  * @param uri The Namespace URI.
  * @return An enumeration of all prefixes declared in the
  *         current context.
  * @see #getPrefix
  * @see #getDeclaredPrefixes
  * @see #getURI
  */
  acdk::util::RIterator getPrefixes(IN(RString) uri);
  
  
  /**
  * Return an enumeration of all prefixes declared in this context.
  *
  * <p>The empty (default) prefix will be included in this 
  * enumeration; note that this behaviour differs from that of
  * {@link #getPrefix} and {@link #getPrefixes}.</p>
  *
  * @return An enumeration of all prefixes declared in this
  *         context.
  * @see #getPrefixes
  * @see #getURI
  */
  acdk::util::RIterator getDeclaredPrefixes ()
  {
    return currentContext->getDeclaredPrefixes();
  }
  
  static RString getAbsoluteURI(IN(RString) base, IN(RString) uri);
  static RString getBaseURI(IN(RString) uri);
private:  
  RContextArray contexts;
  RContext currentContext;
  int contextPos;
};

} // namespace helpers
} // namespace sax
} // namespace xml
} // namespace org

#endif //org_xml_sax_helpers_NamespaceSupport_h
