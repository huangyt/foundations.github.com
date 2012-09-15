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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Properties.h,v 1.24 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_Properties_h
#define acdk_util_Properties_h

#include <acdk.h>

#include <acdk/io/PrintWriter.h>
#include <acdk/io/Reader.h>

#include "HashMap.h"
#include "HashSet.h"

namespace acdk {
namespace util {



ACDK_DECL_CLASS(Properties);

/**
  used PropertiesChangeListener
*/
enum PropertiesChangeAction
{
  /**
    a single String was changed
  */
  PropChangeSetProperty       = 0x0001,
  /*
    a map of entries was changed
  */
  PropChangeSetMapProperty    = 0x0002,
  /**
    a array of string was changed
   */
  PropChangeSetArrayProperty  = 0x0004
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, PropertiesChangeAction);

ACDK_DECL_INTERFACE(PropertiesChangeListener);

/**
  interface to listen to changes on a Properties set.
  This callback is only called, if a modifing operation is
  called directly on the Properties.
  see also PropertiesChangeAction
*/
class ACDK_CORE_PUBLIC PropertiesChangeListener
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(PropertiesChangeListener)
public:
  /**
    Callback when Property was changed
    
    @param action see PropertiesChangeAction
    @param props the properties that was changed
    @param key the key changed
    @param value String, Map or StringArray, depending on action
  */
  virtual void propertyChanged(PropertiesChangeAction action, IN(RProperties) props, IN(RString) key, IN(RObject) value) = 0;
};

/**
  API: Java with extension<br/>

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.24 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC Properties
: extends HashMap
, implements ::acdk::io::Serializable
, implements ::acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(Properties)
protected:
  RProperties _defaults;
  RPropertiesChangeListenerArray _listener;
public:
  Properties(IN(RProperties) def = Nil);
  /**
    see RString getProperty(IN(RString) key, IN(RString) defaultValue, bool withDefaults = true);
  */
  RString getProperty(IN(RString) key);
  /**
    retrieve a property as String
    @param a key to the properties
    @param defaultValue will be returned if the property entry cannot be found
    @param withDefaults if true (default) the value will also be searched in the parent resp. default
           property set
    @return the found value or the param defaultValue if not found
  */
  RString getProperty(IN(RString) key, IN(RString) defaultValue, bool withDefaults = true);
  /**
    ACDK extension return a list of properties as map
    If keyStart is "my.prop" and the properties contains
    the values
      my.prop.a=A
      my.prop.b=B
      my.prop.c=C
    this method returns a map
      a => A
      b => B
      c => C
    
      If no element was found returns an empty map.

    This method computes the Map every time it will be called, so don't use it in inner loops.
    
  */
  RMap getMapProperty(IN(RString) keyStart, bool withDefaults = true);
  /**
    set the map of value/keys in current Property
    Before inserting all entries with starting keyStart + "." will be deleted
    @see getMapProperty
  */
  void setPropertyMap(IN(RString) keyStart, IN(RMap) map);
  
  /**
    ADCK extension return a list of property values a array
    if keyStart is "my.prop" and start = 1 (default is 0)
    and the properties contains following values:
      my.prop.1=A
      my.prop.2=C
      my.prop.3=B
      my.prop.5=X
    this method returns a string array containing "A", "C", "B"
    if no element was found returns an empty array
  */

  RStringArray getArrayProperty(IN(RString) keyStart, int start = 0, bool withDefaults = true);
  /**
    set the array of string in the current properties map
    Before inserting all entries with starting keyStart + "." will be deleted
    @param keyStart property key name
    @param values new values. If this is Nil only the old entries will be deleted
    @param start start number occour in the property keyname as keyStart.start
    @see getArrayProperty
  */
  void setArrayProperty(IN(RString) keyStart, IN(RStringArray) values, int start = 0);
  /**
    this method replaces $(KEY) with corresponding values in this properties.
    System::getProperties()->eval("$(ACDKHOME)/csf"); will expand for example to 
    "/artefaktur/acdk/csf" if ACDKHOME=/artefaktur/acdk
    If expr is "$(KEY)", but this property has no value for KEY the original expr
    will be in the resulting string.
    @param expr a string, which may contains $(KEY) 
    @param recursive call eval recursivelly, as long $(KEY) are in the evaluated string
  */
  RString eval(IN(RString) expr, bool recursive = false);
  /**
    writes the elements into the printwriter
  */
  void list(IN(acdk::io::RPrintWriter) out, bool withDefaults = true);
  /**
    Load a properties file using java literal decoding
  */
  void load(IN(acdk::io::RReader) in);
  /**
    get a iterator for all keys of this properties
    @param withDefaults if true (the default) also the names of parent resp. default
           property set will be returned
  */
  RIterator propertyNames(bool withDefaults = true);
  /**
    set the property value
  */
  RObject setProperty(IN(RString) key, IN(RString) value);

  void store(IN(acdk::io::RWriter) out, IN(RString) header, bool withDefaults = true);
  //void save(IN(acdk::io::RWriter) out, IN(RString) header, bool withDefaults = true);
  
  RProperties defaults() { return _defaults; }
  void setDefaults(IN(RProperties) defaults) { _defaults = defaults; }
  /** 
    merge existant properties with new properties.
    Property values with the same key from the new properties
    will be overwritten.
  */
  void mergeProperties(IN(RProperties) props);
  /**
    add a new listener
  */
  void addPropertyChangeListener(IN(RPropertiesChangeListener) listener);
  /**
    remove a previous added listener
    @param listener this has to be the same instance as previously added
  */
  void removePropertyChangeListener(IN(RPropertiesChangeListener) listener);
  /**
    Interface from cloneable.
    clone ignores all entries, where the key is not a string.
    clone only copy the reference, and do not clone the values itself
    clone will also clone Properties::_default.
  */
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alloc);
  /**
    try to load properties file on given name.
    The name "acdk.util.MyProps" tries to load
    $ACDKHOME/cfg/acdk/util/MyProps.properties"
    @param name alternativally also acdk/util/MyProps notation is allowed
    @return Nil if not found
  */
  static RProperties loadProperties(IN(RString) name);
protected:
  static RString _format(IN(RString) key, IN(RString) value);
  /// deletes all entries with keys starting with keyStart in this properties
  foreign void _deleteKeys(IN(RString) keyStart); 
  /// used internally to notify listener
  foreign inline void _notifyListener(PropertiesChangeAction action, IN(RString) key, IN(RObject) obj)
  {
    if (_listener == Nil)
      return;
    _notifyListener2(action, key, obj);
  }
  foreign void _notifyListener2(PropertiesChangeAction action, IN(RString) key, IN(RObject) obj);
  friend class PropertiesIterator;
};


} // util
} // acdk


#endif //acdk_util_Properties_h

