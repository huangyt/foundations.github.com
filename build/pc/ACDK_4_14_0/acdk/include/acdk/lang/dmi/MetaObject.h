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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/MetaObject.h,v 1.20 2005/04/09 19:26:52 kommer Exp $

#ifndef acdk_lang_dmi_MetaObject_h
#define acdk_lang_dmi_MetaObject_h

#include "MetaAttribute.h"


namespace acdk {
namespace lang {
namespace dmi {

ACDK_DECL_INTERFACE(MetaObject);

ACDK_DECL_INTERFACE(MetaObjectListener);

/**
  listen on register a new MetaObject into type info database
*/
class ACDK_CORE_PUBLIC MetaObjectListener
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(MetaObjectListener)
public:
  virtual void onRegister(IN(RMetaObject) metaObject) = 0;
  virtual void onUnregister(IN(RMetaObject) metaObject) = 0;
};



/** 
  A MetaObject is a class with meta information
  about a language construct
  
  @author Roger Rene Kommer (kommer@artefaktur.com)
*/

class ACDK_CORE_PUBLIC MetaObject
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(MetaObject)
public:
  /**
    return all meta information connected with this MetaObject
  */
  virtual RMetaAttributeArray getMetaAttributes();
  /**
    return all meta information connected with this MetaObject and this
    Object.
   */
  virtual RMetaAttributeArray getInstanceMetaAttributes(IN(RObject) obj);
  /**
    return meta atribute named by keyname
  */
  virtual RMetaAttribute getMetaAttribute(IN(RString) keyname);
  RObject getObjectMetaAttribute(IN(RString) keyname);
  /*
    return meta attribute as ScriptVar
  */
  foreign virtual ScriptVar getScriptVarMetaAttribute(IN(RString) keyname);
  /**
    return meta atribute named by keyname and obj instance
  */
  virtual RMetaAttribute getInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname);
  /**
    return meta atribute named by keyname and obj instance as ScriptVar
  */
  foreign virtual ScriptVar getScriptVarInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname);
  /**
    return true if a meta attribute is available under keyname
  */
  virtual bool hasMetaAttribute(IN(RString) keyname);
  /**
    return true if a meta attribute is available under obj instance and keyname 
  */
  virtual bool hasInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname);
  /**
    set a new a meta attribute under keyname 
  */
  virtual void setMetaAttribute(IN(RString) keyname, IN(RObject) obj);
  /**
    set ScriptVar as MetaAttribute
  */
  foreign virtual void setMetaAttribute(IN(RString) keyname, const ScriptVar& attr);
  /**
    set a new attribute attr for this obj instance under keyname
  */
  virtual void setInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname, IN(RObject) attr);
  /**
    set a new attribute attr for this obj instance under keyname as ScriptVar
  */
  foreign virtual void setInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname, const ScriptVar& attr);
  /**
    delete a a meta attribute under keyname 
  */
  virtual void deleteMetaAttribute(IN(RString) keyname);
  /**
    delete a a meta attribute for obj instance under keyname 
  */
  virtual void deleteInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname);
  /**
    internal should be implemented by Class, Method, Member, and so on
  */
  foreign virtual MetaInfo* getMetaInfo() { return 0; }
  /**
    print the information 
    @param format combination of acdk::lang::dmi::TypeNameFormat
  */
  virtual RString toTypeString(int format = TpFtFormatStandard) = 0;

  static void registerListener(IN(RMetaObjectListener) listner);
  static void unRegisterListener(IN(RMetaObjectListener) listner);
};


ACDK_DECL_CLASS(MetaObjectImpl);

/**
  Default implementation of MetaObject
  MetaObject are reflection objets like Class, Method, etc.
  @see acdk::lang::dmi::MetaObject
*/
class ACDK_CORE_PUBLIC MetaObjectImpl
: extends ::acdk::lang::Object
, implements MetaObject
{
  ACDK_WITH_METAINFO(MetaObjectImpl)
protected:
  foreign MetaInfo* _metaInfo;
public:
  MetaObjectImpl()
    : _metaInfo(0)
  {
  }
  foreign MetaObjectImpl(MetaInfo* metaInfo )
  : _metaInfo(metaInfo)
  {
  }
  foreign virtual MetaInfo* getMetaInfo() { return _metaInfo; }
  foreign virtual void setMetaInfo(MetaInfo* metainfo) { _metaInfo = metainfo; }
  virtual RString toTypeString(int format = TpFtFormatStandard);
};

} // dmi
} // lang
} // acdk


#endif //acdk_lang_dmi_MetaObject_h

