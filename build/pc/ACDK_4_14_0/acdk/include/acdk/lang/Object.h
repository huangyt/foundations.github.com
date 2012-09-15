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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Object.h,v 1.51 2005/04/13 12:55:09 kommer Exp $

#ifndef acdk_lang_Object_h
#define acdk_lang_Object_h

#ifndef acdk_h
#include <acdk.h>
#endif // acdk_h


#include "sys/ObjectHeap.h"
#include "sys/ObjectLockPool.h"
#include "ObjectImpl.h"
#include "dmi/ClazzInfo.h"
#include "dmi/StdDispatch.h"

namespace acdk {
namespace io {
  
  ACDK_DECL_INTERFACE(Reader);
  ACDK_DECL_INTERFACE(Writer);
   
  ACDK_DECL_INTERFACE(FilterWriter);
  ACDK_DECL_INTERFACE(FilterReader);

  ACDK_DECL_INTERFACE(DataWriter);
  ACDK_DECL_INTERFACE(DataReader);

  ACDK_DECL_INTERFACE(ObjectWriter);
  ACDK_DECL_INTERFACE(ObjectReader);

} // io
} // acdk

namespace acdk {
namespace lang {


ACDK_DECL_CLASS(Class);

class String;
class Comparable;
class RString;

//ACDK_DECL_CLASS(Object);
class Object;
typedef ::RefHolder< ::acdk::lang::Object> RObject;

/** 
  Root of all Classes, except Interfaces.
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.51 $
  @date $Date: 2005/04/13 12:55:09 $
*/  

class ACDK_CORE_PUBLIC Object
: public foreign ObjectBase
, public foreign ::acdk::lang::dmi::StdDispatch
{
  ACDK_WITH_METAINFO(Object)
private:
  
  
public:
  static RObject create_instance();
  virtual RClass getClass();
#ifdef ACDK_NOMETAINFO
  
  static RClass GetClass() { return Nil; }
  foreign virtual const ::acdk::lang::dmi::ClazzMethodInfo* standardDispatch(IN(acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo = 0, const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0) { return 0; }
  foreign static const ::acdk::lang::dmi::ClazzMethodInfo* StandardDispatch(IN(acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo = 0, const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0) { return 0; }
#endif

public:
  /// copy constructor 
  foreign Object(const Object&);
  /// the default and only constructor
  Object();


  
public:
  /** 
    Java has no destructor, C++ does.<br>
      There is also a virtual void finalize(), which will 
      be called by the destructor, but only for compatiblity reasons. 
  */
  foreign virtual ~Object();
public:

   /// implemented from InterfaceBase
  foreign Object* _getObjectPtr() { return this; }
  
   /// implemented of StdDispatch
  foreign Object* getDmiTarget(bool& forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci) { forwarded = false; return this; }
  /** 
    overwrite this, if strip or add user defined fields.<br>
      Fields are used for serialisation and related services. 
  */
  foreign virtual dmi::SysFields& getImplFields(dmi::SysFields& fields);
  /**
    @see serialized_clone
  */
  virtual RObject clone();
  /**
    @see clone
  */
  foreign virtual RObject clone(sys::Allocator* alloc);
  /**
    Normally check for equality (not identity)
    Object::equals() is true only if both objects are the same 
    instance.
    @see serialized_equals()
  */
  virtual bool equals(IN(RObject) o);
  /**
    @see serialized_hashCode
  */
  virtual int hashCode() 
  { 
    return ACDK_CAST_PTR2INT(this);
  }
  
  /**
    Uses internal metainfo of classes
    to duplicate the instance calling clone()
    for each member.
    This makes a flat copy of an existant class.
    The class must provide a static create_instance()
    method or a default constructor.
    @param deep if true it will call clone() on each
                Object member. Otherwise only the reference
                will be copied.
    @param deepserialized will only be used if deep is true
           if true does not use clone() for member clones, but
           deepserialized() itself
    @note Use this method only on classes, which doesn't have
    cyclic references. Otherwhise this method will hang.

  */
  RObject serialized_clone(bool deep, bool deepserialized = true);
  
  /**
    Uses internal metainfo of classes
    to check equals() on each element.
    @param recursive_serialized call recursiv serialized_equals()
                                for the members instead of equals()
  */
  bool serialized_equals(IN(RObject) o, bool recursive_serialized);
  
  /**
    Uses internal metainfo of classes
    to call on each member hashCode()
    Use this method only on classes, which doesn't have
    cyclic references. Otherwhise this method will hang.
    @param recursive_serialized call recursiv serialized_hashCode()
                                for the members instead of hashCode()
  */
  int serialized_hashCode(bool recursive_serialized);
  /**
    return a string representation of this object
    Object::toString() return getName() by default
  */
  virtual RString toString();
  /** 
    returns the ClassName for this object by default 
  */
  foreign virtual RString getName();
  /**
    compares t objects 
    Object::compareTo() UnsupportedOperationException
    @return 0 if equal 1 if this object is greater, -1 if ohter
    @throw UnsupportedOperationException
  */
  virtual int compareTo(IN(RObject) o);

  /**
    This is for java-compatibility. 
    For real clean using Destructor is a better way.
    But different to destructors inside finalize() 
    calls to virtual methods will still be dispatched to
    derived classes.
  */
  virtual void finalize();
  /**
    Send signal to first objects waiting on this object.
  */
  ACDK_METHODATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy)) 
  virtual void notify();

  /**
    Send signal to all objects waiting on this object.
  */
  ACDK_METHODATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy)) 
  virtual void notifyAll();
  /**
    Wait on this synchronized object
    @param timeoutms timeout in milli seconds. if timeoutms = 0 wait for ever
    @param timoutus timeout in microseconds. if timeoutus = 0 wait for ever
  */
  ACDK_METHODATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy)) 
  virtual void wait(int timeoutms = 0, int timeoutus = 0);

  
  /**
    Standard implementation for serialization.
    See also: gw_ref[acdk_hb_mi_serialization].
    API: JDK / modified
  */
  foreign virtual void writeObject(IN(acdk::io::RObjectWriter) out, IN(RClass) cls);
  /**
    Standard implementation for serialization.
    See also: gw_ref[acdk_hb_mi_serialization].
    API: JDK / modified
  */
  foreign virtual void readObject(IN(acdk::io::RObjectReader) in, IN(RClass) cls);
  
  
  /**
    Standard implementation for serialization.
    API: JDK / modified
  */
  foreign virtual RObject writeReplace() { return this; }
  /**
    Standard implementation for serialization.
    API: JDK / modified
  */
  foreign virtual RObject readResolve() { return this; }
  
};

/**
  Check if obj is an instance of type T
  @ingroup acdkkeywords
*/
#define instanceof(obj, T) (dynamic_cast<const T*>(obj.impl()) || dynamic_cast<const T*>(obj.iptr()))
/**
  alias to instanceof
  @ingroup acdkkeywords
*/
#define instanceOf(obj, T) instanceof(obj, T)

/** @internal */
#define Object_defined


} // lang
} // acdk

#include "sys/RefHolder3Inl.h"

#endif //acdk_lang_Object_h

