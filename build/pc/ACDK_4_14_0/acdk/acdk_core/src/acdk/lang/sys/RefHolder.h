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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/RefHolder.h,v 1.71 2005/04/25 13:19:34 kommer Exp $

#ifndef acdk_lang_sys_RefHolder_h
#define acdk_lang_sys_RefHolder_h

#include <stddef.h>

#include "sys.h"
#include "core_vector.h"
#ifdef ACDK_USE_EXT_REFERER
#include "RefHeap.h"
#endif

/** 
  use implements to derived from interfaces or
    extending an interface 
  @ingroup acdkkeywords
*/
#define implements virtual public
/** 
  use extends to derived normal classes
  You MUST NOT use implements (virtual) in this case
  otherwise Memory-Management will not work
  @ingroup acdkkeywords
*/
#define extends public


using namespace ::acdk::lang;

template <class T> class RefHolder;
template <class T> class ObjectArrayImpl;
//template <class T> class RObjectArrayImpl;

#ifdef _MSC_VER
#pragma warning (disable : 4786) // template to long regarding debugging
#endif 

/** 
  Represents a 'null' Reference (equivalent to a null pointer)
  @ingroup acdkkeywords
*/
enum NilRef
{
  Nil = 0
};


/** this function is only hack to solve forward declaration problems */
namespace acdk {
  namespace lang {

    class InterfaceBase;
    class ObjectBase;
    class Object;
    
    typedef ::RefHolder< ::acdk::lang::Object> RObject;
    class String;
    ACDK_CORE_PUBLIC String* createNewString(const char* str);
    
    namespace sys {
      class Allocator;
    }
    namespace dmi {
      class ClazzInfo;
      class ScriptVar;
    }
  }
}

namespace {

#if !defined(DOXYGENONLY)

/** @internal */ 
template <class Base, class Candidate>
class DeriveChecker
{
  typedef char (&no)[1];
  typedef char (&yes)[2];
  
  struct Meth
  {
    static yes Test(Base*);
    static no Test(...);
  };
public:
  enum { 
    CandidateIsDerivedFromBase = 
      (sizeof(Meth::Test((Candidate*)0)) == sizeof(yes))
  };
};
#endif //!defined(DOXYGENONLY)

} // anon namespace

#if !defined(DOXYGENONLY)

/** @internal */
template <class Base, class Derived>
inline
bool is_base_of(const Derived& o)
{
  return DeriveChecker<Base, Derived>::CandidateIsDerivedFromBase;
}

/** @internal */
template <class ToT, class FromT>
inline
ToT object_cast(const FromT& f)
{
  if (is_base_of<ToT>(f))
    return static_cast<ToT>(f);
  return dynamic_cast<ToT>(f);
}

/** @internal */
template <class ToT, class FromT>
inline
ToT interface_cast(const FromT& f)
{
   if (is_base_of<ToT>(f))
    return static_cast<ToT>(f);
  return dynamic_cast<ToT>(f);
}

/** @internal */
#define REFH_TRYDOWNCAST_OBJ(ret, T, oiptr) \
  if (is_base_of<T>(*(oiptr))) \
    ret = static_cast<T*>(oiptr); \
  else \
    ret = dynamic_cast<T*>(oiptr);


#ifndef ACDK_NO_NULLPOINTER_CHECKING 
/** @internal */
#  define ACDK_ASSERT_NULL_PTR(ptr) \
      if (ptr == 0) ::acdk::lang::ObjectBase::_throwNullPointerException();
#else
#  define ACDK_ASSERT_NULL_PTR(ptr) 
#endif
#endif //!defined(DOXYGENONLY)

/**
  Helper method to throw a cast exception
  @throw ClassCastException
  @param clazz target type
  @param iface source type
*/
ACDK_CORE_PUBLIC void badCast(const ::acdk::lang::dmi::ClazzInfo* clazz, ::acdk::lang::ObjectBase* iface);

/**
  Helper method to throw a cast exception
  @throw ClassCastException
  @param clazz target type
  @param iface source type
*/
ACDK_CORE_PUBLIC void badBasicArrayCast(const ::acdk::lang::dmi::ClazzInfo* clazz, ::acdk::lang::ObjectBase* iface);

/**
  Helper method to throw a cast exception
  @throw ClassCastException
  @param clazz target type
  @param iface source type
*/
ACDK_CORE_PUBLIC void badObjectArrayCast(const ::acdk::lang::dmi::ClazzInfo* clazz, ::acdk::lang::ObjectBase* iface);


#if !defined(DOXYGENONLY)

/** @internal */
inline void* acdk_allocate(size_t size);
/** @internal */
inline void acdk_deallocate(void* ptr);
/** @internal */
inline void* acdk_allocate(size_t size, ::acdk::lang::sys::Allocator* allocator);
/** @internal */
inline void acdk_deallocate(void* ptr, ::acdk::lang::sys::Allocator* allocator);

template <class I> class InterfaceHolder;



//#define USE_REFHOLDER1_H
//#include "RefHolder1.h"
//#include "RefHolder2.h" not working
#include "RefHolder3.h"
//#include "RefHolder4.h"
//#include "RefHolder5.h"


template <class T>
inline
bool operator==(NilRef nil, const RefHolder<T>& o)
{
  return o.iptr() == 0;
}

template <class T>
inline
bool operator!=(NilRef nil, const RefHolder<T>& o)
{
  return o.iptr() != 0;
}

template <class T>
inline
bool operator==(NilRef nil, const InterfaceHolder<T>& o)
{
  return o.iptr() == 0;
}

template <class T>
inline
bool operator!=(NilRef nil, const InterfaceHolder<T>& o)
{
  return o.iptr() != 0;
}



template <class T>
inline 
bool equalsObjects(const RefHolder<T>& f, const RefHolder<T>& s)
{
  if (f == Nil && s == Nil)
    return true;
  if (f == Nil || s == Nil)
    return false;
  return s->equals(f);
}
#endif //!defined(DOXYGENONLY)

/**
  Macro to declare a ACDK class
  It defines following types:
  - ClassName
  - RClassName
  - ClassNameArray
  - RClassNameArray
  @ingroup acdkmacros
  @ingroup acdksmartptr
*/
#define ACDK_DECL_CLASS(ClassName) \
class ClassName; \
typedef ::RefHolder<ClassName> R##ClassName; \
typedef ::ObjectArrayImpl<R##ClassName> ClassName##Array; \
typedef ::RObjectArrayImpl<R##ClassName> R##ClassName##Array




/**
  Macro to declare a ACDK interface
  It defines following types:
  - ClassName
  - RClassName
  - ClassNameArray
  - RClassNameArray
  @ingroup acdkmacros
  @ingroup acdksmartptr
*/
#define ACDK_DECL_INTERFACE(InterfaceName) \
class InterfaceName; \
typedef ::InterfaceHolder<InterfaceName> R##InterfaceName; \
typedef ::ObjectArrayImpl<R##InterfaceName> InterfaceName##Array; \
typedef ::RObjectArrayImpl<R##InterfaceName> R##InterfaceName##Array



/**
  Macro to declare a ACDK exception type
  @param ClassName name of the exception type
  @param SuperName name of the derived exception type
  It defines following types:
  - ClassName
  - RClassName
  - ClassNameArray
  - RClassNameArray
  @ingroup acdkmacros
  @ingroup acdksmartptr
  @see ACDK_DECL_THROWABLE_FQ
*/
#define ACDK_DECL_THROWABLE(ClassName, SuperName) \
class ClassName; \
typedef ::ThrowableHolder<ClassName, R##SuperName> R##ClassName; \
typedef ::ObjectArrayImpl<R##ClassName> ClassName##Array; \
typedef ::RObjectArrayImpl<R##ClassName> R##ClassName##Array

/**
  Macro to declare a ACDK exception type
  @param ClassName name of the exception type
  @param sns namespace of the derived exception type
  @param SuperName name of the derived exception type
  It defines following types:
  - ClassName
  - RClassName
  - ClassNameArray
  - RClassNameArray
  @ingroup acdkmacros
  @ingroup acdksmartptr
*/
#define ACDK_DECL_THROWABLE_FQ(ClassName, sns, SuperName) \
class ClassName; \
typedef ::ThrowableHolder<ClassName, sns R##SuperName> R##ClassName; \
typedef ::ObjectArrayImpl<R##ClassName> ClassName##Array; \
typedef ::RObjectArrayImpl<R##ClassName> R##ClassName##Array


/**
  Macro to declare a ACDK class Array.
  This is usefull to declare Arrays from Arrays
  It defines following types:
  - ClassNameArray
  - RClassNameArray
  @ingroup acdkmacros
  @ingroup acdksmartptr
*/
#define ACDK_DECL_CLASS_ARRAY(ClassName) \
typedef ::ObjectArrayImpl<R##ClassName> ClassName##Array; \
typedef ::RObjectArrayImpl<R##ClassName> R##ClassName##Array

/**
  alias to ACDK_DECL_CLASS_ARRAY 
  @ingroup acdkmacros
*/
#define ACDK_DECL_ARRAY(ClassName) ACDK_DECL_CLASS_ARRAY(ClassName)

/** 
  used to import the given ACDK-Class into current namespace 
  imports following type names:
  - ClassName
  - RClassName
  - ClassNameArray
  - RClassNameArray

  @param Namespace the namspace of the class
  @param ClassName the class name
  @ingroup acdkmacros
  @ingroup acdksmartptr
*/
#define USING_CLASS(Namespace, ClassName) \
  using Namespace ClassName;\
  using Namespace R##ClassName; \
  using Namespace ClassName##Array; \
  using Namespace R##ClassName##Array

/**
  use to define meta info for enumerations which are declared and used inside a executable or static library
  @ingroup acdkmacros
*/
#define ACDK_DEF_ENUM(EnumName) \
class EnumName##MetaInf \
{ \
public: \
  static ::acdk::lang::dmi::ClazzEnumInfo* GetEnumInfo(); \
}

/**
  use to define meta info for enumerations which are declared and used inside shared library
  @ingroup acdkmacros
*/

#define ACDK_DEF_LIB_ENUM(export, EnumName) \
class export EnumName##MetaInf \
{ \
public: \
  static ::acdk::lang::dmi::ClazzEnumInfo* GetEnumInfo(); \
}

/**
  container to collect references of fields
  @internal
*/
typedef acdk::lang::sys::core_vector<RObject*> FieldReferences;


#ifndef ACDK_NOMETAINFO

/**
  Use this to delcare a class as ACDK-Class with available Metainformatione.
  You can use mmmc to generate Metainformation for a given class or package.
  @ingroup acdkmacros
  @ingroup acdkmetainfo
*/

#define ACDK_WITH_METAINFO(ClassName) \
public: \
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return clazzInfo(); } \
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo(); \
  static ::acdk::lang::RClass GetClass(); \
  virtual void getCollectableFields(FieldReferences& fields); \
  inline static void* _castToInterfacePtr(::acdk::lang::Object* optr) { return reinterpret_cast<void*>(dynamic_cast<ClassName*>(optr)); } \
private:   \
  friend struct ClassName##_MetainfoWrapper;

/**
  This version is similar to ACDK_WITH_METAINFO, but
  the methods declarations has to be written into the class.
  @ingroup acdkmacros
  @ingroup acdkmetainfo
*/
#define ACDK_WITH_METAINFO2(ClassName) \
private: \
  friend struct ClassName##_MetainfoWrapper;



/**
  Produces only information needed
  for the Garbage collector, but no further Metainfo
  @ingroup acdkmacros
  @ingroup acdkmetainfo
*/
#define ACDK_WITH_MI_GCINFO \
public: \
  virtual void getCollectableFields(FieldReferences& fields); \
private: \

/**
  Produces only information about member
  no DMI interface or information about functions
  @ingroup acdkmacros
  @ingroup acdkmetainfo
  */
#define ACDK_WITH_MI_MEMBER(ClassName)  \
public: \
  virtual void getCollectableFields(FieldReferences& fields); \
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return clazzInfo(); } \
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo(); \
  static ::acdk::lang::RClass GetClass(); \
  static void* _castToInterfacePtr(::acdk::lang::Object* optr) { return reinterpret_cast<void*>(dynamic_cast<ClassName*>(optr)); } \
private: \
  friend struct ClassName##_MetainfoWrapper;



#else //ACDK_NOMETAINFO
#define ACDK_WITH_METAINFO(ClassName)
#define ACDK_WITH_MI_GCINFO
#endif //ACDK_NOMETAINFO


#ifndef ACDK_NOMETAINFO
/**
  If you don't want to use Metainformation, you can use this
  macro. The purpose is for debugging only, otherwise not recommended!
  @ingroup acdkmacros
  @ingroup acdkmetainfo
*/

#define DECL_ACDK_DUMMYMETACLASS \
  public: \
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo() { return 0; } \
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return 0; } \
  static ::acdk::lang::RClass GetClass() { return Nil; } \
  virtual void getCollectableFields(FieldReferences& fields) { } \
  



/**
  A given ACDK should not implement its own meta info, but just use
  the metainfo from a given super class.

  @ingroup acdkmacros
  @ingroup acdkmetainfo
*/
#define DECL_ACDK_DEFAULT_METACLASS(Super) \
  public: \
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo() { return Super::getClazzInfo(); } \
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return Super::clazzInfo(); } \
  static ::acdk::lang::RClass GetClass() { return Super::GetClass(); } \
  virtual void getCollectableFields(FieldReferences& fields) { Super::getCollectableFields(fields); } \
  

#else //ACDK_NOMETAINFO
#define DECL_ACDK_DUMMYMETACLASS
#define DECL_ACDK_DEFAULT_METACLASS(Super)
#endif //ACDK_NOMETAINFO

/** 
  include this token at top in an header, which doesn't contain any
  meta info related declarations
  The acdkmc doesn't try to parse the header any more.
  @ingroup acdkmacros
  @ingroup acdkmetainfo
*/
#define ACDK_NO_METAINFO_HEADER

/** 
  used to tag a class as a ACDK-class.
  Normally you should not need this macro
  @ingroup acdkmacros
  @ingroup acdkmetainfo
*/
#define ACDK_CLASS

/** 
  used to tag a class as a ACDK-Interface.
  Normally you should not need this macro
  @ingroup acdkmacros
  @ingroup acdkmetainfo
*/
#define ACDK_INTERFACE

/** 
  used to tag a field as not serializable, cloneable, etc 
  You can also use the keyword mutable
  @ingroup acdkmacros
  @ingroup acdkmetainfo
*/
#define ACDK_MUTABLE

/** 
  used to tag a field as not serializable
  @ingroup acdkmetainfo
  @ingroup acdkkeywords
*/
#define transient

/** 
  use to tag classes, methods or arguments as
  not compatible with ACDK.
  For theses classes/methods no metainformation will be created
  @ingroup acdkmetainfo
  @ingroup acdkkeywords
*/
#define foreign 

/**
  use to tag classes or methods not to generate
  dmi proxy for this class/method
  @ingroup acdkmetainfo
  @ingroup acdkkeywords
*/
#define final

/**
  ACDK_DECL_ENUM is only used as hint for acdkmc to
  detect enumeration types, which are declared in 
  another header
  @ingroup acdkmetainfo
*/
#define ACDK_DECL_ENUM(EnumType) 
/**
  Same as ACDK_DECL_ENUM but allows to fully qualify
  the enumeration type.
  another header
  @ingroup acdkmetainfo
*/
#define ACDK_DECL_ENUM_FQ(NameSpace, EnumType) 

/**
  methods, tagged as overwrite will generate
  DmiProxy methods, but no metainfo, because the metainfo
  from a base class can be used.
  @ingroup acdkmetainfo
  @ingroup acdkkeywords
*/
#define overwrite virtual

#ifndef upcast_explicit
# if _MSC_VER >= 1300
#   define upcast_explicit explicit
# else
#   define upcast_explicit 
# endif
#endif


/** 
  Convert Object allocated on Stack or static into a RefHolder.
  Prevents from deleting, if Reference count is 0
  @ingroup acdksmartptr
*/
template <class RT, class T>
RT asStackRef(const T& obj) 
{
  obj.setStackRef(true);
  return RT(const_cast<T*>(&obj));
}

/** 
  SR(Type, object) is just a more brief macro for the template:
  template <class RT, class T> RT asStackRef(const T& obj) 
  @ingroup acdksmartptr
*/
#define SR(Type, obj) asStackRef<R##Type, Type>(obj)
/**
  fully qualified version of macro SR(Type, obj)
  @ingroup acdksmartptr
*/
#define SR_FQ(ns, Type, obj) asStackRef<ns R##Type, ns Type>(obj)

/**  
  A Proxy for synchronized access to Object.
  operator->() return pointer to Object
  @ingroup acdksmartptr
*/
template <class T>
class LockedProxy
{
  T* _obj;
public:
  LockedProxy(T* obj)
  : _obj(obj)
  {
    _obj->lock();
  }
  ~LockedProxy()
  {
    _obj->unlock();
  }
  T* operator->() { return _obj; }
  const T* operator->() const { return _obj; }
};


#ifdef IN
# undef IN
#endif

#ifdef OUT
# undef OUT
#endif
#ifdef INOUT
# undef INOUT
#endif


/**
  Tag an argument for sending the Object by Reference from Client to Server.
  Type can be a type of RObject or supported basic type.
  @ingroup acdkkeywords
*/
#define IN(type) const type &
/** 
  Same as macro IN(type)
  @ingroup acdkkeywords
*/
#define INP(type) const type &

/**
  Tag an argument as output parameter .
  Type can be a type of RObject or supported basic type.
  @ingroup acdkkeywords
*/
#define OUT(type) type&
/** 
  Same as macro OUT(type)
  @ingroup acdkkeywords
*/
#define OUTP(type) type&

/**
  Tag an argument for send/receive it.
  Type can be a type of RObject or supported basic type.
  @ingroup acdkkeywords
*/
#define INOUT(type) type&
/**
  Same as INOUT(type)
  @ingroup acdkkeywords
*/
#define INOUTP(type) type&

/**
  Used for performance improvemnts for return values.
  return a reference of val, not a copy
  (Must be non const, otherwise operator-> doesn't work)
  @ingroup acdkkeywords
*/
#define RETOUT(val) OUT(val)
/** 
  Alias to RETOUT(val)
  @ingroup acdkkeywords
*/
#define OUTRET(val) OUT(val)


/**
  Transfer parameter by value from caller to callee.
  These defined are only used as hints to acdkmc in case of remoting
  @ingroup acdkkeywords
*/
#define BYVAL(T) IN(T)
/**
  Transfer parameter by value from caller to callee.
  these defined are only used as hints to acdkmc in case of remoting
  @ingroup acdkkeywords
*/
#define BYVALIN(T) IN(T)  //::acdk::lang::dmi::TSendMarshaler<T>
/**
  Transfer parameter by value from calee to caller.
  these defined are only used as hints to acdkmc in case of remoting
  @ingroup acdkkeywords
*/
#define BYVALOUT(T) OUT(T) //old  ::acdk::lang::dmi::TReceiveMarshaler<T>
/**
  Transfer parameter by value from caller to callee and back.
  these defined are only used as hints to acdkmc in case of remoting
  @ingroup acdkkeywords
*/
#define BYVALINOUT(T) INOUT(T) //old ::acdk::lang::dmi::TSendReceiveMarshaler<T>
/**
  Transfer parameter by reference from caller to callee.
  these defined are only used as hints to acdkmc in case of remoting
  @ingroup acdkkeywords
*/
#define BYREF(T) IN(T)
/**
  Transfer parameter by reference from caller to callee.
  these defined are only used as hints to acdkmc in case of remoting
  @ingroup acdkkeywords
*/
#define BYREFIN(T) IN(T)
/**
  Transfer parameter by reference from callee to caller.
  these defined are only used as hints to acdkmc in case of remoting
  @ingroup acdkkeywords
*/
#define BYREFOUT(T) OUT(T)
/**
  Transfer parameter by reference from caller to callee and back.
  these defined are only used as hints to acdkmc in case of remoting
  @ingroup acdkkeywords
*/
#define BYREFINOUT(T) INOUT(T)

#endif // acdk_lang_sys_RefHolder_h

