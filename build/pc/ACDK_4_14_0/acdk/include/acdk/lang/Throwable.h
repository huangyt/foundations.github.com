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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Throwable.h,v 1.28 2005/04/09 19:26:51 kommer Exp $

#ifndef acdk_lang_Throwable_h
#define acdk_lang_Throwable_h


#include "String.h"
#include "ObjectArrayBase.h"
#include "InOutPreDeclaration.h"

#include "StackFrame.h"
#include "sys/BackTrace.h"


namespace acdk {
namespace lang {

ACDK_DECL_CLASS(StackTraceElement);



ACDK_DECL_THROWABLE(Throwable, Object);


ACDK_DECL_INTERFACE(ThrowListener);
/**
  listen on exception before they are thrown via the THROWx macros.
  This is for example usefull Debugger implementations, which want
  to create breakpoints on thrown exceptions.
*/
class ACDK_CORE_PUBLIC ThrowListener
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ThrowListener)
public:
  /**
    before an exception will be thrown, the listener will be called
    if one of these listener returns false, the exception will not be thrown
  */
  virtual bool onThrow(IN(RThrowable) ex, int line, IN(RString) file) = 0;
};

/**
  Throwable is the root of all exceptions in the framework.
  There is also a mayor different regarding Memory Management 
  of exceptions. You cannot throw R[exceptionname], because otherwise
  structured catches will not work.
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.28 $
  @date $Date: 2005/04/09 19:26:51 $
*/
class ACDK_CORE_PUBLIC Throwable 
: extends ::acdk::lang::Object
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Throwable)
protected:
  RString _what;
  RThrowable _cause;
public:
  /// native C++ Backtrace information
  foreign ::acdk::lang::sys::BackTrace _stackFrame;
private:
  /**
    this is Nil until getStackTrace will be called
  */
  RStackFrameArray _stackFrames;
public:

  /**
    @internal 
  */
  static RObject create_instance() { return new Throwable(); }
  /** 
    Default constructor
  */
  Throwable();
  /**
    Constructor with given string describing the reason
  */
  Throwable(IN(RString) what, IN(RThrowable) cause = Nil);
  Throwable(IN(RThrowable) cause);
  foreign virtual ~Throwable();

  /**
    @return description/reason of this exception 
  */
  virtual RString getMessage();
  virtual RThrowable getCause();
  virtual RThrowable initCause(IN(RThrowable) cause);
  /**
    print call stack to given writer
  */
  virtual void printStackTrace(IN(acdk::io::RPrintWriter) out);
  /**
    print call stack to System::err
  */
  virtual void printStackTrace();
  /**
    return stackTrace
  */
  virtual RStackFrameArray getStackFrames();
  foreign bool equals(IN(RThrowable) obj)
  {
    return _what->equals(obj->_what);
  }
  foreign bool equals(IN(RObject) obj)
  {
    if (obj == Nil)
      return false;
    if (instanceof(obj, Throwable) == false)
      return false;
    return equals(RThrowable(obj));
  }
  virtual void writeObject(IN(acdk::io::RObjectWriter) out, IN(RClass) cls);

  /**
    @internal DMI
    This method throws the Exception using the function register
    as __throwExceptionFunc in MetaAttribute of the current class
    @param onlyIfHasMeta throws only exception, if this concret class has meta info
  */
  void throwException(bool onlyIfHasMeta = false);

  static void registerThrowListener(IN(RThrowListener) listner);
  static void unregisterThrowListener(IN(RThrowListener) listner);
  foreign static bool onThrow(IN(RThrowable) ex, int line, const char* file);
protected:
};



#ifdef _MSC_VER
#  pragma warning( disable : 4127) //Bedingter Ausdruck ist konstant while(false)
#endif //_MSC_VER


/** 
  to avoid too lengthy in the form:
  
  throw RUsallyNotVeryShortExceptionName(new UsallyNotVeryShortExceptionName(new String("blabla")));
  
  use the form:

  THROW1(UsallyNotVeryShortExceptionName, new String("blalba"));
  @ingroup acdkkeywords
*/

#define THROW0(ClassName) \
do { \
  ::acdk::lang::RString __tstr = new ::acdk::lang::String(::acdk::lang::RString(#ClassName) \
    + ". thrown in "  __FILE__  ":"  \
    + ::acdk::lang::String::valueOf(__LINE__)); \
  R##ClassName ex__(new ClassName(__tstr)); \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex__), __LINE__, __FILE__) == true) \
    throw ex__; \
} while (false)

/**
  Version of THROW0 with namespace
  @see THROW0
  @ingroup acdkkeywords
*/
#define THROW0_FQ(ns, ClassName) \
do { \
  ::acdk::lang::RString __tstr = new ::acdk::lang::String(::acdk::lang::RString(#ns#ClassName) \
    + ". thrown in "  __FILE__  ":"  \
    + ::acdk::lang::String::valueOf(__LINE__)); \
  ns R##ClassName ex__(new ns ClassName(__tstr)); \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex__), __LINE__, __FILE__) == true) \
    throw ex__; \
} while (false)



/**
  @see THROW0
  @ingroup acdkkeywords
*/
#define THROW1(ClassName, arg1) \
do { \
  R##ClassName ex__(new ClassName(arg1)); \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex__), __LINE__, __FILE__) == true) \
    throw ex__; \
} while (false)

/**
  @see THROW0
  @ingroup acdkkeywords
*/
#define THROW1_FQ(ns, ClassName, arg1) \
do { \
  ns R##ClassName ex__(new ns ClassName(arg1)); \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex__), __LINE__, __FILE__) == true) \
    throw ex__; \
} while (false)

/**
  @see THROW0
  @ingroup acdkkeywords
*/
#define THROW2(ClassName, arg1, arg2) \
do { \
  R##ClassName ex__(new ClassName(arg1, arg2)); \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex__), __LINE__, __FILE__) == true) \
    throw ex__; \
} while (false)

/**
  @see THROW0
  @ingroup acdkkeywords
*/
#define THROW2_FQ(ns, ClassName, arg1, arg2) \
do { \
  ns R##ClassName ex__(new ns ClassName(arg1, arg2)); \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex__), __LINE__, __FILE__) == true) \
    throw ex__; \
} while (false)


/**
  @see THROW0
  @ingroup acdkkeywords
*/
#define THROW3(ClassName, arg1, arg2, arg3) \
do { \
  R##ClassName ex__(new ClassName(arg1, arg2, arg3)); \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex__), __LINE__, __FILE__) == true) \
    throw ex__; \
} while (false)

/**
  @see THROW0
  @ingroup acdkkeywords
*/
#define THROW3_FQ(ns, ClassName, arg1, arg2, arg3) \
do { \
  ns R##ClassName ex__(new ns ClassName(arg1, arg2, arg3)); \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex__), __LINE__, __FILE__) == true) \
    throw ex__; \
} while (false)

/**
  args is a argument list with round brackets.
  use this following way
  THROWX(MyThrowable, (1, 2, "asdf", 4));
  @ingroup acdkkeywords
*/
#define THROWX(ClassName, args) \
do { \
  R##ClassName ex__(new ClassName##args); \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex__), __LINE__, __FILE__) == true) \
    throw ex__; \
} while (false)

/**
  THROWX with namespace
  @ingroup acdkkeywords
*/
#define THROWX_FQ(ns, ClassName, args) \
do { \
  ns R##ClassName ex__(new ns ClassName##args); \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex__), __LINE__, __FILE__) == true) \
    throw ex__; \
} while (false)


/**
 use this template to re-throw an exception instance.
 The THROW_INSTANCE() only works, if the exceptions
 has extended meta info
 @code
  try {
    try {
      THROW0(NoSuchMethodException);
    } catch (RException ex) {
      THROW_INSTANCE(ex); // this should throw RNoSuchMethodException internally
    } 
  } catch (RNoSuchMethodException ex) {
    // reach here
  } catch (RException ex) {
    testAssertComment(false, "structured throwing via THROW_INSTANCE failed");
  }
 @endcode
*/
template <class ThrowableHolderType>
void throwThrowableInstance(const ThrowableHolderType& ex) 
{
  typedef typename ThrowableHolderType::Type RefExType;
  //const type_info& reft = typeid(RefExType);
  //const type_info& instt = typeid(*ex.iptr());
  if (typeid(RefExType) == typeid(*ex.iptr()))
    throw ex; 
  ex->throwException(true); 
  throw ex; 
}
/**
  if you have also an RThrowable reference use this macro to throw
  @code
  try {
    RThrowable ex = ...;
    throw ex;           // this will catched by RThrowable 
                        // independed by real exception type
    THROW_INSTANCE(ex); // this uses Metainfo to throw the correct type
                        // and may also be catch by RMyException
  } catch (RMyException ex) {
  } catch (RThrowable ex) {
  }
  @endcode
  @ingroup acdkkeywords
*/
#define THROW_INSTANCE(ex) \
do { \
  if (::acdk::lang::Throwable::onThrow(::acdk::lang::RThrowable(ex), __LINE__, __FILE__) == true) \
    throwThrowableInstance(ex); \
} while (false)

} // lang
} // acdk

#endif //acdk_lang_Throwable_h

