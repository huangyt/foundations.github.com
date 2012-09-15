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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/Compiler.h,v 1.35 2005/04/26 12:52:47 kommer Exp $

#ifndef acdk_Compiler_h
#define acdk_Compiler_h

#include "Platform.h"

#if defined(DOXYGENONLY) // documentation only
// general compiler flags
/**
  ACDK_SUPPORT_ANSI_SPECIALIZATION use
  template <> void foo(char c)
  instead of
  template <char> void foo(char c)
  @ingroup acdkplatformmacros
*/
#define ACDK_SUPPORT_ANSI_SPECIALIZATION

/**
  and,or,xor, etc. are reserved token
  @ingroup acdkplatformmacros
*/
#define ACDK_HAS_ALTERNATIVE_TOKEN

/**
  is defined if compiler needs explicite type in function templates
  @code
  template <class T> void foo()
  {
    T dummy;
#ifdef ACDK_ALT_CALL_TEMPLATED_FUNCTION
    anotherMemberTemplate<T>(dummy);
#else
    anotherMemberTemplate(dummy);
#endif
  }
  @endcode
  @ingroup acdkplatformmacros
*/
#define ACDK_ALT_CALL_TEMPLATED_FUNCTION

/**
  If reference to super class needs fully qualified name
  @code
  namspace somewhere {
  class A
  {
    void foo() { }
  };
  }
  class B : public somewhere::A
  {
    void foo()
    {
#ifdef ACDK_NEED_FQ_SUPER_QUALIFIER
      somewhere::A::foo();
#else
      A::foo();
#endif
    }
  };
  @endcode
  @ingroup acdkplatformmacros
*/
#define ACDK_NEED_FQ_SUPER_QUALIFIER

/**
  See code example.
@code
template <class A>
struct X
{
  template <class B> void foo(B& b);
};

#if defined(ACDK_MEMBERTEMPL_TEMPLATE_TEMPLATE)
template <class A> template <class B>
#else
template <class A, class B>
#endif
void X<A>::foo(B& b) {}
@endcode
@ingroup acdkplatformmacros
*/
#define ACDK_MEMBERTEMPL_TEMPLATE_TEMPLATE
/**
  
  template_static is used to mark a template member function
  as static.
  @ingroup acdkplatformmacros
*/
#define template_static static

/**
  is defined if null pointer checking will
  be done by structured C exception or signal handling
  @ingroup acdkplatformmacros
*/
#define ACDK_NO_NULLPOINTER_CHECKING


/**
  is defined if platform need to export shared library symbols
  (Windows)
  @ingroup acdkplatformmacros
*/
#define ACDK_NEED_DLLEXPORT


/**
  is defined if compiler supports user defined delete operator
  @ingroup acdkplatformmacros
*/
#define ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE


/**
  is defined if compiler include has struct timeval
  @ingroup acdkplatformmacros
*/
#define ACDK_HAS_STRUCT_TIMEVAL


/**
  is defined if compiler has long long type
  @ingroup acdkplatformmacros
*/
#define ACDK_HAVE_LONG_LONG

/**
  Used to export a member/method/class from a DLL/SO
  @ingroup acdkplatformmacros
*/
#define ACDK_DLL_EXPORT __declspec(dllexport)
/**
  Used to import a member/method/class from a DLL/SO
  @ingroup acdkplatformmacros
*/
# define ACDK_DLL_IMPORT __declspec(dllimport)
/**
  Used to hide symbol from exported symbols (private to DLL/SO).
  This will probably be supported by gcc 4.x
  @ingroup acdkplatformmacros
*/
# define ACDK_DLL_PRIVATE

#endif //defined(DOXYGENONLY) documentation only

/**
    Used to add throwing declarations to methods
    for example:
    @code
    void foo() THROW1(RMyException)
    @endcode
    with will be expanded to
    @code 
      void foo() throw(RMyException, ::acdk::lang::RThrowable)
    @endcode 
    On some platform this macro expands to nothing (due buggy C++ implementation
  @ingroup acdkkeywords
  @ingroup acdkmacros
*/
#define THROWS1(ex) throw(ex, ::acdk::lang::RThrowable)
/**
  @see  THROWS1
  @ingroup acdkkeywords
  @ingroup acdkmacros
*/
#define THROWS2(ex1, ex2) throw(ex1, ex2, ::acdk::lang::RThrowable)
/**
  @see  THROWS1
  @ingroup acdkkeywords
  @ingroup acdkmacros
*/
#define THROWS3(ex1, ex2, ex3) throw(ex1, ex2, ex3, ::acdk::lang::RThrowable)
/**
  @see  THROWS1
  @ingroup acdkkeywords
  @ingroup acdkmacros
*/
#define THROWS4(ex1, ex2, ex3, ex4) throw(ex1, ex2, ex3, ex4, ::acdk::lang::RThrowable)


#if defined(OS_DARWIN) || defined(__BORLANDC__)
# undef THROWS1
# define THROWS1(ex)
# undef THROWS2
# define THROWS2(ex1, ex2)
# undef THROWS3
# define THROWS3(ex1, ex2, ex3)
# undef THROWS4
# define THROWS4(ex1, ex2, ex3, ex4)
#endif //defined(OS_DARWIN) || defined(__BORLANDC__)

// =================================================================
// Compiler Settings
// =================================================================

//
//  MS VC6  Compiler
//
#ifdef _MSC_VER
# pragma warning(disable: 4100)
// vc8 memcpy, etc. are not secure warnings
# define _CRT_SECURE_NO_DEPRECATE
# define WIN32_THREADS
# define ACDK_SUPPORT_ANSI_SPECIALIZATION
# define ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
# define template_static static
# define ACDK_NEED_DLLEXPORT

// use structured exception handling
# define ACDK_USE_MSC_STRUCTURED_C_HANDLING 1
# if defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
#   define ACDK_NO_NULLPOINTER_CHECKING 1
# endif //ACDK_USE_MSC_STRUCTURED_C_HANDLING
/**
   understand following:
   template <typename T> inline StringConcenator operator+(const char* s1, const T& t)
*/
# define ACDK_HAS_OPERATORS_ON_BASICS_ONLY

# if _MSC_VER >= 1300
/**
  does not understand following
  template <class OT>  explicit RefHolder(const RefHolder<OT>& other);
  RefHolder(const RefHolder<T>& other);

*/
#   define ACDK_NO_EXPLICIT_OVERLOADING_BUG 1
# endif //_MSC_VER >= 1300
# if _MSC_VER >= 1400 // VC8 aka Visual Studio 2005
# define ACDK_NEED_FQ_SUPER_QUALIFIER
# pragma warning(disable: 4290) // C++ exception specification ignored except to indicate a function is not __declspec(nothrow)
# pragma warning(disable: 4250) // 'acdk::lang::Integer' : inherits 'acdk::lang::Object::acdk::lang::Object::_getObjectPtr' via dominance
# pragma warning(disable: 4251) // 'acdk::util::BucketNode::_next' : class 'RefHolder<T>' needs to have dll-interface to be used by clients of class 'acdk::util::BucketNode'
# pragma warning(disable: 4996) // 'write' was declared deprecated
# endif

# define ACDK_DLL_EXPORT __declspec(dllexport)
# define ACDK_DLL_IMPORT __declspec(dllimport)
# define ACDK_DLL_PRIVATE 
#endif //_MSC_VER


/**
    is true if either not gcc or at least version Major.Minor
 */
#define ACDK_CHECK_GCC_VERSION(Major, Minor) \
  ((!(defined(__GNUC__)) || (__GNUC__ > Major) || ((__GNUC__ == Major) && (__GNUC_MINOR__ >= Minor))))


////////////////////////////////////////////////////////////////////////
//  Gnu-Compiler. egcs 1.1.2, gcc 2.95.1 - 3.4
//
#ifdef __GNUG__
# if !defined(__alpha__) && !defined(__ppc__) && !defined(__amd64__)
#  define ACDK_NO_SIZE_T 1
# endif
# ifdef _LONGLONG
#   define ACDK_INT64BIT
# endif
# define HAS_UNISTD_H 1
# define ACDK_HAS_STRUCT_TIMEVAL 1
# define ACDK_HAVE_LONG_LONG 1
# define ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
# define template_static static
// gcc 2.95.1 doesn't support it
# define ACDK_SUPPORT_ANSI_SPECIALIZATION

// gcc 3.0 has alternate token 'and' 'or' and so on
# define ACDK_HAS_ALTERNATIVE_TOKEN

# if ACDK_CHECK_GCC_VERSION(3, 4)
#   define ACDK_MEMBERTEMPL_TEMPLATE_TEMPLATE
# endif
// needed to get recursive mutex
# ifndef _GNU_SOURCE
#   define _GNU_SOURCE
# endif

# if defined(ACDK_OS_WIN32) || defined(ACDK_MINGW)
#   if !defined(ACDK_MINGW)
#     define ACDK_MINGW
#   endif
#   if !defined(ACDK_OS_WIN32)
#     define ACDK_OS_WIN32
#   endif
#  define ACDK_NEED_DLLEXPORT
#  define WIN32_THREADS
#  define ACDK_DLL_EXPORT __declspec(dllexport)
#  define ACDK_DLL_IMPORT __declspec(dllimport)
#  define ACDK_DLL_PRIVATE 
# else // defined(ACDK_OS_WIN32)
#  if ACDK_CHECK_GCC_VERSION(4, 0)
#   define ACDK_DLL_EXPORT __attribute__ ((visibility("default")))
//#   define ACDK_DLL_IMPORT __attribute__ ((visibility("hidden")))
#   define ACDK_DLL_IMPORT 
#   define ACDK_DLL_PRIVATE 
#  else
#   define ACDK_DLL_EXPORT 
#   define ACDK_DLL_IMPORT 
#   define ACDK_DLL_PRIVATE 
#  endif //ACDK_CHECK_GCC_VERSION(4, 0)
# endif  // defined(ACDK_OS_WIN32)
#endif //__GNUG__



////////////////////////////////////////////////////////////
// Borland C++-Builder 4/5
//
#ifdef __BORLANDC__
// Borland 5.51  __BORLANDC__ == 0x551
# ifndef __linux__
#   define WIN32_THREADS
#   define ACDK_NEED_DLLEXPORT
# else
#   define POSIX_THREADS
#   define ACDK_ATOMIC_USE_PTHREAD
# endif //__linux__
# define ACDK_SUPPORT_ANSI_SPECIALIZATION
# define ACDK_NO_ARRAY_DELETE
# define ACDK_NEED_FQ_SUPER_QUALIFIER
# if __BORLANDC__ >= 0x600
#   define template_static
# else
#   define template_static static
# endif //__BORLANDC__ >= 0x600

# define ACDK_DLL_EXPORT __declspec(dllexport)
# define ACDK_DLL_IMPORT __declspec(dllimport)
# define ACDK_DLL_PRIVATE 

# define ACDK_MEMBERTEMPL_TEMPLATE_TEMPLATE

/**
  borland sometime has problems with RThrowable and other Holder to
  Exceptions while linking. ACDK_BCC_RTHROWABLE_DEFINITION
  fixes this.
  @ingroup acdkplatformmacros
*/

# define ACDK_BCC_RTHROWABLE_DEFINITION(ClassName) \
namespace { \
void bcc##ClassName##Fix() \
{ \
  try { \
    throw R##ClassName(new ClassName()); \
  } catch (R##ClassName ex) { \
  } \
} \
}

# define ACDK_BCC_RTHROWABLE_DEFINITION_FQ(ns, ClassName) \
namespace { \
void bcc##ClassName##Fix() \
{ \
  try { \
    throw ns R##ClassName(new ns ClassName()); \
  } catch (ns R##ClassName ex) { \
  } \
} \
}

#else //__BORLANDC__

# define ACDK_BCC_RTHROWABLE_DEFINITION(ClassName)
# define ACDK_BCC_RTHROWABLE_DEFINITION_FQ(ns, ClassName)
#endif //__BORLANDC__


////////////////////////////////////////////////////////////
// Sun WorkShop 6
//
#if defined(__SUNPRO_CC)

// # define ACDK_SUPPORT_ANSI_SPECIALIZATION
# define POSIX_THREADS
# define HAS_UNISTD_H 1
# define ACDK_HAS_STRUCT_TIMEVAL 1
# define ACDK_HAVE_LONG_LONG 1
# define ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
# define ACDK_ALT_CALL_TEMPLATED_FUNCTION
# define ACDK_HAS_ALTERNATIVE_TOKEN
# define template_static static // this will cause an warning, but otherwise there will be an error
# define ACDK_DLL_EXPORT 
# define ACDK_DLL_IMPORT 
# define ACDK_DLL_PRIVATE 
#endif // __SUNPRO_CC



//
// Metroworks
//
#if defined(__MWERKS__)
# define ACDK_OS_WIN32
# define ACDK_METROWORKS
# define WIN32_THREADS
# define ACDK_SUPPORT_ANSI_SPECIALIZATION
# define ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
# define ACDK_HAS_ALTERNATIVE_TOKEN
# define template_static static
# define ACDK_DLL_EXPORT 
# define ACDK_DLL_IMPORT 
# define ACDK_DLL_PRIVATE 
#endif //defined(__MWERKS__)

//
// Intel CC 6.0 Linux
//
#if defined(__INTEL_COMPILER)
# define ACDK_SUPPORT_ANSI_SPECIALIZATION
# define ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
# define ACDK_HAS_ALTERNATIVE_TOKEN
# define template_static static
# if !defined(ACDK_OS_WIN32)
#   define HAS_UNISTD_H 1
# endif
# define ACDK_DLL_EXPORT 
# define ACDK_DLL_IMPORT 
# define ACDK_DLL_PRIVATE 
#endif //defined(__INTEL_COMPILER)


#if (defined(ACDK_OS_LINUX) || defined(ACDK_OS_SOLARIS)) && !defined(ACDK_OS_CYGWIN32)
# ifndef POSIX_THREADS
#  define POSIX_THREADS
# endif
#endif //OS_LINUX

#if defined(ACDK_OS_CYGWIN32) || defined(_MSC_VER) || (defined(__BORLANDC__) && !defined(__linux__))
#  define WIN32_THREADS
#endif

#ifdef WIN32_THREADS
#  define ACDK_WIN32_THREADS
#endif

#ifdef POSIX_THREADS
#  define ACDK_POSIX_THREADS
#endif

/**
  ACDK_FQ_SUPER_QUALIFIER is a helper macro to select super class in initialization list:
  @code
  struct A : public ::otherns::B
  {
    A() : ACDK_FQ_SUPER_QUALIFIER(::otherns::, B()) { }
  };
  @endcode
  @see ACDK_NEED_FQ_SUPER_QUALIFIER
  @ingroup acdkplatformmacros
*/

#ifdef ACDK_NEED_FQ_SUPER_QUALIFIER
# define ACDK_FQ_SUPER_QUALIFIER(namespace, class) namespace class
#else
# define ACDK_FQ_SUPER_QUALIFIER(namespace, class) class
#endif



#endif //acdk_Compiler_h

