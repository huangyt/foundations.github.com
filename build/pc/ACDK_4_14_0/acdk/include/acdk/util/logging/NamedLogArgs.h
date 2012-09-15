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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/NamedLogArgs.h,v 1.8 2005/04/25 13:19:34 kommer Exp $

#ifndef acdk_util_logging_NamedLogArgs_h
#define acdk_util_logging_NamedLogArgs_h

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/Process.h>
#include <acdk/util/SysDate.h>
#include <acdk/lang/dmi/ScriptVar.h>

#include "Level.h"

#if ACDK_CHECK_GCC_VERSION(4, 0)
class NamedLogArgs;
#else
class ACDK_CORE_PUBLIC NamedLogArgs;
#endif

namespace acdk {
namespace util {
namespace logging {

ACDK_DECL_CLASS(NamedParameter);
/**
  wrapper to a named paramater for logging purpose.
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC NamedParameter
: extends ::acdk::lang::Object
{
public:
  acdk::lang::RString name;
  /**
    Either String or NamedParameterArray
  */
  //acdk::lang::RObject value;
  acdk::lang::dmi::ScriptVar value;
  NamedParameter()
  {
  }
  NamedParameter(IN(RString) n, IN(acdk::lang::dmi::ScriptVar) v)
  : name(n)
  , value(v)
  {
  }
  NamedParameter(INP(acdk::lang::RString) n, bool v)
  : name(n)
  , value(v)
  {
  }
  NamedParameter(INP(acdk::lang::RString) n, int v)
  : name(n)
  , value(v)
  {
  }
  NamedParameter(INP(acdk::lang::RString) n, char v)
  : name(n)
  , value(v)
  {
  }
  NamedParameter(INP(acdk::lang::RString) n, byte v)
  : name(n)
  , value(v)
  {
  }
  NamedParameter(INP(acdk::lang::RString) n, short v)
  : name(n)
  , value(v)
  {
  }
  NamedParameter(INP(acdk::lang::RString) n, jlong v)
  : name(n)
  , value(v)
  {
  }
  NamedParameter(INP(acdk::lang::RString) n, double v)
  : name(n)
  , value(v)
  {
  }
  NamedParameter(INP(acdk::lang::RString) n, const char* v)
  : name(n)
  , value(new acdk::lang::String(v))
  {
  }
#if defined(ACDK_USE_MFC)
  NamedParameter(INP(acdk::lang::RString) n, const CString& v)
  : name(n)
  , value(new acdk::lang::String((const char*)v))
  {
  }
#endif // defined(ACDK_USE_MFC)
  acdk::lang::RString toString()
  {
    acdk::lang::StringBuffer sb;
    sb.append(name); sb.append("=");
    if (value.type == acdk::lang::dmi::ScriptVar::ObjectType && 
        instanceof(value.getObjectVar(), NamedParameterArray) == true)
    {
      sb.append("[");
      sb.append(value->toString());
      sb.append("]");
    }
    else 
    {
      sb.append(value.toCode());
    }
    return sb.toString();
  }
};

} // logging
} // util
} // acdk

/**
  Utility to support acdk::util::logging
  Used in the LOG_NPV, LOG_NPC and LOG_NPS macros
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC NamedLogArgs
{
  acdk::util::logging::RNamedParameterArray _args;
public:
  NamedLogArgs()
  : _args(new acdk::util::logging::NamedParameterArray(0))
  {
  }
  NamedLogArgs(INP(acdk::util::logging::RNamedParameter) na)
  : _args(new acdk::util::logging::NamedParameterArray(1))
  {
    _args[0] = na;
  }
  NamedLogArgs(INP(acdk::util::logging::RNamedParameter) na, INP(acdk::util::logging::RNamedParameter) na2) 
    : _args(new acdk::util::logging::NamedParameterArray(2))
  {
    _args[0] = na;
    _args[1] = na2;
  }
  NamedLogArgs& operator<<(INP(acdk::util::logging::RNamedParameter) na)
  {
    _args->append(na);
    return *this;
  }
  int size() const { return _args->length(); }
  acdk::util::logging::RNamedParameter& operator[](int i) { return _args[i]; }
  operator acdk::util::logging::RNamedParameterArray() { return _args; }
};

/**
  alias to LOG_NPV(name, v)
  @ingroup acdklogging
*/
#define LOG_NP(name, v) \
  ::acdk::util::logging::RNamedParameter(new ::acdk::util::logging::NamedParameter(RCS(#name), ::acdk::lang::inOf(v)))

/**
  @param name must be a identifier without spaces
  @param v value
  @ingroup acdklogging
*/
#define LOG_NPV(name, v) LOG_NP(name, v)

/**
  @param name a string as label
  @param v value
*/
#define LOG_SPV(name, v) \
  ::acdk::util::logging::RNamedParameter(new ::acdk::util::logging::NamedParameter(name, ::acdk::lang::inOf(v)))


/**
  @param name must be a identifier without spaces
  @param v class instance, which supports the 
           NamedLogArgs&  operator<<(NamedLogArgs& nla, INP(Class) cls)
  @ingroup acdklogging
*/
#define LOG_NPC(name, v) \
    ::acdk::util::logging::RNamedParameter(new ::acdk::util::logging::NamedParameter(RCS(#name), LOG_NPS(v)))
  
#define LOG_NPS(stream) \
  const_cast<NamedLogArgs&>((const NamedLogArgs&)NamedLogArgs()) << stream

/**
  @param name a string
  @param v class instance, which supports the 
           NamedLogArgs&  operator<<(NamedLogArgs& nla, INP(Class) cls)
  @ingroup acdklogging
*/

#define LOG_SPC(name, v) \
    ::acdk::util::logging::RNamedParameter(new ::acdk::util::logging::NamedParameter(name, LOG_NPS(v)))

#if !defined(DOXYGENONLY)

template <typename T>
struct LogStreamWrapper
{
  typedef NamedLogArgs& (T::*NamedLogStreamFunction)(NamedLogArgs&) const;
  
  const T& _thisRef;
  NamedLogStreamFunction _funcRef;
  LogStreamWrapper(const T& ref, NamedLogArgs& (T::*func)(NamedLogArgs&) const):  _thisRef(ref), _funcRef(func) {}
};

template <class T> 
LogStreamWrapper<T>
makeLogStreamWrapper(const T& t, NamedLogArgs& (T::*func)(NamedLogArgs&) const) 
{ 
  return LogStreamWrapper<T>(t, func);
}



template <class T>
NamedLogArgs& operator<<(NamedLogArgs& s, const LogStreamWrapper<T> & w)
{
  typedef typename LogStreamWrapper<T> :: NamedLogStreamFunction NamedLogStreamFunction;
  NamedLogStreamFunction f = w._funcRef;
  return ((w._thisRef).*w._funcRef)(s);
}

#define LOG_NPZ(Label, sobj, toFunc) \
  ::acdk::util::logging::RNamedParameter(new ::acdk::util::logging::NamedParameter(#Label, LOG_NPS(makeLogStreamWrapper(sobj, toFunc))))

#endif //!defined(DOXYGENONLY)



  


#endif //acdk_util_logging_NamedLogArgs_h

