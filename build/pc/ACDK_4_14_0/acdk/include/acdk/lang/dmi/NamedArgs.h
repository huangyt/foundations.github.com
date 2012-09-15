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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/NamedArgs.h,v 1.10 2005/02/05 10:44:58 kommer Exp $

#ifndef acdk_lang_dmi_NamedArgs_h
#define acdk_lang_dmi_NamedArgs_h


class NamedArgs;

/**
  Wrapper to support named arguments in DMI
*/
foreign 
class ACDK_CORE_PUBLIC NamedArg
{
public:
  const char* _name;
  const ::acdk::lang::dmi::ScriptVar* _val;

  NamedArg() 
  : _name(0) 
  , _val(0)
  {} // for vector
  NamedArg(const char* name, const ::acdk::lang::dmi::ScriptVar& val)
  : _name(name)
  , _val(&val)
  {
  }

  const char* name() const { return _name; }
  const ::acdk::lang::dmi::ScriptVar& value() const { return *_val; }

  inline NamedArgs operator<<(const NamedArg& na);
  inline NamedArgs operator,(const NamedArg& na);
  NamedArg& operator=(const NamedArg& other)
  {
    _val = other._val;
    _name = other._name;
    return *this;
  }
  
};

#define ACDK_NAMEDARG(name, val) NamedArg(name, inOf(val))

/**
  Wrapper to support named arguments in DMI
*/
foreign 
class ACDK_CORE_PUBLIC NamedArgs
{
public:
  ::acdk::lang::sys::core_vector<NamedArg> _args;
  typedef ::acdk::lang::sys::core_vector<NamedArg>::iterator iterator;
  typedef ::acdk::lang::sys::core_vector<NamedArg>::const_iterator const_iterator;

  NamedArgs()
  : _args(0)
  {
  }
  NamedArgs(const NamedArg& na) 
    : _args(1)
  {
    _args[0] = na;
  }
  NamedArgs(const NamedArg& na, const NamedArg& na2) 
    : _args(2)
  {
    _args[0] = na;
    _args[1] = na2;
  }
  NamedArgs& operator<<(const NamedArg& na)
  {
    _args.push_back(na);
    return *this;
  }
  NamedArgs& operator,(const NamedArg& na)
  {
    return operator<<(na);
  }
  int size() const { return _args.size(); }
  NamedArg& operator[](int i) { return _args[i]; }
  const NamedArg& operator[](int i) const { return _args[i]; }
};


inline
NamedArgs NamedArg::operator<<(const NamedArg& na)
{
  return NamedArgs(*this, na);
}

inline
NamedArgs NamedArg::operator,(const NamedArg& na)
{
  return operator<<(na);
}


#endif // acdk_lang_dmi_NamedArgs_h

