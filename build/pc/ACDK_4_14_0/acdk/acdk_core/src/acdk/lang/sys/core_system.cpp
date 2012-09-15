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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_system.cpp,v 1.17 2005/03/07 20:41:36 kommer Exp $

#include <acdk.h>


#include "core_system.h"
#include "core_vector.h"
#include "core_pair.h"

#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Double.h>
#include <acdk/io/File.h>
#include <acdk/io/FileStatus.h>
#include <acdk/util/StringTokenizer.h>

namespace acdk {
namespace lang {
namespace sys {

ExecutionState core_system::execution_state = BeforeMain;
bool core_system::isInMain = false;
bool core_system::acdk_core_static_bound = false;
  

  //static
void
core_system::setState(ExecutionState state)
{
  execution_state = state;
  if (execution_state == InMain)
    isInMain = true;
  else
    isInMain = false;
}

//static
int 
core_system::core_main(MainFunction mf, int argc, char* argv[], char* envp[])
{
  
  execution_state = InMain;
  if (intialize() == false) {
    sys::coreout << "core_system: intialization failed! exit now." << sys::eofl;
    return 1;
  }
  int ret = mf(argc, argv, envp);
  deintialize();
  execution_state = AfterMain;
  return ret;
}


core_vector<core_pair< core_system::InitializeFunction, void*> >& 
getIntializer()
{
  static core_vector<core_pair<core_system::InitializeFunction, void*> > gintializer;
  return gintializer;
}

core_vector<core_pair<core_system::DeinitializeFunction, void*> >& 
getDeintializer()
{
  static core_vector<core_pair<core_system::DeinitializeFunction, void*> > gdeintializer;
  return gdeintializer;
}



//static 
void 
core_system::registerIntializer(InitializeFunction f, void* arg/* = 0*/)
{
  core_pair<core_system::InitializeFunction, void*> t = core_make_pair(f, arg);
  getIntializer().push_back(t);
}

//static 
void 
core_system::registerDeintializer(DeinitializeFunction f, void* arg/* = 0*/)
{
  getDeintializer().push_back(core_make_pair(f, arg));
}


//static 
bool 
core_system::intialize()
{
  bool erg = true;
  for (int i = 0; i < getIntializer().size(); i++) 
  {
    core_pair<InitializeFunction, void*>& p = getIntializer()[i];
    erg &= p.first(p.second);
  }
  return erg;
}

//static 
void 
core_system::deintialize()
{
  for (int i = 0; i < getDeintializer().size(); i++) 
  {
    core_pair<DeinitializeFunction, void*>& p = getDeintializer()[i];
    p.first(p.second);
  }
}

void 
core_vector_throw_out_of_index(int size, int idx)
{
  StringBuffer sb(50);
  sb << "core_vector out of index: size=" << size << "; access index=" << idx;
  THROW3(IndexOutOfBoundsException, sb.toString(), idx, size);
}



RString
core_system::fq_executable_filename(IN(RString) name)
{
#if defined(ACDK_OS_WIN32)
  if (name->length() > 2 && name->charAt(1) == ':')
    return name;
#else
  if (name->length() > 1 && name->charAt(0) == '/')
    return name;
#endif
  if (name->indexOf('/') != -1 || name->indexOf('\\') != -1)
  {
    acdk::io::File f(acdk::io::File::getCWD(), name);
    if (f.exists() == true)
      return f.getCanonicalPath();
  }
  RStringArray sa = System::getEnvPath();
  for (int i = 0; i < sa->length(); ++i)
  {
    RString t = sa[i] + acdk::io::File::separator() + name;
    if (acdk::io::FileStatus(t).exists() == true)
      return t;
  }
  return name;
}

//static 
bool 
core_system::isPtrInStack(void* ptr)
{
  volatile int localVar = 0;
  char* localPtr = (char*)&localVar;
  char* testPtr = (char*)ptr;
  if (testPtr > localPtr - 100000 && testPtr < localPtr + 100000)
  {
    //sys::coreout << "Ptr is in Stack: " << (void*)testPtr << "; localPtr: " << (void*)localPtr << sys::eofl;
    return true;
  }
  return false;
}


namespace {
bool 
isPrim(int number, core_vector<int>& primes) 
{
  for (int i = 0; i < primes.size(); i++)
    if ((number % primes[i]) == 0)
      return false;
  return true;
}

void 
fillPrimes(int num, core_vector<int>& primes) 
{
  int i = 2;
  primes.push_back(i);
  i++;
  for (; i < num; ++i) {
    if (isPrim(i, primes))
      primes.push_back(i);
  }
}



} // anon namesapce

int ACDK_CORE_PUBLIC core_get_next_prim(int num)
{
  if (num == 0)
    return 0;
  core_vector<int> primes(0, num / 3, 0);
  fillPrimes(num, primes);
  while (isPrim(num, primes) == false) 
  {
    primes.push_back(num);
    num++;
  }
  return num;
}

bool ACDK_CORE_PUBLIC core_is_prim(int num)
{
  core_vector<int> primes(0, num / 3, 0);
  fillPrimes(num, primes);
  return isPrim(num, primes);
}

ACDK_CORE_PUBLIC core_endline eofl;
ACDK_CORE_PUBLIC core_output coreout;


core_output& 
core_output::operator<<(const char* text)
{
  printf(text);
  return *this;
}
core_output& 
core_output::operator<<(IN(RObject) obj)
{
  if (obj == Nil)
    return operator<<("Nil");
  return operator<<(obj->toString());
}

core_output& 
core_output::operator<<(IN(RString) text)
{
  if (text == Nil)
    return operator<<("Nil");
  return operator<<(text->c_str());
}

core_output& 
core_output::operator<<(int i)
{
  return operator<<(Integer::toString(i)->c_str());
}

core_output& 
core_output::operator<<(jlong i)
{
  return operator<<(Long::toString(i)->c_str());
}

core_output& 
core_output::operator<<(double d)
{
  return operator<<(Double::toString(d)->c_str());
}

core_output& 
core_output::operator<<(void* ptr)
{
  operator<<("0x");
  return operator<<(Integer::toString(int(ptr))->c_str());
}

core_output& 
core_output::operator<<(const core_endline& oef)
{
  return operator<<("\n");
}
core_output& 
core_output::operator<<(bool val)
{
	return operator<<(val == true ? "true" : "false");
}

} // namespace sys 
} //namespace lang 
} // namespace acdk 




