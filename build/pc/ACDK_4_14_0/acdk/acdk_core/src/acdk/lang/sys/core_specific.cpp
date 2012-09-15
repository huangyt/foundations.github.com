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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_specific.cpp,v 1.32 2005/03/31 12:26:40 kommer Exp $

#include <acdk.h>

#include "core_specific.h"
#include "core_vector.h"
#include "core_fastmutex.h"
#include "core_guard.h"
#include "core_thread_id.h"
#include "core_syslocal.h"

#include <acdk/lang/ThreadLocalImpl.h>
#include <acdk/lang/System.h>


namespace acdk {
namespace lang {
namespace sys {



/**
  Holds the values of one thread.
*/
class core_tls_values
{
public:
  core_vector<void*> _values;
  core_tls_values()
  : _values(0, 20, 0)
  {
  }
  ~core_tls_values();
  void* get(int idx)
  {
    if (_values.size() <= idx)
      return 0;
    return _values[idx];
  }
  void set(int idx, void* value)
  {
    _values.ensureSize(idx + 1);
    _values[idx] = value;
    if (idx >= _values.size())
      _values.erase(_values.begin() + idx + 1);
  }
};




/*
core_tls_values* get_tls_table() 
{ 
  return (core_tls_values*)core_local_core_tls_values; 
}
void set_tls_table(core_tls_values* table) 
{ 
  core_local_core_tls_values = (int)table; 
}
*/

/*
#else // ACDK_WIN32_NO_LATE_BIND
static DWORD core_local_core_tls_handle = 0; 

inline core_tls_values* get_tls_table() 
{ 
  if (core_local_core_tls_handle == 0)
  {
    core_local_core_tls_handle = TlsAlloc();
  }
  return (core_tls_values*)TlsGetValue(core_local_core_tls_handle); 
}


inline void set_tls_table(core_tls_values* table) 
{ 
  TlsSetValue(core_local_core_tls_handle, table); 
}

#endif // ACDK_WIN32_NO_LATE_BIND
#endif //WIN32_THREADS
*/



/** 
  Represents value entries in a a given thread
*/
struct core_tls_value_table
{
  core_thread_id _threadId;
  core_tls_values* _table;
  core_tls_value_table()
  : _threadId()
  , _table(0)
  {
  }
  core_tls_value_table(core_thread_id threadId, core_tls_values* table)
  : _threadId(threadId)
  , _table(table)
  {
  }
  void destroyValues(core_specific* spec);
  void destroyThread()
  {
    delete _table;
    _table = 0;  
  }
};


/**
  A global table, which holds all registered core_tls_table
*/
class core_tls_tables
{
  core_vector<core_tls_value_table> _entries;
  core_vector<core_specific*> _specifics;
  core_vector<core_specific::CleanUpFunc> _cleanFunctions;
  core_fastmutex _mutex;
  bool _inited;
public:
  core_tls_tables()
  : _entries(20, 20, core_tls_value_table())
  , _specifics(20, 20, 0)
  , _cleanFunctions(20, 20, 0)
  , _inited(true)
  {
  }
  ~core_tls_tables()
  {
    /** because is allocated globally, this prevents to 
        call methods after destruction
    */
    _inited = false;
  }
  void insert(core_tls_values* table)
  {
    core_static_lock_guard<core_fastmutex> lock(_mutex);

    for (int i = 0; i < _entries.size(); ++i)
    {
      if (_entries[i]._table == 0) 
      {
        _entries[i]._table = table;
        _entries[i]._threadId = core_thread_id::get_current();
        return;
      }
    }
    _entries.push_back(core_tls_value_table( core_thread_id::get_current(), table));
  }
  void remove_current()
  {
    core_static_lock_guard<core_fastmutex> lock(_mutex);

    core_thread_id ctid =  core_thread_id::get_current();
    for (int i = 0; i < _entries.size(); ++i)
    {
      if (_entries[i]._threadId == ctid) {
        _entries[i].destroyThread();
        return;
      }
    }
    // should never reach
  }
  void insert_specific(core_specific* spec)
  {
    core_static_lock_guard<core_fastmutex> lock(_mutex);
    int i;
    for (i = 0; i < _specifics.size(); ++i) 
    {
      if (_specifics[i] == 0) {
        spec->_value_index = i;
        _specifics[i] = spec;
        _cleanFunctions[i] = core_specific::cleanup;
        return;
      }
    }
    spec->_value_index =  _specifics.size();
    _specifics.push_back(spec);
    _cleanFunctions.push_back((core_specific::CleanUpFunc)core_specific::cleanup);
  }
  void remove_specific(core_specific* spec)
  {
    if (_inited == false)
      return;
    core_static_lock_guard<core_fastmutex> lock(_mutex);
    for (int i = 0; i < _entries.size(); ++i)
    {
      _entries[i].destroyValues(spec);
    }
    _specifics[spec->_value_index] = 0;
    _cleanFunctions[spec->_value_index] = 0;
  }
  void register_cleanup(int idx, core_specific::CleanUpFunc cleanup)
  {
    if (_inited == false)
      return;
    core_static_lock_guard<core_fastmutex> lock(_mutex);
    _cleanFunctions[idx] = cleanup;
  }
  core_tls_values* get_core_specific_table();
  void cleanup_core_specific_table();
  void cleanup(int idx, void* ptr)
  {
    core_static_lock_guard<core_fastmutex> lock(_mutex);
    if (_cleanFunctions[idx] != 0) 
    {
      _cleanFunctions[idx](ptr);
      _cleanFunctions[idx] = 0;
    }
  }
  void get_thread_locals(core_vector<void*>& erg, core_thread_id tid)
  {
    core_static_lock_guard<core_fastmutex> lock(_mutex);
    for (int i = 0; _entries.size(); ++i)
    {
      if (_entries[i]._threadId == tid) {
        if (_entries[i]._table == 0)
          return;
        core_vector<void*>& values = _entries[i]._table->_values;
        for (int j = 0; j < values.size(); j++) 
        {
          if (values[j] != 0)
            erg.push_back(values[j]);
        }
        return;
      }
    }
  }
  void get_locals(core_vector<void*>& erg, int idx)
  {
    // ### implement?
  }
};



core_tls_tables& get_tls_tables()
{
  /* to make sure, that it will initialized
     the right time. especially on static
     specific<type> declarations
  */
  static core_tls_tables g_core_tls_tables;
  
  return g_core_tls_tables;
}

void core_tls_value_table::destroyValues(core_specific* spec)
{
  if (_table == 0)
    return;
  get_tls_tables().cleanup(spec->get_key(), _table->get(spec->get_key()));
  _table->set(spec->get_key(), 0);
}

core_tls_values::~core_tls_values()
{
  for (int i = 0; i < _values.size(); i++) 
  {
    if (_values[i] != 0) 
    {
      void* p = _values[i];
      _values[i] = 0;
      get_tls_tables().cleanup(i, p);
    }
  }
}


DEFINE_STATIC_SPECIFIC(core_tls_values*, gcore_local_core_tls_values);

core_tls_values* 
core_tls_tables::get_core_specific_table()
{
  static core_tls_values* globalStaticValues = 0;
  static ThreadLocalImpl dynLocal;
  if (System::isInMain() == false)
  {
    if (globalStaticValues == 0)
      globalStaticValues = new core_tls_values();
    return globalStaticValues;
  }
#if defined(ACDK_SUPPORT_STATIC_THREADLOCAL)  
  if (core_system::acdk_core_static_bound == true)
  {
    if (gcore_local_core_tls_values == 0)
      gcore_local_core_tls_values = new core_tls_values();
    return gcore_local_core_tls_values;
  }
#endif

  core_tls_values* ctv = (core_tls_values*)dynLocal.get();
  if (ctv == 0)
  {
    ctv = new core_tls_values();
    dynLocal.set((void*)ctv);
  }
  return ctv;
}

void 
core_tls_tables::cleanup_core_specific_table() 
{
  
}


//#if defined(_MSC_VER) //==============================================
/*
core_tls_values* 
core_tls_tables::get_core_specific_table()
{
  core_tls_values* _table = get_tls_table();
  if (_table != 0)
    return _table;
  _table = new core_tls_values();
  set_tls_table(_table);
  get_tls_tables().insert(_table);
  return _table;
}


void 
core_tls_tables::cleanup_core_specific_table() 
{
  //core_tls_values*& _table = (core_tls_values*&)core_local_core_tls_values();
  //cleanup_current();

}
*/
/*
#elif defined(ACDK_OS_LINUX) //==============================================


char* _lastStackAddressTop = 0;
char* _lastStackAddressBottom = 0;

const size_t StandardStackSize = 2 * 1024 * 1024; // 2MB

//core_specific_table _mainThreadTable;

core_tls_values* 
core_tls_tables::get_core_specific_table()
{
  
  char csf;
  char *sp = &csf;
  if (sp >= _lastStackAddress)
    return &_mainThreadTable;
  else if (sp >= _lastStackAddressBottom && sp < _lastStackAddressTop)
    return &__pthread_manager_thread;

  return (core_tls_values*)(((unsigned long)sp | (STACK_SIZE-1))+1) - 1;
}


void 
core_tls_tables::cleanup_core_specific_table() 
{
  
}
*/

//#else                        // defined(_MSC_VER) ==============================================
/*
//typdef ::acdk::lang::ThreadLocalImpl  ThreadLocal;
::acdk::lang::ThreadLocalImpl gSpecificTable;

core_tls_values* 
core_tls_tables::get_core_specific_table()
{
  
  core_tls_values* _table = (core_tls_values*)gSpecificTable.get();
  if (_table != 0)
    return _table;
  
  _table = new core_tls_values();
  gSpecificTable.set(_table);
  return _table;
}


void 
core_tls_tables::cleanup_core_specific_table() 
{
  //core_tls_values*& _table = (core_tls_values*&)core_local_core_tls_values();
  //cleanup_current();

}
*/

//#endif                        //// defined(_MSC_VER)==============================================




core_specific::core_specific()
: _value_index(0)
//, _cleanup(core_specific::cleanup)
{
  get_tls_tables().insert_specific(this);
  
}

void core_specific::register_cleanup(core_specific::CleanUpFunc fnc)
{
  get_tls_tables().register_cleanup(get_key(), fnc);
}

//virtaul 
core_specific::~core_specific()
{
  get_tls_tables().remove_specific(this);
}

void 
core_specific::set(void *t)
{
  core_tls_values* table = get_tls_tables().get_core_specific_table();
  table->set(_value_index, t);
}

void* 
core_specific::get()
{
  core_tls_values* table = get_tls_tables().get_core_specific_table();
  return table->get(_value_index);
}

//static 
void 
core_specific::thread_cleanup()
{
  get_tls_tables().remove_current();
}

void core_specific::get_locals(core_vector<void*>& erg)
{
}

//static 
void 
core_specific::get_thread_locals(core_vector<void*>& erg, core_thread_id tid)
{
  get_tls_tables().get_thread_locals(erg, tid);
}

} // namespace sys 
} //namespace lang 
} // namespace acdk 



