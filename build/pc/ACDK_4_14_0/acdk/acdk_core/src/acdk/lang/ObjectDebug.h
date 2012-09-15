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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ObjectDebug.h,v 1.19 2005/04/09 19:26:49 kommer Exp $

#if 0

#ifndef acdk_lang_ObjectDebug_h
#define acdk_lang_ObjectDebug_h

#ifdef _MSC_VER
// why MS doesn't ship an STL, which compiles without thousends of warnings?
// this supress not all
#  pragma warning(disable: 4786)
#endif 
#include "sys/core_vector.h"
#include "sys/core_system.h"



namespace acdk {
namespace lang {



/** 
  @author Roger Rene Kommer
  @version $Revision: 1.19 $
  @date $Date: 2005/04/09 19:26:49 $
  @internal
*/  

foreign class ACDK_CORE_PUBLIC DebugStackFrame 
{
public:
  DebugStackFrame* upper;
  sys::core_string name;
  typedef std::vector<const Object*>  ObjectStack;
  ObjectStack stack;
  int _created;
  int _destroyed;
  size_t _BytesAllocated;
  DebugStackFrame(DebugStackFrame* uf, const char* strname)
  : upper(uf),
    name(strname),
    stack(),
    _created(0),
    _destroyed(0),
    _BytesAllocated(0)
  {
  }
  void addObject(const Object* obj, size_t size);
  bool removeObject(const Object* obj);
  bool hasObject(const Object* obj, bool recursiv = false) const;
  void printObjects(sys::core_output& os) const;
  int objectCount() const { return stack.size(); }
  size_t totalAllocated() const { return _BytesAllocated; }
  bool checkRecursivReferences() const;
  bool gc(bool recursiv = false);
  
  bool gc(const Object* obj);
private:
  bool _setInFieldToNil(const Object* obj, bool recursiv);
  //old:
  typedef std::map<const Object*, int> GcObjectStack;
  static bool gc(const Object* obj, GcObjectStack& stack);
  static bool checkRecursivReference(const RObject* field);
  void _setInFieldToNil(const Object* obj, GcObjectStack& gcstack);

};

/* ====================== DebugObjectPool =========================== */
/** 
  API: ACDK internal<br>
  @author Roger Rene Kommer
  @version $Revision: 1.19 $
  @date $Date: 2005/04/09 19:26:49 $
  @internal
*/  
foreign 
class ACDK_CORE_PUBLIC DebugObjectPool 
{
  mutable DebugStackFrame* frame;
  mutable bool _inDebug;
public:
  static int debugLevel;
  DebugObjectPool()
  : frame(0),
    _inDebug(false)
  {
    
  }
  ~DebugObjectPool()
  {
    DebugStackFrame* curframe = frame;
    while (curframe != 0) {
      DebugStackFrame* tframe = curframe->upper;
      delete curframe;
      curframe = tframe;
    }
    frame = 0;
  }
  void pushFrame(const char* name);
  void popFrame();
  void addObject(const Object* obj, size_t size);
  void removeObject(const Object* obj);
  // prints only Object in current frame
  void printObjects(sys::core_output& os);
  // prints Objects from all frames
  void printAllObjects(sys::core_output& os);
  int objectCount() const;
  int objectAllCount() const;
  void print(const char* msg, int debuglevel = 0);
  bool checkRecursivAllReferences() const;
  bool checkRecursivReferences() const;
  bool hasObject(const Object* obj) const;
  /// try to garbage collect in current Frame
  bool gc();
  /// try to garbage collect in all Frames
  bool gcAll();
  /// checks if obj is garbage, and releases it if necessary
  bool gc(const Object* obj);
private:
  void ensureFrame() const
  {
    if (frame == 0)
      frame = new DebugStackFrame(0, "Top");
  }
  
};

/// @internal
extern DebugObjectPool _debugObjectPool;


} // lang
} // acdk

#endif //acdk_lang_ObjectDebug_h

#endif //0
