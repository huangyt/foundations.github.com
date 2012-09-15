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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StackFrame.h,v 1.5 2005/04/28 15:00:03 kommer Exp $

#ifndef acdk_lang_StackFrame_h
#define acdk_lang_StackFrame_h

#include <acdk/io/Serializable.h>

namespace acdk {
namespace lang {
namespace reflect {
  ACDK_DECL_CLASS(Method);
}
}
}

namespace acdk {
namespace lang {
namespace dmi {
  class DmiObject;
  typedef ::RefHolder<DmiObject> RDmiObject;
}
}
}


namespace acdk {
namespace lang {

ACDK_DECL_INTERFACE(StackFrameLocal);
/**
  represents a local variable in a StackFrame
*/
class ACDK_CORE_PUBLIC StackFrameLocal
: implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(StackFrameLocal)
public:
  /** return the name of the local veriable */
  virtual RString getName() = 0;
  /** 
    return the value of the local variable as 
    Object. Basic types are wrapped.
  */
  virtual RObject getObjectValue() = 0;
  /**
    return the variable
  */
  virtual acdk::lang::dmi::RDmiObject getVariable() = 0;
};


ACDK_DECL_INTERFACE(StackFrame);

/**
  interface for a Stack Trace description
*/
class ACDK_CORE_PUBLIC StackFrame
: implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(StackFrame)
public:
  /**
    should return true if frame has information about file and line
    @see getFileName()
    @see getCurrentSourceLine()
  */
  virtual bool hasFileAndLine() = 0;
  /**
    should return true if frame has information of local variables
    @see getLocals() 
  */
  virtual bool hasLocals() = 0;
  /**
    return true if a function signature is available
    @see getFunctionSignature()
  */
  virtual bool hasFunctionSignature() = 0;
  /**
    return true if has current line line of instruction
    @see getCurrentSourceLine()
  */
  virtual bool hasCurrentSourceLine() = 0;
  /**
    return true if frame has name of the library
    executing this frame
    @see getLibraryName()
  */
  virtual bool hasLibararyName() = 0;
  /**
    return true if a Method is available
    @see getMethod()
  */
  virtual bool hasMethod() = 0;
  /**
    return true if this frame is in native code
  */
  virtual bool isNative() = 0;
  /**
    return the file number, where the method is defined/declared
  */
  virtual int getFileLineNo() = 0;
  /**
    return the file name of source code
  */
  virtual RString getFileName() = 0;
  /**
    return the function signature
  */
  virtual RString getFunctionSignature() = 0;
  /**
    return the source line of current code instruction
  */
  virtual RString getCurrentSourceLine() = 0;
  /**
    return library name
  */
  virtual RString getLibraryName() = 0;
  /**
    return the Method of current executed method
  */
  virtual acdk::lang::reflect::RMethod getMethod() = 0;
  /**
    String -> Object
      if Object itself is Serializable insert the Object itself
      otherwise use toString()
  */
  virtual RStackFrameLocalArray getLocals() = 0;
};

ACDK_DECL_CLASS(NativeStackFrame);

/**
  implements a StackFrame of native C++ code
*/
class ACDK_CORE_PUBLIC NativeStackFrame
: extends acdk::lang::Object
, implements StackFrame
{
  ACDK_WITH_METAINFO(NativeStackFrame)
protected:
  int _programCounter;
  RString _functionName;
  RString _libraryName;
  RString _sourceFile;
  int _sourceLine;
public:
  NativeStackFrame()
  : _programCounter(-1)
  , _sourceLine(-1)
  {
  }
  NativeStackFrame(int pc, IN(RString) funcName, IN(RString) libName, IN(RString) sourceFile = Nil, int sourceLine = -1)
    : _programCounter(pc)
    , _functionName(funcName)
    , _libraryName(libName)
    , _sourceFile(sourceFile)
    , _sourceLine(sourceLine)
  {
  }
  virtual bool hasFileAndLine() { return _sourceLine != -1; }
  virtual bool hasLocals() { return false; }
  virtual bool hasFunctionSignature() { return true; }
  virtual bool hasCurrentSourceLine() { return false; }
  virtual bool hasLibararyName() { return true; }
  virtual bool hasMethod() { return false; }
  virtual bool isNative() { return true; }
  virtual int getFileLineNo() { return _sourceLine; }
  virtual RString getFileName() { return _sourceFile; }
  virtual RString getFunctionSignature() { return _functionName; }
  virtual RString getCurrentSourceLine() { return Nil; }
  virtual RString getLibraryName() { return _libraryName; }
  virtual RStackFrameLocalArray getLocals();
  virtual acdk::lang::reflect::RMethod getMethod();
};

} // lang
} // acdk

#endif //acdk_lang_StackFrame_h

