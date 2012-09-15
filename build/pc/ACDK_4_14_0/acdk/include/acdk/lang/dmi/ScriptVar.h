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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/ScriptVar.h,v 1.74 2005/04/19 09:21:28 kommer Exp $

#ifndef acdk_lang_dmi_ScriptVar_h
#define acdk_lang_dmi_ScriptVar_h

//#include <vector>
#include <new>
#include <typeinfo>

#if defined(__BORLANDC__)
#include <mem.h>
#endif //defined(__BORLANDC__)

#include "MetaInfoFlags.h"

template <class T> class BasicArray;
template <class T> class RObjectArrayImpl;

namespace acdk {
namespace lang {

  class RString;
  typedef ::RObjectArrayImpl<RString> RStringArray;
  ACDK_DECL_CLASS(Class);

} // lang
} // acdk


namespace acdk {
namespace lang {
namespace dmi {

using namespace ::acdk::lang;


class ClazzMethodInfo;
class DmiClient;
class ScriptVar;
/**
  ScriptVarArray is used as canonical form for DMI arguments
*/
typedef ::acdk::lang::sys::core_vector<ScriptVar> ScriptVarArray;


/**
  a combination is used as castflags in many ScriptVar methods
  These flags controls how to cast types
*/

enum ScriptVarCastFlags
{
  /** 
    alow to cast integer types from and to character types
  */
  SVCastSVCastChar2Int = 0x0001,
  /**
    allow to cast integer types to float types and vice versa
  */
  SVCastInt2Float    = 0x0002,
  /**
    allow to cast number != 0 to bool
  */
  SVCastNum2Bool     = 0x0004,
  /**
    allow to cast a boolean to a number
    true is 1 and false is 0
  */
  SVCastBool2Number  = 0x0008,
  /**
    allow to cast if type is object to (obj != Nil)
  */
  SVCastObject2Bool  = 0x0010,
  /**
    try to decode a string as enumeration value symbol
    and cast it to a int
  */
  SVCastString2EnumInt = 0x0020,
  /**
    Automatically cast from Integer to int, Float to float, etc. and vice versa
  */
  SVCastAutobox       = 0x0100,
  /**
    do a hard reinterpret. 
    This option should only be used in very rare cases.
  */
  SVCastReinterpret  = 0x0200,
  /**
    check if in case of a casting a wider type to a smaller
    type if the value fits to the target type.
    If not throw exception
  */
  SVCastCheckOvervflow = 0x0400,
  /**
    on operations requires a basic type (for example int)
    try to decode strings
  */
  SVCastDecodeString  = 0x0800,
  /** 
    on operations which requires directly or indirectly
    a string type automatically cast the type to a string
  */
  SVCastEncodeString  = 0x1000,
  /**
    automatically unwrapp instances of DmiObject
  */
  SVCastWrapDmiObject   = 0x2000,
  /**
    automatically wrap any type to DmiObject
  */
  SVCastUnwrapDmiObject = 0x4000,

  SVCastStdFlags = SVCastInt2Float | SVCastNum2Bool | SVCastObject2Bool | SVCastAutobox | SVCastUnwrapDmiObject | SVCastWrapDmiObject
};

ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, ScriptVarCastFlags);

#if 0
# define ACDK_SCRIPTVAR_CLEARVAR() memset(&var, '\0', sizeof(var));
#else
# define ACDK_SCRIPTVAR_CLEARVAR()
#endif

/**
  ScriptVar is a generic container for all other ACDK types.
  It can hold the values by value or by reference.
  Additionally the declared type can be set, which may differ to
  to the concret type of the hold value.

  ScriptVar also provides the standard operations like addition, multiplication
  may used inside scripting languages.

  It mainly used in Dynamic Method Invokation (DMI) interfaces.

  @see gw_ref[acdk_hb_dmi_scriptvar]
  @see acdk::lang::dmi::DmiObject
  @throw Operation on ScriptVar (operatuions, conversion, assignmnet)
         can throw acdk::lang::DmiTypeConversionException
  @author Roger Rene Kommer
*/
foreign
class ACDK_CORE_PUBLIC ScriptVar
{
public:
  /**
    the typed the ScriptVar can hold
  */
  enum Type
  {
    /**
      Uninitialized or void type
    */
    UnknownType = 0,
    /**
      contains bool
    */
    BoolType,
    /**
      contains char
    */
    CharType,
    /**
      contains ucchar
    */
    UcCharType,
    /**
      contains byte
    */
    ByteType,
    /**
      contains short
    */
    ShortType,
    /**
      contains int
    */
    IntType,
    /**
      contains jlong
    */
    LongType, // i64
    /**
      contains float
    */
    FloatType,
    /**
      contains double
    */
    DoubleType,
    /**
      contains RObject
    */
    ObjectType,
    /**
      contains reference to bool value
    */
    BoolRefType,
    /**
      contains reference to char value
    */
    CharRefType,
    /**
      contains reference to ucchar value
    */
    UcCharRefType,
    /**
      contains reference to byte value
    */
    ByteRefType,
    /**
      contains reference to short value
    */
    ShortRefType,
    /**
      contains reference to int value
    */
    IntRefType,
    /**
      contains reference to jlong value
    */
    LongRefType,
    /**
      contains reference to float value
    */
    FloatRefType,
    /**
      contains reference to double value
    */
    DoubleRefType,
    /**
      contains reference to RObject reference
    */
    ObjectRefType
  };
  Type type;

  /**
    The flags see
    acdk::lang::dmi::MetaInfoFlags
  */
  int flags;

  /**
    Container of the possible types
  */
  union TypeUnion
  {
    bool boolVal;
    char charVal;
    ucchar uccharVal;
    byte byteVal;
    short shortVal;
    int intVal;
    jlong longVal;
    float floatVal;
    double doubleVal;
    char object_buf[sizeof(RObject)];
    //Object* object;

    /// reference types
    bool* boolRef;
    char* charRef;
    ucchar* uccharRef;
    byte* byteRef;
    short* shortRef;
    int* intRef;
    jlong* longRef;
    float* floatRef;
    double* doubleRef;

    struct {
      /**
        Reference to Object or Interface reference
      */
      RObject* objectRef;
      /**
          true in case the objectRef itself has been allocated
          by this ScriptVar
      */
      bool ownsObjectRef;
    } oref;
  };

  /**
    The value
  */
  TypeUnion var;
   /**
    if _clazzType is 0 the DmiObject has a weak type definition and
    may change its underlying type.
    If _clazzType is not 0, the assign methods check if type is assignable
  */
  const ClazzInfo* _clazzType;

  /**
    Standard constructor as Unknown Type
  */
  ScriptVar()
  : type(UnknownType)
  , flags(0)
  , _clazzType(0)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
  }
  
  /**
    ScriptVar contains Boolean value
  */
  ScriptVar(bool c, int fl = MiAiIn, const ClazzInfo* ci = 0)
  : type(BoolType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.boolVal = c;
  }

  /**
    ScriptVar contains a bool reference
  */
  ScriptVar(bool* c, int fl/* = MiAiOut*/, const ClazzInfo* ci = 0)
  : type(BoolRefType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.boolRef = c;
  }
  ScriptVar(char c, int fl = MiAiIn, const ClazzInfo* ci = 0)
  : type(CharType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.charVal = c;
  }
  ScriptVar(char* c, int fl/* = MiAiOut*/, const ClazzInfo* ci = 0)
  : type(CharRefType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.charRef = c;
  }
  ScriptVar(ucchar c, int fl = MiAiIn, const ClazzInfo* ci = 0)
  : type(UcCharType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.uccharVal = c;
  }
  ScriptVar(ucchar* c, int fl/* = MiAiOut*/, const ClazzInfo* ci = 0)
  : type(UcCharRefType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.uccharRef = c;
  }
  ScriptVar(byte c, int fl/* = MiAiIn*/, const ClazzInfo* ci = 0)
  : type(ByteType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.byteVal = c;
  }
  ScriptVar(byte* c, int fl/* = MiAiOut*/, const ClazzInfo* ci = 0)
  : type(ByteRefType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.byteRef = c;
  }
  ScriptVar(short c, int fl = MiAiIn, const ClazzInfo* ci = 0)
  : type(ShortType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.shortVal = c;
  }
  ScriptVar(short* c, int fl/* = MiAiOut*/, const ClazzInfo* ci = 0)
  : type(ShortRefType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.shortRef = c;
  }
  ScriptVar(int c, int fl = MiAiIn, const ClazzInfo* ci = 0)
  : type(IntType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.intVal = c;
  }
  ScriptVar(int* c, int fl = MiAiOut, const ClazzInfo* ci = 0)
  : type(IntRefType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.intRef = c;
  }
  ScriptVar(jlong c, int fl = MiAiIn, const ClazzInfo* ci = 0)
  : type(LongType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.longVal = c;
  }
  ScriptVar(jlong* c, int fl/* = MiAiOut*/, const ClazzInfo* ci = 0)
  : type(LongRefType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.longRef = c;
  }
  ScriptVar(float c, int fl = MiAiIn, const ClazzInfo* ci = 0)
  : type(FloatType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.floatVal = c;
  }
  ScriptVar(float* c, int fl/* = MiAiOut*/, const ClazzInfo* ci = 0)
  : type(FloatRefType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.floatRef = c;
  }
  ScriptVar(double c, int fl = MiAiIn, const ClazzInfo* ci = 0)
  : type(DoubleType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.doubleVal = c;
  }
  ScriptVar(double* c, int fl/* = MiAiOut*/, const ClazzInfo* ci = 0)
  : type(DoubleRefType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    var.doubleRef = c;
  }
  ScriptVar(NilRef nilval, int fl = MiAiIn, const ClazzInfo* ci = 0)
  : type(ObjectType)
  , flags(fl)
  , _clazzType(ci)
  {
    ACDK_SCRIPTVAR_CLEARVAR()
    _init(RObject(Nil));
  }


  /**
    Contains a RObject value
  */
  ScriptVar(IN(RObject) obj, int fl = MiAiIn, const ClazzInfo* ci = 0);

  /**
    Contains a RObject value
  */
  ScriptVar(Object* obj, int fl = MiAiIn, const ClazzInfo* ci = 0)
  : type(ObjectType)
  , flags(MiAiIn)
  , _clazzType(ci)
  {
    _init(obj);
  }

  /**
    Contains a RObject value
  */

  ScriptVar(InterfaceBase* obj, int fl = MiAiIn, const ClazzInfo* ci = 0);



  /**
    contains a RObject reference
  */
  ScriptVar(RObject* c, int fl = MiAiOut, const ClazzInfo* ci = 0);
  /**
    Contains a RObject with contains a String with given cstr
  */
  ScriptVar(const char* cstr);
  /**
    copy constructor
  */
  ScriptVar(const ScriptVar& o);
  /**
    constructs a ScriptVar from another scriptvar
    with possibility to change flags and declared type
  */
  ScriptVar(const ScriptVar& o, int flags, const ClazzInfo* ci = 0);
  ~ScriptVar();

  /**
    This returns the ClazzInfo of the declared type.
    If the ScriptVar doesn't have an explicit set type (_clazzType == 0)
    it return the dynamic type of the contained value.
    
    In case it contains a basic type it returns the
    ClazzInfo::getBasicTypeClazz<T>()
    In case it is a Nil object it returns the
    Object::clazzInfo()
    In case of unknown type it returns ClazzInfo::getUnknownBasicClazz()
    @return the declared ClazzInfo or the ClazzInfo the contained value
    @see getValueClazzInfo() 
    
  */
  inline const ClazzInfo* getClazzInfo() const
  {
    if (_clazzType != 0)
      return _clazzType;
    return getValueClazzInfo();
  }
  /**
    different to getClazzInfo() it always return
    the dynamic type info of the contained value.
    @see getClazzInfo()
  */
  const ClazzInfo* getValueClazzInfo() const;
  /**
    returns an initialized instance of ScriptVar
  */
  static ScriptVar getInitialized(const acdk::lang::dmi::ClazzInfo* ci);
  /**
    returns basic type. in Case of Object always object
  */
  static const ClazzInfo* getClazzInfo(Type typ);
  /**
    return true, if type is reference type
  */
  static inline bool isReference(Type tp)
  {
    return tp >= BoolRefType && tp <= ObjectRefType;
  }

  /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(bool c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(bool* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(char c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(ucchar c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(char* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(const char* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(ucchar* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(byte c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(byte* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(short c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(short* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(int c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(int* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(jlong c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(jlong* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(float c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(float* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(double c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(double* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(const RObject& obj);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(Object* c);
   /**
    assign operator for a bool value
    @throw DmiTypeConversionException if incompatible type
  */
  ScriptVar& operator=(RObject* c);

  /**
    For serialized mappings it may be needed to set a
    Object Reference (RObject) as own reference.
    The RObject* will be allocated by new
  */
  void setOwnObjectReference(IN(RObject) obj = Nil);
  /**
    returns a bool value.
    In case of reference type it returns corresponding contained value type
    In case of integer all values != 0 returned true
    In case of RObject returns true if obj != Nil
  */
  bool getBoolVar(short castFlags = SVCastStdFlags) const;

  /**
    return bool reference.

    @throw DmiTypeConversionException If not bool reference or bool value
  */
  bool& getBoolRef();
  /**
    returns char value
    In case of reference type it returns corresponding contained value type
    In case of Number it just cast it to a char
    In case of RObject contains acdk::lang::Character or acdk::lang::Number
      it returns the casted value .
    @throw otherwise DmiTypeConversionException
  */
  char getCharVar(short castFlags = SVCastStdFlags) const;

  /**
    return char reference
    @throw DmiTypeConversionException If not char reference
  */
  char& getCharRef();
  /**
    @see getCharVar
  */
  ucchar getUcCharVar(short castFlags = SVCastStdFlags) const;

  /**
    @see getCharRef
  */
  ucchar& getUcCharRef();


  /**
    @see getCharVar
  */
  byte getByteVar(short castFlags = SVCastStdFlags) const;

  /**
    @see getCharRef
  */
  byte& getByteRef();

  /**
    @see getCharVar
  */
  short getShortVar(short castFlags = SVCastStdFlags) const;

  /**
    @see getCharRef
  */
  short& getShortRef();

  /**
    @see getCharVar
  */
  int getIntVar(short castFlags = SVCastStdFlags) const;

  /**
    @see getCharRef
  */
  int& getIntRef();

  /**
    @see getCharVar
  */
  jlong getLongVar(short castFlags = SVCastStdFlags) const;

  /**
    @see getCharVar
  */
  jlong& getLongRef();

  /**
    @see getCharVar
  */
  float getFloatVar(short castFlags = SVCastStdFlags) const;

  /**
    @see getCharVar
  */
  float& getFloatRef();

  /**
    @see getCharVar
  */
  double getDoubleVar(short castFlags = SVCastStdFlags) const;

  /**
    @see getCharVar
  */
  double& getDoubleRef();

  RObject getObjectVar(short castFlags = SVCastStdFlags, const ClazzInfo* ci = 0) const;

  /**
    @see getCharVar
  */
  RObject& getObjectRef();

  /**
    return StringVar
    @throw BadCastException If it is not a String containing, BadCastException will be thrown
  */
  RString getStringVar(short castFlags = SVCastStdFlags) const;

  /**
    return the void pointer to the data;
    Node the return pointer is only valid as long this ScriptVar exists.
    In case of holding reference types it returns also the pointer the value
    and not a pointer to a pointer to the value
  */
  void* getDataPtr();
  /**
    return true if unknown type.
    This means ScriptVar is not initialized or void
  */
  inline bool isUndef() const
  {
    return type == UnknownType;
  }
  /**
    return true if type is unknown/void
  */
  inline bool isVoid() const { return type == UnknownType; }

  /** contains true if the hold value has type bool */
  bool isBoolType() const { return type == BoolType || type == BoolRefType; }
  /** contains true if the hold value has type char */
  bool isCharType() const { return type == CharType || type == CharRefType; }
  /** contains true if the hold value has type ucchar */
  bool isUcCharType() const { return type == UcCharType || type == UcCharRefType; }
  /** contains true if the hold value has type byte */
  bool isByteType() const { return type == ByteType || type == ByteRefType; }
  /** contains true if the hold value has type short */
  bool isShortType() const { return type == ShortType || type == ShortRefType; }
  /** contains true if the hold value has type int */
  bool isIntType() const { return type == IntType || type == IntRefType; }
  /** contains true if the hold value has type jlong */
  bool isLongType() const { return type == LongType || type == LongRefType; }
  /** contains true if the hold value has type float */
  bool isFloatType() const { return type == FloatType || type == FloatRefType; }
  /** contains true if the hold value has type double */
  bool isDoubleType() const { return type == DoubleType || type == DoubleRefType; }
  /** contains true if the hold value has type Object */
  bool isObjectType() const { return type == ObjectType || type == ObjectRefType; }
  /**
    return true if the hold type is byte, short, int or long
  */
  bool isIntegerType() const { return isByteType() || isShortType() || isIntType() || isLongType(); }
  /** return true if hold type is char or ucchar */
  bool isCharacterType() const { return isCharType() || isUcCharType(); }
  /** return true if hold type is float or double */
  bool isFloatingType() const { return isFloatType() || isDoubleType(); }
  /** return true if hold type is byte, short, int, long, float or double */
  bool isNumberType() const { return isIntegerType() || isFloatingType(); }
  /**
    return true if the hold type is a String instance
  */
  bool isStringType() const;
  /**
    return true if containing value can be casted to a boolean type
  */
  bool isBoolean(short castFlags = SVCastStdFlags) const;
  /**
    return true if containing value can be casted to a character type
  */
  bool isCharacter(short castFlags = SVCastStdFlags) const;
  /**
    return true if containing can be casted to a integer type
  */
  bool isInteger(short castFlags = SVCastStdFlags) const;
  /**
    return true if containing value can be casted to a floating type
  */
  bool isFloating(short castFlags = SVCastStdFlags) const;
  /**
    return true if the containing value can be casted to a number type 
  */
  inline bool isNumber(short castFlags = SVCastStdFlags) const
  {
    return isInteger(castFlags) || isFloating(castFlags);
  }
  /**
    this returns true if this is a castable float, but no castable integer 
  */
  bool isOnlyFloating(short castFlags) const;
  
  /**
    return true if the containing type can be casted to a string
  */
  bool isString(short castFlags = SVCastStdFlags) const;

  /**
    return true if containing type can be casted to a Object
  */
  inline bool isObject(short castFlags = SVCastStdFlags) const
  {
    return type == ObjectType || type == ObjectRefType;
  }
  
  /**
    return the size of the stored type.
    If Type is dynamic (object) it returns -1
    In case reference is stored return the size of the
    underlying type and not of the pointer
  */
  int getTypeStorageSize() const;
  /// arithmetic operators
  ScriptVar addition(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  /// arithmetic operators
  ScriptVar subtraction(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  /// arithmetic operators
  ScriptVar multiply(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  /// arithmetic operators
  ScriptVar divide(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  /// arithmetic operators
  ScriptVar modulo(const  ScriptVar& other, short castFlags = SVCastStdFlags) const; // %
  /**
    Test for equality
    For basic type must has same value
    For object types equals() has to return true
  */
  ScriptVar equal(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  /**
    Test for same
    For basic type must has same value
    For object types has to reference to same object
  */
  ScriptVar same(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  /**
    return false if this object is not equal to other object. 
    if contained value is a object type it calls equals on this value
  */
  ScriptVar not_equal(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  /**
    return true if this is greater than the argument
  */
  ScriptVar greater_than(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  /**
    return true if this is less than the argument
  */
  ScriptVar less_than(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  ScriptVar greater_or_equal(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  ScriptVar less_or_equal(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  bool isTrue(short castFlags = SVCastStdFlags) const;
  ScriptVar logical_and(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  ScriptVar logical_not(short castFlags = SVCastStdFlags) const;
  ScriptVar logical_or(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  ScriptVar logical_xor(const ScriptVar& other, short castFlags = SVCastStdFlags) const;

  ScriptVar binary_and(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  ScriptVar binary_or(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  ScriptVar binary_xor(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  ScriptVar binary_leftshift(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  ScriptVar binary_rightshift(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  ScriptVar binary_rightshift_unsigned(const ScriptVar& other, short castFlags = SVCastStdFlags) const;
  ScriptVar binary_not(short castFlags = SVCastStdFlags) const;
  /**
    only valid for number.
    return switched signed value
  */
  ScriptVar negation(short castFlags = SVCastStdFlags) const;

  /**
    returns the type name is ACDK coding
    <li> bool
    <li> ::acdk::lang::RString
  */
  RString getTypeInfo() const;
  /**
    internal helper to retrive value of given type
  */
  template <class T>
  T getBasicType(const T& dummy) const
  {
    if (typeid(T) == typeid(bool))
      return (T)getBoolVar();
    else if (typeid(T) == typeid(char))
      return (T)getCharVar();
    else if (typeid(T) == typeid(byte))
      return (T)getByteVar();
    else if (typeid(T) == typeid(short))
      return (T)getShortVar();
    else if (typeid(T) == typeid(int))
      return (T)getIntVar();
    else if (typeid(T) == typeid(long))
      return (T)getIntVar();
    else if (typeid(T) == typeid(jlong))
      return (T)getLongVar();
    else if (typeid(T) == typeid(float))
      return (T)getFloatVar();
    else if (typeid(T) == typeid(double))
      return (T)getDoubleVar();
    return 0;
  }
  /** 
    assign given object to this 
  */
  void assign(Object* obj, short castFlags = SVCastStdFlags);
  /** 
    assign given object to this 
  */
  void assign(IN(RObject) obj, short castFlags = SVCastStdFlags) { assign(&obj, castFlags); }
  /** 
    assignment operator
  */
  ScriptVar& operator=(const ScriptVar& o);
  /**
    reset the script var to as void/UnknownType scriptvar
  */
  void reset();
  /**
    This operator throws Exception if type is object var or reference
  */
  Object* operator->();

  /**
    Converts the value to string for debugging purposes
  */
  RString toString() const;

  /**
    the value as code constants
    <li> 123
    <li> true
    <li> "This is a Text"
  */
  RString toCode() const;

  /**
    Release internal recouces
  */
  void dispose();

  /** used for templates */
  void get(bool& b) const { b = getBoolVar();  }

  void get(char& b) const { b = getCharVar();  }
  void get(ucchar& b) const { b = getUcCharVar();  }
  void get(byte& b) const { b = getByteVar();  }
  void get(short& b) const { b = getShortVar();  }
  void get(int& b) const { b = getIntVar();  }
  void get(jlong& b) const { b = getLongVar();  }
  void get(float& b) const { b = getFloatVar();  }
  void get(double& b) const { b = getDoubleVar();  }
  void get(RObject& b) const;

  // operator RObject and other will implemented by constructor of RefHolder<T>(const ScriptVar&)

  operator bool() const { return getBoolVar(); }
  operator char() const { return getCharVar(); }
  operator ucchar() const { return getUcCharVar(); }
  operator byte() const { return getByteVar(); }
  operator short() const { return getShortVar(); }
  operator int() const { return getIntVar(); }
  operator jlong() const { return getLongVar(); }
  operator float() const { return getFloatVar(); }
  operator double() const { return getDoubleVar(); }
  /**
    return a copy of this.
    In case this holds a reference, the contained value
    will be copied to a value.
  */
  ScriptVar inOf() const;
  /**
    return this as reference type
    @note this instance has to be hold as long also the
          reference exits, otherwise crash!
  */
  ScriptVar outOf();
  /**
    return this as reference type
    @note this instance has to be hold as long also the
          reference exits, otherwise crash!
  */
  ScriptVar inoutOf();

  /**
    Throws DmiTypeConversionException. parameter wanted and existant are
    not checked, only used to error message
   */
  static void _throwWrongType(const ClazzInfo* wanted, const ClazzInfo* existant);
  /**
    Cast this variable to another
    if it is is an object, use the _cast() operator
    of the object to cast to requested type
  */
  ScriptVar _castScriptVar(const ClazzInfo* ci, short castFlags = SVCastStdFlags) const;
protected:
  /**  throws DmiTypeConversionException if type is not matching exaclty */
  inline void _checkType(Type tp) const
  {
    if (type != tp)
      _throwWrongType(tp);
  }
  /** throws a DmiTypeConversionException */
  void _throwWrongType(Type wanted) const;

  /**
    @return "(int, ::acdk::lang::StringBuffer)"
  */
  static RString getAsCodeArguments(ScriptVarArray& args);
  void _init(Object* obj);
  void _init(RObject* obj);
  void _initScriptVar(const ScriptVar& o);
  RObject& _getrobject();
  const RObject& _getrobject() const;
  inline void _setrobject(Object* o);
  inline void _setrobject(IN(RObject) o);
  inline void _deleterobject();
  inline void _checkCastNum2Bool(short castFlags) const
  {
    if ((castFlags & SVCastNum2Bool) == 0)
      _throwWrongType(CharType);
  }
  inline void _checkCastBool2Num(short castFlags, Type requestedType) const
  {
    if ((castFlags & SVCastBool2Number) == 0)
      _throwWrongType(requestedType);
  }
  
  inline void _checkCastChar2Int(short castFlags, Type requestedType) const
  {
    if ((castFlags & SVCastSVCastChar2Int) == 0)
      _throwWrongType(requestedType);
  }
  inline void _checkCastInt2Float(short castFlags, Type requestedType) const
  {
    if ((castFlags & SVCastInt2Float) == 0)
      _throwWrongType(requestedType);
  }
  inline void _checkFloat2Char(short castFlags, Type requestedType) const
  {
    _checkCastChar2Int(castFlags, requestedType);
    _checkCastInt2Float(castFlags, requestedType);
  }
  inline void _checkCastAutoboxing(short castFlags, Type requestedType) const
  {
    if ((castFlags & SVCastAutobox) == 0)
      _throwWrongType(requestedType);
  }
 
  /*
  inline void _checkOverflow(short castFlags, Type requestedType, jlong val, jlong max) const
  {
    if ((castFlags & SVCastCheckOvervflow) == 0 || val <= max)
      return;
    _throwOverflow(requestedType, val, max);
  }
  inline void _checkOverflow(short castFlags, Type requestedType, double val, double max) const
  {
    if ((castFlags & SVCastCheckOvervflow) == 0 || val <= max)
      return;
    _throwOverflow(requestedType, val, max);
  }*/
  inline bool _getBool() const { return type == BoolRefType ? *var.boolRef : var.boolVal; }
  inline char _getChar() const { return type == CharRefType ? *var.charRef : var.charVal; }
  inline ucchar _getUcChar() const { return type == UcCharRefType ? *var.uccharRef : var.uccharVal; }
  inline byte _getByte() const { return type == ByteRefType ? *var.byteRef : var.byteVal; }
  inline short _getShort() const { return type == ShortRefType ? *var.shortRef : var.shortVal; }
  inline int _getInt() const { return type == IntRefType ? *var.intRef : var.intVal; }
  inline jlong _getLong() const { return type == LongRefType ? *var.longRef : var.longVal; }
  inline float _getFloat() const { return type == FloatRefType ? *var.floatRef : var.floatVal; }
  inline float _getDouble() const { return type == DoubleRefType ? *var.doubleRef : var.doubleVal; }
  inline RObject _getObject() const { return type == ObjectRefType ? *var.oref.objectRef : _getrobject(); }
};

#if !defined(DOXYGENONLY)

typedef const ::acdk::lang::dmi::ClazzMethodInfo* (*StandardDispatchFunction)(IN(acdk::lang::RString) fname,
                                                         ::acdk::lang::dmi::ScriptVar& ret,
                                                         ::acdk::lang::dmi::ScriptVarArray& args,
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf);
typedef const ::acdk::lang::dmi::ClazzMethodInfo* (*DynamicDispatchFunction)(
                                                        ::acdk::lang::Object* This,
                                                         IN(acdk::lang::RString) fname,
                                                         ::acdk::lang::dmi::ScriptVar& ret,
                                                         ::acdk::lang::dmi::ScriptVarArray& args,
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf);

/**
  to access a field.
  @param This may be 0 in case of static members
  @param flags Modifier::ReadClazzField reads the field otherwise writes the field
               Modifier::
  @internal
*/
typedef const ::acdk::lang::dmi::ClazzFieldInfo* (*FieldAccessorFunction)(
                                    ::acdk::lang::Object* This,
                                      IN(acdk::lang::RString) fname,
                                      ::acdk::lang::dmi::ScriptVar& var,
                                      ::acdk::lang::dmi::DmiClient& dc,
                                      int flags,
                                      const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                      const ::acdk::lang::dmi::ClazzFieldInfo* fieldinf);

/**
   for template code
   @internal
*/
inline void get(const ScriptVar& sv, bool& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = sv.getBoolVar(castFlags);  }
/// @internal
inline void get(const ScriptVar& sv, char& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = sv.getCharVar(castFlags);  }
/// @internal
inline void get(const ScriptVar& sv, ucchar& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = sv.getUcCharVar(castFlags);  }
/// @internal
inline void get(const ScriptVar& sv, byte& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = sv.getByteVar(castFlags);  }
/// @internal
inline void get(const ScriptVar& sv, short& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = sv.getShortVar(castFlags);  }
/// @internal
inline void get(const ScriptVar& sv, int& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = sv.getIntVar(castFlags);  }
/// @internal
inline void get(const ScriptVar& sv, jlong& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = sv.getLongVar(castFlags);  }
/// @internal
inline void get(const ScriptVar& sv, float& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = sv.getFloatVar(castFlags);  }
/// @internal
inline void get(const ScriptVar& sv, double& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = sv.getDoubleVar();  }
/// @internal
template <class T> inline void get(const ScriptVar& sv, RefHolder<T>& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = (RefHolder<T>)sv.getObjectVar(castFlags, type);  }
/// @internal
template <class T> inline void get(const ScriptVar& sv, InterfaceHolder<T>& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) { b = (InterfaceHolder<T>)sv.getObjectVar(castFlags, type);  }
template <class T> inline void get(const ScriptVar& sv, RObjectArrayImpl<T>& b, short castFlags = SVCastStdFlags, const ::acdk::lang::dmi::ClazzInfo* type = 0) 
{ b = (RObjectArrayImpl<T>)sv.getObjectVar(castFlags, type);  }

/// @internal
inline void set(ScriptVar& sv, bool b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, char b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, ucchar b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, byte b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, short b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, int b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, jlong b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, float b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, double b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, bool* b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, char* b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, byte* b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, short* b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, int* b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, jlong* b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, float* b) { sv = b;  }
/// @internal
inline void set(ScriptVar& sv, double* b) { sv = b;  }
/// @internal
template <class T> inline void set(ScriptVar& sv, RefHolder<T>& b) { sv = (RObject)b;  }
/// @internal
template <class T> inline void set(ScriptVar& sv, InterfaceHolder<T>& b) { sv = (RObject)b.impl();  }
/// @internal
template <class T> inline void set(ScriptVar& sv, RefHolder<T>* b) { sv = reinterpret_cast<RObject*>(b);  }
/// @internal
template <class T> inline void set(ScriptVar& sv, InterfaceHolder<T>* b) { sv = reinterpret_cast<RObject*>(b);  }




#endif //!defined(DOXYGENONLY)

} // dmi


/**
  cast a type to the 'any' Type ScriptVar as input variable
  on a given value type the corresponding reference type.
  
  @ingroup acdkkeywords
*/
template <class T>
::acdk::lang::dmi::ScriptVar inOf(T* t)
{
  return ::acdk::lang::dmi::ScriptVar(::acdk::lang::RObject(t), ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable
  Additionally the held type should be from type cls
  @ingroup acdkkeywords
*/
template <class T>
inline
::acdk::lang::dmi::ScriptVar inOfAs(T* t, IN(::acdk::lang::RClass) cls);

/**
  cast a type to the 'any' Type ScriptVar as input variable
  @ingroup acdkkeywords
*/
template <typename T>
inline
::acdk::lang::dmi::ScriptVar inOf(const RefHolder<T>& t)
{
  return ::acdk::lang::dmi::ScriptVar(RObject(t),
                   ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  Additionally the held type should be from type cls
  @ingroup acdkkeywords
*/
template <typename T>
inline
::acdk::lang::dmi::ScriptVar inOfAs(const RefHolder<T>& t, IN(::acdk::lang::RClass) cls);

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  Additionally the held type should be from type cls
  @ingroup acdkkeywords
*/
template <typename T>
inline
::acdk::lang::dmi::ScriptVar inOf(const InterfaceHolder<T>& t)
{
  return ::acdk::lang::dmi::ScriptVar(RObject(t), ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  Additionally the held type should be from type cls
  @ingroup acdkkeywords
*/
template <typename T>
inline
::acdk::lang::dmi::ScriptVar inOfAs(const InterfaceHolder<T>& t, IN(::acdk::lang::RClass) cls);

template <typename T, typename S>
inline
::acdk::lang::dmi::ScriptVar inOf(const ThrowableHolder<T, S>& t)
{
  return ::acdk::lang::dmi::ScriptVar(RObject(t), ::acdk::lang::dmi::MiAiIn);
}
/**
  cast a type to the 'any' Type ScriptVar as input variable.
  Additionally the held type should be from type cls
  @ingroup acdkkeywords
*/
template <typename T, typename S>
inline
::acdk::lang::dmi::ScriptVar inOfAs(const ThrowableHolder<T, S>& t,  IN(::acdk::lang::RClass) cls);

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inOf(bool t)
{
  return ::acdk::lang::dmi::ScriptVar(t, ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inOf(char t)
{
  return ::acdk::lang::dmi::ScriptVar(t, ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inOf(ucchar t)
{
  return ::acdk::lang::dmi::ScriptVar(t, ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inOf(byte t)
{
  return ::acdk::lang::dmi::ScriptVar(t, ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inOf(short t)
{
  return ::acdk::lang::dmi::ScriptVar(t, ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inOf(int t)
{
  return ::acdk::lang::dmi::ScriptVar(t, ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inOf(jlong t)
{
  return ::acdk::lang::dmi::ScriptVar(t, ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inOf(float t)
{
  return ::acdk::lang::dmi::ScriptVar(t, ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inOf(double t)
{
  return ::acdk::lang::dmi::ScriptVar(t, ::acdk::lang::dmi::MiAiIn);
}

/**
  cast a type to the 'any' Type ScriptVar as input variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inOf(::acdk::lang::dmi::ScriptVar& t)
{
  return t.inOf();
}


/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
template <typename T>
inline
::acdk::lang::dmi::ScriptVar outOf(const RefHolder<T>& t)
{
  return ::acdk::lang::dmi::ScriptVar((::acdk::lang::RObject*)const_cast<RefHolder<T>& >(t)._ref_this(),
                   ::acdk::lang::dmi::MiAiOut);
}

// implementation see in Class.h
/**
  cast a type to the 'any' Type ScriptVar as output variable.
  Additionally the held type should be from type cls
  @ingroup acdkkeywords
*/
template <typename T>
inline
::acdk::lang::dmi::ScriptVar outOfAs(const RefHolder<T>& t,  IN(::acdk::lang::RClass) cls);

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
template <typename T>
inline
::acdk::lang::dmi::ScriptVar outOf(const InterfaceHolder<T>& t)
{
  return ::acdk::lang::dmi::ScriptVar((::acdk::lang::RObject*)const_cast<InterfaceHolder<T>& >(t)._ref_this(),
                   ::acdk::lang::dmi::MiAiOut);
}

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  Additionally the held type should be from type cls
  @ingroup acdkkeywords
*/
template <typename T>
inline
::acdk::lang::dmi::ScriptVar outOfAs(const InterfaceHolder<T>& t,  IN(::acdk::lang::RClass) cls);

// ### continue out of as

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
template <typename T, typename S>
inline
::acdk::lang::dmi::ScriptVar outOf(const ThrowableHolder<T, S>& t)
{
  return ::acdk::lang::dmi::ScriptVar((::acdk::lang::RObject*)const_cast<ThrowableHolder<T, S>& >(t)._ref_this(),
                   ::acdk::lang::dmi::MiAiOut);
}

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar outOf(bool& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut);
}

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar outOf(char& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut);
}

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar outOf(ucchar& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut);
}

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar outOf(byte& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut);
}

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar outOf(short& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut);
}


/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar outOf(int& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut);
}

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar outOf(jlong& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut);
}

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar outOf(float& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut);
}

/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar outOf(double& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut);
}


/**
  cast a type to the 'any' Type ScriptVar as output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar outOf(::acdk::lang::dmi::ScriptVar& t)
{
  return t.outOf();
}
/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
template <typename T>
inline
::acdk::lang::dmi::ScriptVar inoutOf(const RefHolder<T>& t)
{
  return ::acdk::lang::dmi::ScriptVar((::acdk::lang::RObject*)const_cast<RefHolder<T>& >(t)._ref_this(),
                   ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}
/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
template <typename T>
inline
::acdk::lang::dmi::ScriptVar inoutOf(const InterfaceHolder<T>& t)
{
  return ::acdk::lang::dmi::ScriptVar((::acdk::lang::RObject*)const_cast<InterfaceHolder<T>& >(t)._ref_this(),
                   ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}

/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
template <typename T, typename S>
inline
::acdk::lang::dmi::ScriptVar inoutOf(const ThrowableHolder<T, S>& t)
{
  return ::acdk::lang::dmi::ScriptVar((::acdk::lang::RObject*)const_cast<ThrowableHolder<T, S>& >(t)._ref_this(),
                   ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}

/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inoutOf(bool& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}

/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inoutOf(char& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}

/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inoutOf(ucchar& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}
/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inoutOf(byte& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}

/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inoutOf(short& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}

/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inoutOf(int& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}

/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inoutOf(jlong& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}
/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inoutOf(float& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}

/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inoutOf(double& t)
{
  return ::acdk::lang::dmi::ScriptVar(&t, ::acdk::lang::dmi::MiAiOut | ::acdk::lang::dmi::MiAiIn);
}

/**
  on a given type to the 'any' Type ScriptVar as input/output variable.
  @ingroup acdkkeywords
*/
inline ::acdk::lang::dmi::ScriptVar inoutOf(::acdk::lang::dmi::ScriptVar& t)
{
  return t.inoutOf();
}

/// @internal
template <class T>
inline ::acdk::lang::dmi::ScriptVar getScriptVarOf(T& t, int flags)
{
  if (flags & ::acdk::lang::dmi::MiAiOut)
  {
    if (flags & ::acdk::lang::dmi::MiAiIn)
      return inoutOf(t);
    return outOf(t);
  }
  return inOf(t);
}

/// @internal
template <class T>
inline ::acdk::lang::dmi::ScriptVar getScriptVarOf(T& t)
{
  return inOf(t);
}
/// @internal
template <>
inline ::acdk::lang::dmi::ScriptVar getScriptVarOf< ::acdk::lang::dmi::ScriptVar>(::acdk::lang::dmi::ScriptVar& t)
{
  return t;
}

/**
  convert the given type to the 'any' type ScriptVar with the ByValue flag set
  @ingroup acdkkeywords
*/
template <class T>
inline ::acdk::lang::dmi::ScriptVar byVal(T& t)
{
  ::acdk::lang::dmi::ScriptVar ret = getScriptVarOf(t);
  return ::acdk::lang::dmi::ScriptVar(ret, (ret.flags | ::acdk::lang::dmi::MiAiByval) & ~::acdk::lang::dmi::MiAiByref, ret._clazzType);
}

/**
  convert the given type to the 'any' type ScriptVar with the ByRef flag set
  @ingroup acdkkeywords
*/
template <class T>
inline ::acdk::lang::dmi::ScriptVar byRef(T& t)
{
  ::acdk::lang::dmi::ScriptVar ret = getScriptVarOf(t);
  return ::acdk::lang::dmi::ScriptVar(ret, (ret.flags | ::acdk::lang::dmi::MiAiByref) & ~::acdk::lang::dmi::MiAiByval, ret._clazzType);
}


} // lang
} // acdk

#endif // acdk_lang_dmi_ScriptVar_h

