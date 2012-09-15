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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/jniext.h,v 1.17 2005/04/25 13:20:46 kommer Exp $

#ifndef acdk_java_jniext_h
#define acdk_java_jniext_h

#if !defined(DOXYGENONLY)

#include <acdk.h>

#include <jni.h>
#include <acdk/java/JavaInterpreter.h>

namespace acdk {
namespace java {

RObject jobject2aobject(JNIEnv *env, jobject jobj);



// only use this as stack
#define S2JS(str) str->convert(CCUtf8)->c_str()

void handleJException(JNIEnv* env);
void mapException(JNIEnv* env, IN(RThrowable) ex);

inline
void checkExceptions(JNIEnv* env)
{
  if (env->ExceptionCheck() == 0)
    return;
  handleJException(env);
}

bool isString(JNIEnv* env, jobject obj);

class JLocalRes
{
public:
  template <class T> 
  static T init(JNIEnv* env, const T& t)
  {
    return t;
  }
  template <class T> 
  static T duplicate(JNIEnv* env, const T& t)
  {
    //return static_cast<T>(env->NewLocalRef(t));
    return t;
  }
  template <class T> 
  static void release(JNIEnv* env, const T& t)
  {
    //env->DeleteLocalRef(t);
  }
};

class JGlobalRes
{
public:
  template <class T> 
  static T init(JNIEnv* env, const T& t)
  {
    return duplicate(env, t);
  }
  template <class T> 
  static T duplicate(JNIEnv* env, const T& t)
  {
    if (t == 0)
      return t;
    return static_cast<T>(env->NewGlobalRef(t));
  }
  template <class T> 
  static void release(JNIEnv* env, const T& t)
  {
    if (t != 0)
      env->DeleteGlobalRef(t);
  }
};

template <class T, class R = JLocalRes>
class JObjectImpl
{
protected:
  //JNIEnv* _env;
  T _t;
public:
  typedef R JRes;
  JObjectImpl(JNIEnv* env, const T& t = 0)
  : _t(R::init(env, t))
  {
  }
  JObjectImpl(const JObjectImpl<T, R>& t)
  : _t(0)
  {
    if (t._t == 0)
      return;
    _t = JRes::duplicate(getjenv(), t.impl());
    //_t = _env->NewLocalRef(t._t);
  }

  virtual ~JObjectImpl()
  {
    dispose();
  }
  JObjectImpl<T, R>& operator=(const T& t)
  {
    if (_t == t)
      return *this;
    dispose();
    _t = JRes::duplicate(getjenv(), t);
    //_t = t;
    return *this;
  }
  void dispose()
  {
    if (_t)
    {
      JRes::release(getjenv(), _t);
      _t = 0;
    }
  }
  const T& impl() const { return _t; }
  operator T () { return _t; }
};

typedef JObjectImpl<jobject> JObject;

template <class R = JLocalRes> 
class JStringImpl
: public JObjectImpl<jstring, R>
{
protected:
  mutable jboolean _isCopy;
  mutable RString _str;
  mutable const char* _cptr;
public:
  typedef JObjectImpl<jstring, R> Super;
  JStringImpl(JNIEnv* env, const jstring& t = 0)
  : Super(env, t)
  , _isCopy(false)
  , _str(Nil)
  , _cptr(0)
  {
  }
  JStringImpl(const JStringImpl& other)
  : Super(other)
  , _isCopy(false)
  , _str(Nil)
  , _cptr(0)
  {
  }
  JStringImpl(JNIEnv* env, IN(RString) str)
  : Super(env)
  , _isCopy(false)
  , _str(Nil)
  , _cptr(0)
  {
    _t = env->NewStringUTF(S2JS(str));
  }
  JStringImpl<R>& operator=(jstring jstr)
  {
    if (_t == jstr)
      return *this;
    Super::operator=(jstr);
    clearString();
    return *this;
  }
  JStringImpl<R>& operator=(const JStringImpl<R>& other)
  {
    if (_t == other._t)
      return *this;
    Super::operator=(other);
    clearString();
    return *this;
  }
  JStringImpl<R>& operator=(const char* cstr)
  {
    dispose();
    clearString();
    _t = getjenv()->NewStringUTF(cstr);
    return *this;
  }
  JStringImpl<R>& operator=(IN(RString) str)
  {
    return operator=(S2JS(str));
  }
  ~JStringImpl()
  {
    clearString();
  }
  int length() const
  {
    return getjenv()->GetStringUTFLength(_t);
  }
  
  operator const char* () const 
  {
    return cstr();
  }
  ::acdk::lang::RString toString() const
  {
    if (_str == Nil)
      _str = new ::acdk::lang::String(cstr(), CCUtf8 | NormalSST);
    return _str;
  }
  
  bool equals(const char* other) const
  {
    return toString()->equals(other);
  }
  bool equals(IN(RString) other) const 
  {
    return toString()->equals(other); 
  }
  void clearString()
  {
    //if (_isCopy == true) // always free the ptr;
   getjenv()->ReleaseStringUTFChars(_t, _cptr);
    _isCopy = false;
    _str = Nil;
  }
private:
  const char* cstr() const
  {
    if (_cptr != 0)
      return _cptr;
    if (_t == 0)
      return "";
    _cptr = getjenv()->GetStringUTFChars(_t, &_isCopy);
    return _cptr;
  }
};


typedef JStringImpl<JLocalRes> JString;

template <class R = JLocalRes> 
class JObjectArrayImpl
: public JObjectImpl<jobjectArray, R>
{
public:
  typedef JObjectArrayImpl<R> ThisClass;
  typedef JObjectImpl<jobjectArray, R> Super;
  JObjectArrayImpl(JNIEnv* env, const jobjectArray& t = 0)
  : Super(env, t)
  {
  }
  JObjectArrayImpl(const JObjectArrayImpl<R>& other)
  : Super(other)
  {
  }
  ThisClass& operator=(const ThisClass& other)
  {
    Super::operator=(other);
    return *this;
  }
  ThisClass operator=(jobjectArray other)
  {
    Super::operator=(other);
    return *this;
  }
  int length() const 
  {
    int len = getjenv()->GetArrayLength(_t);
    checkExceptions(getjenv());
    return len;
  }
  int size() const { return length(); }

  JObject operator[](int idx) 
  {
    JObject o(getjenv());
    o = getjenv()->GetObjectArrayElement(_t, idx);
    checkExceptions(getjenv());
    return o;
  }
};

typedef JObjectArrayImpl<JLocalRes> JObjectArray;

RString Class_getName(JNIEnv* env, jobject t);
jobject Class_getField(JNIEnv* env, jobject t, IN(RString) name);

template <class R = JLocalRes> 
class JClassImpl
: public JObjectImpl<jclass, R>
{
public:
  typedef JClassImpl<R> ThisClass;
  typedef JObjectImpl<jclass, R> Super;
  JClassImpl(JNIEnv* env, const jclass& t = 0)
  : Super(env, t)
  {
  }
  JClassImpl(const JClassImpl<R>& other)
  : Super(other)
  {
  }
  static JClassImpl getObjectClass(JNIEnv* env, jobject obj)
  {
    JClassImpl ret(env);
    ret = env->GetObjectClass(obj);
    if (ret == 0)
      checkExceptions(env);
    return ret;
  }
  
  static JClassImpl findClass(JNIEnv* env, const char* classname)
  {
    JClassImpl ret(env);
    ret = env->FindClass(classname);
    if (ret == 0)
      checkExceptions(env);
    return ret;
  }
  static JClassImpl findClass(JNIEnv* env, IN(RString) classname)
  {
    return findClass(env, S2JS(classname));
  }
  ThisClass& operator=(const ThisClass& other)
  {
    Super::operator=(other);
    return *this;
  }
  ThisClass operator=(jclass other)
  {
    Super::operator=(other);
    return *this;
  }
  RString getName()
  {
    return  Class_getName(getjenv(), _t);
  }
  jobject getField(IN(RString) fieldname)
  {
    return Class_getField(getjenv(), _t, fieldname);
  }
};


typedef JClassImpl<JLocalRes> JClass;

typedef JObjectImpl<jthrowable> JThrowable;

class JMethod
{
  jmethodID _methodId;
public:
  static JMethod constructor(JNIEnv* env, jclass jcls, IN(RString) signature)
  {
    return JMethod(env, jcls, "<init>", S2JS(signature), true);
  }
  JMethod(JNIEnv* env, jclass jcls, IN(RString) name, IN(RString) signature, bool useasClass)
  : _methodId(0)
  {
    _methodId = env->GetMethodID(jcls, S2JS(name), S2JS(signature));
    if (_methodId == 0)
      checkExceptions(env);
  }
  JMethod(JNIEnv* env, jobject jobj, IN(RString) name, IN(RString) signature)
  :_methodId(0)
  {
    _methodId = env->GetMethodID(
                          env->GetObjectClass(jobj), S2JS(name), S2JS(signature));
    if (_methodId == 0)
      checkExceptions(env);
  }
  bool callBooleanMethod(jobject jobj, ...) 
  {
    va_list args;
	  va_start(args, jobj);
	  bool result = getjenv()->CallBooleanMethodV(jobj, _methodId, args);
	  va_end(args);
	  return result;
  }
  char callCharMethod(jobject jobj, ...) 
  {
    va_list args;
	  va_start(args, jobj);
	  char result = (char)getjenv()->CallCharMethodV(jobj, _methodId, args);
	  va_end(args);
	  return result;
  }
  byte callByteMethod(jobject jobj, ...) 
  {
    va_list args;
	  va_start(args, jobj);
	  byte result = getjenv()->CallByteMethodV(jobj, _methodId, args);
	  va_end(args);
	  return result;
  }
  short callShortMethod(jobject jobj, ...) 
  {
    va_list args;
	  va_start(args, jobj);
	  short result = getjenv()->CallShortMethodV(jobj, _methodId, args);
	  va_end(args);
	  return result;
  }
  int callIntMethod(jobject jobj, ...) 
  {
    va_list args;
	  va_start(args, jobj);
	  int result = getjenv()->CallIntMethodV(jobj, _methodId, args);
	  va_end(args);
	  return result;
  }
  jlong callLongMethod(jobject jobj, ...) 
  {
    va_list args;
	  va_start(args, jobj);
	  jlong result = getjenv()->CallLongMethodV(jobj, _methodId, args);
	  va_end(args);
	  return result;
  }
  float callFloatMethod(jobject jobj, ...) 
  {
    va_list args;
	  va_start(args, jobj);
	  float result = getjenv()->CallFloatMethodV(jobj, _methodId, args);
	  va_end(args);
	  return result;
  }
  double callDoubleMethod(jobject jobj, ...) 
  {
    va_list args;
	  va_start(args, jobj);
	  double result = getjenv()->CallDoubleMethodV(jobj, _methodId, args);
	  va_end(args);
	  return result;
  }
   jobject callObjectMethod(jobject jobj, ...) 
  {
    va_list args;
	  jobject result;
	  va_start(args, jobj);
	  result = getjenv()->CallObjectMethodV(jobj, _methodId, args);
	  va_end(args);
	  return result;
  }
  RString callStringMethod(jobject jobj, ...) 
  {
    va_list args;
	  jobject result;
	  va_start(args, jobj);
	  result = getjenv()->CallObjectMethodV(jobj, _methodId, args);
	  va_end(args);
    return (RString)::acdk::java::jobject2aobject(getjenv(), result);
  }
  jmethodID methodId() { return _methodId; }
};

#define JFIELD_ACCESS(ctype, jtype) \
  ctype getStatic##jtype(jclass cls) \
  { \
    ctype ret = getjenv()->GetStatic##jtype##Field(cls, _fieldId); \
    checkExceptions(getjenv()); \
    return ret; \
  } \
  void setStatic##jtype(jclass cls, ctype v)  \
  { \
    getjenv()->SetStatic##jtype##Field(cls, _fieldId, v); \
    checkExceptions(getjenv()); \
  } \
  ctype get##jtype(jobject jobj) \
  { \
    ctype ret = getjenv()->Get##jtype##Field(jobj, _fieldId); \
    checkExceptions(getjenv()); \
    return ret; \
  } \
  void set##jtype(jobject cls, ctype v)  \
  { \
    getjenv()->Set##jtype##Field(cls, _fieldId, v); \
    checkExceptions(getjenv()); \
  } \


class JField
{
  //JNIEnv* _env; 
  jfieldID _fieldId;
public:
  JField(JNIEnv* env, jclass cls, IN(RString) name, IN(RString) signature, bool isstatic = false)
  : _fieldId(0)
  {
    if (isstatic == true)
      _fieldId = env->GetStaticFieldID(cls, S2JS(name), S2JS(signature));
    else
      _fieldId = env->GetFieldID(cls, S2JS(name), S2JS(signature));
    checkExceptions(env);
  }
  jfieldID fieldId() { return _fieldId; }
  
  JFIELD_ACCESS(bool, Boolean)
  JFIELD_ACCESS(char, Char)
  JFIELD_ACCESS(byte, Byte)
  JFIELD_ACCESS(short, Short)
  JFIELD_ACCESS(int, Int)
  JFIELD_ACCESS(jlong, Long)
  JFIELD_ACCESS(float, Float)
  JFIELD_ACCESS(double, Double)
  JFIELD_ACCESS(jobject, Object)
  
};

#undef JFIELD_ACCESS

class JStackFrame 
{
  JNIEnv* _env; 
public:
  JStackFrame(JNIEnv* env, int capacity = 32)
  : _env(env)
  {
    //#ifndef ACDK_OS_LINUX
    if (_env->PushLocalFrame(capacity) != 0)
      checkExceptions(_env);
    //#endif
  }
  ~JStackFrame()
  {
    //#ifndef ACDK_OS_LINUX
    _env->PopLocalFrame(0);
    //#endif
  }
};

}
}

#endif //!defined(DOXYGENONLY)
#endif //acdk_java_jniext_h

