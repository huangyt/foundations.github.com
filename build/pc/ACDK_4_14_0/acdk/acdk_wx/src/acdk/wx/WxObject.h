// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*-
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/WxObject.h,v 1.23 2005/03/31 16:31:26 kommer Exp $

#ifndef acdk_wx_WxObject_h
#define acdk_wx_WxObject_h

#include "Config.h"

#include <acdk.h>
#include <malloc.h>

#if defined(_MSC_VER)
# pragma warning(disable:4355)
#endif

namespace acdk {
namespace wx {


//#define WX_LOCAL_DEBUG

#if defined(WX_LOCAL_DEBUG)
#define WXDOUT(msg) do { sys::coreout << msg << sys::eofl; } while(false)
#else
#define WXDOUT(msg) do { } while(false)
#endif

inline uc2char* astr_to_ucs2(uc2char* buffer, IN(RString) str)
{
  String::iterator it = str->begin();
  String::iterator end = str->end();
  uc2char* tptr;
  for (tptr = buffer; it < end; ++it, ++tptr)
    *tptr = *it;
  *tptr = 0;
  return buffer;
}

inline uc4char* astr_to_ucs4(uc4char* buffer, IN(RString) str)
{
  String::iterator it = str->begin();
  String::iterator end = str->end();
  uc4char* tptr;
  for (tptr = buffer; it < end; ++it, ++tptr)
    *tptr = *it;
  *tptr = 0;
  return buffer;
}

inline char* astr_to_char(char* buffer, IN(RString) str)
{
  String::iterator it = str->begin();
  String::iterator end = str->end();
  char* tptr;
  for (tptr = buffer; it < end; ++it, ++tptr)
    *tptr = *it;
  *tptr = 0;
  return buffer;
}


#define ASTRING_TO_STK_UCS2(str) astr_to_ucs2((uc2char*)core_alloca((str->length() + 1) * sizeof(uc2char)), str)
#define ASTRING_TO_STK_UCS4(str) astr_to_ucs4((uc4char*)core_alloca((str->length() + 1) * sizeof(uc4char)), str)
#define ASTRING_TO_STK_CHAR(str) astr_to_char((char*)core_alloca((str->length() + 1)), str)

#if defined(ACDK_USE_WX_UNICODE)
# if defined(ACDK_OS_WIN32)
#  define WXS2S(wxstr) (new String(wxstr.c_str()))
#  define S2WXS(str) (wxString((wxChar*)ASTRING_TO_STK_UCS2(str)))
#  define WXCPTR2S(wxCharPtr) SCS(wxCharPtr)
# else
#  define WXS2S(wxstr) (new String(wxstr.c_str()))
#  define S2WXS(str) (wxString((wxChar*)ASTRING_TO_STK_UCS4(str)))
#  define WXCPTR2S(wxCharPtr) SCS((uc4char*)wxCharPtr)
# endif

#else // No unicode
# define WXS2S(wxstr) SCS(wxstr.c_str())
# define S2WXS(str) (wxString((wxChar*)ASTRING_TO_STK_CHAR(str)))
# define WXCPTR2S(wxCharPtr) SCS(wxCharPtr)
#endif



/**
  internal
*/
struct ACDK_WX_PUBLIC AcdkShadowClientDataObject
: public wxClientData
{
  //int magic;// = 0xacdaacda;
  acdk::lang::RObject shadow;
  acdk::lang::RObject clientDataObject;
  AcdkShadowClientDataObject(IN(acdk::lang::RObject) shadow_) 
  : shadow(shadow_)
  {
  }
  ~AcdkShadowClientDataObject()
  {
  }
};

template <class AcdkT, class wxT>
inline
AcdkT* createAcdkObjectFromWx(wxT* wxObj, bool owns)
{
  if (wxObj == 0)
    return 0;
  wxEvtHandler* wxHandler = dynamic_cast<wxEvtHandler*>(wxObj);
  if (wxHandler != 0)
  {
    wxClientData* cldata = wxHandler->GetClientObject();
    if (cldata != 0)
    {
      AcdkShadowClientDataObject* acshdw = dynamic_cast<AcdkShadowClientDataObject*>(cldata);
      if (acshdw != 0)
      {
        return (AcdkT*)&acshdw->shadow;
      }
    }
  }
  return new AcdkT(wxObj, owns);
}

//#define RETURN_WXPTR2CLS(Class, expr) wx##Class* tptr__ = (expr); if (tptr__ == 0) return Nil; return new Class(tptr__, false)
#define RETURN_WXPTR2CLS(Class, expr) return createAcdkObjectFromWx<Class, wx##Class>(expr, false)
/// will owns the return ptr
//#define RETURN_WXPTR2CLS_O(Class, expr) wx##Class* tptr__ = (expr); if (tptr__ == 0) return Nil; return new Class(tptr__, true)
#define RETURN_WXPTR2CLS_0(Class, expr) return createAcdkObjectFromWx<Class, wx##Class>(expr, true)

template <class T>
T& wxLoose(T& t) { t->ownsWxObject(false); return t; }

#define CLS2WXPTR(expr) ((expr) == Nil ? 0 : (expr)->getWx())
/// convert ACDK class to WX and loose ownership
#define CLS2WXPTR_L(expr) ((expr) == Nil ? 0 : (wxLoose(expr))->getWx())

#define CLS2WXREF(expr) ((expr)->toWx())
#define CLS2WXOUTREF(expr) ((expr)->toWx())

// convert wx ref to acdk. acdk does not own ptr
#define WXREF2CLS(expr) ::acdk::wx::fromWx(expr)
//#define WXVAL2CLS(Class, expr) new Class(expr)
#define WXVAL2CLS(Class, expr) new Class(expr) // don't try to create from wxEvent //createAcdkObjectFromWx<Class, wx##Class>(&expr, false)
//#define WXPTR2CLS(Class, expr) new Class(expr, false)
#define WXPTR2CLS(Class, expr) createAcdkObjectFromWx<Class, wx##Class>(expr, false)

#define WX_FWD_CLASS_1(ClassName, ConstrArg1) \
class ClassName##Fwd: public wx##ClassName \
{ \
public:\
  WX_FWD_CLASS_CONSTRUCTOR_DEFAULT(ClassName##Fwd) \
  WX_FWD_CLASS_CONSTRUCTOR_1(ClassName##Fwd, ConstrArg1) \
};

/*
#define ACDK_WX_STD_WX_ACCESSOR(ClassName) \
inline wx##ClassName* getWx() { _ownsWxObject = false; return static_cast<wx##ClassName*>(_wxObject); } \
inline const wx##ClassName* getWx() const { _ownsWxObject = false; return static_cast<const wx##ClassName*>(_wxObject); } \
inline wx##ClassName& toWx() { return *static_cast<wx##ClassName*>(_wxObject); } \
inline const wx##ClassName& toWx() const { return *static_cast<const wx##ClassName*>(_wxObject); }
*/

#define ACDK_WX_STD_WX_ACCESSOR(ClassName) \
inline wx##ClassName* getWx() { return static_cast<wx##ClassName*>(_wxObject); } \
inline const wx##ClassName* getWx() const { return static_cast<const wx##ClassName*>(_wxObject); } \
inline wx##ClassName& toWx() { return *static_cast<wx##ClassName*>(_wxObject); } \
inline const wx##ClassName& toWx() const { return *static_cast<const wx##ClassName*>(_wxObject); }

#define ACDK_WX_STD_MEMBERS(ClassName, SuperName) \
inline ClassName(wx##ClassName* wxobj, bool owns = false) : SuperName(wxobj, owns) { } \
inline ClassName(const wx##ClassName& wxobj, bool owns = false) : SuperName(wxobj, owns) { } \
ACDK_WX_STD_WX_ACCESSOR(ClassName)


#define ACDK_WX_STD_VAL_MEMBERS(ClassName, SuperName) \
inline ClassName(const wx##ClassName& wxobj) : SuperName(new wx##ClassName(wxobj), true) { } \
ACDK_WX_STD_WX_ACCESSOR(ClassName)


inline RString toWx(const wxString& wxs) { return WXS2S(wxs); }

/*
  {
    RWindow win = new Window();
    // win is owned by acdk
  }
  {
    RWindow win = new Window();
    other->addChild(win); // loose ownership
  } // win must not be destroyed, because is still owned by underlying wxWindow

*/
foreign
class ACDK_WX_PUBLIC WxForward
{
  bool _owns;
public:
  WxForward(bool owns = true) : _owns(owns) {}

  virtual ~WxForward()
  {
    releaseForward();
  }
  void releaseForward()
  {
    if (_owns == false)
    {
      acdk::lang::Object* obj = getObject();
      if (obj != 0)
        obj->releaseRef();
    }
  }
  virtual acdk::lang::Object* getObject() { return 0; }

  void ownsWxObject(bool owns)
  {
    if (owns == false && _owns == true)
    {
      getObject()->addRef();
      _owns = false;
    }
    else if (owns == true && _owns == false)
    {
      getObject()->releaseRef();
      _owns = true;
    }
  }
};


template <class T>
struct AcdkForwarder
{
  T* _forward;
  bool _ownsForward;
  AcdkForwarder()
    : _forward(0)
    , _ownsForward(false)
  {
  }
  ~AcdkForwarder()
  {
    if (_ownsForward == true && _forward != 0)
    {
      WXDOUT("Forwarder rc=" << _forward->refCount());
      _forward->releaseRef();
    }
  }
  void setOwningForward(T* t)
  {
    _ownsForward = true;
    _forward = t;
    _forward->addRef(true);
  }
};



ACDK_DECL_CLASS(WxObject);
/**
  see wxObject
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.23 $
  @date $Date: 2005/03/31 16:31:26 $
*/
class ACDK_WX_PUBLIC WxObject
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(WxObject)

protected:
  foreign wxObject* _wxObject;
  foreign WxForward* _forward;
  mutable bool _ownsWxObject;
  //RWxObject _castedObject;
public:
  foreign acdk::lang::Object* _cast(const acdk::lang::dmi::ClazzInfo* ci);
  WxObject() : _wxObject(0), _ownsWxObject(false) {}
  foreign WxObject(wxObject* obj, bool owns = true);
  foreign WxObject(const wxObject& obj, bool owns = false);
  ~WxObject();

  ACDK_WX_STD_WX_ACCESSOR(Object)
  foreign inline void ownsWxObject(bool own) { _ownsWxObject = own; if (_forward != 0) _forward->ownsWxObject(own);  }
  foreign inline bool ownsWxObject() { return _ownsWxObject; }
private:
  foreign void _assignShadow(wxObject* wxObject, bool own);
  foreign Object* _getShadow();
};

typedef Object* (*InstanceCreator)();
struct RegisterWxCreator
{
  RegisterWxCreator(InstanceCreator creator, const wxClassInfo* wxClassInfo);
};
/**
  Internal template to wrapp a wx value type
*/
template <typename T>
class WxValStruct
: extends acdk::lang::Object
{
protected:
  T _wxObject;
public:
  WxValStruct(const T& obj): _wxObject(obj) {}
  ~WxValStruct()
  {
  }
  void setWx(T* wxo) { _wxObject = *wxo; }
  T* getWx() { return &_wxObject; }
  const T* getWx() const { return &_wxObject; }
  const T& toWx() const { return _wxObject; }
  T& toWx() { return _wxObject; }
  //inline void ownsWxObject(bool own) { _ownsWxObject = own; }
  //inline bool ownsWxObject() const { return _ownsWxObject; }
};

/**
  Internal template to wrapp a wx type, which should not be copied
*/
template <typename T>
class WxNonCopyStruct
: extends acdk::lang::Object
{
protected:
  T* _wxObject;
  mutable bool _ownsWxObject;
public:
  WxNonCopyStruct() : _wxObject(0), _ownsWxObject(false) {}
  WxNonCopyStruct(T* obj, bool owns = true) : _wxObject(obj), _ownsWxObject(owns)  {}
  WxNonCopyStruct(const T& obj, bool owns = false) : _wxObject(const_cast<T*>(&obj)), _ownsWxObject(owns)  {}
  ~WxNonCopyStruct()
  {
    if (_ownsWxObject == true)
      delete _wxObject;
  }
  void setWx(T* wxo) { _wxObject = wxo; }
  T* getWx() { return _wxObject; }
  const T* getWx() const { return _wxObject; }
  const T& toWx() const { return *_wxObject; }
  inline void ownsWxObject(bool own) { _ownsWxObject = own; }
  inline bool ownsWxObject() { return _ownsWxObject; }
};


} // wx
} // acdk
typedef wxObject wxWxObject;

#endif //acdk_wx_WxObject_h
