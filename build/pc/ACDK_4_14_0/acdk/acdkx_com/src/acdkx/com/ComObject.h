// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any CoObjectmmercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/ComObject.h,v 1.11 2005/03/14 17:59:13 kommer Exp $


#ifndef acdkx_com_ComObject_h
#define acdkx_com_ComObject_h

#include "IUnknown.h"

extern  const GUID CLSID_acdk_lang_Object;

typedef IHolder< ::IDispatch> RIDispatch;

namespace acdkx {
namespace com {

ACDK_DECL_CLASS(ComObject);



/**
  Proxy to a scriptable COM ojbect. 
  With a ComObject a ActiveX Object can be used, quite similar
  it can be done in Visual Basic or other Windows Host scripting
  languages.
  
  @sample
  <code>
  // C++ acdk
    RComObject obj = ComObject::New("Word.Application");

    obj->poke("Visible", ScriptVar(true));
    RComObject docs = obj->peekI("Documents");
    RComObject doc = docs->invokeI("Add", "");
    RComObject sel = obj->peekI("ActiveWindow")->peekI("Selection");
    RString s("This is ACDK");
    sel->invokev("TypeText", "S", RString("This is "));
    sel->peekI("Font")->poke("Bold", true);
    sel->invokev("TypeText", "S", RString("ACDK "));
    sel->peekI("Font")->poke("Bold", false);
    sel->invokev("TypeText", "S", RString("instrumenting Word!"));
    
    Thread::sleep(3000);
    
    obj->invokev("Quit", "i", 0);
  </code>

  This class can also used in scripting enviromnents:
  <code>
  (defun say-word-hello ()
  (set cls (invoke (invoke-static 'acdk.lang.ClassLoader 'getSystemClassLoader) 'findClass 'acdkx.com.ComObject))
  (set app (invoke-static 'acdkx.com.ComObject 'New "Word.Application"))
  (poke app 'Visible T)
  (set doc (invoke (peek app 'Documents) 'Add))
  (set sel (peek (peek app 'ActiveWindow) 'Selection))
  (invoke sel 'TypeText "This is ")
  (poke (peek sel 'Font) 'Bold 1)
  (invoke sel 'TypeText "ACDK")
  (poke (peek sel 'Font) 'Bold 0)
  (invoke sel 'TypeText " instrumenting Word through acdklisp")
  (sleep 3000)
  (invoke app 'Quit 0)
  </code>
)
*/
class ACDKX_COM_PUBLIC ComObject 
: extends ::acdk::lang::Object

{
public: 
  /** not using the standard meta info, becuase this class is used as a proxy */
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return clazzInfo(); } 
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return &_clazzInfo; } 
  virtual ::acdk::lang::RClass getClass() { return GetClass(); }
  static ::acdk::lang::RClass GetClass() { return ::acdk::lang::Class::getSingeltonClass(clazzInfo()); }
  static ::acdk::lang::RObject create_array(int length = 0) { return Nil; }
  static ::acdk::lang::RObject create_array_array(int firstLength = 0, int secondLength = 0) { return Nil; }
  virtual void getCollectableFields(FieldReferences& fields) { }
  virtual ::acdk::lang::dmi::SysFields getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz = 0) 
  { return ::acdk::lang::dmi::SysFields(); }

  
  static const ::acdk::lang::dmi::ClazzMethodInfo* _invoke_dynamic(  ::acdk::lang::Object* This, 
                                                          IN(RString) fname, 
                                                          ::acdk::lang::dmi::ScriptVar& ret, 
                                                          ::acdk::lang::dmi::ScriptVarArray& args, 
                                                          ::acdk::lang::dmi::DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                          const ::acdk::lang::dmi::ClazzMethodInfo* methinf);
  static const ::acdk::lang::dmi::ClazzMethodInfo* _invoke_static(  IN(RString) fname, 
                                          ::acdk::lang::dmi::ScriptVar& ret, 
                                          ::acdk::lang::dmi::ScriptVarArray& args, 
                                          ::acdk::lang::dmi::DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                          const ::acdk::lang::dmi::ClazzMethodInfo* methinf); 
  /*
  virtual  const ::acdk::lang::dmi::ClazzMethodInfo* standardDispatch(  const char* fname, 
                                                          ::acdk::lang::dmi::ScriptVar& ret, 
                                                          ::acdk::lang::dmi::ScriptVarArray& args, 
                                                          ::acdk::lang::dmi::DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                          const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0);
  static const ::acdk::lang::dmi::ClazzMethodInfo* StandardDispatch(const char* fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, 
                                                                ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, 
                                                                const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                                const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0);
  */
  virtual ::acdk::lang::dmi::ScriptVar getMember(IN(RString) fieldname, ::acdk::lang::dmi::DmiClient& dc, int flags, const ::acdk::lang::dmi::ClazzInfo* type_requested = 0);
  virtual void setMember(IN(RString) fieldname, const ::acdk::lang::dmi::ScriptVar& newval, ::acdk::lang::dmi::DmiClient& dc, int flags);
  
private:   
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo;

private:
  foreign ::RIDispatch _idispatch;
  bool _unwrapDmiObject;
public:
  /**
    Internal Constructor to wrapp a IDispatch interface 
    with ComObject

    @see ComObject::New
  */
  foreign ComObject(IN(RIDispatch) id = 0)
  : _idispatch(id)
  , _unwrapDmiObject(true)
  {
  }
  ComObject(IN(RString) classname);
  virtual acdk::lang::dmi::ScriptVar New(IN(RString) classname, acdk::lang::dmi::ScriptVarArray& args);

  /**
    creates a new COM Object. 
    @param classname is the name of COM object like "Word.Application"
  */
  static ::acdk::lang::dmi::ScriptVar New(IN(RString) classname);
  /**
    Should an wrapped ACDK Object parameter be unwrapped or not
    If flag is false, the DMI interface returns ComObject's (except String)
    otherwise the type will be casted.
  */
  void unwrapAcdkObject(bool flag) { _unwrapDmiObject = flag; }
  bool unwrapAcdkObject() { return _unwrapDmiObject; }
  ::IDispatch* getIDispatch() { return _idispatch; }
  

};

} // namespace com
} // namespace acdkx 



#endif //acdkx_com_ComObject_h

