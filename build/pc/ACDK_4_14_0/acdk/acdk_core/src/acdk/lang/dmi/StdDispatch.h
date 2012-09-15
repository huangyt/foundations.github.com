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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/StdDispatch.h,v 1.65 2005/04/17 11:13:41 kommer Exp $

#ifndef acdk_lang_dmi_StdDispatch_h
#define acdk_lang_dmi_StdDispatch_h


#include "DmiClient.h"
#include "AcdkDmiClient.h"
#include "NamedArgs.h"

namespace acdk {
namespace lang {
namespace dmi {

/* ### not used anymore, remove it later

///  Used for checking parameter types
///  @internal

struct ArgumentExprType
{
  const acdk::lang::dmi::ClazzInfo* _type;

  /// name of argument, may be 0 
  const char* _argName;
  /// in case of named arg, safe argument position index 
  int _position;
  ArgumentExprType()
  : _type(0)
  , _argName(0)
  , _position(-1)
  {
  }
  ArgumentExprType(const acdk::lang::dmi::ClazzInfo* type, const char* name = 0, int position = -1)
  : _type(type)
  , _argName(name)
  , _position(position)
  {
  }
};

struct ArgumentExprType;
/// @internal
typedef acdk::lang::sys::core_vector<ArgumentExprType> ArgumentExprTypes;

*/

/** 
  StdDispatch is a basic Interface implemented by every ACDK object and interface
  for dynamic call an ACDK method.
    
  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.65 $
  @date $Date: 2005/04/17 11:13:41 $
*/
foreign 
class ACDK_CORE_PUBLIC StdDispatch
{
public:
  virtual ~StdDispatch() {}
  /**
    returns the ClazzInfo of this object instance
    must not be 0
  */
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo() = 0;

  /** 
    Method invoke calls should forwarded to the DmiTarget
    @param forwarded, true if call should be forwareded
           to returned Object
    @param ci ClazzInfo should be used to locate functions/members
              May not be set, than this->getClazzInfo() will be used.
    @return should never be 0
  */
  virtual ::acdk::lang::Object* getDmiTarget(bool& forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci) = 0;
  /**
    wrapps the call getDmiTarget(bool& forwarded) with a loop
    as long forwarded is true
  */
  ::acdk::lang::Object* getDmiTarget(const ::acdk::lang::dmi::ClazzInfo*& ci);
  /**
    return the DMI client which should be used for this object
    In normal case this is the AcdkDmiClient.
  */
  virtual DmiClient& getDmiClient();
  
  static const ClazzMethodInfo* _invoke_dynamic(  ::acdk::lang::Object* This, 
                                                          IN(::acdk::lang::RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf); 

  static const ClazzMethodInfo* _invoke_dynamic_super(  ::acdk::lang::Object* This, 
                                                          IN(::acdk::lang::RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf); 
  static const ClazzMethodInfo* _invoke_static(  IN(::acdk::lang::RString) fname, 
                                          ScriptVar& ret, 
                                          ScriptVarArray& args, 
                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf); 
   static const ClazzMethodInfo* _invoke_static_super(  IN(::acdk::lang::RString) fname, 
                                          ScriptVar& ret, 
                                          ScriptVarArray& args, 
                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf); 

  /**
    non existant method. Will throw exception
  */
  static const ClazzMethodInfo* _invoke_notexistant(  ::acdk::lang::Object* This, 
                                                          IN(::acdk::lang::RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf); 
 /**
    calls a method of this object
    used in generated stub
    @param fname name of function
    @param ret return value of this function
    @param args arguments of the method
    @param dc client, calling this method
    @param namedArgs The last n values in args are named arguments. 
            if no named arguments passed, caller should pass Nil
    @param flags for the method (like static, public)
    @param methinf If client caches method information
    @throw DmiException in case of DMI-Errors (like wrong parameter)
    @return the called ClazzMethodInfo. Client may use this cache this value for the next call to improve performance
  */
   virtual  const ClazzMethodInfo* standardDispatch(  IN(::acdk::lang::RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo = 0,
                                                          const ClazzMethodInfo* methinf = 0);
protected:
  /**
    This version of StandardDispatch is used in the generated stub 
    and should not called directly
  */
   static const ClazzMethodInfo* StandardDispatch(IN(::acdk::lang::RString) fname, 
                                                         ScriptVar& ret, 
                                                         ScriptVarArray& args, 
                                                         DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ClazzInfo* clazzinfo = 0,
                                                         const ClazzMethodInfo* methinf = 0);

  /**
    calls a static method of this object
    This is the core implemention method for the invoke_static family

    @param classname name of the class
    @param fname name of function
    @param ret return value of this function
    @param args arguments of the method
    @param dc client, calling this method
    @param flags normally MiPublic | MiStatic
    @param namedArgs The last n values in args are named arguments. 
           if no named arguments passed, caller should pass Nil
    @param methinf If client caches method information
    @return the called ClazzMethodInfo. Client may use this cache this value for the next call to improve performance
  */
   static const ClazzMethodInfo* StandardDispatch(IN(::acdk::lang::RString) classname, 
                                                         IN(::acdk::lang::RString) fname, 
                                                         ScriptVar& ret, 
                                                         ScriptVarArray& args, 
                                                         DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags, 
                                                         const ClazzInfo* clazzinfo = 0,
                                                         const ClazzMethodInfo* methinf = 0);
public:
  /** simpified interface to standardDispatch */
   foreign ScriptVar invokeMethod(IN(RString) funcname, ScriptVarArray& args, int flags = MiPublic);
   
   foreign ScriptVar invokeMethod(IN(RString) funcname, ScriptVarArray& args, IN(NamedArgs) nargs, int flags = MiPublic);

  /** simpified interface to standardDispatch */
   foreign ScriptVar invokeMethod(  IN(RString) funcname, ScriptVarArray& args
                                              , DmiClient& dc
                                              , IN(::acdk::lang::RStringArray) namedArgs
                                              , int flags = MiPublic
                                              );
   /** simpified interface to standardDispatch */
   foreign ScriptVar invokeMethod(  IN(RString) funcname, ScriptVarArray& args
                                              , IN(::acdk::lang::RStringArray) namedArgs
                                              , int flags = MiPublic
                                              );
  /** simpified interface to StandardDispatch */
  foreign static ScriptVar invokeStaticMethod( IN(RString) classname
                                                           , IN(RString) funcname
                                                           , ScriptVarArray& args
                                                           , int flags = MiPublic | MiStatic
                                                           );
  /** simpified interface to StandardDispatch with named arguments */
  foreign static ScriptVar invokeStaticMethod( IN(RString) classname
                                                           , IN(RString) funcname
                                                           , ScriptVarArray& args
                                                           , IN(NamedArgs) nargs
                                                           , int flags = MiPublic | MiStatic
                                                           );

  /** simpified interface to StandardDispatch */
   foreign static ScriptVar invokeStaticMethod( IN(RString) classname
                                                           , IN(RString) funcname
                                                           , ScriptVarArray& args
                                                           , DmiClient& dc
                                                           , IN(::acdk::lang::RStringArray) namedArgs
                                                           , int flags = MiPublic | MiStatic
                                                           );
  
  /** 
    retreive a static member variable
    @param clazz the ClazzInfo
    @param fieldname name of the member
    @param dc DmiClient to use to convert types
    @param flags normally MiPublic | MiStatic
           can also be MiAiOut (| MiAiIn) to retrieve as left hand value
    @param type_requested Type for target ScriptVar. if zero no conversion will be done
    @throw NoSuchElementException if given Member not exists
    @throw ParamsMismatchException if type cannot be converted
  */
   static ScriptVar getStaticMember(const ClazzInfo* clazz, IN(::acdk::lang::RString) fieldname, DmiClient& dc, int flags, const ClazzInfo* type_requested = 0);

  /** 
    retreive a member variable of this object
    @param fieldname name of the member
    @param dc DmiClient to use to convert types
    @param flags normally MiPublic
    @param type_requested Type for target ScriptVar. if zero no conversion will be done
    @throw NoSuchElementException if given Member not exists
    @throw ParamsMismatchException if type cannot be converted
  */
   virtual ScriptVar getMember(IN(::acdk::lang::RString) fieldname, DmiClient& dc, int flags, const ClazzInfo* type_requested = 0);
  
  /** 
    set a static member variable
    core implementetion
    @param clazz the ClazzInfo
    @param fieldname name of the member
    @param newval new value of the member
    @param dc DmiClient to use to convert types
    @param flags normally MiPublic | MiStatic
    @throw NoSuchElementException if given Member not exists
    @throw ParamsMismatchException if type cannot be converted
  */
   static void setStaticMember(const ClazzInfo* clazz, IN(::acdk::lang::RString) fieldname, const ScriptVar& newval, DmiClient& dc, int flags);
  
  /** 
    set a member variable.
    This is the core implementetion. You may use poke() instead
    
    @param clazz the ClazzInfo
    @param fieldname name of the member
    @param newval new value of the member
    @param dc DmiClient to use to convert types
    @param flags normally MiPublic 
    @throw NoSuchElementException if given Member not exists
    @throw ParamsMismatchException if type cannot be converted
    @see poke
  */
   virtual void setMember(IN(::acdk::lang::RString) fieldname, const ScriptVar& newval, DmiClient& dc, int flags = MiPublic);
    
   /**
    return all fields of this instance using DMI
    @param flags combination of Modifier
    @param ci retrieve this ClazzInfo. If 0 clazzInfo for this object will be used
    @see acdk::lang::dmi::ClazzInfo::getFields();
  */
  virtual SysFields getInternalFields(int flags, const ClazzInfo* clazz = 0);

  /**
    get a field by name. May use getInternalFields if not cannot found by
    using ClazzInfo
    @param name name of the field
    @param flags combination of MetaInfoFlags
    @param clazz search in clazz. If 0 clazzInfo for this object will be used
  */
  SysField getInternalField(IN(RString) name, int flags, const ClazzInfo* clazz = 0);
  /**
    Used from DmiProxyBase to determine if given method is overwritten by dmi
    @param ci ClazzInfo of the current script object instance
    @param mi ClazzMethodInfo of the DmiProxy
    @return true, if given method is overloaded by DMI scripting object
  */
  bool isDmiOverLoaded(const ClazzInfo* ci, const ClazzMethodInfo* mi);
  /**
    Used from DmiProxyBase to determine if given method is overwritten by dmi
    @param funcname name of the function
    @param ci ClazzInfo of the current script object instance
    @param mi ClazzMethodInfo may be 0 if caller doesn't know a method info
    @param args null terminated list of arguments to match
  */  
  virtual bool isDmiOverLoaded(const ClazzInfo* ci, IN(RString) funcname, const ClazzMethodInfo* mi, ClazzMethodArgInfo** const args);
  /**
   Creates a new Object of given classname
   @param classname Name of class
   @param sign signature of methods
   @param args arguments
   @param nargs named arguments
   @see invoke
   @throw DmiException on DMI-related errors
  */
  static ScriptVar New(IN(RString) classname, ScriptVarArray& args, DmiClient& dc = AcdkDmiClient::getDmiClient());
  static ScriptVar New(IN(RString) classname, ScriptVarArray& args, IN(RStringArray) nargs, DmiClient& dc = AcdkDmiClient::getDmiClient());
  static ScriptVar New(IN(RString) classname, ScriptVarArray& args, IN(NamedArgs) nargs, DmiClient& dc = AcdkDmiClient::getDmiClient());

  static ScriptVar New(IN(RString) name)
  {
    ScriptVarArray sa(0);
    return New(name, sa);
  }
  static ScriptVar New(IN(RString) name, const ScriptVar& s0)
  {
    ScriptVarArray sa(1);
    sa[0] = s0;
    return New(name, sa);
  }
  static ScriptVar New(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1)
  {
    ScriptVarArray sa(2);
    sa[0] = s0;
    sa[1] = s1;
    return New(name, sa);
  }
  static ScriptVar New(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2)
  {
    ScriptVarArray sa(3);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    return New(name, sa);
  }
  static ScriptVar New(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3)
  {
    ScriptVarArray sa(4);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    return New(name, sa);
  }
  static ScriptVar New(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                       , const ScriptVar& s4)
  {
    ScriptVarArray sa(5);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    return New(name, sa);
  }
  static ScriptVar New(IN(RString) name, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(0);
    return New(name, sa, nargs);
  }
  static ScriptVar New(IN(RString) name, const ScriptVar& s0, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(1);
    sa[0] = s0;
    return New(name, sa, nargs);
  }
  static ScriptVar New(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(2);
    sa[0] = s0;
    sa[1] = s1;
    return New(name, sa, nargs);
  }
  static ScriptVar New(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(3);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    return New(name, sa, nargs);
  }
  static ScriptVar New(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(4);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    return New(name, sa, nargs);
  }
  static ScriptVar New(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                       , const ScriptVar& s4, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(5);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    return New(name, sa, nargs);
  }
  ScriptVar invoke(IN(RString) name)
  {
    ScriptVarArray sa(0); 
    return invokeMethod(name, sa); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0)
  {
    ScriptVarArray sa(1); 
    sa[0] = s0;
    return invokeMethod(name, sa); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1)
  {
    ScriptVarArray sa(2);
    sa[0] = s0;
    sa[1] = s1;
    return invokeMethod(name, sa); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2)
  {
    ScriptVarArray sa(3);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    return invokeMethod(name, sa); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3)
  {
    ScriptVarArray sa(4);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    return invokeMethod(name, sa); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                   , const ScriptVar& s4)
  {
    ScriptVarArray sa(5);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    return invokeMethod(name, sa); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                   , const ScriptVar& s4, const ScriptVar& s5)
  {
    ScriptVarArray sa(6);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    return invokeMethod(name, sa); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                   , const ScriptVar& s4, const ScriptVar& s5, const ScriptVar& s6)
  {
    ScriptVarArray sa(7);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    return invokeMethod(name, sa); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                   , const ScriptVar& s4, const ScriptVar& s5, const ScriptVar& s6, const ScriptVar& s7)
  {
    ScriptVarArray sa(8);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    sa[7] = s7;
    return invokeMethod(name, sa); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                   , const ScriptVar& s4, const ScriptVar& s5, const ScriptVar& s6, const ScriptVar& s7
                                   , const ScriptVar& s8)
  {
    ScriptVarArray sa(9);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    sa[7] = s7;
    sa[8] = s8;
    return invokeMethod(name, sa); 
  }
  
  ScriptVar invoke(IN(RString) name, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(0); 
    return invokeMethod(name, sa, nargs); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(1); 
    sa[0] = s0;
    return invokeMethod(name, sa, nargs); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(2);
    sa[0] = s0;
    sa[1] = s1;
    return invokeMethod(name, sa, nargs); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(3);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    return invokeMethod(name, sa, nargs); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(4);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    return invokeMethod(name, sa, nargs); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                   , const ScriptVar& s4, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(5);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    return invokeMethod(name, sa, nargs); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                   , const ScriptVar& s4, const ScriptVar& s5, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(6);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    return invokeMethod(name, sa, nargs); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                   , const ScriptVar& s4, const ScriptVar& s5, const ScriptVar& s6, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(7);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    return invokeMethod(name, sa, nargs); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                   , const ScriptVar& s4, const ScriptVar& s5, const ScriptVar& s6, const ScriptVar& s7, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(8);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    sa[7] = s7;
    return invokeMethod(name, sa, nargs); 
  }
  ScriptVar invoke(IN(RString) name, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, const ScriptVar& s3
                                   , const ScriptVar& s4, const ScriptVar& s5, const ScriptVar& s6, const ScriptVar& s7
                                   , const ScriptVar& s8, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(9);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    sa[7] = s7;
    sa[8] = s8;
    return invokeMethod(name, sa, nargs); 
  }

  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname)
  {
    ScriptVarArray sa(0); 
    return invokeStaticMethod(classname, methodname, sa); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0)
  {
    ScriptVarArray sa(1);
    sa[0] = s0;
    return invokeStaticMethod(classname, methodname, sa);
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1)
  {
    ScriptVarArray sa(2);
    sa[0] = s0;
    sa[1] = s1;
    return invokeStaticMethod(classname, methodname, sa); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2)
  {
    ScriptVarArray sa(3);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    return invokeStaticMethod(classname, methodname, sa); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3)
  {
    ScriptVarArray sa(4);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    return invokeStaticMethod(classname, methodname, sa); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3
                                                                              , const ScriptVar& s4)
  {
    ScriptVarArray sa(5);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    return invokeStaticMethod(classname, methodname, sa); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3
                                                                              , const ScriptVar& s4, const ScriptVar& s5)
  {
    ScriptVarArray sa(6);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    return invokeStaticMethod(classname, methodname, sa); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3
                                                                              , const ScriptVar& s4, const ScriptVar& s5
                                                                              , const ScriptVar& s6)
  {
    ScriptVarArray sa(7);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    return invokeStaticMethod(classname, methodname, sa); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3
                                                                              , const ScriptVar& s4, const ScriptVar& s5
                                                                              , const ScriptVar& s6, const ScriptVar& s7)
  {
    ScriptVarArray sa(8);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    sa[7] = s7;
    return invokeStaticMethod(classname, methodname, sa); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3
                                                                              , const ScriptVar& s4, const ScriptVar& s5
                                                                              , const ScriptVar& s6, const ScriptVar& s7
                                                                              , const ScriptVar& s8)
  {
    ScriptVarArray sa(9);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    sa[7] = s7;
    sa[8] = s8;
    return invokeStaticMethod(classname, methodname, sa); 
  }
  
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(0); 
    return invokeStaticMethod(classname, methodname, sa, nargs); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(1);
    sa[0] = s0;
    return invokeStaticMethod(classname, methodname, sa, nargs);
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(2);
    sa[0] = s0;
    sa[1] = s1;
    return invokeStaticMethod(classname, methodname, sa, nargs); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1, const ScriptVar& s2, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(3);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    return invokeStaticMethod(classname, methodname, sa, nargs); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(4);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    return invokeStaticMethod(classname, methodname, sa, nargs); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3
                                                                              , const ScriptVar& s4, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(5);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    return invokeStaticMethod(classname, methodname, sa, nargs); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3
                                                                              , const ScriptVar& s4, const ScriptVar& s5, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(6);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    return invokeStaticMethod(classname, methodname, sa, nargs); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3
                                                                              , const ScriptVar& s4, const ScriptVar& s5
                                                                              , const ScriptVar& s6, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(7);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    return invokeStaticMethod(classname, methodname, sa, nargs); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3
                                                                              , const ScriptVar& s4, const ScriptVar& s5
                                                                              , const ScriptVar& s6, const ScriptVar& s7, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(8);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    sa[7] = s7;
    return invokeStaticMethod(classname, methodname, sa, nargs); 
  }
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, const ScriptVar& s0, const ScriptVar& s1
                                                                              , const ScriptVar& s2, const ScriptVar& s3
                                                                              , const ScriptVar& s4, const ScriptVar& s5
                                                                              , const ScriptVar& s6, const ScriptVar& s7
                                                                              , const ScriptVar& s8, IN(NamedArgs) nargs)
  {
    ScriptVarArray sa(9);
    sa[0] = s0;
    sa[1] = s1;
    sa[2] = s2;
    sa[3] = s3;
    sa[4] = s4;
    sa[5] = s5;
    sa[6] = s6;
    sa[7] = s7;
    sa[8] = s8;
    return invokeStaticMethod(classname, methodname, sa, nargs); 
  }
  

  /**
    Reads a member with given fieldname
    @param fieldname field to read
    @param flags normally set to MiPublic
            can also be MiAiOut (| MiAiIn) to retrieve as left hand value        
    @return the field value
    @throw DmiException on DMI-related errors
  */
   ScriptVar peek(IN(RString) fieldname, int flags = MiPublic);

   
   /**
    reads a static member
    @param classname name of class
    @param fieldname name of field
    @param flags normally set to MiPublic  | MiStatic
            can also be MiAiOut (| MiAiIn) to retrieve as left hand value        
  */
   static ScriptVar peek_static(IN(RString) classname, IN(RString) fieldname, int flags = MiPublic | MiStatic);
  

  
  /**
    Writes a public object field.
    @param fieldname name of the object member
    @param arg the new value to set
    @throw NoSuchElementException if given Member not exists
    @throw ParamsMismatchException if type cannot be converted    
  */
   void poke(IN(RString) fieldname, const ScriptVar& arg, int flags = MiPublic);
  
 
  
  /**
    Writes a public static class field.
    @param classname the name of the class 
    @param member the name of the member variable
    @param val the new value
    @throw NoSuchElementException if given Member not exists
    @throw ParamsMismatchException if type cannot be converted
  */
   static void poke_static(IN(RString) classname, IN(RString) member, const ScriptVar& val, int flags = MiPublic | MiStatic);
  
  
  /** 
    @param clazz the class where to find
           will be set to the ClazzInfo owns the found method
    @param fname name of function
    @param args Arguments of the function
    @param namedArgs names of the named arguements
    @param dc used for parameter converting
    @param flags the given flags the method must be match.
           in normal case this will be PUBLIC or STATIC | PUBLIC or none
    @param methinf may reinterpreted as int for method has if flags has MiIvViaHash
  */
   static const ClazzMethodInfo* lookupMethod( const ClazzInfo*& clazz
                                                          , IN(::acdk::lang::RString) fname
                                                          , ScriptVarArray& args
                                                          , IN(RStringArray) namedArgs
                                                          , DmiClient& dc
                                                          , int flags
                                                          , const ClazzMethodInfo* methinf = 0
                                                          );
  /**
    standard callback provided by DmiClient.
    calls lookupMethod
  */
  static const ClazzMethodInfo* lookupMethod_cb( const ClazzInfo*& clazz
                                                          , IN(::acdk::lang::RString) fname
                                                          , ScriptVarArray& args
                                                          , IN(RStringArray) namedArgs
                                                          , DmiClient& dc
                                                          , int flags
                                                          , const ClazzMethodInfo* methinf
                                                          )
  {
    return lookupMethod(clazz, fname, args, namedArgs, dc, flags, methinf);
  }
  static  const ClazzMethodInfo*   lookupMethod(  const ClazzInfo*& clazz
                                                , IN(::acdk::lang::RString) fname
                                                , acdk::lang::dmi::ClazzMethodArgInfo** const args
                                                , DmiClient& dc
                                                , int flags
                                                );

   /**
    Same as lookupMethod, but look only for non-polymorphic methods
  */
  static const ClazzMethodInfo* lookupMethodNoPolymorph( const ClazzInfo*& clazz 
                                                          , IN(::acdk::lang::RString) fname
                                                          , ScriptVarArray& args
                                                          , IN(RStringArray) namedArgs
                                                          , DmiClient& dc
                                                          , int flags
                                                          , const ClazzMethodInfo* methinf = 0
                                                          );
  /**
    find method using the methodhashvalue.
    This method identification in [sS]tandardDispatch
    will be used if the MiIvViaHash is set
    @param clazz will be set to class owning the method
           will not changed if method cannot be found
    @param flags combination of Modifier flags
    @return 0 if method cannot be found
  */
  static const ClazzMethodInfo* lookupMethod( const ClazzInfo*& clazz 
                                                          , int methodhash
                                                          , int flags
                                                          );
  /**
    Find first function with given name and flags
    @see lookupMethod
  */
   static const ClazzMethodInfo* lookupMethod( const ClazzInfo*& clazz
                                                          , IN(::acdk::lang::RString) fname
                                                          , int flags
                                                          );
  /** 
     ???

  */
   static const ClazzMethodInfo*  _lookupMethod(  const ClazzInfo*& clazz
                                                , IN(::acdk::lang::RString) fname
                                                , ScriptVarArray& args
                                                , DmiClient& dc
                                                , int flags
                                                , const ClazzMethodInfo* methinf
                                                );
  /**
    returns a method if found which matching args and named args
  */
  static const ClazzMethodInfo* _lookupMethod( const ClazzInfo*& clazz
                                              , IN(::acdk::lang::RString) fname
                                              , ScriptVarArray& args
                                              , IN(RStringArray) namedargs
                                              , DmiClient& dc
                                              , int flags
                                              , const ClazzMethodInfo* methinf
                                              );
  /**
    Finds a method.
    @param clazz the Class where to find
    @param fname name of method
    @param args The parameters.
                Note if ClazzMethodArgInfo.label is 0 or "" name of argument will be ignored
    @param namedargs the last n args are named. 
          Can be Nil.
    @param dc used to find out type distance
    @param flags flags of the method
  */
   static const ClazzMethodInfo*  _lookupMethod(  const ClazzInfo*& clazz
                                                , IN(::acdk::lang::RString) fname
                                                , ::acdk::lang::sys::core_vector<ClazzMethodArgInfo>& args
                                                , IN(RStringArray) namedargs
                                                , DmiClient& dc
                                                , int flags
                                                //, const ClazzMethodInfo* methinf
                                                );

  /** 
    find all function with given name and flag
    @param clazz where to find.
    @param fname name of function
    @param flags normally PUBLIC/STATIC
    @param vec all found methods
  */
   static void findFunctions(const ClazzInfo* clazz, IN(::acdk::lang::RString) fname, int flags, ClazzMethodInfoVec& vec);
  
  /**
    Search for given method
    @arg exactMatch if true the types of the arguments must match exactly
         otherwise they only must be assignable
    @arg flags = MiIvDeclared
    @return 0 if method cannot be found.
  */
   static const ClazzMethodInfo* findMethod(const ClazzInfo*& clazz, const FunctionSignature& signature, bool exactMatch = true, 
                                            int flags = MiIvDeclared);
   // ### not used anymore, may be removed static const ClazzMethodInfo* findMethod(const ClazzInfo*& ci, IN(RString) funcname, ArgumentExprTypes& types, int flags, bool exactMatch);
   /**
    internal implementation to read a static member
    @param flags if MiAiOut is set, returns a LValue 
   */
   static ScriptVar getStaticMember(const ClazzInfo* clazz, const ClazzFieldInfo* field, int flags);
   static void setStaticMember(const ClazzInfo* clazz, const ClazzFieldInfo* field, const ScriptVar& value,  DmiClient& dc, int flags);
   static ScriptVar _getMember(::acdk::lang::Object* This, const ClazzInfo* clazz, const ClazzFieldInfo* field, IN(::acdk::lang::RString) fieldname, DmiClient& dc, int flags);
   static void _setMember(::acdk::lang::Object* This, const ScriptVar& val, const ClazzInfo* clazz, const ClazzFieldInfo* field, IN(::acdk::lang::RString) fieldname, DmiClient& dc, int flags);
};


} // dmi
} // lang
} // acdk



#endif //acdk_lang_sys_StdDispatch_h
