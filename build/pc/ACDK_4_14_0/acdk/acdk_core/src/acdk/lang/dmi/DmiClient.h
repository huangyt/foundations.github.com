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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/DmiClient.h,v 1.18 2005/04/18 14:53:16 kommer Exp $

#ifndef acdk_lang_dmi_DmiClient_h
#define acdk_lang_dmi_DmiClient_h




namespace acdk {
namespace lang {
namespace dmi {

typedef  const ClazzMethodInfo* (*LookupMethodFnc)( const ClazzInfo*& clazz
                                                   , IN(acdk::lang::RString) fname
                                                          , ScriptVarArray& args
                                                          , IN(RStringArray) namedArgs
                                                          , DmiClient& dc
                                                          , int flags
                                                          , const ClazzMethodInfo* methinf
                                                          );

/** 
  DmiClient is an Interface implemented by Clients using the DMI interfaces, 
  like Scripting engines and COM/CORBA middleware
  
  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.18 $
  @date $Date: 2005/04/18 14:53:16 $
*/

class ACDK_CORE_PUBLIC DmiClient
{
private:
  DmiClient(const DmiClient& other) {}
public:

  LookupMethodFnc _lookupFunc;
  /**
    Format to use in case of DmiExceptions
  */
  int _formatFlags;
  DmiClient(int formatFlags = TpFtAcdkType | TpFtFqName);
  virtual ~DmiClient() {}
  /**
   this method now returns the difference beetween the types.
     0 for an exact match, -1 for incompatible types, various
     differences from 1 to 7 for compatible upcasts, from 257 to
    263 for maybe compatible downcasts.
   > 300 for intepreted casts ( read int out of string convert to string)
  */
  virtual int typeDistance(const ScriptVar& arg, const ClazzInfo* toType) = 0;
  /**
    @see int typeDistance(const ScriptVar& arg, const ClazzInfo* toType);
  */
  virtual int typeDistance(const ClazzInfo* fromType, const ClazzInfo* toType) = 0;

  /**
    After checked with typeDistance() use this method to do the cast.
    This method may changes the type of value.

    @param value the value to cast. On succes the type value may be changed by this method
    @param type information of the target type, implemented by the target member/method argument
  */
  virtual void castTo(ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType) = 0;
  /**
    return the standard casting flags as a combination of ScriptVarCastFlags
    @see ScriptVarCastFlags
  */
  virtual int getScriptVarCastFlags() const = 0;
  /**
    return true if this object/class has ClazzInfo structures for static 
    dispatching.
    true for standard ACDK objects, AAL.
    false for scripting clients, where matching arguments only can checked at invokation time.
  */
  virtual bool provideMethodClazzInfo() { return true; }
  /**
    return a thread local copy of the current invokation flags
    @see acdk::lang::dmi::ClazzInvokeInfo
  */
  
  static int getCurInvokeFlags();
  static void setCurInvokeFlags(int flags);
};



} // dmi
} // lang
} // acdk




#endif //acdk_lang_dmi_DmiClient_h

