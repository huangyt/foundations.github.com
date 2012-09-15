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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/MetaAttribute.h,v 1.9 2005/02/05 10:44:58 kommer Exp $

#ifndef acdk_lang_dmi_MetaAttribute_h
#define acdk_lang_dmi_MetaAttribute_h



namespace acdk {
namespace lang {
namespace dmi {


ACDK_DECL_CLASS(MetaAttribute);

/** 
  MetaAttribute is connected with a unit, class, method or parameter
  
  @author Roger Rene Kommer (kommer@artefaktur.com)
*/

class ACDK_CORE_PUBLIC MetaAttribute
: public acdk::lang::Object
{
  ACDK_WITH_METAINFO(MetaAttribute)
public:
  int type;
  RString name;
  RObject value;
  MetaAttribute(int t, IN(RString) nam, IN(RObject) val = Nil)
  : type(t)
  , name(nam)
  , value(val)
  {
  }
};

ACDK_DECL_CLASS(ScriptVarMetaAttribute);
class ACDK_CORE_PUBLIC ScriptVarMetaAttribute
: public acdk::lang::Object
{
public:
  int type;
  RString name;
  ScriptVar value;
  ScriptVarMetaAttribute(int t, IN(RString) nam, IN(ScriptVar) val = Nil)
  : type(t)
  , name(nam)
  , value(val)
  {
  }
};



} // dmi
} // lang
} // acdk


#endif //acdk_lang_dmi_MetaAttribute_h

