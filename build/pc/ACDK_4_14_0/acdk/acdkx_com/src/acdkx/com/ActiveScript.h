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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/ActiveScript.h,v 1.8 2005/02/05 10:45:38 kommer Exp $


#ifndef acdkx_com_ActiveScript_h
#define acdkx_com_ActiveScript_h

#include "IUnknown.h"

#if !defined(__BORLANDC__)
#define ACDKX_ORB_WITH_ACTIVESCRIPT
#endif


#if defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
#include <ACTIVSCP.H>
#endif


namespace acdkx {
namespace com {

ACDK_DECL_CLASS(ActiveScript);
ACDK_DECL_CLASS(ActiveScriptSite);

/**
  With ActiveScript a VBScript or JavaScript
  interpreter can be embedded in a ACDK application.
*/
class ACDKX_COM_PUBLIC ActiveScript 
: extends ::acdk::lang::Object
{
#if defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
  IHolder<IActiveScriptParse> _interface;
  RActiveScriptSite _activeScriptSide;
#endif //defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
public:
  ActiveScript(IN(RString) language);
  void parseEval(IN(RString) code);
  /**
    Sets a variable in global namespace of the interpreter
    The variable will always be an Acdk.Object and not converted
    to an native COM class. Therefore if you want to set
    an String value you have to use following code 
    <c>globalvar.toString()</c> in the script code to 
    receive the native string.
  */
  void setVar(IN(RString) name, IN(RObject) obj);
};

} // namespace com 
} // namespace acdkx 


#endif //acdkx_com_ActiveScript_h

