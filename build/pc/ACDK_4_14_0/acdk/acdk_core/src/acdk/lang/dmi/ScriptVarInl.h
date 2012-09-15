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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/ScriptVarInl.h,v 1.10 2005/04/18 14:22:28 kommer Exp $

#ifndef acdk_lang_dmi_ScriptVarInl_h
#define acdk_lang_dmi_ScriptVarInl_h

namespace acdk {
namespace lang {
namespace dmi {


inline void ScriptVar::_setrobject(Object* o) { ::new ((void*)var.object_buf) RObject(o); }
inline void ScriptVar::_setrobject(IN(RObject) o) { ::new ((void*)var.object_buf) RObject(o); }
inline void ScriptVar::_deleterobject() { _getrobject().~RObject(); }

inline bool 
ScriptVar::isStringType() const 
{ 
  return isObjectType() == true && instanceof(getObjectVar(), String); 
}

} // dmi
} // lang
} // acdk

#endif //acdk_lang_dmi_ScriptVarInl_h
