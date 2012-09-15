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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/SystemError.h,v 1.13 2005/04/09 19:26:51 kommer Exp $

#ifndef acdk_lang_SystemError_h
#define acdk_lang_SystemError_h

#include "Error.h"

namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(SystemError, Error);

/** 
  The class SystemError overtakes the role of the java VirtualMachineError
  and signal a failure in a operation system call
  API: Java & ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:51 $
  @todo do not inline functions with sys::coreout etc.

*/  
class ACDK_CORE_PUBLIC SystemError 
: extends Error
{
  ACDK_WITH_METAINFO(SystemError)
public:
  SystemError();
  SystemError(IN(RString) what);
};


#define ACDK_SYS_ERROR(arg) THROW1(SystemError, arg)

#define ACDK_SYS_CALL0(method, cond) \
  if (((method ()) cond) == false && ::acdk::lang::sys::core_system::inMain()) \
    THROW1(SystemError, RString("*** SystemError at " __FILE__ ":") + String::valueOf(__LINE__) +  ". Failed: " #method)  
    
#define ACDK_SYS_CALL1(method, arg1, cond) \
  if ((( method ( arg1 )) cond) == false  && ::acdk::lang::sys::core_system::inMain()) \
    THROW1(SystemError, RString("*** SystemError at " __FILE__ ":") + String::valueOf(__LINE__) +  ". Failed: " #method "(" #arg1 ")" #cond )  

#define ACDK_SYS_CALL2(method, arg1, arg2, cond) \
  if ((( method ( arg1, arg2 )) cond) == false  && ::acdk::lang::sys::core_system::inMain()) \
    THROW1(SystemError, RString("*** SystemError at " __FILE__ ":") + String::valueOf(__LINE__) +  ". Failed: " #method "(" #arg1 ", " #arg2 ")" #cond )  

#define ACDK_SYS_CALL3(method, arg1, arg2, arg3, cond) \
  if ((( method ( arg1, arg2, arg3 )) cond) == false  && ::acdk::lang::sys::core_system::inMain()) \
    THROW1(SystemError, RString("*** SystemError at " __FILE__ ":") + String::valueOf(__LINE__) +  ". Failed: " #method "(" #arg1 ", " #arg2 ", " #arg3 ")" #cond )  

#define ACDK_SYS_CALL4(method, arg1, arg2, arg3, arg4, cond) \
  if ((( method ( arg1, arg2, arg3, arg4 )) cond) == false  && ::acdk::lang::sys::core_system::inMain()) \
    THROW1(SystemError, RString("*** SystemError at " __FILE__ ":") + String::valueOf(__LINE__) +  ". Failed: " #method "(" #arg1 ", " #arg2 ", " #arg3 ", " #arg4 ")" #cond )  

#define ACDK_SYS_CALL5(method, arg1, arg2, arg3, arg4, arg5, cond) \
  if ((( method ( arg1, arg2, arg3, arg4, arg5 )) cond) == false  && ::acdk::lang::sys::core_system::inMain()) \
    THROW1(SystemError, RString("*** SystemError at " __FILE__ ":") + String::valueOf(__LINE__) +  ". Failed: " #method "(" #arg1 ", " #arg2 ", " #arg3 ", " #arg4 ", " #arg5 ")" #cond )  

}
}

#endif //acdk_lang_SystemError_h

