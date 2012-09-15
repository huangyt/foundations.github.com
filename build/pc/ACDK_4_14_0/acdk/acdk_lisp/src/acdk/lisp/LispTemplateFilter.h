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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/LispTemplateFilter.h,v 1.11 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_lisp_LispTemplateFilter_h
#define acdk_lisp_LispTemplateFilter_h

#include "lisp.h"
#include "LispEnvironment.h"

#include <acdk/text/text.h>

#include <acdk/text/Template.h>

namespace acdk {
namespace lisp {

using namespace acdk::lang;
using namespace acdk::text;



ACDK_DECL_CLASS(LispTemplateFilter);

/** 
  To parse an text file with embedded Lisp-Code
  
  
  @author Roger Rene Kommer
  @version $Revision: 1.11 $
  @date $Date: 2005/04/08 10:53:20 $
  @see acdk::text::TemplateFilter
*/
class ACDK_ACDK_LISP_PUBLIC LispTemplateFilter 
: extends ::acdk::lang::Object,
  implements ::acdk::text::TemplateFilter
{
  ACDK_WITH_METAINFO(LispTemplateFilter)
private:
  RLispEnvironment _env;
public:
  LispTemplateFilter(IN(RLispEnvironment) env);
  virtual RString filter(IN(RString) text);
  virtual RString filter(IN(RStringArray) matches);
  static RString filter(IN(RLispEnvironment) env, IN(acdk::io::RFile) file);
  static RString filter(IN(RLispEnvironment) env, IN(RString) file);
};


} // namespace lisp 
} // namespace acdk 

#endif //acdk_lisp_LispTemplateFilter_h

