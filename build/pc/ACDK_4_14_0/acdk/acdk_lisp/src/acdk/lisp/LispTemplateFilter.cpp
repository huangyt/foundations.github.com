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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/LispTemplateFilter.cpp,v 1.11 2005/03/08 18:54:12 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/util/HashSet.h>
#include "lisp.h"
#include "LispTemplateFilter.h"
#include "LispEnvironment.h"

namespace acdk {
namespace lisp {

using namespace acdk::lang;
using namespace acdk::text;

LispTemplateFilter::LispTemplateFilter(IN(RLispEnvironment) env)
: _env(env)
{
}

//virtual 
RString 
LispTemplateFilter::filter(IN(RString) text)
{
  //System::out->println("Filter str: " + text);
  RLispVar lvar = _env->parseEval(text);
  //return lvar->toString();
  RString terg = _env->lastEvaledString();
  return terg;
}

//virtual 
RString 
LispTemplateFilter::filter(IN(RStringArray) matches)
{
  System::out->println("Filter array: " + matches->toString());
  return "";
}



//static 
RString 
LispTemplateFilter::filter(IN(RLispEnvironment) env, IN(acdk::io::RFile) file)
{
  RString text = env->loadUnparsedFile(file->getCanonicalPath());
  return filter(env, text);
}


//static 
RString 
LispTemplateFilter::filter(IN(RLispEnvironment) env, IN(RString) text)
{
  Template templ(text);
  templ.registerPatternListener("\\%\\[(.*?)\\]\\%", new LispTemplateFilter(env));
  RString erg = templ.filter(2, 2);
  return erg;
}

} // namespace lisp 
} // namespace acdk 


