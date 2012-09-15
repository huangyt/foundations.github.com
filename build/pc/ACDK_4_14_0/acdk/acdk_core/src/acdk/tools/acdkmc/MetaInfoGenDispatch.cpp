// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/acdkmc/MetaInfoGenDispatch.cpp,v 1.14 2004/02/27 00:24:57 kommer Exp $
//

#if 0

#include "MetaInfoGenDispatch.h"
#include <acdk/io/File.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/locale/Encoding.h>

namespace acdk {
namespace tools {
namespace acdkmc {

using namespace acdk::lang;
using namespace acdk::lang::dmi;



void 
MetaInfoGenProxy::generate(const acdk::lang::dmi::ClazzInfo* ci)
{
  RString header = File(_outdir, RString(ci->name) + "_proxy.h").getCanonicalPath();
  StringBuffer sb;
  // ### write include guard
  // ### 
  RPrintWriter out = new PrintWriter(new FileWriter(header), acdk::locale::Encoding::getAsciiEncoding()->getEncoder());


}

void 
MetaInfoGenProxy::generate(IN(RString) ns)
{
  const UnitInfo* ui = UnitInfo::findCreateUnit(ns->c_str());
  const ClazzInfo* ci = ClazzInfo::getRoot();
  while (ci != 0)
  {
    if ((void*)ci->parent == (void*)ui)
      generate(ci);
    ci = ci->_next;
  }
}

} // acdkmc
} // tools
} // acdk

#endif //0

