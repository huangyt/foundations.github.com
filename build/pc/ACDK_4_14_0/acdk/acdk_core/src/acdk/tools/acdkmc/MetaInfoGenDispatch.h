// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/acdkmc/MetaInfoGenDispatch.h,v 1.1 2003/08/10 10:12:27 kommer Exp $
//
// $Log: MetaInfoGenDispatch.h,v $
// Revision 1.1  2003/08/10 10:12:27  kommer
// initial revision
//
// Revision 1.12  2003/06/19 14:37:16  kommer
// source comment header ajusted
//
// Revision 1.11  2002/08/29 18:31:07  kommer
// introduces acdk_tools_mc
//
// Revision 1.10  2002/05/20 14:11:01  kommer
// dos2unix
//
// Revision 1.9  2002/05/12 18:15:35  kommer
// OUT/IN also encoded into signature
//
// Revision 1.8  2001/12/31 14:11:16  kommer
// panta rei
//
// Revision 1.7  2001/12/24 00:35:48  kommer
// dos2unix
//
// Revision 1.6  2001/12/21 13:21:35  kommer
// make namedArgs to reference type
//
// Revision 1.5  2001/12/19 22:08:52  kommer
// panta rei
//
// Revision 1.4  2001/12/14 12:04:20  kommer
// dos2unix
//
// Revision 1.3  2001/12/09 00:22:15  kommer
// introduced IN() for Object parameters
//
// Revision 1.2  2001/12/07 22:53:06  kommer
// ajust namespace
//
// Revision 1.1  2001/12/02 13:48:06  kommer
// initial revision
//
// Revision 1.12  2001/11/21 20:59:10  kommer
// panta rei
//
// Revision 1.11  2001/11/18 03:40:07  kommer
// introduced const in Clazz*
//
// Revision 1.10  2001/11/17 23:24:00  kommer
// test for new Dispatch
//
// Revision 1.9  2001/11/17 22:48:45  kommer
// revised dispatch
//
// Revision 1.8  2001/08/12 15:25:00  kommer
// dos2unix
//
// Revision 1.7  2001/08/11 13:52:57  kommer
// panta rei
//
// Revision 1.6  2001/05/05 18:12:25  kommer
// panta rei
//
// Revision 1.5  2001/04/28 11:52:40  kommer
// panta rei
//
// Revision 1.4  2001/04/27 18:50:03  kommer
// enum is valid type and ACDK2IDL first sketch
//
// Revision 1.3  2001/04/16 10:37:21  kommer
// panta rei
//
// Revision 1.2  2001/01/24 13:28:32  kommer
// panta rei
//
// Revision 1.1.1.1  2000/12/11 18:05:16  kommer
// ACDK Free edition
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:19  roger
// initial acdk sources
//
// Revision 1.4  2000/08/28 07:50:16  roger
// changed standardDispatch()
//
// Revision 1.3  2000/06/22 20:00:09  roger
// rollback of jb changes, restoring orignal (on my machine working) code
//
// Revision 1.1  2000/04/14 09:09:49  roger
// *** empty log message ***
//
// Revision 1.3  2000/03/14 11:59:03  roger
// panta rei
//
// Revision 1.2  2000/03/13 18:52:48  roger
// panta rei
//
// Revision 1.1  2000/03/05 11:21:41  roger
// panta rei
//
#ifndef acdk_tools_acdkmc_MetaInfoGenDispatch_h
#define acdk_tools_acdkmc_MetaInfoGenDispatch_h

#include "ClassInfo.h"


namespace acdk {
namespace tools {
namespace acdkmc {

class MetaInfoGenProxy
: extends acdk::lang::Object
{
  RString _outdir;
public:
  MetaInfoGenProxy(IN(RString) outputdir);
  /**
    Generated proxy classes for given namespace / unit
  */
  void generate(IN(RString) ns);
  void generate(const acdk::lang::dmi::ClazzInfo* ci);
};


} // acdkmc
} // tools
} // acdk

#endif //acdk_tools_acdkmc_MetaInfoGenDispatch_h
