// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/acdkmc/ClassInfo.h,v 1.17 2005/01/14 19:59:45 kommer Exp $
//
// $Log: ClassInfo.h,v $
// Revision 1.17  2005/01/14 19:59:45  kommer
// panta rei
//
// Revision 1.16  2003/06/19 14:37:16  kommer
// source comment header ajusted
//
// Revision 1.15  2003/06/19 13:17:06  kommer
// merged acdk-3-unicode into MAIN
//
// Revision 1.14.2.1  2003/03/18 10:25:58  kommer
// split metainfo into base_metainf and ext_metainf
//
// Revision 1.14  2002/09/21 18:12:57  kommer
// dos2unix
//
// Revision 1.13  2002/09/16 10:53:04  kommer
// panta rei
//
// Revision 1.12  2002/08/30 00:45:27  kommer
// dos2unix
//
// Revision 1.11  2002/08/29 18:31:07  kommer
// introduces acdk_tools_mc
//
// Revision 1.10  2002/06/13 13:12:28  kommer
// implemented handling of DmiProxy
//
// Revision 1.9  2002/04/19 10:07:32  kommer
// dos2unix
//
// Revision 1.8  2002/04/19 09:52:58  kommer
// panta rei
//
// Revision 1.7  2001/12/21 18:23:57  kommer
// VC compatibility
//
// Revision 1.6  2001/12/14 12:04:20  kommer
// dos2unix
//
// Revision 1.5  2001/12/09 00:22:15  kommer
// introduced IN() for Object parameters
//
// Revision 1.4  2001/12/07 22:53:06  kommer
// ajust namespace
//
// Revision 1.3  2001/12/07 02:02:36  kommer
// Resolved ambiguity
//
// Revision 1.2  2001/12/06 11:10:25  kommer
// documentation and changes regarding BadCast
//
// Revision 1.1  2001/12/02 13:46:54  kommer
// initial revision
//
// Revision 1.17  2001/11/09 13:24:47  kommer
// dos2unix
//
// Revision 1.16  2001/10/19 20:17:02  kommer
// add missing includes
//
// Revision 1.15  2001/09/30 17:36:35  kommer
// panta rei
//
// Revision 1.14  2001/09/30 12:32:28  kommer
// changed related DMI2
//
// Revision 1.13  2001/06/01 08:55:18  kommer
// panta rei
//
// Revision 1.11  2001/05/20 12:11:42  kommer
// dos2unix
//
// Revision 1.10  2001/05/19 01:22:51  kommer
// large clazzinfo.cpp will now splittet
//
// Revision 1.9  2001/05/05 18:12:25  kommer
// panta rei
//
// Revision 1.8  2001/04/28 14:12:50  kommer
// panta rei
//
// Revision 1.7  2001/04/28 11:52:40  kommer
// panta rei
//
// Revision 1.6  2001/04/27 18:50:03  kommer
// enum is valid type and ACDK2IDL first sketch
//
// Revision 1.5  2001/04/16 10:37:21  kommer
// panta rei
//
// Revision 1.4  2001/03/23 18:55:14  kommer
// panta rei
//
// Revision 1.3  2001/03/23 09:57:26  kommer
// panta rei
//
// Revision 1.2  2001/01/24 13:28:32  kommer
// panta rei
//
// Revision 1.1.1.1  2000/12/11 18:05:15  kommer
// ACDK Free edition
//
// Revision 1.2  2000/12/05 14:01:33  roger
// panta rei
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:19  roger
// initial acdk sources
//
// Revision 1.9  2000/08/21 11:32:52  roger
// panta rei
//
// Revision 1.8  2000/07/23 11:59:32  kai
// Bugfix in isKnownType(): Known types beginning with namespace now
// supported (e.g. 'acdk::lang::Integer').
//
// Revision 1.7  2000/07/22 23:52:53  kai
// Arrghh!!! bool fehlte an einigen Stellen.
//
// Revision 1.6  2000/06/22 20:00:09  roger
// rollback of jb changes, restoring orignal (on my machine working) code
//
// Revision 1.3  2000/04/24 15:18:51  roger
// fix some gcc warnings
//
// Revision 1.2  2000/04/24 10:26:41  roger
// throws now EOFException if reading behind stream
//
// Revision 1.1  2000/04/14 09:09:35  roger
// *** empty log message ***
//
// Revision 1.22  2000/03/28 10:44:53  roger
// modified metainfo
//
// Revision 1.21  2000/03/21 20:23:51  roger
// renamed main to go because of bcc bug
//
// Revision 1.20  2000/03/10 18:12:01  roger
// panta rei
//
// Revision 1.19  2000/03/05 11:21:41  roger
// panta rei
//
// Revision 1.18  2000/02/14 22:09:34  roger
// new ACDK_DECL_CLASS
//
// Revision 1.17  2000/02/10 15:50:28  roger
// ACDK_DECL_CLASS 2 ACDK_DECL_THROWABLE
//
// Revision 1.16  2000/02/08 16:29:39  roger
// RefHolder and Arrays changed
//
// Revision 1.15  2000/01/03 13:19:34  roger
// removed const in ACDK-Std-Calls
//
// Revision 1.14  1999/11/30 15:00:44  roger
// fixed some minor namespace problems
//
// Revision 1.13  1999/10/24 11:58:07  roger
// acdk::* changed to ::acdk::*
//
// Revision 1.12  1999/10/21 18:04:32  roger
// Copyright notice updated
//
// Revision 1.11  1999/10/04 16:00:40  roger
// renamed M/MLib => ACDK
//
// Revision 1.10  1999/10/04 08:10:25  roger
// renamed M\MLib => ACDK
//
// Revision 1.9  1999/09/30 19:00:46  roger
// type parsing revised
//
// Revision 1.8  1999/09/29 13:12:59  roger
// panta rei
//
// Revision 1.7  1999/09/16 19:48:54  roger
// little type
//
// Revision 1.6  1999/09/10 17:51:41  roger
// using ACDK_DECLCLASS for R*-Declarations
//
// Revision 1.5  1999/09/02 15:04:27  roger
// dos2unix
//
// Revision 1.4  1999/08/27 17:17:36  roger
// dos2unix
//
// Revision 1.3  1999/08/23 21:36:27  roger
// meta info related stuff
//
// Revision 1.2  1999/08/23 19:39:49  roger
// dos2unix
//
// Revision 1.1  1999/08/23 19:03:23  roger
// initial release
//

#ifndef acdk_mc_ClassInfo_h
#define acdk_mc_ClassInfo_h

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>

#include <acdk/io/File.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/io/StreamTokenizer.h>
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/util/ArrayList.h>
#include <acdk/tools/mc/MetaCompiler.h>
#include <acdk/tools/mc/ClassInfo.h>
#include <acdk/tools/mc/ModuleInfo.h>

namespace acdk {
namespace tools {
/**
  Implements ACDK Metacompiler for generating class metainfo and DMI interfaces.
*/ 
namespace acdkmc {

using namespace acdk::lang;
using namespace acdk::lang::reflect;
using namespace acdk::io;
using namespace acdk::util;
using namespace acdk::tools::mc;





ACDK_DECL_CLASS(ClassModulCompiler);





ACDK_DECL_CLASS(ClassModulCompiler);

class ClassModulCompiler
: extends acdk::tools::mc::MetaCompiler
{
  RString _path;
  RArrayList _modules;
public:
  bool all_inModules;
  ClassModulCompiler(IN(RString) path)
  : MetaCompiler(),
    _path(path),
    _modules(new ArrayList()),
    all_inModules(true)
  {
  }
  static void help();
  static int go(RStringArray args);
  int generate();
  void writeMIHs();
  void writeClassInfo();
  void writeClassInfo(RString outfile, RPrintWriter stuboutfile, IN(::acdk::util::RIterator) it, int count);
  void writeClsInfoCPPHeader(IN(RPrintWriter) out);
  void writeModuleHeaderIncludes(IN(RPrintWriter) out, IN(RPrintWriter) stubout);
  void dump(IN(RPrintWriter) out, IN(RString) ind);
  bool checkContext();
  void setbaseFileName();
  void addModuleAttrCode(IN(RString) code);
  
  /// ACDK2IDL related
  //bool hasOrbDefinitions();
  //void writeExtIdl(IN(RString) fname);
  static RClassModulCompiler getClassModuleCompiler();

};




extern RClassModulCompiler gClassModulCompiler;




} // acdkmc
} // tools
} // acdk

#endif //acdk_mc_ClassInfo_h
