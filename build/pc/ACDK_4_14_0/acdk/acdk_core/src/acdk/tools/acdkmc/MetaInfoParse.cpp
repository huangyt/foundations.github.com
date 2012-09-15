// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/acdkmc/MetaInfoParse.cpp,v 1.16 2003/06/19 14:37:16 kommer Exp $
//
// $Log: MetaInfoParse.cpp,v $
// Revision 1.16  2003/06/19 14:37:16  kommer
// source comment header ajusted
//
// Revision 1.15  2003/06/19 13:17:06  kommer
// merged acdk-3-unicode into MAIN
//
// Revision 1.14.2.1  2003/03/18 10:25:58  kommer
// split metainfo into base_metainf and ext_metainf
//
// Revision 1.14  2002/08/30 00:45:27  kommer
// dos2unix
//
// Revision 1.13  2002/08/29 18:31:07  kommer
// introduces acdk_tools_mc
//
// Revision 1.12  2002/06/30 00:35:26  kommer
// panta rei
//
// Revision 1.11  2002/06/16 12:15:14  kommer
// support the ACDK_WITH_DMIPROXY class attribute
//
// Revision 1.10  2002/06/13 13:12:28  kommer
// implemented handling of DmiProxy
//
// Revision 1.9  2002/06/10 17:17:57  kommer
// handled THROWSx in metainfo not correctly
//
// Revision 1.8  2002/05/20 14:11:01  kommer
// dos2unix
//
// Revision 1.7  2002/05/12 18:16:20  kommer
// support the BYVAL* argument attributes
//
// Revision 1.6  2002/04/07 17:15:29  kommer
// dos2unix
//
// Revision 1.5  2002/04/06 23:26:56  kommer
// bug when pure virtual function are not compatible fixed
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
// Revision 1.1  2001/12/02 13:48:35  kommer
// initial revision
//
// Revision 1.17  2001/11/28 21:04:26  kommer
// panta rei
//
// Revision 1.16  2001/11/16 20:36:29  kommer
// reorganized ClazzInfo structure for better support of COM
//
// Revision 1.15  2001/11/09 13:22:01  kommer
// dos2unix
//
// Revision 1.14  2001/09/30 17:36:35  kommer
// panta rei
//
// Revision 1.13  2001/08/12 15:25:00  kommer
// dos2unix
//
// Revision 1.12  2001/08/11 13:52:57  kommer
// panta rei
//
// Revision 1.11  2001/05/19 01:22:51  kommer
// large clazzinfo.cpp will now splittet
//
// Revision 1.10  2001/05/12 16:55:28  kommer
// Removed warning if method is declared foreign
//
// Revision 1.9  2001/05/05 18:12:25  kommer
// panta rei
//
// Revision 1.8  2001/04/30 13:09:22  kommer
// panta rei
//
// Revision 1.7  2001/04/28 14:09:16  kommer
// panta rei
//
// Revision 1.6  2001/04/28 11:52:40  kommer
// panta rei
//
// Revision 1.5  2001/04/27 18:50:03  kommer
// enum is valid type and ACDK2IDL first sketch
//
// Revision 1.3  2001/03/02 17:49:16  kommer
// enhanced for CORBA
//
// Revision 1.2  2001/01/24 13:28:32  kommer
// panta rei
//
// Revision 1.1.1.1  2000/12/11 18:05:18  kommer
// ACDK Free edition
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:19  roger
// initial acdk sources
//
// Revision 1.5  2000/06/22 20:00:09  roger
// rollback of jb changes, restoring orignal (on my machine working) code
//
// Revision 1.2  2000/04/24 10:26:41  roger
// throws now EOFException if reading behind stream
//
// Revision 1.1  2000/04/14 09:09:52  roger
// *** empty log message ***
//
// Revision 1.19  2000/03/13 18:52:48  roger
// panta rei
//
// Revision 1.18  2000/03/10 18:12:01  roger
// panta rei
//
// Revision 1.17  2000/03/05 11:21:41  roger
// panta rei
//
// Revision 1.16  2000/02/07 15:37:19  wolle
// dos2unix
//
// Revision 1.15  2000/02/01 13:16:24  wolle
// mutable wird nun ignoriert
//
// Revision 1.14  2000/01/03 13:19:36  roger
// removed const in ACDK-Std-Calls
//
// Revision 1.13  1999/12/12 22:19:58  roger
// implements instead of public virtual for base Interfaces
//
// Revision 1.12  1999/11/30 15:00:44  roger
// fixed some minor namespace problems
//
// Revision 1.11  1999/10/21 18:04:32  roger
// Copyright notice updated
//
// Revision 1.10  1999/10/04 16:00:40  roger
// renamed M/MLib => ACDK
//
// Revision 1.9  1999/10/04 08:10:25  roger
// renamed M\MLib => ACDK
//
// Revision 1.8  1999/09/29 11:27:49  kai
// Supports now prefix ACDK_.
//
// Revision 1.7  1999/09/28 16:23:04  roger
// little bug in parsing ::label in global scope
//
// Revision 1.6  1999/09/20 14:04:22  kai
// Ignores now keyword friend.
//
// Revision 1.5  1999/08/29 22:17:11  roger
// panta rei
//
// Revision 1.4  1999/08/27 17:17:38  roger
// dos2unix
//
// Revision 1.3  1999/08/23 21:36:29  roger
// meta info related stuff
//
// Revision 1.2  1999/08/23 19:39:51  roger
// dos2unix
//
// Revision 1.1  1999/08/23 19:03:26  roger
// initial release
//

