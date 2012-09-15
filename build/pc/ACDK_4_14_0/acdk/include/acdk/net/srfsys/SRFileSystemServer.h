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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srfsys/SRFileSystemServer.h,v 1.6 2005/02/05 10:45:30 kommer Exp $


#ifndef acdk_net_srsync_SRFileSystemServer_h
#define acdk_net_srsync_SRFileSystemServer_h

#include <acdk.h>
#include <acdk/io/FileSystem.h>
#include <acdk/io/BinaryObjectWriter.h>
#include <acdk/io/BinaryObjectReader.h>
#include <acdk/net/Socket.h>
#include "srfsys.h"

#include "FileInfo.h"

namespace acdk {
namespace net {
namespace srfsys {

USING_CLASS(::acdk::lang::, String);
USING_CLASS(::acdk::io::, File);
USING_CLASS(::acdk::io::, FileImpl);


ACDK_DECL_CLASS(SRFileSystemServer);

class ACDK_NET_SRFSYS_PUBLIC SRFileSystemServer
: extends ::acdk::lang::Thread
{
  
public:
  bool _shutdown;
  RString _host;
  int _port;
  acdk::lang::RThreadGroup clientThreadGroup;
  bool _neverOverwriteFile;
  SRFileSystemServer(IN(RString) host = Nil, int port = 7777) 
  : _shutdown(false)
  , _host(host)
  , _port(port)
  , clientThreadGroup(new acdk::lang::ThreadGroup("SRFileServerClients"))
  , _neverOverwriteFile(false)
  {
  }
  static RFileInfoArray loadFileSystem(IN(RFile) root, bool recursive = true);
  void run();
};

} // namespace srfsys
} // namespace net
} // namespace acdk 


#endif //acdk_net_srsync_SRFileSystemServer_h
