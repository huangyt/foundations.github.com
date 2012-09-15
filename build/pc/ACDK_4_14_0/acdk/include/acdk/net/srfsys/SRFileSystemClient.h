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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srfsys/SRFileSystemClient.h,v 1.7 2005/02/05 10:45:30 kommer Exp $


#ifndef acdk_net_srsync_SRFileSystemClient_h
#define acdk_net_srsync_SRFileSystemClient_h

#include <acdk.h>
#include <acdk/io/FileSystem.h>
#include <acdk/io/BinaryObjectWriter.h>
#include <acdk/io/BinaryObjectReader.h>
#include <acdk/net/Socket.h>
#include <acdk/net/TransRateReader.h>
#include <acdk/net/TransRateWriter.h>

#include "srfsys.h"

#include "FileInfo.h"

namespace acdk {
namespace net {
namespace srfsys {

USING_CLASS(::acdk::lang::, String);
USING_CLASS(::acdk::io::, File);
USING_CLASS(::acdk::io::, FileImpl);

ACDK_DECL_CLASS(SRFileSystemClient);
ACDK_DECL_CLASS(SRFileSystemServer);

class ACDK_NET_SRFSYS_PUBLIC SRFileSystemClient
: extends ::acdk::lang::Object
, implements ::acdk::io::FileSystem
{
 ACDK_WITH_METAINFO(SRFileSystemClient)
public:
  
  RString _host;
  RString _root;
  mutable ::acdk::net::RSocket _server;
  mutable ::acdk::net::RTransRateReader _transReader;
  mutable ::acdk::net::RTransRateWriter _transWriter;
  mutable ::acdk::io::RObjectReader _bin;
  mutable ::acdk::io::RObjectWriter _bout;
  RFileInfoArray _files;
public:
  /** used for deserialization */
  SRFileSystemClient() 
  {
  }
  SRFileSystemClient(IN(RInetAddress) address, int port);
  ~SRFileSystemClient()
  {
    disconnect();
  }
  bool connect(IN(RString) constr, IN(RString) username, IN(RString) pass);
  void connect(IN(RInetAddress) address, int port);
  static RString _protocolName;
  
  virtual RString getRootName() { return _protocolName + _host; }

  bool login(IN(RString) name, IN(RString) passwd);
  /**
    Client function: retrive file content from server
  */
  RbyteArray retriveFile(IN(RFileInfo) fileInfo);
  /**
    Client function: send file content to server
  */
  void sendFile(IN(RFileInfo) fileInfo, IN(RbyteArray) cont);
  
  /**
    Send ping to server
    @param returnping server should send back a ping
  */
  bool ping(bool returnping = true);
  /**
    shutdown the connected server
  */
  void shutdownServer();
  /**
    load the file tree
  */
  RFileInfoArray loadFileTree(IN(RString) root, bool recursive = true); 
  
  virtual bool ownsFile(IN(RString) fqfname)
  {
    return fqfname->startsWith(_protocolName + _host) == true;
  }
  /**
    @param directory the absolute name with out the FSname
    @param listflags a combination of ListFlags
    @return list of files
  */
  virtual RFileArray listFiles(IN(RString) directory, int listflags);
  /**
    creates an instance of given file. 
    @param path is without the FS name
    @return returns a new File.
  */
  virtual RFile file(IN(RString) path);
  
  /**
    returns a file implementation for this
    full qualified file implementation
  */
  virtual RFileImpl getFileImpl(IN(RString) fqpath);
  /**
    find if given file is loaded
  */
  RFileInfo findFile(IN(RString) fqfile);
  /**
    disconnects from server
  */
  void disconnect();
protected:
  void updateTransRate();
};

} // namespace srfsys
} // namespace net
} // namespace acdk 


#endif //acdk_net_srsync_SRFileSystemClient_h
