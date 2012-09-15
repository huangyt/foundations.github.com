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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srfsys/SRFileSystemClient.cpp,v 1.13 2005/02/05 10:45:30 kommer Exp $

#include <acdk.h>
#include <acdk/lang/CmdLineParseException.h>
#include <acdk/io/FileStandardImpl.h>
#include <acdk/util/SysDate.h>
#include <acdk/net/ServerSocket.h>
#include <acdk/net/TransRateReader.h>
#include <acdk/net/TransRateWriter.h>
#include <acdk/lang/sys/core_memcheck.h>
#include <acdk/util/logging/Log.h>
/*
#include <acdk/vfile/InflaterReader.h>
#include <acdk/vfile/DeflateWriter.h>
*/
#include "Message.h"
#include "SRFileSystemClient.h"
#include "SRFileImpl.h"
#include "ObjectLogReader.h"
#include "ObjectLogWriter.h"

namespace acdk {
namespace net {
namespace srfsys {


void splitLastElementOfPath(IN(RString) path, OUT(RString) dir, OUT(RString) fname)// ### static method of File
{
  int lidx1 = fname->lastIndexOf('/');
  int lidx2 = fname->lastIndexOf('\\');
  if (lidx1 == -1)
  {
    if (lidx2 == -1)
    {
      dir = "";
      fname = path;
      return;
    }
    dir = path->substr(0, lidx2);
    fname = path->substr(lidx2);
    return;
  } else if (lidx2 == -1 || lidx2 < lidx1) {
    dir = path->substr(0, lidx1);
    fname = path->substr(lidx1);
  } else {
    dir = path->substr(0, lidx2);
    fname = path->substr(lidx2);
  }
}


USING_CLASS(::acdk::io::, FileSystem);

RString SRFileSystemClient::_protocolName = "srsync:";


SRFileSystemClient::SRFileSystemClient(IN(RInetAddress) address, int port)
{
  connect(address, port); 
}

bool 
SRFileSystemClient::connect(IN(RString) constr, IN(RString) username, IN(RString) pass)
{
  if (_server != Nil)
  {
    _server = Nil;
  }
  RString server = constr;
  if (server->lastIndexOf(':') == -1)
    THROW1_FQ(::acdk::lang::, Throwable, "Invalid format on internetaddress: " + server);
  int port = Integer::parseInt(server->substr(server->lastIndexOf(':') + 1));
  server = server->substr(0, server->lastIndexOf(':'));
  ::acdk::net::RInetAddress address = ::acdk::net::InetAddress::getByName(server);
  
  connect(address, port);
  login(username, pass);
  return true;
}

void SRFileSystemClient::connect(IN(RInetAddress) address, int port)
{
  _host = address->toString() + port;
  ACDK_NLOGP("acdk.net.srfsys", Info, "Connect to Server", LOG_NPV("Server", &_host));
  _server = new Socket(address, port);
  //_transReader = new TransRateReader(new acdk::vfile::InflaterReader(&_server->getInputStream()), 0);
  _transReader = new TransRateReader(_server->getInputStream(), 0);
  _transWriter = new TransRateWriter(_server->getOutputStream(), 0);

  _transReader->setBytesPerSecondRate(Integer::parseInt(System::getProperties()->getProperty("SRS_NET_RECV_LIMIT", "0")));
  _transWriter->setBytesPerSecondRate(Integer::parseInt(System::getProperties()->getProperty("SRS_NET_SEND_LIMIT", "0")));
  RTransListenerImpl tli = new TransListenerImpl();
  _transReader->_listener = _transWriter->_listener = (RTransListener)tli;
  acdk::util::logging::RLogger logger = acdk::util::logging::LogManager::getCreateLogger("acdk.net.srfsys");
  _bin = new ObjectLogReader(new ::acdk::io::BinaryObjectReader(&_transReader), logger);
  //_transWriter = new TransRateWriter(new acdk::vfile::DeflateWriter(_server->getOutputStream()), 0);
  _transWriter = new TransRateWriter(_server->getOutputStream(), 0);
  _bout = new ObjectLogWriter(new ::acdk::io::BinaryObjectWriter(&_transWriter), logger);
}


bool 
SRFileSystemClient::login(IN(RString) name, IN(RString) passwd)
{
  if (_server == Nil)
    THROW1(IOException, "SRFileSystemClient::login(). Not connected to a server");
  LoginMessage login;
  login.user = name;
  login.pass = passwd;
  _bout->writeObject(&login);
  RReplyMessage msg = (RReplyMessage)_bin->readObject();
  if (msg->code != 0) 
    return false;
  return true;
}

RFileInfoArray 
SRFileSystemClient::loadFileTree(IN(RString) root, bool recursive)
{
  if (_server == Nil)
    THROW1(IOException, "SRFileSystemClient::loadFileTree(). Not connected to a server");
  _root = root;
  GetDirMessage getdir;
  getdir.path = root;
  getdir.recursive = recursive;
  _bout->writeObject(&getdir);
  RMessage msg = (RMessage)_bin->readObject();
  if (instanceof(msg, ReturnDirMessage) == false)
  {
    msg->execute(_bout, Nil); // ## probable ExceptionMessage
    return new FileInfoArray(0);
  } 
  RReturnDirMessage redmsg = (RReturnDirMessage)msg;
  if (redmsg->code != 0) 
    THROW1(IOException, "Retrive dirs failed: " + redmsg->message);
  return _files = redmsg->files;
}



//virtual 
RFileArray 
SRFileSystemClient::listFiles(IN(RString) directory, int listflags)
{
  // #### to implement
  return new FileArray(0);
}

//virtual 
RFile 
SRFileSystemClient::file(IN(RString) path)
{
  if (_host == Nil)
    return new File(path);
  RFileInfo fi = findFile(getRootName() + path);
  if (fi == Nil)
  {
    fi = new FileInfo();
    splitLastElementOfPath(path, fi->dir, fi->name);
    fi->exists(false);
  }
  return new File(new SRFileImpl(this, fi));
}




RFileInfo
SRFileSystemClient::findFile(IN(RString) fqpath)
{
  if (fqpath->startsWith(getRootName()) == false)
    return Nil;
  RString fpath = fqpath->substr(getRootName()->length());
  RString dir; RString name;
  splitLastElementOfPath(fpath, dir, name);
  for (int i = 0; i < _files->length(); ++i)
  {
    if (name->equals(_files[i]->name) == true &&
        dir->equals(_files[i]->name) == true)
        return _files[i];
  }
  return Nil;
}

//virtual 
RFileImpl 
SRFileSystemClient::getFileImpl(IN(RString) fqpath)
{
  if (_host == Nil)
    return new ::acdk::io::FileStandardImpl(fqpath);
  
  RFileInfo fi = findFile(fqpath);
  if (fi == Nil)
  {
    fi = new FileInfo();
    splitLastElementOfPath(fqpath, fi->dir, fi->name);
    fi->exists(false);
  }
  return new SRFileImpl(this, fi);
}

RbyteArray 
SRFileSystemClient::retriveFile(IN(RFileInfo) fileInfo)
{
  if (_server == Nil)
    THROW1(IOException, "SRFileSystemClient::login(). Not connected to a server");
  updateTransRate();
  FileOpMessage fop(FileReceive);
  fop.file = fileInfo;
  _bout->writeObject(&fop);
  
  
  {
    RMessage redmsg = (RMessage)_bin->readObject();
    if (instanceof(redmsg, FileOpMessage) == true)
      return RFileOpMessage(redmsg)->data;
    THROW1(IOException, "Cannot retrive file data from Server: " + fileInfo->toString());
    return Nil;
  }
}

bool 
SRFileSystemClient::ping(bool returnping)
{
  if (_server == Nil)
    THROW1(IOException, "SRFileSystemClient::login(). Not connected to a server");
  AdminMessage msg(AdminCmdPing);
  msg.expectReply(returnping);
  _bout->writeObject(&msg);
  if (returnping == false)
    return true;
  RAdminMessage retmsg = (RAdminMessage)_bin->readObject();
  return retmsg->adminCommand == AdminCmdPing;
}

void 
SRFileSystemClient::shutdownServer()
{
  if (_server == Nil)
    THROW1(IOException, "SRFileSystemClient::login(). Not connected to a server");
  AdminMessage msg(AdminCmdShutdown);
  msg.expectReply(false);
  _bout->writeObject(&msg);
  _bout->flush();
}

void 
SRFileSystemClient::sendFile(IN(RFileInfo) fileInfo, IN(RbyteArray) cont)
{
  if (_server == Nil)
    THROW1(IOException, "SRFileSystemClient::login(). Not connected to a server");
  updateTransRate();
  FileOpMessage msg(FileSend);
  msg.file = fileInfo;
  msg.data = cont;
  _bout->writeObject(&msg);
  RMessage redmsg = (RMessage)_bin->readObject();
  redmsg->execute(_bout, Nil);
}


void 
SRFileSystemClient::updateTransRate()
{
  if (_transReader == Nil)
    return;
  _transReader->setBytesPerSecondRate(Integer::parseInt(System::getProperties()->getProperty("SRS_NET_RECV_LIMIT", "0")));
  _transWriter->setBytesPerSecondRate(Integer::parseInt(System::getProperties()->getProperty("SRS_NET_SEND_LIMIT", "0")));
}

void
SRFileSystemClient::disconnect()
{
  return;

  if (_server == Nil)
    return;
  AdminMessage msg(AdminDisconnect);
  msg.expectReply(true);
  _bout->writeObject(&msg);
  _bin->readObject();
  Thread::sleep(300);
  _server = Nil;
}

} // namespace srfsys
} // namespace net
} // namespace acdk 



