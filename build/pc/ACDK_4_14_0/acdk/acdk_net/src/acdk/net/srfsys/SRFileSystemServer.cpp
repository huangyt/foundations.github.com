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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srfsys/SRFileSystemServer.cpp,v 1.18 2005/02/05 10:45:30 kommer Exp $


#include <acdk/io/FileStandardImpl.h>
#include <acdk/io/FileStatus.h>

#include <acdk/util/SysDate.h>
#include <acdk/util/logging/Log.h>

#include <acdk/security/MessageDigest.h>

#include <acdk/net/ServerSocket.h>
#include <acdk/net/TransRateReader.h>
#include <acdk/net/TransRateWriter.h>
/*
#include <acdk/vfile/InflaterReader.h>
#include <acdk/vfile/DeflateWriter.h>
*/
#include "Message.h"
#include "SRFileSystemServer.h"
#include "SRFileImpl.h"
#include "ObjectLogReader.h"
#include "ObjectLogWriter.h"

namespace acdk {
namespace net {
namespace srfsys {

class FileSystemServerTask
: extends ::acdk::lang::Thread
{

  RSRFileSystemServer server;
  RSocket client;
public:
  FileSystemServerTask(IN(RSRFileSystemServer) srv, RSocket clnt)
  : Thread(srv->clientThreadGroup, Nil, "SRClient")
  , server(srv)
  , client(clnt)
  {
  }
  virtual void run(); 
};



void FileSystemServerTask::run()
{
  try {
    TransListenerImpl tli;
    //acdk::net::TransRateReader transReader(new ::acdk::vfile::InflaterReader(client->getInputStream()), 0);
    //acdk::net::TransRateWriter transWriter(new ::acdk::vfile::DeflateWriter(client->getOutputStream()), 0);
    acdk::net::TransRateReader transReader(client->getInputStream(), 0);
    acdk::net::TransRateWriter transWriter(client->getOutputStream(), 0);
    transReader._listener = transWriter._listener = &tli;
    BinaryObjectReader _bin(&transReader);
    acdk::util::logging::RLogger logger = acdk::util::logging::LogManager::getCreateLogger("acdk.net.srfsys");
    ObjectLogReader bin(&_bin, logger);
    BinaryObjectWriter _bout(&transWriter);
    ObjectLogWriter bout(&_bout, logger);
    bool connected = true;
    while (connected == true) 
    {
      bool trySentEx = false;
      transReader.setBytesPerSecondRate(Integer::parseInt(System::getProperties()->getProperty("SRS_NET_RECV_LIMIT", "0")));
      transWriter.setBytesPerSecondRate(Integer::parseInt(System::getProperties()->getProperty("SRS_NET_SEND_LIMIT", "0")));
      RMessage msg;
      try {
        msg = (RMessage)bin.readObject();
        if (instanceof(msg, AdminMessage) == true && RAdminMessage(msg)->adminCommand == AdminDisconnect)
          connected = false;
        msg->execute(&bout, this->server);
      } catch (::acdk::io::RIOException ex) {
        ACDK_NLOGP("acdk.net.srfsys", Warn, "Catched IOException", 
                                            LOG_NPV("Client", &client) << 
                                            LOG_NPV("IOException", &ex->getMessage()));
        connected = false;
      } catch (RThrowable ex) {
        if (msg != Nil && msg->expectReply() == true)
        {
          if (trySentEx == true)
          {
            connected = false;
            ACDK_NLOGP("acdk.net.srfsys", Error, "Catched Repeated IOException", LOG_NPV("Client", &client) << 
                                                                                 LOG_NPV("IOException", &ex->getMessage()));
            break;
          }
          trySentEx = true;
          try {
            ExceptionMessage exmsg(ex);
            bout.writeObject(&exmsg);
          } catch (RThrowable ex) {
            ACDK_NLOGP("acdk.net.srfsys", Error, "Catched Repeated Throwable", LOG_NPV("Client", &client) << 
                                                                               LOG_NPV("Throwable", &ex->getMessage()));
            connected = false;
            break;
          }
        }
      }
    }
  } catch (::acdk::io::REOFException ex) {
    ACDK_NLOGP("acdk.net.srfsys", Info, "End of File", LOG_NPV("Client", &client) <<  
                                                       LOG_NPV("Exception", &ex->getMessage()));
  } catch (::acdk::io::RIOException ex) {
    ACDK_NLOGP("acdk.net.srfsys", Warn, "IO Error", LOG_NPV("Client", &client) <<  
                                                    LOG_NPV("IOException", &ex->getMessage()));
  }    
  ACDK_NLOGP("acdk.net.srfsys", Info, "Finished client connection", LOG_NPV("Client", &client));
}

//virtual 
void 
SRFileSystemServer::run()
{
  
  try {
    RServerSocket server;
    if (_host == Nil)
      server = new ServerSocket(_port, 50, Nil);
    else {
      RInetAddress addr;
      addr = InetAddress::getByName(_host);
      server = new ServerSocket(_port, 50, addr);
    }
    RInetAddress iaddr = server->getInetAddress();
    if (iaddr == Nil)
      iaddr = new InetAddress(0);
    InetAddress localInet(0);

    ACDK_NLOGP("acdk.net.srfsys", Info, "SRFileSystem running", LOG_NPV("Address", &iaddr) <<
                                                                LOG_NPV("Port", server->getLocalPort()));
    
    while (_shutdown == false)
    {
      RSocket client = server->accept();
      RThread ct = new FileSystemServerTask(this, client);
      ct->start();
      Thread::sleep(200);
    }
    ThreadArray childThreads(0);
    clientThreadGroup->enumerate(RThreadArray(&childThreads), true);
    for (int i = 0; i < childThreads.length(); ++i)
    {
      childThreads[i]->join();
    }
  } catch (::acdk::io::REOFException ) {
    // noting
  } catch (::acdk::io::RIOException ex) {
    ACDK_NLOGP("acdk.net.srfsys", Info, "IOException caugth in SRFileSystemServer::run", LOG_NPV("ex", &ex->getMessage()));
  }    
  
}

jlong 
getFileDigest(IN(RFile) f)
{
  //::acdk::security::RMessageDigest md = ::acdk::security::MessageDigest::getInstance("SHA");
  MemWriter memwriter;
  FileReader fin(f);
  fin.trans(&memwriter);
  byteArray& ba = *memwriter.getBuffer();
  int len = ba.length();
  jlong erg = 0;
  for (int i = 0; i < len; ++i)
  {
     erg = jlong(31) * erg + jlong(ba[i]);
  }
  return erg;
  //md->update(memwriter.getBuffer());
  //return *((jlong*)md->digest()->data());
}

RFileInfoArray
SRFileSystemServer::loadFileSystem(IN(RFile) root, bool recursive)
{
  RFileSystem srvfs = root->getFileImpl()->getFileSystem();
  RFileArray ra = srvfs->listFiles(root->getPath(), 
    (recursive == true ? FileListRecursive : 0) | 
      FileListBoth | FileListAllReadable);

  RFileInfoArray rootfi = new FileInfoArray(ra->length());
  
  for (int i = 0; i < ra->length(); ++i)
  {
    
    rootfi[i] = new FileInfo();
    rootfi[i]->name = ra[i]->getName();
    rootfi[i]->dir = ra[i]->getParent();
    FileStatus fstatus(ra[i]->getPath());

    rootfi[i]->size = fstatus.length();
    rootfi[i]->exists(fstatus.exists());
    rootfi[i]->canRead(fstatus.canRead());
    rootfi[i]->canWrite(fstatus.canWrite());
    rootfi[i]->isDirectory(fstatus.isDirectory());
    rootfi[i]->isFile(fstatus.isFile());
    rootfi[i]->isHidden(ra[i]->isHidden());
    rootfi[i]->created = fstatus.created();
    rootfi[i]->modified = fstatus.lastModified();
    rootfi[i]->digest = 0;
    /*
    if (rootfi[i]->isFile() == true && rootfi[i]->canRead() == true)
      rootfi[i]->digest = getFileDigest(ra[i]);
    */
  }
  return rootfi;
}

} // namespace srfsys
} // namespace net
} // namespace acdk 



