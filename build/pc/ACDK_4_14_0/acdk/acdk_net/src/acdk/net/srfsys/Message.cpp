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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srfsys/Message.cpp,v 1.13 2005/02/05 10:45:30 kommer Exp $



#include "Message.h"
#include <acdk/lang/sys/core_memcheck.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace net {
namespace srfsys {



//virtual 
void 
Message::execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver) 
{
  if (expectReply() == false)
    return;
  ReplyMessage reply;
  reply.code = 1;
  reply.message = "Unexpected Message received";
  out->writeObject(&reply);
}

//virtual 
void 
LoginMessage::execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver)
{
  if (expectReply() == false)
    return;
  ReplyMessage reply;
  reply.code = 0;
  out->writeObject(&reply);
}

//virtual 
void 
ReplyMessage::execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver)
{
  Message::execute(out, fserver);
}

//virtual 
void 
GetDirMessage::execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver)
{
  File f(path);
  ReturnDirMessage reply;
  reply.path = path;
  reply.files = SRFileSystemServer::loadFileSystem(&f, recursive);
  out->writeObject(&reply);
}

//virtual 
void 
ReturnDirMessage::execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver)
{
  Message::execute(out, fserver);
}

//virtual 
void 
FileOpMessage::execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver)
{
  int retstatus = 0;
  RString retmsg;
  RThrowable activeEx;
  switch(fop)
  {
  case FileNop:
    
    break;
  case FileDelete:
  {
    if (fserver != Nil && fserver->_neverOverwriteFile == true)
      break;
    acdk::io::File tf(file->getPath());
    if (tf.deleteFile() != 0)
      retstatus = 1;
    break;
  }
  case FileSend:
  {
    if (file->isDirectory() == true) {
      try {
        acdk::io::File tf(file->getPath());
        if (tf.exists() == true)
        {
          retmsg = "Directory already exists: " + tf.getCanonicalPath();
        } else {
          if (tf.mkdirs() == true)
          {
            tf.setLastModified(file->modified);
            tf.setFileCreated(file->created);
          } else {
            retstatus = 1;
            retmsg = "Cannot create Directory: " + tf.getCanonicalPath();
          }
        }
      } catch (RThrowable ex) {
        activeEx = ex;
        break;
      }
    } else if (file->isFile() == true) {
      acdk::lang::sys::core_memcheck check;
      try {
        acdk::io::File tf(file->getPath());
        if (fserver != Nil && fserver->_neverOverwriteFile == true && tf.exists() == true)
        {
          ACDK_NLOGP("acdk.net.srfsys", Warn, "File exists, will not overwritten: ", LOG_NPV("File", &file->getPath()));
          retstatus = 1;
          break;
        }
        MemReader(data).trans(tf.getWriter());
        tf.setLastModified(file->modified);
        tf.setFileCreated(file->created);
      } catch (RThrowable ex) {
        activeEx = ex;
        break;
      }
    }
    break;
  }
  case FileReceive:
  {
    if (file->isDirectory() == true) {
      acdk::io::File tf(file->getPath());
      FileOpMessage fopout;
      fopout.fop = FileReceive;
      fopout.file = file;
      fopout.data = Nil;
      out->writeObject(&fopout);
    } else if (file->isFile() == true) {
      
      {
      FileOpMessage fopout;
      try {      
        
        acdk::io::MemWriter mout;
        acdk::io::File(file->getPath()).getReader()->trans(&mout);
        fopout.fop = FileReceive;
        fopout.file = file;
        fopout.data = mout.getBuffer();
      } catch (RThrowable ex) {
        activeEx = ex;
        break;
      }
      {
        acdk::lang::sys::core_memcheck check;
        out->writeObject(&fopout);
      }
      }
      return;
    }
  }
  case FileMove:
  {
    RString orgfile = new String(data);
    acdk::io::File sf(orgfile);
    
    acdk::io::File tf(file->getPath());
    sf.renameTo(&tf);
    tf.setLastModified(file->modified);
    tf.setFileCreated(file->created);
    break;
  }
  }
  if (expectReply() == true)
  {
    if (activeEx != Nil) {
      ReplyMessage replmsg(MsgReplyMessage, 255, activeEx->getMessage());
      out->writeObject(&replmsg);
    } else {
      ReplyMessage replmsg(MsgReplyMessage, retstatus, retmsg == Nil ? RString("") : retmsg);
      out->writeObject(&replmsg);
    }
  }
}

//virtual 
void 
AdminMessage::execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver)
{
  switch (adminCommand)
  {
  case AdminCmdNop:
   if (expectReply() == true)
    {
      AdminMessage replmsg(AdminCmdNop);
      out->writeObject(&replmsg);
    }
    break;
  case AdminCmdShutdown:
  {
    fserver->_shutdown = true;
    if (expectReply() == true)
    {
      AdminMessage replmsg(AdminCmdPing);
      out->writeObject(&replmsg);
    }
    SRFileSystemClient remotefs(InetAddress::getLocalHost(), 7777);
    remotefs.ping(false);
    break;
  }
  case AdminDisconnect:
  case AdminCmdPing:
    if (expectReply() == true)
    {
      AdminMessage replmsg(AdminCmdPing);
      out->writeObject(&replmsg);
    }
    break;
  }
}

//foreign virtual 
void 
ExceptionMessage::execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver)
{
  throw exception;
}

} // namespace srfsys
} // namespace net
} // namespace acdk 



