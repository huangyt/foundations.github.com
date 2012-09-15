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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srfsys/Message.h,v 1.12 2005/02/05 10:45:30 kommer Exp $


#ifndef acdk_net_srsync_Message_h
#define acdk_net_srsync_Message_h

#include <acdk.h>
#include "Config.h"
#include "FileInfo.h"

#include <acdk/io/ObjectWriter.h>
#include "SRFileSystemClient.h"
#include "SRFileSystemServer.h"

namespace acdk {
namespace net {
namespace srfsys {

USING_CLASS(::acdk::lang::, String);

enum MessageCommand
{
  Unknown = 0,
  MsgLogin,
  MsgReplyMessage,
  MsgGetDirList,
  MsgReturnDirList,
  MsgFileOp,
  MsgAdmin,
  MsgException
};
ACDK_DEF_LIB_ENUM(ACDK_NET_SRFSYS_PUBLIC, MessageCommand);

ACDK_DECL_CLASS(Message);
ACDK_DECL_CLASS(LoginMessage);
ACDK_DECL_CLASS(ReplyMessage);
ACDK_DECL_CLASS(GetDirMessage);
ACDK_DECL_CLASS(ReturnDirMessage);
ACDK_DECL_CLASS(FileOpMessage);
ACDK_DECL_CLASS(AdminMessage);

const int MsgExcpectReply = 0x0001;

class ACDK_NET_SRFSYS_PUBLIC Message
: extends ::acdk::lang::Object
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Message)
public:
  transient int command;
  int flags;
  Message(int cmd = Unknown)
  : command(cmd)
  , flags(0)
  {
  }
  bool expectReply() { return flags & MsgExcpectReply; }
  void expectReply(bool f) { f == true ? flags |= MsgExcpectReply : flags &= ~MsgExcpectReply; }
  /**
    @param out where to write the return message
    @param fserver will Nil on client side
  */
  virtual void execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver);
};

/**
  Login at server
  returns a ReplyMessage. 
*/
class ACDK_NET_SRFSYS_PUBLIC LoginMessage
: extends Message
{
  ACDK_WITH_METAINFO(LoginMessage)
public:
  RString user;
  RString pass;
  LoginMessage()
  : Message(MsgLogin)
  {
    flags |= MsgExcpectReply;
  }
  LoginMessage(IN(RString) u, IN(RString) p)
    : user(u)
    , pass(p)
  {
    flags |= MsgExcpectReply;
  }
  foreign virtual void execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver);
  foreign virtual RString toString()
  {
    return "LoginMessage: User=[" + user + "];";
  }
};

class ACDK_NET_SRFSYS_PUBLIC ReplyMessage
: extends Message
{
  ACDK_WITH_METAINFO(ReplyMessage)
public:
  /** 
    code == 0 all ok
  */
  int code;
  RString message;
  ReplyMessage(int type = MsgReplyMessage, int c = 0, IN(RString) msg = Nil)
  : Message(type)
  , code(c)
  , message(msg)
  {
  }
  foreign virtual void execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver);
  foreign virtual RString toString()
  {
    return RString("ReplyMessage: Code=[") + code + "]; Message=[" + message + "]; ";
  }
};

class ACDK_NET_SRFSYS_PUBLIC GetDirMessage
: extends Message
{
  ACDK_WITH_METAINFO(GetDirMessage)
public:
  RString path;
  bool recursive;
  GetDirMessage()
  : Message(MsgGetDirList)
  , recursive(false)
  {
    flags |= MsgExcpectReply;
  }
  foreign virtual void execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver);
  foreign virtual RString toString()
  {
    return "GetDirMessage: Path=[" + path + "]; ";
  }
};


class ACDK_NET_SRFSYS_PUBLIC ReturnDirMessage
: extends ReplyMessage
{
  ACDK_WITH_METAINFO(ReturnDirMessage)
public:
  RString path;
  bool recursive;
  RFileInfoArray files;

  ReturnDirMessage()
    : ReplyMessage(MsgReturnDirList)
    , recursive(false)
  {
  }
  foreign virtual void execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver);
  foreign virtual RString toString()
  {
    return "ReturnDirMessage: Path=[" + path + "]; Files=[" + files->toString() + "]; ";
  }
};

enum FileOp
{
  FileNop = 0x0000,
  /** 
    File on Server will be deleted 
    Client expects ReplyMessage
  */
  FileDelete  = 0x0001,
  
  /** 
    File send from client to Server.
    Client expects ReplyMessage
  */
  FileSend    = 0x0002,
  /** 
    File should be send from Server To Client 
    Client expects FileOpMessage with FileSend back
  */
  FileReceive = 0x0004,
  /**
    Move a file from one place to another
    data contains the original FQ file name.
  */
  FileMove    = 0x0005
};
ACDK_DEF_LIB_ENUM(ACDK_NET_SRFSYS_PUBLIC, FileOp);

enum FopFlags
{
  DontOverWriteExistant = 0x0001
};
ACDK_DEF_LIB_ENUM(ACDK_NET_SRFSYS_PUBLIC, FopFlags);

class ACDK_NET_SRFSYS_PUBLIC FileOpMessage
: extends Message
{
  ACDK_WITH_METAINFO(FileOpMessage)
public:
  FileOp fop;
  RFileInfo file;
  RbyteArray data;
  int fopflags;
  FileOpMessage(FileOp op = FileNop)
  : Message(MsgFileOp)
  , fop(op)
  , fopflags(0)
  {
    flags |= MsgExcpectReply;
  }
  foreign virtual void execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver);
  foreign RString toString()
  {
    return RString("FileOpMessage: Fop=[") + fop + "]; File=[" + file->toString() + "]; DataLength=[" + (data == Nil ? 0 : data->length()) + "]; ";
  }
  bool overwriteExistant() 
  {
    return (fopflags & DontOverWriteExistant) == 0;
  }
};

enum AdminCommand
{
  AdminCmdNop = 0x01,
  AdminDisconnect = 0x02,
  AdminCmdShutdown = 0x03,
  AdminCmdPing = 0x04
};
ACDK_DEF_LIB_ENUM(ACDK_NET_SRFSYS_PUBLIC, AdminCommand);

class ACDK_NET_SRFSYS_PUBLIC AdminMessage
: extends Message
{
  ACDK_WITH_METAINFO(AdminMessage)
public:
  int adminCommand;

  AdminMessage()
  : Message(MsgAdmin)
  , adminCommand(AdminCmdNop)
  {
  }
  AdminMessage(AdminCommand cmd)
  : Message(MsgAdmin)
  , adminCommand(cmd)
  {
  }
  foreign virtual void execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver);
};

ACDK_DECL_CLASS(ExceptionMessage);

class ACDK_NET_SRFSYS_PUBLIC ExceptionMessage
: extends ReplyMessage
{
  ACDK_WITH_METAINFO(ExceptionMessage)
public:
  RThrowable exception;
  ExceptionMessage()
  : ReplyMessage(MsgException)
  {
    expectReply(false);
  }
  ExceptionMessage(RThrowable ex)
  : ReplyMessage(MsgException)
  , exception(ex)
  {
    expectReply(false);
  }
  foreign virtual void execute(IN(::acdk::io::RObjectWriter) out, IN(RSRFileSystemServer) fserver);
  foreign virtual RString toString()
  {
    return "ExceptionMessage: Exception=[" + exception->toString() + "]; ";
  }
};

ACDK_DECL_CLASS(TransListenerImpl);
class TransListenerImpl
: extends ::acdk::lang::Object
, implements ::acdk::net::TransListener
{
  virtual bool listen(IN(byte) block, int block_size, int millisecs)
  {
    System::out->print(".");
    System::out->flush();
    return true;
  }
};


} // namespace srfsys
} // namespace net
} // namespace acdk 


#endif //acdk_net_srsync_Message_h
