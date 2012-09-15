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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ftp/FTPClient.h,v 1.8 2005/03/26 15:01:09 kommer Exp $

#ifndef acdk_net_ftp_FTPClient_h
#define acdk_net_ftp_FTPClient_h

#include "Config.h"
#include <acdk/io/CharReader.h>
#include <acdk/io/CharWriter.h>
#include <acdk/io/FileInfo.h>
#include "../ServerSocket.h"
#include "../Socket.h"

namespace acdk {
namespace net {
namespace ftp {

foreign enum FtpReplyCodes
{
  /**
    110 Restart marker reply.
             In this case, the text is exact and not left to the
             particular implementation; it must read:
                  MARK yyyy = mmmm
             Where yyyy is User-process data stream marker, and mmmm
             server's equivalent marker (note the spaces between markers
             and "=").
  */
  FtpRestart = 110, 
  /*
         120 Service ready in nnn minutes.
  */
  /**
         125 Data connection already open; transfer starting.
  */
  FtpDataConnectionOpenStartTransfer = 125,
  /**
         150 File status okay; about to open data connection.
  */
  FtpFileStatusOk = 150,
  /**
    200 Command okay.
  */
  FtpCommanOk = 200,
  /*
         202 Command not implemented, superfluous at this site.
         211 System status, or system help reply.
         212 Directory status.
         213 File status.
         214 Help message.
             On how to use the server or the meaning of a particular
             non-standard command.  This reply is useful only to the
             human user.
         215 NAME system type.
             Where NAME is an official system name from the list in the
             Assigned Numbers document.
      */
  /**
         220 Service ready for new user.
    */
    FtpServiceReady = 220,
    /*
         221 Service closing control connection.
             Logged out if appropriate.
         225 Data connection open; no transfer in progress.
    */
    /**
         226 Closing data connection.
             Requested file action successful (for example, file
             transfer or file abort).
    */
    FtpClosedDataConnection = 226,

    
    /**
         227 Entering Passive Mode (h1,h2,h3,h4,p1,p2).
    */
    FtpEnteringPassiveMode = 227,
    /**
         230 User logged in, proceed.
    */
    FtpUserLoggedIn = 230,
    /**
    250 Requested file action okay, completed.
    */
    FtpFileActionOk = 250,
    /**
         257 "PATHNAME" created.
    */
    FtpPathNameExists = 257,
    /**
         331 User name okay, need password.
    */
    FtpUserOkNeedPass = 331,
    /*
         332 Need account for login.
    */
    /**
         350 Requested file action pending further information.
    */
    FtpPendingFurtherAction = 350,
    /**
    Requested file action not taken.
             File unavailable (e.g., file busy).
    */
    FtpTransferAborted = 450, 
    /*
         421 Service not available, closing control connection.
             This may be a reply to any command if the service knows it
             must shut down.
         425 Can't open data connection.
         426 Connection closed; transfer aborted.
         451 Requested action aborted: local error in processing.
         452 Requested action not taken.
             Insufficient storage space in system.
    */
      /**
            500 Syntax error, command unrecognized.
             This may include errors such as command line too long.
      */
      FtpSyntaxError = 500,
    /*
         501 Syntax error in parameters or arguments.
         502 Command not implemented.
         503 Bad sequence of commands.
    */
    /**
         504 Command not implemented for that parameter.
    */
    FtpParameterNotImplemented = 504,
    /*
         530 Not logged in.
         532 Need account for storing files.
    */
    /**
         550 Requested action not taken.
             File unavailable (e.g., file not found, no access).
    */
    FtpFileNotAvailable = 550,
    /*
         551 Requested action aborted: page type unknown.
         552 Requested file action aborted.
             Exceeded storage allocation (for current directory or
             dataset).
         553 Requested action not taken.
             File name not allowed.
*/
  FtpAny100 = 0xFFF1,
  FtpAny200 = 0xFFF2,
  FtpAny300 = 0xFFF3,
  FtpAny400 = 0xFFF4,
  FtpAny500 = 0xFFF5,

  /**
    used to accept any code
  */
  FtpAny   = 0xFFFF
};

foreign enum FtpMode
{
  FtpStream,
  FtpBlock,
  FtpCompressed
};

foreign enum FtpType
{
  FtpAscii,
  FtpBinary
};

/**
  internal state of the FTP client
*/
foreign enum FtpClientState
{
  /**
    No connection yet
  */
  FtpClientInit       = 0x0000, 
  /**
    connection established
  */
  FtpClientReady      = 0x0001,
  /**
    Currently transfering command
  */
  FtpClientInProcess  = 0x0002,
  /**
    getRemoteFileWriter() was called
  */
  FtpClientInSend     = 0x0004,
  /**
    getRemoteFileReader() was called
  */
  FtpClientInReceive  = 0x0008
};

ACDK_DECL_CLASS(FTPClient);


class ACDK_NET_FTP_PUBLIC FTPClient
: extends acdk::lang::Object
{
  RSocket _commandChannel;
  RSocket _dataChannel;
  ::acdk::io::RCharReader _cmdReader;
  ::acdk::io::RCharWriter _cmdWriter;
  FtpMode _ftpMode;
  RString _serverDataAddress;
  int _serverDataPort;
  RString _pwd;
  FtpType _ftpType;
  int _clientState;
public:

  FTPClient() 
  : _ftpMode(FtpStream)
  , _serverDataPort(0)
  , _ftpType(FtpAscii) 
  , _clientState(FtpClientInit)
  {}
  RString toString();
  void connect(IN(RString) host, int port);
  void login(IN(RString) user, IN(RString) pass);
  RStringArray listFiles();
  ::acdk::io::RFileInfoArray listFileInfos();
  RString dirList(IN(RString) cmd = "LIST -aL");
  RString getCwd();
  void mkdir(IN(RString) dirname);
  void sendFile(IN(RFile) localFile, IN(RString) remoteName);
  void receiveFile(IN(RString) remoteName, IN(RFile) localFile);
  void deleteFile(IN(RString) remoteName);
  void deleteDirectory(IN(RString) remoteName);
  void rename(IN(RString) fromName, IN(RString) toName);
  /**
    return true if new dir is set
  */
  bool setCwd(IN(RString) newDir);
  /**
    return true if parent directory was entered
  */
  bool setCwdUp();
  void passiveMode();
  void blockMode();
  void setFtpTransferType(FtpType type);
  void sendCommand(IN(RString) cmd);
  /**
    return a file writer to remote file.
    has to be closed via 
    closeRemoteFileWriter()
    No other ftp command are allowed between 
    getRemoteFileWriter and closeRemoteFileWriter
  */
  acdk::io::RWriter getRemoteFileWriter(IN(RString) remoteName);
  /**
    @see getRemoteFileWriter
  */
  void closeRemoteFileWriter();
  /**
    @see getRemoteFileWriter
  */
  acdk::io::RReader getRemoteFileReader(IN(RString) remoteName);
  /**
    @see getRemoteFileReader
   */
  void closeRemoteFileReader();
protected:
  void ensureDataChannel();
  void resetDataChannel();
  void getFeatures();
  /** 
    throws IOException if not FtpClientReady or FtpClientInProcess
  */
  void checkState(int posmask = FtpClientReady, int negmask = FtpClientInProcess | FtpClientInSend | FtpClientInReceive);
  
  RString readResponse(OUT(int) code, int* expectedCode = 0);
  RString readDataAsciiResponse(bool withCmdResponse = true);
};

} // namespace ftp
} // namespace acdk
} // namespace net

#endif //acdk_net_ftp_FTPClient_h

