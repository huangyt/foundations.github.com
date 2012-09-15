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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ftp/FTPClient.cpp,v 1.10 2005/03/26 15:01:09 kommer Exp $


#include "FTPClient.h"

#include <acdk/io/CharToByteWriter.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/util/StringTokenizer.h>
#include <acdk/text/RegExp.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace net {
namespace ftp {

struct StateStack
{
  int& _state;
  int _sicState;
  StateStack(int& state, int newState)
  : _state(state)
  , _sicState(state)
  {
    _state |= newState;
  }
  ~StateStack()
  {
    _state = _sicState;
  }
};

USING_CLASS(::acdk::io::, FileInfo);


RString 
FTPClient::toString()
{
  if (_commandChannel == Nil)
  {
    return "<not connected>";
  }
  return _commandChannel->toString();
}

void 
FTPClient::connect(IN(RString) host, int port)
{
  //_dataChannel = new ServerSocket();
  _commandChannel = new Socket(host, port);
  _cmdReader = new ByteToCharReader(_commandChannel->getInputStream());
  _cmdWriter = new CharToByteWriter(_commandChannel->getOutputStream());
  int code = 0;
  int expcodes[] = { FtpServiceReady, 0 };
  RString s = readResponse(code, expcodes);
  
  _clientState |= FtpClientReady;
  getFeatures();
}

void 
FTPClient::ensureDataChannel()
{
  if (_dataChannel == Nil)
    passiveMode();
}
void 
FTPClient::resetDataChannel()
{
  _serverDataAddress = Nil;
  _serverDataPort = -1;
  _dataChannel->close();
  _dataChannel = Nil;
  
}

RString 
FTPClient::readResponse(OUT(int) code, int* expectedCodes)
{
  RReader r = _commandChannel->getInputStream();
  bool readmultiline = false;
  StringBuffer sb;
  StringBuffer msb;
  int ret;
  code = 0;
  bool attnNewMultiline = false;
  /*
  int available = r->available();
  if (available == 0)
  {
    jlong cd = acdk::util::SysDate().getTime();
    for (int pc = 0; pc < 4; ++pc)
    {
      available = r->available();
      if (available > 0)
        break;
      Thread::sleep(300);
    } 
    if (available == 0)
      return "";
  }
  */
  while (true)
  {
    ret = _cmdReader->readChar();
    if (ret == -1)
      break;
    char c = (char)ret;
    if (c == '\r')
    {
      c = _cmdReader->readChar();
      if (c == '\r') 
      {
        c = _cmdReader->readChar();
      }
      if (c == '\n')
      {
        if (readmultiline == false)
          break;
        attnNewMultiline = true;
        msb.set("");
      }
    }
    else if (attnNewMultiline == true)
    {
      
      if (c == ' ' || c == '-')
      {
        if (msb.length() > 0)
        {
          int ncode = Integer::parseInt(msb.toString());
          if (ncode == code)
            readmultiline = false;
        } 
        else
          attnNewMultiline = false;
      }
      else if (Character::isDigit(c) == false)
        attnNewMultiline = false;
      else
        msb.append(c);
    }
    // #### todo handle multiline http://www.w3.org/Protocols/rfc959/4_FileTransfer.html
    if (code == 0 && (c == ' ' || c == '-'))
    {
      code = Integer::parseInt(sb.toString());
      if (c == '-')
        readmultiline = true;
    }
    sb.append(c);
  }
  ACDK_NLOG("acdk.net.ftp", Info, toString() + "; rcv: " + sb.toString());
  if (expectedCodes != 0)
  {
    for (int i = 0; expectedCodes[i] != 0; ++i)
    {
      if (expectedCodes[i] == code)
        return sb.toString();
      if ((expectedCodes[i] & 0xF000) != 0)
      {
        if (code == FtpAny)
          return sb.toString();

        RString s = Integer::toString(code);
        if ((expectedCodes[i] == FtpAny100 && s->charAt(0) == '1') || 
            (expectedCodes[i] == FtpAny200 && s->charAt(0) == '2') || 
            (expectedCodes[i] == FtpAny300 && s->charAt(0) == '3') || 
            (expectedCodes[i] == FtpAny400 && s->charAt(0) == '4') || 
            (expectedCodes[i] == FtpAny500 && s->charAt(0) == '5') 
            )
          return sb.toString();
      }
    }
    THROW1(IOException, RString("Unexpected FTP reply code: expected: ") + code + ", rcv code: " + sb.toString()); 
  }
  return sb.toString();
}

RString 
FTPClient::readDataAsciiResponse(bool withCmdResponse)
{
  if (_ftpMode == FtpStream)
  {
    ByteToCharReader rin(_dataChannel->getInputStream());
    RString s = rin.readString();
    ACDK_NLOG("acdk.net.ftp", Info, toString() + "; data response: " + s);
    resetDataChannel();
    if (withCmdResponse == true)
    {
      int retcode = 0;
      int expcodes[] = { FtpClosedDataConnection, 0 };
      readResponse(retcode, expcodes);
    }
    return s;
  }
  return Nil;
}

void 
FTPClient::login(IN(RString) user, IN(RString) pass)
{
  checkState();
  {
    StateStack _ss(_clientState, FtpClientInProcess);
    sendCommand("USER " + user);
    int retcode = 0;
    int ulcodes[] = { FtpUserOkNeedPass, 0 };
    RString str = readResponse(retcode, ulcodes);
  
    ACDK_NLOG("acdk.net.ftp", Info, toString() + "; snd: " + "PASS ****");
    _cmdWriter->writeString("PASS " + pass + "\n");
  
    int passcodes[] = { FtpUserLoggedIn, 0 };
    str = readResponse(retcode, passcodes);
  }

  passiveMode();
  //blockMode();
}

RString 
FTPClient::getCwd()
{
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);
  /*
  if (_pwd != Nil)
    return _pwd;
  */
  sendCommand("PWD");
   int retcode = 0;
  int ulcodes[] = { FtpPathNameExists, 0 };
  RString str = readResponse(retcode, 0);
  ::acdk::text::RegExp reg("\\\"(.*?)\\\"");
  RStringArray ma;
  if ((ma = reg.match(str)) != Nil && ma->length() == 2)
  {
    _pwd = ma[1];
  }
  return _pwd;
}

bool
FTPClient::setCwd(IN(RString) newDir)
{
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);
  RString nd = newDir->replace(File::separatorChar(), '/');
  if (nd->equals(_pwd) == true)
    return true;
  
  /*
  if (nd->indexOf(' ') != -1)
    nd = "\"" + nd + "\"";
    */
  if (nd->length() == 0)
    nd = "/";
  sendCommand("CWD " + nd);
  int retcode = 0;
  int ulcodes[] = { FtpFileActionOk, FtpFileNotAvailable, 0 };
  RString ret = readResponse(retcode, ulcodes);
  if (retcode != FtpFileActionOk)
    return false;
  _pwd = nd;
  return true;
}

bool 
FTPClient::setCwdUp()
{
  checkState();
  {
    StateStack _ss(_clientState, FtpClientInProcess);
    sendCommand("CDUP");
    int retcode = 0;
    int ulcodes[] = { FtpFileActionOk, FtpFileNotAvailable, 0 };
    RString ret = readResponse(retcode, ulcodes);
    if (retcode != FtpFileActionOk)
      return false;
  }
  _pwd = getCwd();
  return true;
}

void 
FTPClient::passiveMode()
{
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);

  sendCommand("PASV");
  int retcode;
  int pasvcodes[] = { FtpClosedDataConnection, FtpEnteringPassiveMode, FtpCommanOk, 0 };
  RString str = readResponse(retcode, pasvcodes);
  ::acdk::text::RegExp reg("(\\d+)\\,(\\d+)\\,(\\d+)\\,(\\d+)\\,(\\d+)\\,(\\d+)");
  
  //(h1,h2,h3,h4,p1,p2)
  /* 
  "227 Entering Passive Mode (127,0,0,1,4,51)."
  "227 Data transfer will passively listen to 127,0,0,1,4,51"
  "227 Entering passive mode. 127,0,0,1,4,51"
  */
  RStringArray arr = reg.match(str);
  if (arr->length() != 7)
  {
    // ### todo throw ex
    return;
  }
  
  _serverDataAddress = arr[1] + "." + arr[2] + "." + arr[3] + "." + arr[4];
  int port1 = Integer::parseInt(arr[5]);
  int port2 = Integer::parseInt(arr[6]);
  _serverDataPort = (port1 << 8) + port2;
  _dataChannel = new Socket(_serverDataAddress, _serverDataPort);
  
}

void 
FTPClient::sendCommand(IN(RString) cmd)
{
  ACDK_NLOG("acdk.net.ftp", Info, toString() + "; snd: " + cmd);
  _cmdWriter->writeString(cmd + "\n");
}

void 
FTPClient::getFeatures()
{
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);

  sendCommand("FEAT");
  int code = 0;
  RString resp = readResponse(code, 0);
  if (code == FtpSyntaxError)
    return;
}

void 
FTPClient::blockMode()
{
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);

  sendCommand("MODE B");
  int code = 0;
  //int expcodes = { 
  RString resp = readResponse(code, 0);
  if (code == FtpParameterNotImplemented)
    return;
  _ftpMode = FtpBlock;
}

void 
FTPClient::mkdir(IN(RString) dirname)
{
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);

  sendCommand("MKD " + dirname);
  int code = 0;
  int expcodes[] = { FtpPathNameExists, 0 };
  RString ret = readResponse(code, expcodes);

}

void 
FTPClient::sendFile(IN(RFile) localFile, IN(RString) remoteName)
{
  setFtpTransferType(FtpBinary);
  ensureDataChannel();
  {
    checkState();
    StateStack _ss(_clientState, FtpClientInProcess);

    sendCommand("STOR " + remoteName);
    int code = 0; 
    int expcodes[] = { FtpDataConnectionOpenStartTransfer, FtpFileStatusOk, 0 };
    RString resp = readResponse(code, expcodes);

    RReader in = localFile->getReader();
    in->trans(_dataChannel->getWriter());
    resetDataChannel();
    int expcodes2[] = { FtpFileStatusOk, FtpClosedDataConnection, 0 };
    resp = readResponse(code, expcodes2);
  }
}

RWriter 
FTPClient::getRemoteFileWriter(IN(RString) remoteName)
{
  ensureDataChannel();
  checkState();
  
  RWriter ret;
  {
    StateStack _ss(_clientState, FtpClientInProcess);
    setFtpTransferType(FtpBinary);
    
    sendCommand("STOR " + remoteName);
    int code = 0; 
  
    int expcodes[] = { FtpDataConnectionOpenStartTransfer, FtpFileStatusOk, 0 };
    RString resp = readResponse(code, expcodes);
    ret = _dataChannel->getWriter();
  }
  _clientState |= FtpClientInSend;
  return ret;
}

void
FTPClient::closeRemoteFileWriter()
{
  checkState(FtpClientReady | FtpClientInSend, FtpClientInProcess | FtpClientInReceive);
  _clientState &= ~FtpClientInSend;
  StateStack _ss(_clientState, FtpClientInProcess);

  resetDataChannel();
  int code = 0;
  int expcodes2[] = { FtpFileStatusOk, FtpClosedDataConnection, 0 };
  RString resp = readResponse(code, expcodes2);
  
}

void 
FTPClient::receiveFile(IN(RString) remoteName, IN(RFile) localFile)
{
  
  setFtpTransferType(FtpBinary);
  ensureDataChannel();
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);

  sendCommand("RCV " + remoteName);
  int code = 0; 
  
  int expcodes[] = { FtpDataConnectionOpenStartTransfer, FtpFileStatusOk, 0 };
  RString resp = readResponse(code, expcodes);

  RWriter out = localFile->getWriter();
  _dataChannel->getReader()->trans(out);

  resetDataChannel();
  int expcodes2[] = { FtpFileStatusOk, FtpClosedDataConnection, 0 };
  resp = readResponse(code, expcodes2);
}

RReader
FTPClient::getRemoteFileReader(IN(RString) remoteName)
{
  setFtpTransferType(FtpBinary);
  ensureDataChannel();
  checkState();
  RReader ret;
  {
    StateStack _ss(_clientState, FtpClientInProcess);
    sendCommand("RETR " + remoteName);
    int code = 0; 
    int expcodes[] = { FtpDataConnectionOpenStartTransfer, FtpFileStatusOk, 0 };
    RString resp = readResponse(code, expcodes);
    ret = _dataChannel->getReader();
  }
  _clientState |= FtpClientInReceive;
  return ret;
}

void 
FTPClient::closeRemoteFileReader()
{
  checkState(FtpClientReady | FtpClientInReceive, FtpClientInProcess | FtpClientInSend);
  _clientState &= ~FtpClientInReceive;
  StateStack _ss(_clientState, FtpClientInProcess);

  resetDataChannel();
  int code = 0;
  int expcodes2[] = { FtpFileStatusOk, FtpClosedDataConnection, FtpAny400, 0 };
  RString resp = readResponse(code, expcodes2);
}

void 
FTPClient::deleteFile(IN(RString) remoteName)
{
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);
  sendCommand("DELE " + remoteName);
  int code = 0; 
  int expcodes[] = { FtpFileActionOk, 0 };
  RString resp = readResponse(code, expcodes);
  
}

void 
FTPClient::deleteDirectory(IN(RString) remoteName)
{
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);
  sendCommand("RMD " + remoteName);
  int code = 0; 
  int expcodes[] = { FtpFileActionOk, 0 };
  RString resp = readResponse(code, expcodes);
  
}

void 
FTPClient::rename(IN(RString) fromName, IN(RString) toName)
{
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);
  sendCommand("RNFR " + fromName);
  int code = 0; 
  int expcodes1[] = { FtpPendingFurtherAction, 0 };
  RString resp = readResponse(code, expcodes1);
  sendCommand("RNTO " + toName);
  int expcodes2[] = { FtpFileActionOk, 0 };
  resp = readResponse(code, expcodes2);
}
  
void 
FTPClient::setFtpTransferType(FtpType type)
{
  if (type == _ftpType)
    return;
  if (type == FtpAscii)
    sendCommand("TYPE A");
  else
    sendCommand("TYPE I");
  int code = 0; 
  int expcodes[] = { FtpCommanOk, FtpClosedDataConnection, 0 };
  RString resp = readResponse(code, expcodes);
  _ftpType = type;
}


RStringArray 
FTPClient::listFiles()
{
  RString s = dirList("NLST");

  return acdk::util::StringTokenizer(s, "\r\n").allToken();
}

int unixAccessMaskToFileInfoFlags(IN(RString) access)
{
  if (access == Nil || access->length() == 0)
    return 0;
  int flags = 0;
  if (access->charAt(0) == 'd')
    flags |= FileInfoIsDir;
  else if (access->charAt(0) == '-')
    flags |= FileInfoIsFile;
  if (access->length() < 4)
    return flags;
  if (access->charAt(1) == 'r')
    flags |= FileInfoCanRead;
  if (access->charAt(2) == 'w')
    flags |= FileInfoCanWrite;
  if (access->charAt(3) == 'x')
    flags |= FileInfoCanExec;
  return flags;

} 

::acdk::io::RFileInfo
parseDirList(IN(RString) line)
{
  RString p = "(..........)\\s+(\\d+)\\s+(.+?)\\s+(.+?)\\s+(\\d+)\\s+(.*)";
  acdk::text::RegExp reg(p);
  RStringArray ma;
  RString access;
  RString unknown;
  RString user;
  RString group;
  RString strsize;
  RString rest;
  RString mon;
  RString day;
  RString year;
  RString time;
  RString name;
  int fiflags = 0;
  if ((ma = reg.match(line)) != Nil && ma->length() == 7)
  {
    //System::out->println("matched: " + ma->toString());
    access = ma[1];
    unknown = ma[2];
    user = ma[3];
    group = ma[4];
    strsize = ma[5];
    rest = ma[6];
    /**
      Dec 28 2002
      Jun 21 14:55
    */
    
    acdk::text::RegExp reg1("(...)\\s+(\\d*)\\s+(\\d*\\:\\d*)\\s+(.*)");
    if ((ma = reg1.match(rest)) != Nil &&  ma->length() == 5)
    {
      mon = ma[1];
      day = ma[2];
      time = ma[3];
      name = ma[4];
    }
    else
    {
      acdk::text::RegExp reg1("(...)\\s+(\\d*)\\s+(\\d\\d\\d\\d)\\s+(.*)");
      if ((ma = reg1.match(rest)) != Nil && ma->length() == 5)
      {
        mon = ma[1];
        day = ma[2];
        year = ma[3];
        name = ma[4];
      } 
      else
      {
        System::out->println("cannot parse rest: " + rest);
        /// ### oops
      }
    }
    fiflags = FileInfoExists | unixAccessMaskToFileInfoFlags(access);
  }
  else
  {
    // 10-10-01  03:00AM               110250 giFT-0.9.7.tar.gz
    RString p = "(..)\\-(..)\\-(..)\\s+(\\d+)\\:(\\d+)(..)\\s+(\\d+)\\s+(.*)";
    acdk::text::RegExp reg(p);
    if ((ma = reg.match(line)) != Nil &&  ma->length() == 9)
    {
      day = ma[1];
      mon = ma[2];
      year = ma[3];
      time = ma[4] + ":" + ma[5] + ma[6];
      strsize = ma[7];
      name = ma[8];
      fiflags |= FileInfoExists | FileInfoIsFile;
    }
    else
    {
      // 08-06-03  09:56PM       <DIR>          incoming
      p = "(..)\\-(..)\\-(..)\\s+(\\d+)\\:(\\d+)(..)\\s+\\<DIR\\>\\s+(.*)";
      acdk::text::RegExp reg(p);
      if ((ma = reg.match(line)) != Nil &&  ma->length() == 8)
      {
        day = ma[1];
        mon = ma[2];
        year = ma[3];
        time = ma[4] + ":" + ma[5] + ma[6];
        name = ma[7];
        fiflags |= FileInfoExists | FileInfoIsDir;
      }
      else
      {
        System::out->println("cannot parse line1: " + line);
      }
    }
  }
  if (name == Nil)
    return Nil;
  RFileInfo fi = new FileInfo();
  fi->flags = fiflags;
  fi->name = name;
  // ### parse date and time
  if (strsize != Nil)
    fi->size = Integer::parseInt(strsize);
  return fi;
}

::acdk::io::RFileInfoArray 
FTPClient::listFileInfos()
{
  RString s = dirList();

  RStringArray lines = acdk::util::StringTokenizer(s, "\r\n").allToken();
  ::acdk::io::RFileInfoArray erg = new ::acdk::io::FileInfoArray(0);
  for (int i = 0; i < lines->length(); ++i)
  {
    RFileInfo fi = parseDirList(lines[i]);
    if (fi != Nil)
    {
      fi->dir = _pwd;
      erg->append(fi);
    }
  }
  return erg;
}

RString 
FTPClient::dirList(IN(RString) cmd)
{
  setFtpTransferType(FtpAscii);
  ensureDataChannel();
  checkState();
  StateStack _ss(_clientState, FtpClientInProcess);

  int lstcodes[] = { FtpFileStatusOk, FtpDataConnectionOpenStartTransfer, 0 };
  sendCommand(cmd);
  int retcode = 0;
  RString str = readResponse(retcode, lstcodes);
  
  /*
  ByteToCharReader rin(_dataChannel->getInputStream());
  StringBuffer sb;
  */
  return readDataAsciiResponse();
}

#define FLAG2STRING(name) { name, #name }
struct FlagToString
{
  int flag;
  const char* name;
};
FlagToString _flag2string[] = 
{
  FLAG2STRING(FtpClientReady),
  FLAG2STRING(FtpClientInProcess),
  FLAG2STRING(FtpClientInSend),
  FLAG2STRING(FtpClientInReceive),
  { 0, 0 }
};

RString _stateFlagsToString(int flags)
{
  StringBuffer sb;
  for (int i = 0; _flag2string[i].name != 0; ++i)
    if (_flag2string[i].flag & flags)
      sb << RString(_flag2string[i].name) << RString(" ");
  return sb.toString();
}

void 
FTPClient::checkState(int posmask, int negmask)
{
  if ((_clientState & posmask) != posmask ||
      (_clientState & negmask) != 0)
  {
    StringBuffer sb;
    sb 
        << "FTPClient invalid state. Flags=[" << _stateFlagsToString(_clientState) 
        << "] Required=[" << _stateFlagsToString(posmask) 
        << "] Invalid=[" << _stateFlagsToString(negmask) << "]";
    THROW1(IOException, sb.toString());
  }
}

} // namespace ftp
} // namespace acdk
} // namespace net



