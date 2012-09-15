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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srsync/SyncFileOperation.h,v 1.10 2005/03/31 17:19:54 kommer Exp $

#ifndef acdk_net_srsync_SyncFileOperation_h
#define acdk_net_srsync_SyncFileOperation_h

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/CmdLineParser.h>
#include <acdk/lang/CmdLineParseException.h>

#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>

#include <acdk/util/Arrays.h>
#include <acdk/util/PropertiesListener.h>

#include <acdk/net/srfsys/Message.h>
#include <acdk/net/srfsys/SRFileSystemClient.h>
#include <acdk/net/srfsys/SRFileSystemServer.h>
#include <acdk/lang/sys/core_memcheck.h>
#include <acdk/lang/sys/ObjectHeap.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace net {
namespace srsync {

USING_CLASS(::acdk::io::, File);
using namespace acdk::net::srfsys;

template <class T>
RObjectArrayImpl<T> elementArrayCast(const RObject& other) //### as static member template to RObjectArrayImpl
{
  RObjectArray ra = (RObjectArray) other;
  RObjectArrayImpl<T> ret = new ObjectArrayImpl<T>(ra->length());
  for (int i = 0; i < ret->length(); ++i)
  {
    ret[i] = (T)ra[i];
  }
  return ret;
}

enum SyncFileOp 
{
  InSync = 0,
  CopyToRemote,
  CopyToLocal,
  RemoveRemote,
  RemoveLocal,
  MoveLocal,
  MoveRemote

};


ACDK_DECL_CLASS(SyncFileOperation);

class SyncFileOperation
: extends ::acdk::lang::Object
{
public:
  static bool NeverOverwriteFiles;
  static bool IgnoreFileTime;
  SyncFileOp fop;
  RFileInfo localFileInfo;
  RFileInfo remoteFileInfo;
  SyncFileOperation(SyncFileOp op, RFileInfo lfi, RFileInfo rfi = Nil)
  : fop(op)
  , localFileInfo(lfi)
  , remoteFileInfo(rfi)
  {
  }
  void sync(IN(RSRFileSystemClient) fs)
  {
    try {
    switch(fop)
    {
    case InSync:
      return;
    case CopyToRemote:
    {
      if (localFileInfo->isDirectory() == true) {
        //File(localFileInfo->getPath()).mkdirs();
        fs->sendFile(remoteFileInfo, Nil);
      } else if (localFileInfo->isFile() == true) {
        acdk::io::MemWriter mout;
        File(localFileInfo->getPath()).getReader()->trans(&mout);
        fs->sendFile(remoteFileInfo, mout.getBuffer());
        break;
      } else {
        ACDK_NLOGP("acdk.net.srfsys", Error, "Local file is neither dir nor file", LOG_NPV("LocalPath", &localFileInfo->getPath()));
      }
    }
    case CopyToLocal:
    {
      if (remoteFileInfo->isDirectory() == true)
      {
        File(localFileInfo->getPath()).mkdirs();
      } else if (remoteFileInfo->isFile() == true) {
       
        acdk::lang::sys::core_memcheck check;
        {
        File tf(localFileInfo->getPath());
        if (NeverOverwriteFiles == true || tf.exists() == false)
        {
          MemReader(fs->retriveFile(remoteFileInfo)).trans(tf.getWriter());
        }
        // System::out->println(remoteFileInfo->toString());
        tf.setLastModified(remoteFileInfo->modified);
        tf.setFileCreated(remoteFileInfo->created);
        }
        
      }
      break;
    }
    default:
      break;
    }
    } catch (RThrowable ex) {
      ACDK_NLOGP("acdk.net.srfsys", Error, "Sync failed", LOG_NPV("SyncFileOperation", &toString()) <<
                                                          LOG_NPV("Reason", &ex->getMessage()));
    }
  }
  void print(IN(::acdk::io::RPrintWriter) out)
  {
    switch(fop)
    {
    case InSync:
      out->print("NOP ");
      out->println(localFileInfo->getPath());
      break;
    case CopyToRemote:
      out->print("CPR ");
      out->print("\"");
      out->print(localFileInfo->getPath());
      //out->print("\" \"");
      //out->print(remoteFileInfo->getPath());
      out->println("\"");
      break;
    case CopyToLocal:
      out->print("CPL ");
      out->print("\"");
      out->print(remoteFileInfo->getPath());
      //out->print("\" \"");
      //out->print(localFileInfo->getPath());
      out->println("\"");
      break;
    case RemoveRemote:
      out->print("RMR ");
      out->print("\"");
      out->print(remoteFileInfo->getPath());
      out->println("\"");
      break;      
    case RemoveLocal:
      out->print("RML ");
      out->print("\"");
      out->print(localFileInfo->getPath());
      out->println("\"");
      break;      
    case MoveLocal:
      out->print("MVL ");
      out->print("\"");
      out->print(localFileInfo->getPath());
      out->print("\"");
      out->print(" \"");
      out->print(remoteFileInfo->getPath());
      out->print("\"");
      break;
    case MoveRemote:
      out->print("MVR ");
      out->print("\"");
      out->print(remoteFileInfo->getPath());
      out->print("\"");
      out->print(" \"");
      out->print(localFileInfo->getPath());
      out->print("\"");
      break;
    }
  }
  void store(IN(::acdk::io::RPrintWriter) out)
  {
    switch(fop)
    {
    case InSync:
      out->print("NOP ");
      out->printQuoted(localFileInfo->getPath());
      out->print("\n");
      break;
    case CopyToRemote:
      out->print("CPR ");
      //out->print("\"");
      localFileInfo->dump(out);
      out->print(" ");
      out->printQuoted(remoteFileInfo->getPath());
      out->print("\n");
      break;
    case CopyToLocal:
      out->print("CPL ");
      //out->print("\"");
      remoteFileInfo->dump(out);
      out->print(" ");
      out->printQuoted(localFileInfo->getPath());
      out->print("\n");
      break;
    case RemoveRemote:
      out->print("RMR ");
      out->printQuoted(remoteFileInfo->getPath());
      out->print("\n");
      break;      
    case RemoveLocal:
      out->print("RML ");
      out->printQuoted(localFileInfo->getPath());
      out->print("\n");
      break;
   case MoveLocal:
      out->print("MVL ");
      localFileInfo->dump(out);
      out->print(" ");
      out->printQuoted(remoteFileInfo->getPath());
      out->print("\n");
      break;
    case MoveRemote:
      out->print("MVR ");
      remoteFileInfo->dump(out);
      out->print(" ");
      out->printQuoted(localFileInfo->getPath());
      out->print("\n");
      break;
    }
  }
  static RFileInfo readFileInfo(IN(::acdk::io::RInputReader) in)
  {
    RString fn = in->readQuoted();
    RFileInfo fi = new FileInfo(fn);
    fi->size = Integer::parseInt(in->readAString()->trim());
    fi->flags = Integer::parseInt(in->readAString()->trim());
    fi->created = Long::parseLong(in->readAString()->trim());
    fi->modified = Long::parseLong(in->readAString()->trim());
    fi->digest = Long::parseLong(in->readAString()->trim());
    return fi;
  }
  bool load(IN(::acdk::io::RInputReader) in)
  {
    RString cmd = in->readAString();
    if (cmd == Nil)
      return false;
    if (cmd->equals("NOP") == true)
    {
      fop = InSync;
      RString f = in->readQuoted();
      localFileInfo = new FileInfo(f);
      in->readLine();
    } else if (cmd->equals("CPR") == true || cmd->equals("CPL") == true ||
               cmd->equals("MVL") == true || cmd->equals("MVR") == true) {
      RFileInfo fi = readFileInfo(in);
      RString fn = in->readQuoted();
      if (cmd->equals("CPR") == true)
      {
        fop = CopyToRemote;
        localFileInfo = fi;
        remoteFileInfo = new FileInfo(fn);
        remoteFileInfo->size = fi->size;
        remoteFileInfo->created = fi->created;
        remoteFileInfo->modified = fi->modified;
        remoteFileInfo->flags = fi->flags;
      } else if (cmd->equals("CPL") == true) {
        fop = CopyToLocal;
        remoteFileInfo = fi;
        localFileInfo = new FileInfo(fn);
        localFileInfo->size = fi->size;
        localFileInfo->created = fi->created;
        localFileInfo->modified = fi->modified;
        localFileInfo->flags = fi->flags;
      }  else if (cmd->equals("MVL") == true) {
        fop = MoveLocal;
        remoteFileInfo = fi;
        localFileInfo = new FileInfo(fn);
        localFileInfo->size = fi->size;
        localFileInfo->created = fi->created;
        localFileInfo->modified = fi->modified;
        localFileInfo->flags = fi->flags;
      } else if (cmd->equals("MVR") == true) {
        fop = MoveRemote;
        remoteFileInfo = fi;
        localFileInfo = new FileInfo(fn);
        localFileInfo->size = fi->size;
        localFileInfo->created = fi->created;
        localFileInfo->modified = fi->modified;
        localFileInfo->flags = fi->flags;
      }
      in->readLine();
    }
    return true;
  }
  static RSyncFileOperationArray merge(IN(RString) lpath, IN(RFileInfoArray) locals, IN(RString) rpath, IN(RFileInfoArray) remotes);
};

ACDK_DECL_CLASS(SynFileComparator);

class SynFileComparator
: extends acdk::lang::Object
, implements acdk::util::Comparator
{
public:
  RString localPath;
  RString remotePath;
  SynFileComparator(IN(RString) localp, IN(RString) remotep)
  : Object()
  , localPath(localp)
  , remotePath(remotep)
  {
  }

  virtual int compare(IN(RObject) o1, IN(RObject) o2)
  {
    return compare(RFileInfo(o1), RFileInfo(o2));
  }
  virtual int compare(IN(RFileInfo) loc, IN(RFileInfo) rem)
  {
    if (rem->dir->length() < remotePath->length())
    {
      ACDK_NLOGP("acdk.net.srfsys", Error, "RemotePath not remotePath", LOG_NPV("RemoteDir", &rem->dir) << 
                                                                        LOG_NPV("RemotePath", &remotePath));

      return 1;
    }
    if (loc->dir->length() < localPath->length())
    {
      ACDK_NLOGP("acdk.net.srfsys", Error, "RemotePath not remotePath", LOG_NPV("RemoteDir", &rem->dir) << 
                                                                        LOG_NPV("remotePath", &remotePath));
      return -1;
    }
    RString rdir = rem->dir->substr(remotePath->length());
    RString ldir = loc->dir->substr(localPath->length());
    int t = ldir->compareTo(rdir);
    if (t != 0)
      return t;
    t = loc->name->compareTo(rem->name);
    if (t != 0)
      return t;
    t = rem->digest - loc->digest;
    return t;
  }
};

} //namespace srsync
} //namespace acdk 
} // namespace acdk 



#endif //acdk_net_srsync_SyncFileOperation_h
