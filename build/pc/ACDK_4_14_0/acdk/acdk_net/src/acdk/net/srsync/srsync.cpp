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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srsync/srsync.cpp,v 1.15 2005/02/05 10:45:30 kommer Exp $

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
#include "SyncFileOperation.h"

namespace acdk {
namespace net {
namespace srsync {

using namespace acdk::net::srfsys;
/*
jlong fileTimeToLong(const FILETIME& ft)
{
  LARGE_INTEGER li;
  li.LowPart = ft.dwLowDateTime;
  li.HighPart = ft.dwHighDateTime;
  return li.QuadPart;
}

void timetest()
{
  SYSTEMTIME s;
  memset(&s, 0, sizeof(s));
  s.wYear = 1980;
  //s.wYear = 2002;
  s.wMonth = 1;
  s.wDay = 1;
  FILETIME ft;
  SystemTimeToFileTime(&s, &ft);
  s.wMilliseconds = 1;
  FILETIME ft2;
  SystemTimeToFileTime(&s, &ft2);
  int dif = ft2.dwLowDateTime - ft.dwLowDateTime;
  jlong tf = fileTimeToLong(ft);
  jlong d = acdk::util::SysDate::fileTimeToTime(ft);
  System::out->println(acdk::util::SysDate(d).toString());
  FILETIME ft3;
  acdk::util::SysDate::timeToFileTime(d, ft3);
}
*/

RSyncFileOperationArray loadFops(IN(RString) loadfile)
{
  FileReader fin(loadfile);
  InputReader in(&fin);
  RSyncFileOperationArray fops = new SyncFileOperationArray(0);
  try {
    while (true)
    {
      RSyncFileOperation fop = new SyncFileOperation(InSync, Nil, Nil);
      if (fop->load(&in) == false)
        break;
      fops->append(fop);
    }
  } catch (::acdk::io::REOFException ex) {
  }
  return fops;
}

void storeFops(IN(RString) storefile, IN(RSyncFileOperationArray) fops)
{
  FileWriter fout(storefile);
  PrintWriter out(&fout);
  for (int i = 0; i < fops->length(); ++i)
  {
    fops[i]->store(&out);
  }
}


class SimpleRemoteSync
: public ::acdk::lang::Object
{
public:
  
  static RString help()
  {
    return 
      "srsync -s [-i inetaddress] [-p port]\n"
      "srsync -c server:port -sync [-onlyc2s | -onlys2c] -l localdir -r remotedir\n"
      "srsync -c server:port -rls remotedir\n"
      "srsync -c server:port -rlsr remotedir\n"
      "srsync -c server:port -rmkdir remotedir\n"
      "srsync -c server:port -shell\n"
      "srsync -shell\n"
      "srsync -batch cmd 1 ; cmd 2\n"
      ;

  }
  static RStringArray shellParseArgs(IN(String) cmdline)
  {
    String::iterator it = cmdline.begin();
    String::iterator end = cmdline.end();
    RStringArray ret = new StringArray(0);
    bool inQuote = false;
    String::iterator beginstr = it;
    while (it < end) 
    {
      if (*it == '"')
      {
        if (inQuote == true)
        { 
          ret->append(new String(beginstr, it));
          inQuote  = false;
          beginstr = it + 1;
        } else {
          inQuote = true;
          beginstr = it + 1;
        }
      } else if (*it == ' ') {
        if (beginstr != it) {
          ret->append(new String(beginstr, it));
        }
        while (it < end && *it == ' ')
        {
          ++it;
        }
        beginstr = it;
      }
      ++it;
    }
    if (beginstr != it)
      ret->append(new String(beginstr, it));
    return ret;
  }
  static RString concatFile(IN(RString) oldir, IN(RString) newdir)
  {
    if (newdir->startsWith("/") == true || (newdir->length() >= 2 && newdir->charAt(1) == ':'))
      return newdir;
    return File(oldir, newdir).getCanonicalPath();
  }
  static int shell(SRFileSystemClient& client, PrintWriter& out, InputReader& in)
  {
    RString curdir = "<unspec>";
    RStringArray args;
    do {
      out.print(curdir + " > "); out.flush();
      RString cmdline = in.readLine();
      args = shellParseArgs(*cmdline);
    } while (exec(client, out, args, curdir));
    return 0;
  }
  static bool exec(SRFileSystemClient& client, PrintWriter& out, IN(RStringArray) args, INOUT(RString) curdir)
  {
    try {
      if (args->length() == 0)
        return true;
      RString cmd = args[0];
      if (cmd->equals("exit") == true)
        return false;
      else if (cmd->equals("cd") == true)
      {
        if (args->length() < 2)
          return true;
        curdir = concatFile(curdir, args[1]);
      } else if (cmd->equals("ls") == true || cmd->equals("dir") == true) {
        bool longformat = false;
        if (cmd->equals("dir") == true)
          longformat = true;
        RString cdir = curdir;
        if (args->length() >= 2)
        {
          cdir = concatFile(curdir, args[1]);
        }
        RFileInfoArray fia = client.loadFileTree(cdir, false);
        for (int i = 0; i < fia->length(); ++i)
        {
          if (longformat == true)
            out.println(fia[i]->toString());
          else
            out.println(fia[i]->getPath());
        }
      } else if (cmd->equals("sync") == true) {
        if (args->length() < 3)
        {
          out.println("usage: sync [-onlyc2s] [-onlys2c] [-store cmdfile] [-load cmdfile] remote local");
          return true;
        }
        bool syncs2c = true;
        bool syncc2s = true;
        int startargs = 1;
        RString storefile;
        RString loadfile;
        while (args->length() > startargs)
        {
          if (args[startargs]->equals("-onlyc2s") == true)
            syncs2c = false;
          else if (args[startargs]->equals("-onlys2c") == true)
            syncc2s = false;
          else if (args[startargs]->equals("-store") == true)
          {
            storefile = args[++startargs];
          }
          else if (args[startargs]->equals("-load") == true)
          {
            loadfile = args[++startargs];
          }
          else
            break;
          ++startargs;
        }
        RString rpath = concatFile(curdir, args[startargs]);
        RString lpath = args[startargs + 1];
        RSyncFileOperationArray ra;
        if (loadfile != Nil)
        {
          ra = loadFops(loadfile);
        } else {
          File rf(client._root);
          if (client._root == Nil || client._files == Nil || 
              File(rpath).equals(&rf) == false)
              client.loadFileTree(rpath);
          RFileInfoArray rfa = client._files;

          File localfile(lpath);
          RFileInfoArray lfa = SRFileSystemServer::loadFileSystem(&localfile, true);

          ra = SyncFileOperation::merge(lpath, lfa, rpath, rfa);
        }
        if (storefile != Nil)
        {
          storeFops(storefile, ra);
        } else {
          for (int i = 0; i < ra->length(); ++i)
          {
            if ((syncs2c && ra[i]->fop  == CopyToLocal) ||
              (syncc2s && ra[i]->fop  == CopyToRemote))
            {
              ra[i]->print(&out);
              ra[i]->sync(&client);
            }
          }
        }
      } else if (cmd->equals("mkimage") == true) {
        if (args->length() < 3)
        {
          out.println("usage: mkimage remotedir remoteimagefile");
          return true;
        }
        RString rpath = concatFile(curdir, args[1]);
        RString imagefile = args[2];
        if (rpath->equals(client._root) == false)
            client.loadFileTree(rpath);
        FileWriter fout(imagefile);
        BinaryObjectWriter bout(&fout);
        bout.writeObject(&client);
      
      } else if (cmd->equals("loadimage") == true) {
        if (args->length() < 2)
        {
          out.println("usage: loadimage imagefile");
          return true;
        }
        RString imagefile = args[1];
        FileReader fin(imagefile);
        BinaryObjectReader bin(&fin);
        RSRFileSystemClient cl = (RSRFileSystemClient) bin.readObject();
        for (int i = 0; i < cl->_files->length(); ++i)
        {
          cl->_files[i]->isChecked(false);
        }
        client._root = cl->_root;
        client._files = cl->_files;
      } else if (cmd->equals("connect") == true) {
        if (args->length() != 4)
        {
          out.println("usage: connect host:port user pass");
          return true;
        }
        client.connect(args[1], args[2], args[3]);
      } else if (cmd->equals("shutdown") == true) {
        client.shutdownServer();
      }
    } catch (RThrowable ex) {
      out.println(ex->getMessage());
    }
    
    return true;
  }
  static int acdkmain(RStringArray args)
  {
    //timetest();
    //test2();
    System::out->println(args->toString());
    acdk::lang::CmdLineParser cmdparser;
    try {
      
      cmdparser.addOption("-s", "SRS_AS_SERVER", false, "Run as Server");
      cmdparser.addOption("-c", "SRS_AS_CLIENT", true, "Run as Client expects servername:port as argument");
      cmdparser.addOption("-p", "SRS_PORT", true, "Port Number to listen");
      cmdparser.addOption("-i", "SRS_INET", true, "Inet address to listen");
      cmdparser.addOption("-sync", "SRS_SYNC", false, "Synchronize with Server");
      cmdparser.addOption("-onlyc2s", "SRS_SYNC_C2S", false, "Synchronize only files to server");
      cmdparser.addOption("-onlys2c", "SRS_SYNC_S2C", false, "Synchronize only files to client");
      cmdparser.addOption("-l", "SRS_LDIR", true, "Local directory");
      cmdparser.addOption("-r", "SRS_RDIR", true, "remote directory");
      cmdparser.addOption("-rls", "SRS_RLS", true, "List directory on remote");
      cmdparser.addOption("-rlsr", "SRS_RLSR", true, "List directory on remote recursivally");
      cmdparser.addOption("-shell", "SRS_SHELL", false, "simple remote shell");
      cmdparser.addOption("-batch", "SRS_BATCH", false, "simple remote shell");
      cmdparser.addOption("-neveroverwrite", "SRS_NEVEROVERWRITE", false, "never overwrite existant files");
      cmdparser.addOption("-ignorefiletime", "SRS_IGNOREFILETIME", false, "simple remote shell");
      ::acdk::util::RProperties options = cmdparser.parse(args, true /* ignoreUnknown */);
      


      File cfgfile(System::getModuleDir(), System::getModuleName() + ".cfg");

      acdk::util::RPropertiesListener proplistener;
      if (cfgfile.exists() == true)
      {
        proplistener = new acdk::util::PropertiesListener(cfgfile.getPath(), System::getProperties());
        proplistener->start();
#if defined(ACDK_OS_WIN32)
        System::out->println("Hit Ctrl-Break to reload: " + cfgfile.getPath());
#else 
        System::out->println("Send SIGHUP to reload: " + cfgfile.getPath());
#endif
      }
      if (options->getProperty("SRS_NEVEROVERWRITE", Nil) != Nil) 
        SyncFileOperation::NeverOverwriteFiles = true;
      if (options->getProperty("SRS_IGNOREFILETIME", Nil) != Nil) 
        SyncFileOperation::IgnoreFileTime = true;
      if (options->getProperty("SRS_AS_SERVER", Nil) != Nil) {
        RSRFileSystemServer server;
        RString servaddr;
        int port = 7777;
        if (options->getProperty("SRS_PORT", Nil) != Nil)
          port = Integer::parseInt(options->getProperty("SRS_PORT"));
        if (options->getProperty("SRS_INET", Nil) != Nil)
          servaddr = options->getProperty("SRS_INET");
        server = new SRFileSystemServer(servaddr, port);
        server->_neverOverwriteFile = SyncFileOperation::NeverOverwriteFiles;

        server->start();
        server->join();
      } else if (options->getProperty("SRS_AS_CLIENT", Nil) != Nil) {
        RString server = options->getProperty("SRS_AS_CLIENT");
        if (server->lastIndexOf(':') == -1)
          THROW3_FQ(::acdk::lang::, CmdLineParseException, "Invalid format on internetaddress: " + server, args, &cmdparser);
        int port = Integer::parseInt(server->substr(server->lastIndexOf(':') + 1));

        server = server->substr(0, server->lastIndexOf(':'));
        SRFileSystemClient remotefs(::acdk::net::InetAddress::getByName(server), port);
        remotefs.login("roger", "roger");
        if (options->getProperty("SRS_SYNC", Nil) != Nil) {
          if (options->getProperty("SRS_LDIR", Nil) == Nil || options->getProperty("SRS_RDIR", Nil) == Nil)
            THROW3_FQ(::acdk::lang::, CmdLineParseException, "Need -l and -r options for synchronization", args, &cmdparser);
          if (options->getProperty("SRS_LDIR", Nil) == Nil || options->getProperty("SRS_RDIR", Nil) == Nil)
            THROW3_FQ(::acdk::lang::, CmdLineParseException, "Need -l and -r options for synchronization", args, &cmdparser);
          RString rpath = options->getProperty("SRS_RDIR");
          RString lpath = options->getProperty("SRS_LDIR");

          bool syncs2c = true;
          bool syncc2s = true;
          if (options->getProperty("SRS_SYNC_C2S", Nil) != Nil) 
            syncs2c = false;
          if (options->getProperty("SRS_SYNC_S2C", Nil) != Nil) 
            syncc2s = false;
          if (rpath->equals(remotefs._root) == false)
            remotefs.loadFileTree(rpath);
          RFileInfoArray rfa = remotefs._files;
          
          File localfile(lpath);
          RFileInfoArray lfa = SRFileSystemServer::loadFileSystem(&localfile);
          RSyncFileOperationArray ra = SyncFileOperation::merge(lpath, lfa, rpath, rfa);
          for (int i = 0; i < ra->length(); ++i)
          {
            if ((syncs2c && ra[i]->fop  == CopyToLocal) ||
                (syncc2s && ra[i]->fop  == CopyToRemote))
            {
              ra[i]->print(&System::out);
              ra[i]->sync(&remotefs);
            }
          }
        } 
        if (options->getProperty("SRS_RLS", Nil) != Nil || options->getProperty("SRS_RLSR", Nil) != Nil)
        {
          RString remdir;
          bool recursive = false;
          if (options->getProperty("SRS_RLS", Nil) != Nil)
            remdir = options->getProperty("SRS_RLS");
          else {
            remdir = options->getProperty("SRS_RLSR");
            recursive = true;
          }

          RFileInfoArray rfa = remotefs.loadFileTree(remdir, recursive);
          for (int i = 0; i <   rfa->length(); ++i)
          {
            System::out->println(rfa[i]->getPath());
          }
        } else if (options->getProperty("SRS_SHELL", Nil) != Nil) {
          int ret = shell(remotefs, *System::out, *System::in);
          remotefs.disconnect();
          return ret;
        }
        remotefs.disconnect();
      } else if (options->getProperty("SRS_SHELL", Nil) != Nil) {
        SRFileSystemClient remotefs;
        int ret = shell(remotefs, *System::out, *System::in);
      } else if (options->getProperty("SRS_BATCH", Nil) != Nil) {
        int i;
        for (i = 1; i < args->length(); ++i)
          if (args[i]->equals("-batch") == true)
            break;
        SRFileSystemClient remotefs;
        RStringArray targs = new StringArray(0);
        RString curdir = "<unspec>";
        ++i;
        for (; i < args->length(); ++i)
        {
          if (args[i]->equals(";") == true)
          {
            exec(remotefs, *System::out, targs, curdir);
            targs = new StringArray(0);
          } else {
            targs->append(args[i]);
          }
        }
        if (targs->length() > 0)
          exec(remotefs, *System::out, targs, curdir);
      } else {
        THROW3_FQ(::acdk::lang::, CmdLineParseException, "", args, &cmdparser);
      }
    } catch (::acdk::lang::RCmdLineParseException ex) {
      System::out->println(ex->getMessage());
      System::out->println(help()); 
      cmdparser.printHelp(System::out);
    } catch (::acdk::lang::RThrowable ex) {
      
      System::out->println(ex->getMessage());
      
    }
    /*
    if (args->length() < 2)
    return runAsClient();
    if (args[1]->equals("-s") == true)
    return runAsServer();
    */
    return 0;
  }
};

} //namespace srsync
} //namespace acdk 
} // namespace acdk 



int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(acdk::net::srsync::SimpleRemoteSync::acdkmain, argc, argv, envptr);
}
