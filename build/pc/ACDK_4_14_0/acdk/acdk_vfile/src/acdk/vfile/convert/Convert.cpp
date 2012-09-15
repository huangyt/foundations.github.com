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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/convert/Convert.cpp,v 1.7 2005/02/05 10:45:33 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/SystemError.h>
#include <acdk/lang/CmdLineParser.h>

#include <acdk/io/FileReader.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/StringWriter.h>
#include <acdk/io/CharToByteWriter.h>

#include <acdk/util/Properties.h>

#include <acdk/vfile/zip/ZipFileSystem.h>

namespace acdk {
namespace vfile {
namespace file2rc {

using namespace acdk::lang;
using namespace acdk::io;

class Convert
{
public:
  enum Operation
  {
    Dump,
    Test,
    Extract
  };
  static void extract(RFile f, RString indent, Operation op);
  static void help()
  {
    System::out->println(
"acdk_vfile_convert <archive>\n\n"
"Archive will be written into file system\n"
);
  }
  static int acdkmain(RStringArray sa)
  {
    CmdLineParser cmdline;
    cmdline.addOption("-d", "-d", false, "-d dump to Console");
    cmdline.addOption("-t", "-t", false, "-t test archive and print content table");
    

   try {
      acdk::util::RProperties props = cmdline.parse(sa, false, true);
      Operation op = Extract;
      if (props->containsKey(new String("-t")) == true)
        op = Test;
      else if (props->containsKey(new String("-d")) == true)
        op = Dump;
      if (sa->length() < 2) {
        help();
        return 1;
      }
      acdk::vfile::zip::ZipFileSystem::loadFileSystem();
      File fin(sa[1] + "@");
      extract(&fin, "", op);
    } catch (RThrowable ex) {
      System::out->println(ex->getMessage());
      return 1;
    }
    return 0;
  }
};

void 
Convert::extract(RFile f, RString indent, Operation op)
{

  RStringArray sa = f->list();
  for (int i = 0; i < sa->length(); ++i)
  {
    RFile sf = new File(f, sa[i]); 
    if (sf->isDirectory() == true) {
      System::out->println(indent + "d " + sa[i]);
      File f(sf->getPath());
      if (op == Extract)  {
        if (f.exists() == false && f.mkdir() == false)
          THROW1(SystemError, "cannot craete dir: " + sf->getPath());
      }
      extract(sf, indent + " ", op);
    } else {
      System::out->println(indent + "f " + sa[i]);
      ::acdk::io::RCharWriter out;
      if (op == Dump) {
        System::out->println("============================================");
        out = &System::out;
      } else if (op == Test) {
        out = new StringWriter();
      } else  {
        out = new CharToByteWriter(new ::acdk::io::FileWriter(sf->getPath()));
      }
      RReader r = sf->getReader();
      r->trans(out->getWriter());
    }
  }
}


} //namespace file2rc 
} //namespace vfile 
} //namespace acdk 


int main(int argc, char* argv[], char* envp[])
{
  return acdk::lang::System::main(acdk::vfile::file2rc::Convert::acdkmain, argc, argv, envp);
}



