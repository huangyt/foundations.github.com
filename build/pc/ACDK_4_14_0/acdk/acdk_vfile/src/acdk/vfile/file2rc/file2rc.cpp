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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/file2rc/file2rc.cpp,v 1.7 2005/02/05 10:45:33 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/CharToByteWriter.h>

namespace acdk {
namespace vfile {
namespace file2rc {

using namespace acdk::lang;
using namespace acdk::io;
class File2RC
{
public:
  static void trans(Reader& in, CharWriter& out)
  {
    out.writeString("unsigned char ressource_array[] = {\n  ");

    const int BufferSize = 128;
    byte buffer[BufferSize];
    int readed = 0;
    int written = 0;
    int linewritten = 0;
    do {
      readed = in.read(buffer, 0, BufferSize);
      for (int i = 0; i < BufferSize; ++i)
      {
        if (written > 0) {
          out.writeString(", ");
        } else {
          out.writeString("  ");
        }
        if (buffer[i] < 100) {
          out.writeChar(' ');
          if (buffer[i] < 10)
            out.writeChar(' ');
        }
        out.writeString(Integer::toString(buffer[i]));
        ++written;
        ++linewritten;
        if (linewritten > 20) {
          out.writeString("\n  ");
          linewritten = 0;
        }
      }
    } while (readed == BufferSize);
    out.writeString("\n};\n\nint ressource_size = ");
    out.writeString(Integer::toString(written));
    out.writeString(";\n\n");
    
  }
  static void help()
  {
    System::out->println("file2rc <input> <output>");
  }
  static int acdkmain(RStringArray sa)
  {
    if (sa->length() < 3) {
      help();
      return 1;
    }
    FileReader fin(sa[1]);
    FileWriter fout(sa[2]);
    CharToByteWriter cfout(&fout);
    trans(fin, cfout);
    return 0;
  }
};


} //namespace file2rc 
} //namespace vfile 
} //namespace acdk 


int main(int argc, char* argv[], char* envp[])
{
  return acdk::lang::System::main(acdk::vfile::file2rc::File2RC::acdkmain, argc, argv, envp);
}



