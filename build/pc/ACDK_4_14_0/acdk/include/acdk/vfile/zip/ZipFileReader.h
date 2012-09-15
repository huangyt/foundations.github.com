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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/zip/ZipFileReader.h,v 1.7 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_vfile_zip_ZipFileReader_h
#define acdk_vfile_zip_ZipFileReader_h

#include <acdk.h>


#include "../Config.h"
#include "LocalFileHeader.h"
#include <acdk/io/Storage.h>
#include "ZipFileImpl.h"

namespace acdk {
namespace vfile {
namespace zip {



ACDK_DECL_CLASS(ZipFileReader);

class ACDK_VFILE_PUBLIC ZipFileReader
: extends acdk::lang::Object
, implements acdk::io::Reader
, implements acdk::io::Storage
{
private:
  RZipFileImpl _zip;
  acdk::io::RReader _in;
  int _cpos;
public:
  ZipFileReader(IN(RZipFileImpl) zip);
  ~ZipFileReader();
  foreign int available();
  foreign void close();
  foreign jlong seek(acdk::io::SeekPos seekrel, jlong seekpos);
  foreign jlong skip(jlong n);
  foreign int read();
  foreign int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign int read(byte* buffer, int offset, int len);
  foreign void mark(int readAheadLimit)
  {
    THROW0(UnsupportedOperationException);
  }
  foreign bool markSupported() { return false; }
  foreign void reset();
  foreign bool ready();

  foreign RString getDeviceName();
  foreign bool isWriteable() { return false; }
  foreign bool isReadable() { return true; }
};



} // zip
} // vfile
} // acdk



#endif //acdk_vfile_zip_ZipFileReader_h

