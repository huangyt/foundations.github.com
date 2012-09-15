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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/zip/ZipFileReader.cpp,v 1.9 2005/02/05 10:45:33 kommer Exp $


#include "ZipFileReader.h"
#include <acdk/io/Reader.h>
#include <acdk/io/EOFException.h>
#include <acdk/vfile/InflaterReader.h>

namespace acdk {
namespace vfile {
namespace zip {


ZipFileReader::ZipFileReader(IN(RZipFileImpl) zip)
: _zip(zip)
, _cpos(0)
{
  if (_zip->isDirectory() == true)
    THROW1_FQ(acdk::io::, IOException, RCS("Cannot read from Directory: ") + _zip->getCanonicalPath());
  reset();
}

ZipFileReader::~ZipFileReader()
{
  close();
}

int 
ZipFileReader::available()
{
  return _zip->_lfh->size - _cpos;
}
void 
ZipFileReader::close()
{
  _in->close();
}

jlong 
ZipFileReader::seek(acdk::io::SeekPos seekrel, jlong seekpos)
{
  if (seekrel != acdk::io::SeekCur)
    THROW1_FQ(acdk::io::, IOException, RCS("ZipFileReader::seek() only supports acdk::io::SeekCur"));
  int size = _zip->_lfh->size;
  if (size - _cpos < seekpos)
    seekpos = size - _cpos;
  jlong curpos = _in->seek(acdk::io::SeekCur, 0);
  jlong erg = _in->seek(seekrel, seekpos);
  _cpos += erg - curpos;
  return erg;
}

jlong 
ZipFileReader::skip(jlong n)
{
  int size = _zip->_lfh->size;
  if (size - _cpos < n)
    n = size - _cpos;
  jlong erg = _in->skip(n);
  _cpos += erg;
  return erg;
}

int 
ZipFileReader::read()
{
  int size = _zip->_lfh->size;
  if (size - _cpos == 0) {
    ++_cpos;
    return -1;
  }
  if (size - _cpos < 0)
    THROW1_FQ(acdk::io::, EOFException, "ZipFileReader::read()");
  
   ++_cpos;
  int erg = _in->read();
  if (erg == -1)
    _cpos = size + 1;
  return erg;
}

int 
ZipFileReader::read(IN(RbyteArray) buffer, int offset/* = 0*/, int len/* = -1*/)
{
  if (len == -1)
    len = buffer->length() - offset;
  return read(buffer->data(), offset, len);
} 

int 
ZipFileReader::read(byte* buffer, int offset, int len)
{
  
  int size = _zip->_lfh->size;
  
  if (size - _cpos < 0)
    THROW1_FQ(acdk::io::, EOFException, "ZipFileReader::read()");
  
  if (size - _cpos < len)
    len = size - _cpos;
  int erg = _in->read(buffer, offset, len);
  if (erg == 0) {
    _cpos = size + 1;
    return erg;
  }
  _cpos += erg;
  return erg;
}

void 
ZipFileReader::reset()
{
  RString rn = _zip->getFileSystem()->getRootName();
  if (rn->endsWith("@") == true)
    rn = rn->substr(0, rn->length() - 1);
  
  acdk::io::RReader fin = acdk::io::File(rn).getReader();
  fin->seek(acdk::io::SeekSet, _zip->_lfh->dataOffset);
  int method = _zip->_lfh->method;
  if (method == 8)
    _in = new InflaterReader(&fin);
  else if (method == 0)
    _in = fin;
  else {
    THROW1_FQ(acdk::io::, IOException, RString("unsupported ZIP compression method: ") + method); //better zip
  }
}


bool 
ZipFileReader::ready()
{
  return true;
}


RString 
ZipFileReader::getDeviceName()
{
  return _zip->getCanonicalPath();
}



} // zip
} // vfile
} // acdk




