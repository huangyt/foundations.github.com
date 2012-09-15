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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/SubReader.h,v 1.9 2005/02/05 10:44:54 kommer Exp $

#ifndef acdk_io_SubReader_h
#define acdk_io_SubReader_h

#include <acdk.h>
#include "AbstractFilterReader.h"

namespace acdk {
namespace io {

ACDK_DECL_CLASS(SubReader);

/** 
  A SubReader is a filter, which reads a segement
  from another reader<br>
  API: ACDK
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:44:54 $
  
*/
class ACDK_CORE_PUBLIC SubReader
: extends ::acdk::io::AbstractFilterReader
{
  ACDK_WITH_METAINFO(SubReader)
private:
  int _start;
  int _length;
  int _curpos;
public:
  SubReader(IN(RReader) in, int startoffset, int len);
  foreign int available();
  foreign jlong seek(::acdk::io::SeekPos seekrel, jlong seekpos);
  foreign jlong skip(jlong n);
  foreign int read(byte* buffer, int offset, int len);
  foreign int read() { return AbstractFilterReader::read(); }
  foreign int read(IN(RbyteArray) buffer, int offset, int len) { return read(buffer, offset, len); }
};

} // io
} // acdk



#endif //acdk_io_SubReader_h

