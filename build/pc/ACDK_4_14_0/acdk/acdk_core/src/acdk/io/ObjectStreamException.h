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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ObjectStreamException.h,v 1.10 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_ObjectStreamException_h
#define acdk_io_ObjectStreamException_h


#include "IOException.h"

namespace acdk {
namespace io {


ACDK_DECL_THROWABLE(ObjectStreamException, IOException);


/**
  Signals an error in operations of ObjectReader or ObjectWriter.

  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC ObjectStreamException 
: extends acdk::io::IOException
{
  ACDK_WITH_METAINFO(ObjectStreamException)  
public:
  ObjectStreamException()
  : IOException(false)
  {
  }
  ObjectStreamException(IN(RString) what)
  : IOException(what)
  {
  }
};

ACDK_DECL_THROWABLE(InvalidClassException, ObjectStreamException);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC InvalidClassException 
: extends ObjectStreamException
{
  ACDK_WITH_METAINFO(InvalidClassException)  
public:
  InvalidClassException()
  : ObjectStreamException()
  {
  }
  InvalidClassException(IN(RString) what)
  : ObjectStreamException(what)
  {
  }
};

ACDK_DECL_THROWABLE(NotSerializableException, ObjectStreamException);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC NotSerializableException 
: extends ObjectStreamException
{
  ACDK_WITH_METAINFO(NotSerializableException)  
public:
  NotSerializableException()
  : ObjectStreamException()
  {
  }
  NotSerializableException(IN(RString) what)
  : ObjectStreamException(what)
  {
  }
};

ACDK_DECL_THROWABLE(StreamCorruptedException, ObjectStreamException);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC StreamCorruptedException 
: extends ObjectStreamException
{
  ACDK_WITH_METAINFO(StreamCorruptedException)  
public:
  StreamCorruptedException()
  : ObjectStreamException()
  {
  }
  StreamCorruptedException(IN(RString) what)
  : ObjectStreamException(what)
  {
  }
};





} // io
} // acdk

#endif //acdk_io_ObjectStreamException_h

