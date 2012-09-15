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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/AbstractFormatedOutput.h,v 1.7 2005/02/05 10:45:32 kommer Exp $

#ifndef acdk_text_AbstractFormatedOutput_h
#define acdk_text_AbstractFormatedOutput_h

#include "text.h"
#include <acdk/io/Writer.h>

#include "FormatedOutput.h"

namespace acdk {
namespace text {


ACDK_DECL_CLASS(AbstractFormatedOutput);

class ACDK_TEXT_PUBLIC AbstractFormatedOutput
: public acdk::lang::Object,
  implements FormatedOutput
{
private:
protected:
  acdk::io::RWriter _out;
public:
  AbstractFormatedOutput(IN(acdk::io::RWriter) out)
  : Object(),
    _out(out)    
  {
  }
  foreign virtual RFormatedOutput println(IN(RString) str)
  {
    print(str); br();
    return this;
  }
  foreign virtual RFormatedOutput write(IN(RString) str)
  {
    _out->write((byte*)str->c_str(), 0, str->length());
    return this;
  }
  foreign virtual RFormatedOutput write(char* msg)
  {
    _out->write((byte*)msg, 0, strlen(msg));
    return this;
  }
};


} // text
} // acdk

#endif //acdk_text_AbstractFormatedOutput_h

