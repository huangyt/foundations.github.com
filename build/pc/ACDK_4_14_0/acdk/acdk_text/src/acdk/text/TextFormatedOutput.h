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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/TextFormatedOutput.h,v 1.6 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_text_TextFormatedOutput_h
#define acdk_text_TextFormatedOutput_h

#include "text.h"
#include "TextFormatedOutput.h"

namespace acdk {
namespace text {

ACDK_DECL_CLASS(TextFormatedOutput);

class TextFormatedOutput
: public AbstractFormatedOutput
{
public:
  TextFormatedOutput(IN(RWriter) out)
    : AbstractFormatedOutput(out)
  {
  }
  virtual RFormatedOutput startDocument() { return this; }
  virtual RFormatedOutput endDocument() { return this; }
  
  virtual RFormatedOutput p() { return write("\n\n"); }
  virtual RFormatedOutput br() { return write("\n"); }
  virtual RFormatedOutput print(IN(RString) str) { return write(str); }
  
  virtual RString bold(IN(RString) str) { return RString("<b>") + str + RString("</b>"); }
};


} // text
} // acdk

#endif //acdk_text_TextFormatedOutput_h

