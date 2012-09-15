// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/TextCtrlCharWriter.h,v 1.3 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_TextCtrlCharWriter_h
#define acdk_wx_TextCtrlCharWriter_h

#include "TextCtrl.h"

namespace acdk {
namespace wx {

ACDK_DECL_CLASS(TextCtrlCharWriter);

final
class ACDK_WX_PUBLIC TextCtrlCharWriter
: extends acdk::lang::Object
, implements acdk::io::CharWriter
{
  ACDK_WITH_METAINFO(TextCtrlCharWriter)
protected:
  RTextCtrl _textCtrl;
public:
  TextCtrlCharWriter(IN(RTextCtrl) textCtrl)
  : _textCtrl(textCtrl)
  {
  }
  virtual void writeChar(char c) 
  {
    char buf[2]; buf[0] = c; buf[1] = 0;
    writeString((const char*)buf);
  }
  virtual void writeChar(ucchar c)
  {
    ucchar buf[2]; buf[0] = c; buf[1] = 0;
    writeString((const ucchar*)buf);
  }
  foreign virtual void writeString(const char* cstr) 
  {
    String tstr(cstr);
    writeString(&tstr);
  }
  foreign virtual void writeString(const ucchar* cstr)
  {
    String tstr(cstr);
    writeString(&tstr);
  }
  virtual void writeString(IN(RString) str)
  {
    _textCtrl->appendText(str);
  }
  virtual void flush() {}
  virtual void close() {}
};

} // wx
} // acdk

#endif //acdk_wx_TextCtrlCharWriter_h
