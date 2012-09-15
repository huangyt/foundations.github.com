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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/FormatedOutput.h,v 1.9 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_text_FormatedOutput_h
#define acdk_text_FormatedOutput_h

#include "text.h"

namespace acdk {
namespace text {


ACDK_DECL_INTERFACE(FormatedOutput);


/**
  An Interface to print (interactive) text on the console
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/04/08 10:53:21 $
*/

class ACDK_TEXT_PUBLIC FormatedOutput
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(FormatedOutput)
public:
  virtual RFormatedOutput startDocument() = 0;
  virtual RFormatedOutput endDocument() = 0;
  virtual RFormatedOutput p() = 0;
  virtual RFormatedOutput br() = 0;
  
  /** writes throug uninterpreted */
  virtual RFormatedOutput write(IN(RString) str) = 0;
  virtual RFormatedOutput write(char* str) = 0;

  virtual RFormatedOutput print(IN(RString) str) = 0;
  virtual RFormatedOutput println(IN(RString) str) = 0;

  /** print the text in bold type */
  virtual RString bold(IN(RString) str) = 0;
 
  //virtual RFormatedOutput printOption(RString str) = 0;
};


} // text
} // acdk

#endif //acdk_text_FormatedOutput_h

