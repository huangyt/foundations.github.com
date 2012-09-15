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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Colour.h,v 1.9 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_Colour_h
#define acdk_wx_Colour_h

#include "WxObject.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(Colour);
/**
  see wxColour
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC Colour
: extends WxObject
{
  ACDK_WITH_METAINFO(Colour)
public:
  ACDK_WX_STD_VAL_MEMBERS(Colour, WxObject)
  Colour() : WxObject(new wxColour()) {}
  Colour( byte red, byte green, byte blue )
    : WxObject(new wxColour(red, green, blue))
  {
  }

  Colour( int colRGB ) : WxObject(new wxColour(colRGB)) { }

    // implicit conversion from the colour name
  Colour(IN(RString) colourName) : WxObject(new wxColour(S2WXS(colourName))) { }
  
    
  //void Set( byte red, byte green, byte blue );
  inline void set(byte red, byte green, byte blue) { getWx()->Set(red, green, blue); }
  //void Set(int colRGB);
  inline void set(int colRGB) { getWx()->Set(colRGB); }

  // accessors
  //bool Ok() const {return m_isInit; }
  inline bool ok() const { return getWx()->Ok(); }


  //byte Red() const { return m_red; }
  inline byte red() const { return getWx()->Red(); }
  //byte Green() const { return m_green; }
  inline byte green() const { return getWx()->Green(); }
  //byte Blue() const { return m_blue; }
  inline byte blue() const { return getWx()->Blue(); }

  static RColour getBlack() { return new Colour(*wxBLACK); }
  static RColour getWhite() { return new Colour(*wxWHITE); }
  static RColour getRed() { return new Colour(*wxRED); }
  static RColour getBlue() { return new Colour(*wxBLUE); }
  static RColour getGreen() { return new Colour(*wxGREEN); }
  static RColour getCyan() { return new Colour(*wxCYAN); }
  static RColour getLightGrey() { return new Colour(*wxLIGHT_GREY); }
  static RColour getNullColour() { return new Colour(wxNullColour); }
};


} // wx
} // acdk

#endif //acdk_wx_Colour_h
