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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Font.h,v 1.5 2005/02/06 13:12:12 kommer Exp $

#ifndef acdk_wx_Font_h
#define acdk_wx_Font_h

#include "GDIObject.h"

namespace acdk {
namespace wx {


/**
  see Font, wxFontEncoding, wxFont
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/06 13:12:12 $
*/
enum FontEncoding
{
    FontencodingSystem  /* wxFONTENCODING_SYSTEM*/ = -1,     // system default
    FontencodingDefault  /* wxFONTENCODING_DEFAULT*/,         // current default encoding

    // ISO8859 standard defines a number of single-byte charsets
    FontencodingIso88591  /* wxFONTENCODING_ISO8859_1*/,       // West European (Latin1)
    FontencodingIso88592  /* wxFONTENCODING_ISO8859_2*/,       // Central and East European (Latin2)
    FontencodingIso88593  /* wxFONTENCODING_ISO8859_3*/,       // Esperanto (Latin3)
    FontencodingIso88594  /* wxFONTENCODING_ISO8859_4*/,       // Baltic (old) (Latin4)
    FontencodingIso88595  /* wxFONTENCODING_ISO8859_5*/,       // Cyrillic
    FontencodingIso88596  /* wxFONTENCODING_ISO8859_6*/,       // Arabic
    FontencodingIso88597  /* wxFONTENCODING_ISO8859_7*/,       // Greek
    FontencodingIso88598  /* wxFONTENCODING_ISO8859_8*/,       // Hebrew
    FontencodingIso88599  /* wxFONTENCODING_ISO8859_9*/,       // Turkish (Latin5)
    FontencodingIso885910  /* wxFONTENCODING_ISO8859_10*/,      // Variation of Latin4 (Latin6)
    FontencodingIso885911  /* wxFONTENCODING_ISO8859_11*/,      // Thai
    FontencodingIso885912  /* wxFONTENCODING_ISO8859_12*/,      // doesn't exist currently, but put it
                                    // here anyhow to make all ISO8859
                                    // consecutive numbers
    FontencodingIso885913  /* wxFONTENCODING_ISO8859_13*/,      // Baltic (Latin7)
    FontencodingIso885914  /* wxFONTENCODING_ISO8859_14*/,      // Latin8
    FontencodingIso885915  /* wxFONTENCODING_ISO8859_15*/,      // Latin9 (a.k.a. Latin0, includes euro)
    FontencodingIso8859Max  /* wxFONTENCODING_ISO8859_MAX*/,

    // Cyrillic charset soup (see http://czyborra.com/charsets/cyrillic.html)
    FontencodingKoi8  /* wxFONTENCODING_KOI8*/,            // we don't support any of KOI8 variants
    FontencodingAlternative  /* wxFONTENCODING_ALTERNATIVE*/,     // same as MS-DOS CP866
    FontencodingBulgarian  /* wxFONTENCODING_BULGARIAN*/,       // used under Linux in Bulgaria

    // what would we do without Microsoft? They have their own encodings
        // for DOS
    FontencodingCp437  /* wxFONTENCODING_CP437*/,           // original MS-DOS codepage
    FontencodingCp850  /* wxFONTENCODING_CP850*/,           // CP437 merged with Latin1
    FontencodingCp852  /* wxFONTENCODING_CP852*/,           // CP437 merged with Latin2
    FontencodingCp855  /* wxFONTENCODING_CP855*/,           // another cyrillic encoding
    FontencodingCp866  /* wxFONTENCODING_CP866*/,           // and another one
        // and for Windows
    FontencodingCp874  /* wxFONTENCODING_CP874*/,           // WinThai
    FontencodingCp932  /* wxFONTENCODING_CP932*/,           // Japanese (shift-JIS)
    FontencodingCp936  /* wxFONTENCODING_CP936*/,           // Chinese simplified (GB)
    FontencodingCp949  /* wxFONTENCODING_CP949*/,           // Korean (Hangul charset)
    FontencodingCp950  /* wxFONTENCODING_CP950*/,           // Chinese (traditional - Big5)
    FontencodingCp1250  /* wxFONTENCODING_CP1250*/,          // WinLatin2
    FontencodingCp1251  /* wxFONTENCODING_CP1251*/,          // WinCyrillic
    FontencodingCp1252  /* wxFONTENCODING_CP1252*/,          // WinLatin1
    FontencodingCp1253  /* wxFONTENCODING_CP1253*/,          // WinGreek (8859-7)
    FontencodingCp1254  /* wxFONTENCODING_CP1254*/,          // WinTurkish
    FontencodingCp1255  /* wxFONTENCODING_CP1255*/,          // WinHebrew
    FontencodingCp1256  /* wxFONTENCODING_CP1256*/,          // WinArabic
    FontencodingCp1257  /* wxFONTENCODING_CP1257*/,          // WinBaltic (same as Latin 7)
    FontencodingCp12Max  /* wxFONTENCODING_CP12_MAX*/,

    FontencodingUtf7  /* wxFONTENCODING_UTF7*/,            // UTF-7 Unicode encoding
    FontencodingUtf8  /* wxFONTENCODING_UTF8*/,            // UTF-8 Unicode encoding

    // Far Eastern encodings
        // Chinese
    FontencodingGb2312  /* wxFONTENCODING_GB2312*/ = wxFONTENCODING_CP936, // Simplified Chinese
    FontencodingBig5  /* wxFONTENCODING_BIG5*/ = wxFONTENCODING_CP950,   // Traditional Chinese

        // Japanese (see http://zsigri.tripod.com/fontboard/cjk/jis.html)
    FontencodingShiftJis  /* wxFONTENCODING_SHIFT_JIS*/ = wxFONTENCODING_CP932,  // Shift JIS
    FontencodingEucJp  /* wxFONTENCODING_EUC_JP*/,          // Extended Unix Codepage for Japanese

    FontencodingUnicode  /* wxFONTENCODING_UNICODE*/,         // Unicode - currently used only by
                                    // wxEncodingConverter class
    FontencodingMax  /* wxFONTENCODING_MAX*/
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, FontEncoding);


ACDK_DECL_CLASS(Font);
/**
  see wxFont
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/06 13:12:12 $
*/
class ACDK_WX_PUBLIC Font
: extends GDIObject
{
  ACDK_WITH_METAINFO(Font)
public:
  ACDK_WX_STD_MEMBERS(Font, GDIObject)
  //Font(const wxFont& other) : GDIObject(new wxFont(other)) {}
  Font() : GDIObject(new wxFont()) {}
  Font(int size, int family, int style, int weight, bool underlined = false,
           IN(RString) face = "", FontEncoding encoding = FontencodingDefault)
           : GDIObject(new wxFont(size, family, style, weight, underlined, S2WXS(face), (wxFontEncoding)encoding))
  {
  }
  Font(IN(RString) fontDesc) : GDIObject(new wxFont(S2WXS(fontDesc))) {}
  //virtual int GetPointSize() const;
  inline int getPointSize() const { return getWx()->GetPointSize(); }
    //virtual int GetFamily() const;
  inline int getFamily() const { return getWx()->GetFamily(); }
    //virtual int GetStyle() const;
  inline int getStyle() const { return getWx()->GetStyle(); }
    //virtual int GetWeight() const;
  inline int getWeight() const { return getWx()->GetWeight(); }
    //virtual bool GetUnderlined() const;
  inline bool getUnderlined() const { return getWx()->GetUnderlined(); }
    //virtual wxString GetFaceName() const;
  inline RString getFaceName() const { return WXS2S(getWx()->GetFaceName()); }
    //virtual wxFontEncoding GetEncoding() const;
  inline FontEncoding getEncoding() const { return FontEncoding(getWx()->GetEncoding()); }
  //virtual void SetPointSize(int pointSize);
  inline void setPointSize(int pointSize) { getWx()->SetPointSize(pointSize); }
    //virtual void SetFamily(int family);
  inline void setFamily(int family) { getWx()->SetFamily(family); }
    //virtual void SetStyle(int style);
  inline void setStyle(int style) { getWx()->SetStyle(style); }
    //virtual void SetWeight(int weight);
  inline void setWeight(int weight) { getWx()->SetWeight(weight); }
    //virtual void SetFaceName(const wxString& faceName);
  inline void setFaceName(IN(RString)  faceName) { getWx()->SetFaceName(S2WXS(faceName)); }
    //virtual void SetUnderlined(bool underlined);
  inline void setUnderlined(bool underlined) { getWx()->SetUnderlined(underlined); }
    //virtual void SetEncoding(wxFontEncoding encoding);
  inline void setEncoding(FontEncoding encoding) { getWx()->SetEncoding(wxFontEncoding(encoding)); }
   //wxString GetNativeFontInfoDesc() const;
  inline RString getNativeFontInfoDesc() const { return WXS2S(getWx()->GetNativeFontInfoDesc()); }
    //wxString GetNativeFontInfoUserDesc() const;
  inline RString getNativeFontInfoUserDesc() const { return WXS2S(getWx()->GetNativeFontInfoUserDesc()); }
    //virtual void SetNativeFontInfo(const wxNativeFontInfo& info);
  //inline virtual void setNativeFontInfo(IN(RNativeFontInfo) info) { getWx()->SetNativeFontInfo(CLS2WXREF(info)); }
    //virtual bool IsFixedWidth() const;
  inline virtual bool isFixedWidth() const { return getWx()->IsFixedWidth(); }

  static RFont getNormalFont() { return new Font(*wxNORMAL_FONT); }
  static RFont getSmallFont() { return new Font(*wxSMALL_FONT); }
  static RFont getItalicFont() { return new Font(*wxITALIC_FONT); }
  static RFont getSwissFont() { return new Font(*wxSWISS_FONT); }
};

inline RFont fromWx(const wxFont& font) { return new Font(font); }

} // wx
} // acdk

#endif //acdk_wx_Font_h
