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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/UnicodeTable.h,v 1.9 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_UnicodeTable_h
#define acdk_locale_UnicodeTable_h

#include <acdk.h>
#include "../AcdkCoreConfig.h"

namespace acdk {
namespace locale {

/**
  internal flags with information for a unicode character.
*/
foreign enum CharacterType
{
  Control     = 0x00000001,
  Whitespace  = 0x00000002,
  Seperator   = 0x00000004,
  Punctation  = 0x00000008,
  Symbol      = 0x00000010,
  Number      = 0x00000020,
  Decimal     = 0x00000040,
  Letter      = 0x00000080,
  UpCase      = 0x00000100,
  DownCase    = 0x00000200,
  TitleCase  = 0x00000400,
  Space       = 0x00000800,
  Character = 0x00001000,
  Mark          = 0x00002000,
  NonSpacing    = 0x00004000,
  Modifier     = 0x00008000,
  SpacingCombined = 0x00010000,
  Enclosing     = 0x00020000,
  Format        = 0x00040000,
  Line    = 0x00080000,
  Paragraph   = 0x001000000,
  Surogat   = 0x002000000,
  Other       = 0x004000000,
  /// ACDK internal 
  CJKLetter  = 0x040000000,
  /// ACDK internal 
  Unpecified = 0x080000000
};

#if !defined(DOXYGENONLY)

struct  UnicodeInfo
{
  unsigned short idx;
  const char* name;
  unsigned int flags;
  int decval;
  short upcaseidx;
  short downcaseidx;
  short titlecaseidx;
};

foreign
class  UnicodeTable
{
  enum { 
    UnicodeTableSize = 13717,
    FixupTableSize = 20
  };
  static ACDK_CORE_PUBLIC UnicodeInfo _table[UnicodeTableSize];
  // 0 lower bound
  // 1 upper bound
  // 2 absolut offset
  static ACDK_CORE_PUBLIC int _fixups[FixupTableSize][3];
  static ACDK_CORE_PUBLIC const UnicodeInfo& getCalced(int unicodeChar);
public:
  inline static ACDK_CORE_PUBLIC const UnicodeInfo& get(int unicodeChar)
  {
    if (unicodeChar < _fixups[0][0])
      return _table[unicodeChar];
    return getCalced(unicodeChar);
  }

};

#endif // !defined(DOXYGENONLY)


} // acdk
} // locale

#endif //acdk_locale_UnicodeTable_h
