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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StringUtf8Utils.h,v 1.7 2005/02/05 10:44:57 kommer Exp $

#ifndef acdk_lang_StringUtf8Utils_h
#define acdk_lang_StringUtf8Utils_h


#if defined(ACDK_OS_WIN32) && !defined(OS_CYGWIN32)
# include <TCHAR.H>
#endif


#if defined(UNICODE)
# if !defined(ACDK_OS_DARWIN)
# include  <wchar.h>
# endif
# define tstrlen wcslen
# define tstrncpy wcsncpy
#ifndef _T
# define _T(x) L ## x
#endif
#else // defined(UNICODE)
# define tstrlen strlen
# define tstrncpy strncpy
#ifndef _T
# define _T(x)  x
#endif
#endif //defined(UNICODE)

namespace acdk {
namespace lang {


#define UCS2IDX(idx) (idx << 1) // == idx * 2
#define UCS4IDX(idx) (idx << 2)// == idx * 4

/**
  Internal Helper class to handle UTF8 strings
*/
struct ACDK_CORE_PUBLIC StringUtf8Utils
{
  static void incUtfPtr(const char*& ptr, const char* endptr);
  static void decUtfPtr(const char*& ptr, const char* beginptr);
  static size_t utfDiff(const char* end, const char* begin);
  static bool isAscii(const char* begin, const char* end);
  
  static ucchar fetchWideChar(const char*& begin, const char* end);
  static ucchar toWideChar(const char* begin, const char* end)
  {
    const char* it = begin;
    return fetchWideChar(it, end);
  }
  static int getByteLength(const char* it);
  /**
    return the number of byte a unicode character needs
  */
  static int utflength(ucchar ch);
  
  /**
    returns the number of bytes a unicode string needs
  */
  static int utflength(const ucchar* begin, const ucchar* end);
  static int utflength(const ucchar* begin) { return utflength(begin, begin + uclength(begin)); }

  enum UtfStreamType
  {
    StreamIsAscii,
    StreamIsUtf8,
    StreamIsError
  };
  /**
    check if the character byte stream is a valid 
    UTF8 stream.
    if throwOnFail is true, UTFDataFormatException will be thrown
  */
  static UtfStreamType validUtf8Stream(const byte* begin, const byte* end, bool throwOnFail = true);

  /**
    returns the length of 0 terminated unicode character string
  */
  static int uclength(const ucchar* ch);
  static int uc2length(const uc2char* ch) { return uclength(ch); }
  /**
    returns the length of an 0-terminated uc4char stream
  */
  static int uc4length(const uc4char* ch);
  /**
    writes an unicode char into given chars
    @param it start position to write.
              will be modified and points to next writing 
              position after this method call
    @param end capacity position, if 0 no checking for end is done
    @return 0 is Ok
          -1 is error
          > 0 need chars to encode given unicode character to end
          in case of return value != 0 it position will not be changed.
  */

  static int writeUcToUtf8(byte*& it, byte* end, ucchar ucc);
  /**
    writes the given unicode character range into 8bit char range
    @param it start position to write.
              will be modified and points to next writing 
              position after this method call
    @param end capacity position, if 0 no checking for end is done
    @return 0 is Ok
          -1 is error
          > 0 need byte chars to encode to end
  */
  static int writeUcToUtf8(byte*& it, byte* end, const ucchar* ucbegin, const ucchar* ucend);
  /**
    converts an uc4 character stream to ucchar stream.
    uc4 chars cannot be mappend are mapped to 0xFFFF

    buffer must have enough space to get all length + 1 chars
    @param buffer target to write
    @param source 0 terminated uc4char stream
    @param length length of source, -1 if 0 terminated uc4 string 
    @return just the buffer
  */
  static ucchar* uc4touc(ucchar* buffer, const uc4char* source, int length = -1);
  static ucchar* wcchartouc(ucchar* buffer, const wchar_t* source, int length = -1);
    
  static inline size_t stringlength(const char* str) { return strlen(str); }
  static inline size_t stringlength(const ucchar* str) { return uclength(str); }
  static inline size_t stringlength(const uc4char* str) { return uc4length(str); }
  
  template <typename ToCharType, typename FromCharType>
  static
  inline ToCharType* convertTo(ToCharType* buffer, const FromCharType* source, int length = -1)
  {
    if (length == -1)
      length = stringlength(source);
    ToCharType* tit = buffer;
    register const FromCharType* fit = source;
    register const FromCharType* end = source + length;
    for (; fit < end; ++tit, ++fit)
      *tit = *fit;
    return buffer;
  }
};

} // namespace lang
} // namespace acdk



#endif //acdk_lang_StringUtf8Utils_h
