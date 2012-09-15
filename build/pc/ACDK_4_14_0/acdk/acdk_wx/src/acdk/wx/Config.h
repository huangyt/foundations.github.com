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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Config.h,v 1.29 2005/02/07 17:16:45 kommer Exp $

#ifndef acdk_wx_Config_h
#define acdk_wx_Config_h




#include <acdk/Platform.h>

#if defined(ACDK_OS_WIN32)
# ifndef WINVER
#  define WINVER 0x400
# endif
#endif

#if defined(ACDK_DEBUG)
# define ACDK_USE_WX_DEBUG
#endif


#if defined(ACDK_USE_WX_DEBUG)
#   define __WXDEBUG__
#   define WXDEBUG 1
#endif

#if defined(ACDK_OS_WIN32)
# if !defined(_WINDOWS)
#   define _WINDOWS
# endif

# ifndef wxUSE_GUI
# define wxUSE_GUI 1
# endif


# ifndef WXUSINGDLL
#   define WXUSINGDLL 1
#  endif
// Use Unicode on Windows
# define ACDK_USE_WX_UNICODE
# endif
# if defined(ACDK_USE_WX_UNICODE)
# if !defined(wxUSE_UNICODE)
#   define wxUSE_UNICODE 1
# endif
# ifndef UNICODE
#   define UNICODE
# endif
/*
#else
# define wxUSE_UNICODE 0
# ifdef UNICODE
#   undef UNICODE
# endif
# ifdef _UNICODE
#   undef _UNICODE
#endif
*/
#endif




#if defined(_GNU_SOURCE)
# undef _GNU_SOURCE
#endif
#if defined(HAVE_UNISTD_H)
# undef HAVE_UNISTD_H
#endif


#if defined(ACDK_OS_LINUX)
# if !defined(__WXGTK__)
#   define __WXGTK__
# endif
//# define  FILE_OFFSET_BITS 64
//# define _LARGE_FILES
/*
# if defined(_UNICODE) 
#  undef _UNICODE
#  define UNICODE_WASDEFINED
# endif
# if defined(UNICODE)
#  undef UNICODE
#  define UNICODE_WASDEFINED
# endif
# define wxUSE_UNICODE 0

# undef _T
*/
#endif

#undef overwrite


#if defined(ACDK_OS_UNIX)
# define wxUSE_EXCEPTIONS 1
# define xUSE_XML 1
# define wxUSE_CHOICEBOOK 1
# define wxUSE_DISPLAY 1
# define wxUSE_XRC 1
#endif 
/*
/home/roger/src/wxGTK-2.5.3/include/wx/chkconf.h:428:13: #error "wxUSE_LIBMSPACK must be defined."
/home/roger/src/wxGTK-2.5.3/include/wx/chkconf.h:469:9: #error "wxUSE_LISTBOOK must be defined."
/home/roger/src/wxGTK-2.5.3/include/wx/chkconf.h:517:9: #error "wxUSE_MDI must be defined."
/home/roger/src/wxGTK-2.5.3/include/wx/chkconf.h:621:9: #error "wxUSE_SOUND must be defined."
/home/roger/src/wxGTK-2.5.3/include/wx/chkconf.h:918:13: #error "wxUSE_DYNAMIC_LOADER requires wxUSE_DYNLIB_CLASS."
/home/roger/src/wxGTK-2.5.3/include/wx/chkconf.h:1431:14: #error "MDI requires wxUSE_MDI"
*/

#include <wx/wx.h>
#if wxUSE_UNICODE == 1
# if !defined(ACDK_USE_WX_UNICODE)
#   define ACDK_USE_WX_UNICODE 1
# endif
#endif
#include <wx/image.h>
#include <wx/imaglist.h>
#include <wx/caret.h>
#include <wx/treectrl.h>
#include <wx/xrc/xmlres.h>
#include <wx/splitter.h>
#include <wx/notebook.h>
#include <wx/spinbutt.h>
#include <wx/spinctrl.h>
#include <wx/html/htmlwin.h>
#include <wx/progdlg.h>
#include <wx/tglbtn.h>
#include <wx/clipbrd.h>
#include <wx/dnd.h>
#include <wx/mdi.h>
#include <wx/print.h>
#include <wx/wizard.h>
#include <wx/dcps.h>
#include <wx/cmndata.h>
#include <wx/fontdlg.h>
#include <wx/textdlg.h>
#include <wx/colordlg.h>
#include <wx/tooltip.h>
#define overwrite virtual
#include <acdk.h>

#if defined(ACDK_NEED_DLLEXPORT)
# if defined(IN_ACDK_WX_LIB)
#   define ACDK_WX_PUBLIC __declspec(dllexport)
# elif defined(ACDK_STATIC_LIB)
# 	define ACDK_WX_PUBLIC
# else
#   define ACDK_WX_PUBLIC __declspec(dllimport)
# endif
#else
# define ACDK_WX_PUBLIC
#endif


#define ACDK_CHECK_WX_VERSION(Major, Minor) \
  (wxMAJOR_VERSION > Major || (wxMAJOR_VERSION == Major && wxMINOR_VERSION >= Minor))

namespace acdk {
/**
   Windowing system using wxWindows
*/
namespace wx {

}
} // namespace acdk 
#endif //acdk_wx_Config_h
