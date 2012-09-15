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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ArtProvider.h,v 1.6 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_ArtProvider_h
#define acdk_wx_ArtProvider_h

#include "Bitmap.h"
#include "Icon.h"

namespace acdk {
namespace wx {

  /*
    clients
    wxART_TOOLBAR
wxART_MENU
wxART_FRAME_ICON
wxART_CMN_DIALOG
wxART_HELP_BROWSER
wxART_MESSAGE_BOX
wxART_OTHER
*/
/*
art type
wxART_ADD_BOOKMARK
wxART_DEL_BOOKMARK
wxART_HELP_SIDE_PANEL
wxART_HELP_SETTINGS
wxART_HELP_BOOK
wxART_HELP_FOLDER
wxART_HELP_PAGE
wxART_GO_BACK
wxART_GO_FORWARD
wxART_GO_UP
wxART_GO_DOWN
wxART_GO_TO_PARENT
wxART_GO_HOME
wxART_FILE_OPEN
wxART_PRINT
wxART_HELP
wxART_TIP
wxART_REPORT_VIEW
wxART_LIST_VIEW
wxART_NEW_DIR
wxART_FOLDER
wxART_GO_DIR_UP
wxART_EXECUTABLE_FILE
wxART_NORMAL_FILE
wxART_TICK_MARK
wxART_CROSS_MARK
wxART_ERROR
wxART_QUESTION
wxART_WARNING
wxART_INFORMATION
*/

ACDK_DECL_CLASS(ArtProvider);
/**
  see wxArtProvider
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC ArtProvider
: extends WxObject
{
  ACDK_WITH_METAINFO(ArtProvider)
public:
  // wxArtProvider
  ACDK_WX_STD_MEMBERS(ArtProvider, WxObject)

  // Add new provider to the top of providers stack.
  //static void PushProvider(wxArtProvider *provider);
  inline static void pushProvider(IN(RArtProvider) provider) { wxArtProvider::PushProvider(CLS2WXPTR(provider)); }

  // Remove latest added provider and delete it.
  //static bool PopProvider();
  inline static bool popProvider() { return wxArtProvider::PopProvider(); }

  // Remove provider. The provider must have been added previously!
  // The provider is _not_ deleted.
  //static bool RemoveProvider(wxArtProvider *provider);
  inline static bool removeProvider(IN(RArtProvider) provider) { return wxArtProvider::RemoveProvider(CLS2WXPTR(provider)); }

  // Query the providers for bitmap with given ID and return it. Return
  // wxNullBitmap if no provider provides it.
  //static wxBitmap GetBitmap(const wxString& id, const wxString& client = wxART_OTHER, const wxSize& size = wxDefaultSize);
  inline static RBitmap getBitmap(IN(RString)  id, IN(RString)  client = Nil, IN(RSize) size = Size::defaultSize())
  {
    wxString cl = wxART_OTHER;
    if (client != Nil)
      cl = S2WXS(client);
    return WXVAL2CLS(Bitmap, wxArtProvider::GetBitmap(S2WXS(id), cl, CLS2WXREF(size)));
  }

  // Query the providers for icon with given ID and return it. Return
  // wxNullIcon if no provider provides it.
  //static wxIcon GetIcon(const wxString& id, const wxString& client = wxART_OTHER, const wxSize& size = wxDefaultSize);
  inline static RIcon getIcon(IN(RString)  id, IN(RString)  client = Nil, IN(RSize) size = Size::defaultSize())
  {
    wxString cl = wxART_OTHER;
    if (client != Nil)
      cl = S2WXS(client);
    return WXVAL2CLS(Icon, wxArtProvider::GetIcon(S2WXS(id), cl, CLS2WXREF(size)));
  }

};


} // wx
} // acdk

#endif //acdk_wx_ArtProvider_h
