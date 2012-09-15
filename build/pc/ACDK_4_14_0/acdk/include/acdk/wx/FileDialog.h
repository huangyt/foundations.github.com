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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/FileDialog.h,v 1.6 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_FileDialog_h
#define acdk_wx_FileDialog_h

#include "Dialog.h"

namespace acdk {
namespace wx {

/**
  see FileDialog
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum FileDialogFlags
{
    FDFOpen               /* wxOPEN             */ = 0x0001,
    FDFSave               /* wxSAVE             */ = 0x0002,
    FDFOverwritePrompt   /* wxOVERWRITE_PROMPT */ = 0x0004,
    FDFHideReadonly      /* wxHIDE_READONLY    */ = 0x0008,
    FDFFileMustExist    /* wxFILE_MUST_EXIST  */ = 0x0010,
    FDFMultiple           /* wxMULTIPLE         */ = 0x0020,
    FDFChangeDir         /* wxCHANGE_DIR       */ = 0x0040
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, FileDialogFlags);

ACDK_DECL_CLASS(FileDialog);

/**
  see wxFileDialog
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC FileDialog
: extends Dialog
{
  ACDK_WITH_METAINFO(FileDialog)
public:
  ACDK_WX_STD_MEMBERS(FileDialog, Dialog)
  /**
    @param style combination of FileDialogFlags
  */
  FileDialog(IN(RWindow) parent, 
             IN(RString) message,
             IN(RString) defaultDir = "",
             IN(RString) defaultFile = "",
             IN(RString) wildCard = "*.*",
             int style = 0,
             IN(RPoint) pos = Point::defaultPosition()
             )
             : Dialog(new wxFileDialog(CLS2WXPTR(parent), S2WXS(message), S2WXS(defaultDir), S2WXS(defaultFile), S2WXS(wildCard), style, pos->toWx()))
    {
    }
    //void SetMessage(const wxString& message) { m_message = message; }
  inline void setMessage(IN(RString)  message) { getWx()->SetMessage(S2WXS(message)); }
    //wxString GetMessage() const { return m_message; }
  inline RString getMessage() const { return WXS2S(getWx()->GetMessage()); }
    //void SetPath(const wxString& path);
  inline void setPath(IN(RString)  path) { getWx()->SetPath(S2WXS(path)); }
    //void SetDirectory(const wxString& dir) { m_dir = dir; }
  inline void setDirectory(IN(RString)  dir) { getWx()->SetDirectory(S2WXS(dir)); }
    //void SetFilename(const wxString& name) { m_fileName = name; }
  inline void setFilename(IN(RString)  name) { getWx()->SetFilename(S2WXS(name)); }
    //void SetWildcard(const wxString& wildCard) { m_wildCard = wildCard; }
  inline void setWildcard(IN(RString)  wildCard) { getWx()->SetWildcard(S2WXS(wildCard)); }
    //void SetStyle(long style) { m_dialogStyle = style; }
  inline void setStyle(int style) { getWx()->SetStyle(style); }
    //void SetFilterIndex(int filterIndex) { m_filterIndex = filterIndex; }
  inline void setFilterIndex(int filterIndex) { getWx()->SetFilterIndex(filterIndex); }
    //wxString GetPath() const { return m_path; }
  inline RString getPath() const { return WXS2S(getWx()->GetPath()); }
    //void GetPaths(wxArrayString& paths) const;
  inline void getPaths(IN(RStringArray) paths) const 
  { 
    wxArrayString wxpath;
    for (int i = 0; i < paths->length(); ++i)
      wxpath.Add(S2WXS(paths[i]));
    getWx()->GetPaths(wxpath); 
  }
    //wxString GetDirectory() const { return m_dir; }
  inline RString getDirectory() const { return WXS2S(getWx()->GetDirectory()); }
    //wxString GetFilename() const { return m_fileName; }
  inline RString getFilename() const { return WXS2S(getWx()->GetFilename()); }
    //void GetFilenames(wxArrayString& files) const { files = m_fileNames; }
  inline RStringArray getFilenames() const 
  { 
    wxArrayString wxfiles;
    getWx()->GetFilenames(wxfiles); 
    RStringArray erg = new StringArray(int(wxfiles.GetCount()));
    for (int i = 0; i < int(wxfiles.GetCount()); ++i)
      erg[i] = WXS2S(wxfiles[i]);
    return erg;
  }
    //wxString GetWildcard() const { return m_wildCard; }
  inline RString getWildcard() const { return WXS2S(getWx()->GetWildcard()); }
    //long GetStyle() const { return m_dialogStyle; }
  inline int getStyle() const { return getWx()->GetStyle(); }
    //int GetFilterIndex() const { return m_filterIndex ; }
  inline int getFilterIndex() const { return getWx()->GetFilterIndex(); }
    //virtual int ShowModal();
  inline virtual int showModal() { return getWx()->ShowModal(); }
};

} // wx
} // acdk

#endif //acdk_wx_FileDialog_h
