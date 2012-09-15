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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/XmlResource.h,v 1.5 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_XmlResource_h
#define acdk_wx_XmlResource_h


#include "WxObject.h"

#include "Bitmap.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(XmlResource);

/**
  See wxXmlResource
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC XmlResource
: extends WxObject
{
  ACDK_WITH_METAINFO(XmlResource)
public:
  ACDK_WX_STD_MEMBERS(XmlResource, WxObject)
  XmlResource() : WxObject(new wxXmlResource()) {}
  
  //bool Load(const wxString& filemask);
  inline bool load(IN(RString)  filemask) { return getWx()->Load(S2WXS(filemask)); }
  
  // Initialize handlers for all supported controls/windows. This will
  // make the executable quite big because it forces linking against
  // most of the wxWindows library.
  //void InitAllHandlers();
  inline void initAllHandlers() { getWx()->InitAllHandlers(); }
  
  // Initialize only a specific handler (or custom handler). Convention says
  // that handler name is equal to the control's name plus 'XmlHandler', for example
  // wxTextCtrlXmlHandler, wxHtmlWindowXmlHandler. The XML resource compiler
  // (xmlres) can create include file that contains initialization code for
  // all controls used within the resource.
  //void AddHandler(wxXmlResourceHandler *handler);
  // sample wxGIFHandler
  // ### @todo inline void addHandler(IN(RXmlResourceHandler) handler) { getWx()->AddHandler(CLS2WXPTR(handler)); }
  
  // Add a new handler at the begining of the handler list
  //void InsertHandler(wxXmlResourceHandler *handler);
  // ### @todo inline void insertHandler(IN(RXmlResourceHandler) handler) { getWx()->InsertHandler(CLS2WXPTR(handler)); }
  
  // Removes all handlers
  //void ClearHandlers();
  inline void clearHandlers() { getWx()->ClearHandlers(); }
  
  // Registers subclasses factory for use in XRC. This function is not meant
  // for public use, please see the comment above wxXmlSubclassFactory
  // definition.
  //static void AddSubclassFactory(wxXmlSubclassFactory *factory);
  // ### @todo inline static void addSubclassFactory(IN(RXmlSubclassFactory) factory) { getWx()->AddSubclassFactory(CLS2WXPTR(factory)); }
  
  // Loads menu from resource. Returns NULL on failure.
  //wxMenu *LoadMenu(const wxString& name);
  inline RMenu loadMenu(IN(RString)  name) { RETURN_WXPTR2CLS(Menu, getWx()->LoadMenu(S2WXS(name))); }
  
  // Loads menubar from resource. Returns NULL on failure.
  //wxMenuBar *LoadMenuBar(wxWindow *parent, const wxString& name);
  inline RMenuBar loadMenuBar(IN(RWindow) parent, IN(RString)  name) { RETURN_WXPTR2CLS(MenuBar, getWx()->LoadMenuBar(CLS2WXPTR(parent), S2WXS(name))); }
  
  // Loads menubar from resource. Returns NULL on failure.
  //wxMenuBar *LoadMenuBar(const wxString& name) { return LoadMenuBar(NULL, name); }
  inline RMenuBar loadMenuBar(IN(RString)  name) { RETURN_WXPTR2CLS(MenuBar, getWx()->LoadMenuBar(S2WXS(name))); }
  
#if wxUSE_TOOLBAR
  // Loads a toolbar.
  //wxToolBar *LoadToolBar(wxWindow *parent, const wxString& name);
  // ### @todo inline RToolBar loadToolBar(IN(RWindow) parent, IN(RString)  name) { RETURN_WXPTR2CLS(ToolBar, getWx()->LoadToolBar(CLS2WXPTR(parent), S2WXS(name))); }
#endif
  
  // Loads a dialog. dlg points to parent window (if any).
  //wxDialog *LoadDialog(wxWindow *parent, const wxString& name);
  inline RDialog loadDialog(IN(RWindow) parent, IN(RString)  name) { RETURN_WXPTR2CLS(Dialog, getWx()->LoadDialog(CLS2WXPTR(parent), S2WXS(name))); }
  
  // Loads a dialog. dlg points to parent window (if any). This form
  // is used to finish creation of already existing instance (main reason
  // for this is that you may want to use derived class with new event table)
  // Example (typical usage):
  //      MyDialog dlg;
  //      wxTheXmlResource->LoadDialog(&dlg, mainFrame, "my_dialog");
  //      dlg->ShowModal();
  //bool LoadDialog(wxDialog *dlg, wxWindow *parent, const wxString& name);
  inline bool loadDialog(INOUT(RDialog) dlg, IN(RWindow) parent, IN(RString)  name) 
  { 
    return getWx()->LoadDialog(CLS2WXPTR(dlg), CLS2WXPTR(parent), S2WXS(name)); 
  }
  
  // Loads a panel. panel points to parent window (if any).
  //wxPanel *LoadPanel(wxWindow *parent, const wxString& name);
  // ### @todo inline RPanel loadPanel(IN(RWindow) parent, IN(RString)  name) { RETURN_WXPTR2CLS(Panel, getWx()->LoadPanel(CLS2WXPTR(parent), S2WXS(name))); }
  
  // Loads a panel. panel points to parent window (if any). This form
  // is used to finish creation of already existing instance.
  //bool LoadPanel(wxPanel *panel, wxWindow *parent, const wxString& name);
  // ### @todo inline bool loadPanel(IN(RPanel) panel, IN(RWindow) parent, IN(RString)  name) { return getWx()->LoadPanel(CLS2WXPTR(panel), CLS2WXPTR(parent), S2WXS(name)); }
  
  // Loads a frame.
  //wxFrame *LoadFrame(wxWindow* parent, const wxString& name);
  inline RFrame loadFrame(IN(RWindow) parent, IN(RString)  name) { RETURN_WXPTR2CLS(Frame, getWx()->LoadFrame(CLS2WXPTR(parent), S2WXS(name))); }
  //bool LoadFrame(wxFrame* frame, wxWindow *parent, const wxString& name);
  inline bool loadFrame(IN(RFrame) frame, IN(RWindow) parent, IN(RString)  name) { return getWx()->LoadFrame(CLS2WXPTR(frame), CLS2WXPTR(parent), S2WXS(name)); }
  
  // Load an object from the resource specifying both the resource name and
  // the classname.  This lets you load nonstandard container windows.
  //wxObject *LoadObject(wxWindow *parent, const wxString& name,  const wxString& classname);
  inline RWxObject loadObject(IN(RWindow) parent, IN(RString)  name, IN(RString)  classname) { RETURN_WXPTR2CLS(WxObject, getWx()->LoadObject(CLS2WXPTR(parent), S2WXS(name), S2WXS(classname))); }
  
  // Load an object from the resource specifying both the resource name and
  // the classname.  This form lets you finish the creation of an existing
  // instance.
  //bool LoadObject(wxObject *instance, wxWindow *parent, const wxString& name, const wxString& classname);
  inline bool loadObject(IN(RWxObject)  instance, IN(RWindow) parent, IN(RString)  name, IN(RString)  classname) { return getWx()->LoadObject(CLS2WXPTR(instance), CLS2WXPTR(parent), S2WXS(name), S2WXS(classname)); }
  
  // Loads a bitmap resource from a file.
  //wxBitmap LoadBitmap(const wxString& name);
  inline RBitmap loadBitmap(IN(RString)  name) { return WXVAL2CLS(Bitmap, getWx()->LoadBitmap(S2WXS(name))); }
  
  // Loads an icon resource from a file.
  //wxIcon LoadIcon(const wxString& name);
  // ### @todo inline RIcon loadIcon(IN(RString)  name) { return WXVAL2CLS(Icon, getWx()->LoadIcon(S2WXS(name))); }
  
  // Attaches an unknown control to the given panel/window/dialog.
  // Unknown controls are used in conjunction with <object class="unknown">.
  //bool AttachUnknownControl(const wxString& name, wxWindow *control,  wxWindow *parent = NULL);
  inline bool attachUnknownControl(IN(RString)  name, IN(RWindow) control, IN(RWindow) parent = Nil) { return getWx()->AttachUnknownControl(S2WXS(name), CLS2WXPTR(control), CLS2WXPTR(parent)); }
  
  // Returns a numeric ID that is equivalent to the string id used in an XML
  // resource. To be used in event tables.
  // Macro XRCID is provided for convenience
  static int getXRCID(IN(RString)  name)
  {
    return wxXmlResource::GetXRCID(S2WXS(name));
  }
  static int s2id(IN(RString)  name) { return getXRCID(name); }
  
  // Returns version information (a.b.c.d = d+ 256*c + 256^2*b + 256^3*a).
  //long GetVersion() const { return m_version; }
  inline int getVersion() const { return getWx()->GetVersion(); }
  
  // Compares resources version to argument. Returns -1 if resources version
  // is less than the argument, +1 if greater and 0 if they equal.
  //int CompareVersion(int major, int minor, int release, int revision) const;
  inline int compareVersion(int major, int minor, int release, int revision) const { return getWx()->CompareVersion(major, minor, release, revision); }

  //// Singleton accessors.
  
  // Gets the global resources object or creates one if none exists.
  //static wxXmlResource *Get();
  inline static RXmlResource get() { RETURN_WXPTR2CLS(XmlResource, wxXmlResource::Get()); }
  
  // Sets the global resources object and returns a pointer to the previous one (may be NULL).
  //static wxXmlResource *Set(wxXmlResource *res);
  inline static RXmlResource set(IN(RXmlResource) res) { RETURN_WXPTR2CLS(XmlResource, wxXmlResource::Set(CLS2WXPTR(res))); }
  
  // Returns flags, which may be a bitlist of wxXRC_USE_LOCALE and wxXRC_NO_SUBCLASSING.
  //int GetFlags() const { return m_flags; }
  inline int getFlags() const { return getWx()->GetFlags(); }
  // Set flags after construction.
  //void SetFlags(int flags) { m_flags = flags; }
  inline void setFlags(int flags) { getWx()->SetFlags(flags); }
};

} // wx
} // acdk

#endif //acdk_wx_XmlResource_h
