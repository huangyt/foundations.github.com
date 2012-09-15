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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Wizard.h,v 1.3 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Wizard_h
#define acdk_wx_Wizard_h

#include "Dialog.h"
#include "WizardPage.h"

namespace acdk {
namespace wx {



ACDK_DECL_CLASS(Wizard);
/**
  see wxWizard
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Wizard
: extends Dialog
{
  ACDK_WITH_METAINFO(Wizard)
public:
  ACDK_WX_STD_MEMBERS(Wizard, Dialog)
  /**
    @param style combination of WizardFlags
  */
  Wizard(IN(RWindow) parent, int id,  IN(RString) title, IN(RBitmap) bitmap = Bitmap::getNullBitmap(), IN(RPoint) pos = Point::defaultPosition())
    : Dialog(new wxWizard(CLS2WXPTR(parent), id, S2WXS(title), CLS2WXREF(bitmap), pos->toWx()))
  {
  }
  //void FitToPage(const wxWizardPage* firstPage);
  inline void fitToPage(IN(RWizardPage) firstPage) { getWx()->FitToPage(CLS2WXPTR(firstPage)); }
  //wxWizardPage* GetCurrentPage() const;
  inline RWizardPage getCurrentPage() const { RETURN_WXPTR2CLS(WizardPage, getWx()->GetCurrentPage()); }
  //wxSize GetPageSize() const;
  inline RSize getPageSize() const { return WXVAL2CLS(Size, getWx()->GetPageSize()); }
  //virtual bool HasNextPage(wxWizardPage *page);
  inline virtual bool hasNextPage(IN(RWizardPage) page) { return getWx()->HasNextPage(CLS2WXPTR(page)); }
  //virtual bool HasPrevPage(wxWizardPage *page);
  inline virtual bool hasPrevPage(IN(RWizardPage) page) { return getWx()->HasPrevPage(CLS2WXPTR(page)); }
  //bool RunWizard(wxWizardPage* firstPage);
  inline bool runWizard(IN(RWizardPage) firstPage) { return getWx()->RunWizard(CLS2WXPTR(firstPage)); }
  //void SetPageSize(const wxSize& sizePage);
  inline void setPageSize(IN(RSize) sizePage) { getWx()->SetPageSize(CLS2WXREF(sizePage)); }
};

inline
WizardPage::WizardPage(IN(RWizard) parent, IN(RBitmap) bitmap, IN(RString) resource)
: Panel(new wxWizardPageSimple(CLS2WXPTR(parent), 0, 0, CLS2WXREF(bitmap), S2WXS(resource)),  parent == Nil)
{
}


} // wx
} // acdk

#endif //acdk_wx_Wizard_h
