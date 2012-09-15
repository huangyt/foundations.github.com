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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/WizardPage.h,v 1.3 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_WizardPage_h
#define acdk_wx_WizardPage_h

#include "Panel.h"

namespace acdk {
namespace wx {

ACDK_DECL_CLASS(Wizard);

ACDK_DECL_CLASS(WizardPage);

/**
  see wxWizardPage
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC WizardPage
: extends Panel
{
  ACDK_WITH_METAINFO(WizardPage)
public:
  // wxWizardPage wxWizardPageSimple
  ACDK_WX_STD_MEMBERS(WizardPage, Panel)
  WizardPage(IN(RWizard) parent, IN(RBitmap) bitmap, IN(RString) resource = Nil);
  //wxWizardPage* GetPrev() const;
  // ### @todo create wxWizardPageFwd 
  inline RWizardPage getPrev() const { RETURN_WXPTR2CLS(WizardPage, getWx()->GetPrev()); }
  //wxWizardPage* GetNext() const;
  inline RWizardPage getNext() const { RETURN_WXPTR2CLS(WizardPage, getWx()->GetNext()); }
  //wxBitmap GetBitmap() const;
  inline RBitmap getBitmap() const { return WXVAL2CLS(Bitmap, getWx()->GetBitmap()); }
};


} // wx
} // acdk

#endif //acdk_wx_WizardPage_h
