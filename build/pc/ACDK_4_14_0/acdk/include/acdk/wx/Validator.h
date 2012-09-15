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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Validator.h,v 1.10 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Validator_h
#define acdk_wx_Validator_h

#include "Event.h"

namespace acdk {
namespace wx {

ACDK_DECL_CLASS(Validator);
/**
  see wxValidator
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Validator
: extends EvtHandler
{
  ACDK_WITH_METAINFO(Validator)
public:
  ACDK_WX_STD_MEMBERS(Validator, EvtHandler)
  
  Validator();
  //virtual wxObject *Clone() const;
  inline virtual RObject clone() { return new Validator(); }
    

    // Called when the value in the window must be validated.
    // This function can pop up an error message.
    //virtual bool Validate(wxWindow *parent) { return FALSE; };
  inline virtual bool validate(IN(RWindow) parent) { return getWx()->wxValidator::Validate(CLS2WXPTR(parent)); }

    // Called to transfer data to the window
    //virtual bool TransferToWindow() { return FALSE; }
  inline virtual bool transferToWindow() { return getWx()->wxValidator::TransferToWindow(); }

    // Called to transfer data from the window
    //virtual bool TransferFromWindow() { return FALSE; };
  inline virtual bool transferFromWindow() { return getWx()->wxValidator::TransferFromWindow(); }

    // accessors
    //wxWindow *GetWindow() const { return (wxWindow *)m_validatorWindow; }
  inline RWindow getWindow() const { RETURN_WXPTR2CLS(Window, getWx()->GetWindow()); }
    //void SetWindow(wxWindowBase *win) { m_validatorWindow = win; }
  inline void setWindow(IN(RWindow) win) { getWx()->SetWindow(CLS2WXPTR(win)); }

    // validators beep by default if invalid key is pressed, these functions
    // allow to change it
    //static bool IsSilent() { return ms_isSilent; }
  inline static bool isSilent() { return wxValidator::IsSilent(); }
    //static void SetBellOnError(bool doIt = TRUE) { ms_isSilent = doIt; }
  inline static void setBellOnError(bool doIt = true) { wxValidator::SetBellOnError(doIt); }
  static RValidator defaultValidator() { return new Validator(); }// ## maybe static instance
};


inline
void 
Window::setValidator(IN(RValidator) validator) { getWx()->wxWindow::SetValidator(CLS2WXREF(validator)); }

inline
RValidator 
Window::getValidator() { RETURN_WXPTR2CLS(Validator, getWx()->wxWindow::GetValidator()); }


class ACDK_WX_PUBLIC WxValidatorFwd
: public wxValidator
, public AcdkForwarder<Validator>
{
public:
  WxValidatorFwd() {}
 
  
  virtual wxObject *Clone() const
  {
    WxValidatorFwd* ret = new WxValidatorFwd();
    
    ret->setOwningForward((RValidator)_forward->clone());
    return ret;
  }
 
  // Called when the value in the window must be validated.
  // This function can pop up an error message.
    virtual bool Validate(wxWindow *parent) { return _forward->validate(WXPTR2CLS(Window, parent)); };

    // Called to transfer data to the window
    virtual bool TransferToWindow() { return _forward->transferToWindow(); }

    // Called to transfer data from the window
    virtual bool TransferFromWindow() { return _forward->transferFromWindow(); };
};

inline
Validator::Validator() 
: EvtHandler(new WxValidatorFwd()) 
{
  ((WxValidatorFwd*)getWx())->setOwningForward(this);
}

} // wx
} // acdk

#endif //acdk_wx_Validator_h
