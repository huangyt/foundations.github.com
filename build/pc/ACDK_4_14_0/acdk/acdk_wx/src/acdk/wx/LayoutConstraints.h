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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/LayoutConstraints.h,v 1.9 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_LayoutConstraints_h
#define acdk_wx_LayoutConstraints_h

#include "Window.h"

namespace acdk {
namespace wx {

enum Edge
{
    Left  /* wxLeft*/, 
    Top  /* wxTop*/, 
    Right  /* wxRight*/, 
    Bottom  /* wxBottom*/, 
    Width  /* wxWidth*/, 
    Height  /* wxHeight*/,
    Centre  /* wxCentre*/, 
    Center  /* wxCenter*/ = wxCentre, 
    CentreX  /* wxCentreX*/, 
    CentreY  /* wxCentreY*/
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, Edge);

enum LayoutRelationship
{
    Unconstrained  /* wxUnconstrained = 0*/,
    AsIs  /* wxAsIs*/,
    PercentOf,
    Above,
    Below,
    LeftOf,
    RightOf,
    SameAs,
    Absolute
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, LayoutRelationship);

enum LayoutMargin
{
  LayoutDefaultMargin  = 0/* wxLAYOUT_DEFAULT_MARGIN*/
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, LayoutMargin);

ACDK_DECL_CLASS(LayoutConstraints);

ACDK_DECL_CLASS(IndividualLayoutConstraint);

/**
  see wxIndividualLayoutConstraint
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC IndividualLayoutConstraint
: extends WxObject
{
  ACDK_WITH_METAINFO(IndividualLayoutConstraint)
public:
  // wxIndividualLayoutConstraint
  ACDK_WX_STD_MEMBERS(IndividualLayoutConstraint, WxObject)

  IndividualLayoutConstraint() : WxObject(new wxIndividualLayoutConstraint()) {}
    

    //  = 0 = wxLAYOUT_DEFAULT_MARGIN
    //void Set(wxRelationship rel, wxWindow *otherW, wxEdge otherE, int val, int marg);
  inline void set(LayoutRelationship rel, IN(RWindow) otherW, Edge otherE, int val = 0, int marg = LayoutDefaultMargin) 
  { getWx()->Set(wxRelationship(rel), CLS2WXPTR(otherW), wxEdge(otherE), val, marg); }

    //
    // Sibling relationships
    //
  //void LeftOf(wxWindow *sibling, int marg);
  inline void leftOf(IN(RWindow) sibling, int marg = LayoutDefaultMargin) { getWx()->LeftOf(CLS2WXPTR(sibling), marg); }
    //void RightOf(wxWindow *sibling, int marg);
  inline void rightOf(IN(RWindow) sibling, int marg = LayoutDefaultMargin) { getWx()->RightOf(CLS2WXPTR(sibling), marg); }
    //void Above(wxWindow *sibling, int marg);
  inline void above(IN(RWindow) sibling, int marg = LayoutDefaultMargin) { getWx()->Above(CLS2WXPTR(sibling), marg); }
    //void Below(wxWindow *sibling, int marg);
  inline void below(IN(RWindow) sibling, int marg = LayoutDefaultMargin) { getWx()->Below(CLS2WXPTR(sibling), marg); }

    //
    // 'Same edge' alignment
    //
    //void SameAs(wxWindow *otherW, wxEdge edge, int marg);
  inline void sameAs(IN(RWindow) otherW, Edge edge, int marg = LayoutDefaultMargin) { getWx()->SameAs(CLS2WXPTR(otherW), wxEdge(edge), marg); }

    // The edge is a percentage of the other window's edge
    //void PercentOf(wxWindow *otherW, wxEdge wh, int per);
  inline void percentOf(IN(RWindow) otherW, Edge wh, int per) { getWx()->PercentOf(CLS2WXPTR(otherW), wxEdge(wh), per); }

    //
    // Edge has absolute value
    //
    //void Absolute(int val);
  inline void absolute(int val) { getWx()->Absolute(val); }

    //
    // Dimension is unconstrained
    //
    //void Unconstrained() { relationship = wxUnconstrained; }
  inline void unconstrained() { getWx()->Unconstrained(); }

    //
    // Dimension is 'as is' (use current size settings)
    //
    //void AsIs() { relationship = wxAsIs; }
  inline void asIs() { getWx()->AsIs(); }

    //
    // Accessors
    //
    //wxWindow *GetOtherWindow() { return otherWin; }
  inline RWindow getOtherWindow() { RETURN_WXPTR2CLS(Window, (wxWindow*)getWx()->GetOtherWindow()); }
    //wxEdge GetMyEdge() const { return myEdge; }
  inline Edge getMyEdge() const { return Edge(getWx()->GetMyEdge()); }
  //void SetEdge(wxEdge which) { myEdge = which; }
  inline void setEdge(Edge which) { getWx()->SetEdge(wxEdge(which)); }
    //void SetValue(int v) { value = v; }
  inline void setValue(int v) { getWx()->SetValue(v); }
    //int GetMargin() { return margin; }
  inline int getMargin() { return getWx()->GetMargin(); }
    //void SetMargin(int m) { margin = m; }
  inline void setMargin(int m) { getWx()->SetMargin(m); }
    //int GetValue() const { return value; }
  inline int getValue() const { return getWx()->GetValue(); }
    //int GetPercent() const { return percent; }
  inline int getPercent() const { return getWx()->GetPercent(); }
    //int GetOtherEdge() const { return otherEdge; }
  inline int getOtherEdge() const { return getWx()->GetOtherEdge(); }
    //bool GetDone() const { return done; }
  inline bool getDone() const { return getWx()->GetDone(); }
    //void SetDone(bool d) { done = d; }
  inline void setDone(bool d) { getWx()->SetDone(d); }
    //wxRelationship GetRelationship() { return relationship; }
  inline LayoutRelationship getRelationship() { return LayoutRelationship(getWx()->GetRelationship()); }
    //void SetRelationship(wxRelationship r) { relationship = r; }
  inline void setRelationship(LayoutRelationship r) { getWx()->SetRelationship(wxRelationship(r)); }

    // Reset constraint if it mentions otherWin
    //bool ResetIfWin(wxWindow *otherW);
  inline bool resetIfWin(IN(RWindow) otherW) { return getWx()->ResetIfWin(CLS2WXPTR(otherW)); }

    // Try to satisfy constraint
    //bool SatisfyConstraint(wxLayoutConstraints *constraints, wxWindow *win);
  inline bool satisfyConstraint(IN(RLayoutConstraints) constraints, IN(RWindow) win);

    // Get the value of this edge or dimension, or if this
    // is not determinable, -1.
    //int GetEdge(wxEdge which, wxWindow *thisWin, wxWindow *other) const;
  inline int getEdge(Edge which, IN(RWindow) thisWin, IN(RWindow) other) { return getWx()->GetEdge(wxEdge(which), CLS2WXPTR(thisWin), CLS2WXPTR(other)); }

};



ACDK_DECL_CLASS(LayoutConstraints);

/**
  see wxLayoutConstraints
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC LayoutConstraints
: extends WxObject
{
  ACDK_WITH_METAINFO(LayoutConstraints)
public:
  // wxLayoutConstraints
  ACDK_WX_STD_MEMBERS(LayoutConstraints, WxObject)
  LayoutConstraints() : WxObject(new wxLayoutConstraints(), true) {}
  RIndividualLayoutConstraint left() { return new IndividualLayoutConstraint(&getWx()->left, false); }
  RIndividualLayoutConstraint top() { return new IndividualLayoutConstraint(&getWx()->top, false); }
  RIndividualLayoutConstraint right() { return new IndividualLayoutConstraint(&getWx()->right, false); }
  RIndividualLayoutConstraint bottom() { return new IndividualLayoutConstraint(&getWx()->bottom, false); }
  RIndividualLayoutConstraint width() { return new IndividualLayoutConstraint(&getWx()->width, false); }
  RIndividualLayoutConstraint height() { return new IndividualLayoutConstraint(&getWx()->height, false); }
  RIndividualLayoutConstraint centreX() { return new IndividualLayoutConstraint(&getWx()->centreX, false); }
  RIndividualLayoutConstraint centreY() { return new IndividualLayoutConstraint(&getWx()->centreY, false); }
};


inline bool 
IndividualLayoutConstraint::satisfyConstraint(IN(RLayoutConstraints) constraints, IN(RWindow) win) 
{ return getWx()->SatisfyConstraint(CLS2WXPTR(constraints), CLS2WXPTR(win)); }

inline void 
Window::setConstraints(IN(RLayoutConstraints) constraints) { getWx()->SetConstraints(CLS2WXPTR_L(constraints)); }
    
inline RLayoutConstraints 
Window::getConstraints() const { RETURN_WXPTR2CLS(LayoutConstraints, getWx()->GetConstraints()); }

inline void 
Window::unsetConstraints(IN(RLayoutConstraints) c) { getWx()->UnsetConstraints(CLS2WXPTR_L(c)); }

} // wx
} // acdk

#endif //acdk_wx_LayoutConstraints_h
