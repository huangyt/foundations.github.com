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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ide/inspector/ObjectTreeCtrl.h,v 1.2 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_wx_inspector_ObjectTreeCtrl_h
#define acdk_wx_inspector_ObjectTreeCtrl_h

#include "inspector.h"
#include <acdk/wx/TreeCtrl.h>

namespace acdk {
namespace wx {
namespace ide {
namespace inspector {

enum ObjectFilterFlags
{
  ObjectFilterCurrent = 0,
  ObjectFilterRootsOnly = 1,
  ObjectFilterAllObjects,
  ObjectFilterGcAble
};
ACDK_DEF_LIB_ENUM(ACDK_WX_IDE_PUBLIC, ObjectFilterFlags);

ACDK_DECL_CLASS(ObjectTreeCtrl);
/**
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.2 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_WX_IDE_PUBLIC ObjectTreeCtrl
: extends TreeCtrl
{
  ACDK_WITH_METAINFO(ObjectTreeCtrl)
protected:
  RString _filter;
  int _filterFlags;
  RTextCtrl _overViewTextCtrl;
public:
  ObjectTreeCtrl(IN(RWindow) parent, int id, IN(RPoint) point, IN(RSize) size);
  RTextCtrl getOverViewTextCtrl() { return _overViewTextCtrl; }
  void setOverViewTextCtrl(IN(RTextCtrl) txtctrl) { _overViewTextCtrl = txtctrl; }
  RString getObjectDescription(IN(RObject) obj);
  void reload(ObjectFilterFlags flags = ObjectFilterCurrent);
  void onListBoxExpanded(IN(RTreeEvent) event);
  void onSelChanged(IN(RTreeEvent) event);
  void _expand(IN(RTreeItemId) tid, IN(RObject) o);
  /**
    @param flags one of 
  */
  void fillRoot();
  /**
    @param filter regular expression
    @param flags combination of ObjectFilterFlags
  */
  void setFilter(IN(RString) filter, int flags);
};


} // inspector
} // ide
} // wx
} // acdk

#endif //acdk_wx_inspector_ObjectTreeCtrl_h
