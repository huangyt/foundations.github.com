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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/TreeCtrl.cpp,v 1.4 2005/02/05 10:45:35 kommer Exp $


#include "TreeCtrl.h"

namespace acdk {
namespace wx {

ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeBeginDrag, wxEVT_COMMAND_TREE_BEGIN_DRAG);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeBeginRDrag, wxEVT_COMMAND_TREE_BEGIN_RDRAG);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeBeginLabelEdit, wxEVT_COMMAND_TREE_BEGIN_LABEL_EDIT);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeEndLabelEdit, wxEVT_COMMAND_TREE_END_LABEL_EDIT);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeDeleteItem, wxEVT_COMMAND_TREE_DELETE_ITEM);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeGetInfo, wxEVT_COMMAND_TREE_GET_INFO);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeSetInfo, wxEVT_COMMAND_TREE_SET_INFO);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeItemExpanded, wxEVT_COMMAND_TREE_ITEM_EXPANDED);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeItemExpanding, wxEVT_COMMAND_TREE_ITEM_EXPANDING);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeItemCollapsed, wxEVT_COMMAND_TREE_ITEM_COLLAPSED);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeItemCollapsing, wxEVT_COMMAND_TREE_ITEM_COLLAPSING);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeSelChanged, wxEVT_COMMAND_TREE_SEL_CHANGED);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeSelChanging, wxEVT_COMMAND_TREE_SEL_CHANGING);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeKeyDown, wxEVT_COMMAND_TREE_KEY_DOWN);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeItemActivated, wxEVT_COMMAND_TREE_ITEM_ACTIVATED);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeItemRightClick, wxEVT_COMMAND_TREE_ITEM_RIGHT_CLICK);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeItemMiddleClick, wxEVT_COMMAND_TREE_ITEM_MIDDLE_CLICK);
ACDK_DEFINE_WX_EVENT(TreeEvent, EvtCommandTreeEndDrag, wxEVT_COMMAND_TREE_END_DRAG);


} // wx
} // acdk

