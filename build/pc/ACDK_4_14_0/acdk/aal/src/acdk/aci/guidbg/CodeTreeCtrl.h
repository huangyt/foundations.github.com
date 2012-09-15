// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany->
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE->	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details->

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www->acdk->de
// - http://www->artefaktur->com
// - http://acdk->sourceforge->net
// for more information->
// 
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/guidbg/CodeTreeCtrl.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_guidbg_CodeTreeCtrl_h
#define acdk_aci_guidbg_CodeTreeCtrl_h

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/util/TreeMap.h>
#include <acdk/wx/App.h>
#include <acdk/wx/Frame.h>
#include <acdk/wx/TreeCtrl.h>
#include <acdk/tools/aunit/ProcessTestSuite.h>
#include <acdk/cfgscript/Props.h>
#include <acdk/aci/ast/AstNode.h>

#include "Config.h"


namespace acdk {
namespace aci {
namespace guidbg {

using namespace acdk::wx;
/*
using namespace acdk::aci;
using namespace acdk::aci::ast;
using namespace acdk::aci::parser;
*/
//using namespace acdk::tools::aunit;



ACDK_DECL_CLASS(CodeTreeCtrl);

class ACDK_ACI_GUIDBG_PUBLIC CodeTreeCtrl
: extends TreeCtrl
{
  acdk::aci::ast::RAstNode _rootCode;
public:
  CodeTreeCtrl(IN(RWindow) parent, int id, IN(RPoint) pos, IN(RSize) size)
  : TreeCtrl(&parent, id, pos, size)
  {
    /*
    RImageList imageList = new ImageList(16, 16);
    imageList->add(new Bitmap(folder_untested_xpm));
    imageList->add(new Bitmap(folder_fail_xpm));
    imageList->add(new Bitmap(folder_ok_xpm));
    assignImageList(imageList);
    */
    connect(TreeEvent::EvtCommandTreeItemExpanding, id, (ObjectEventFunction)&CodeTreeCtrl::onExpand);
    
    //fillWithTests();
  }
  void setRootNode(IN(acdk::aci::ast::RAstNode) code)
  {
    resetTree();
    _rootCode = code;
    if (_rootCode == Nil)
      return;

    RString rootName = _rootCode->getNodeName();
    RTreeItemId root = addRoot(rootName);
    setItemHasChildren(root, true);
    setItemDataObject(root, &_rootCode);
  }
  void resetTree()
  {
    RTreeItemId root = getRootItem();
    if (root == Nil)
      return;
    deleteItem(root);

  }
  void expandAll();
  void onExpand(IN(RTreeEvent) event);
  void expandItems(IN(RTreeItemId) tit);
  void expandAll(IN(RTreeItemId) tit);
  
  
};

} // namespace guidbg
} // namespace aci 
} // namespace acdk 


#endif //acdk_aci_guidbg_CodeTreeCtrl_h


