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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/guidbg/SyntaxTreeCtrl.h,v 1.3 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_guidbg_SyntaxTreeCtrl_h
#define acdk_aci_guidbg_SyntaxTreeCtrl_h

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
#include <acdk/aci/parser/SyntaxParseNode.h>

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



ACDK_DECL_CLASS(SyntaxTreeCtrl);

class ACDK_ACI_GUIDBG_PUBLIC SyntaxTreeCtrl
: extends TreeCtrl
{
  acdk::aci::RCompiler _comp;
public:
  SyntaxTreeCtrl(IN(RWindow) parent, int id, IN(RPoint) pos, IN(RSize) size);
  void setCompiler(IN(acdk::aci::RCompiler) comp)
  {
    _comp = comp;
    resetTree();
    
    /*
    RString rootName = _rootCode->getNodeName();
    RTreeItemId root = addRoot(rootName);
    setItemHasChildren(root, true);
    setItemDataObject(root, &_rootCode);
    */
  }
  void resetTree()
  {
    RTreeItemId root = getRootItem();
    if (root == Nil)
      return;
    deleteItem(root);
    fillRoots();
  }
  void fillRoots();
  void expandAll();
  void onExpand(IN(RTreeEvent) event);
  void expandItems(IN(RTreeItemId) tit);
  void expandAll(IN(RTreeItemId) tit);
  void addSyntaxNode(IN(RTreeItemId) tit, IN(acdk::aci::parser::RSyntaxNode) sn);
  
};

} // namespace guidbg
} // namespace aci 
} // namespace acdk 


#endif //acdk_aci_guidbg_SyntaxTreeCtrl_h


