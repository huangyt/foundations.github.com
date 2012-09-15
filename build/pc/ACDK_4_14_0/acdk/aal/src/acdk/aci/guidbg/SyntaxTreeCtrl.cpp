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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/guidbg/SyntaxTreeCtrl.cpp,v 1.4 2005/02/05 10:44:51 kommer Exp $

#include "SyntaxTreeCtrl.h"
#include <acdk/aci/parser/ParseEnv.h>
#include <acdk/aci/Compiler.h>
#include <acdk/aci/ast/Terminal.h>

namespace acdk {
namespace aci {
namespace guidbg {

using namespace acdk::wx;

SyntaxTreeCtrl::SyntaxTreeCtrl(IN(RWindow) parent, int id, IN(RPoint) pos, IN(RSize) size)
: TreeCtrl(&parent, id, pos, size)
{
    /*
    RImageList imageList = new ImageList(16, 16);
    imageList->add(new Bitmap(folder_untested_xpm));
    imageList->add(new Bitmap(folder_fail_xpm));
    imageList->add(new Bitmap(folder_ok_xpm));
    assignImageList(imageList);
    */
  connect(TreeEvent::EvtCommandTreeItemExpanding, id, (ObjectEventFunction)&SyntaxTreeCtrl::onExpand);
    
    //fillWithTests();
}

void 
SyntaxTreeCtrl::fillRoots()
{
  RTreeItemId root = addRoot("Syntax");
  RTreeItemId nonterms = appendItem(root, "non-terms");
  RTreeItemId terms = appendItem(root, "terms");

  acdk::aci::parser::RAstNodeParserTable pt = _comp->getParseEnv()->_currentParseFrame->_codeParserTable;
  RStringArray roots = pt->getKeys();
  for (int i = 0; i < roots->length(); ++i)
  {
    RString rn = roots[i];
    RParseNodeArray pna = pt->get(rn);
    for (int j = 0; j < pna->length(); ++j)
    {
      RParseNode pn = pna[j];
      if (instanceof(pn, TerminalParseNode) == true)
      {
        RTreeItemId ctid = appendItem(terms, rn);
      }
      else
      {
        RTreeItemId ctid = appendItem(nonterms, rn);
        setItemHasChildren(ctid, true);
        setItemDataObject(ctid, &pn);
      }
    }
  }
}

void 
SyntaxTreeCtrl::expandAll()
{
}

void 
SyntaxTreeCtrl::onExpand(IN(RTreeEvent) event)
{
  RTreeItemId tid = event->getItem();  
  expandItems(tid);
}

void 
SyntaxTreeCtrl::expandItems(IN(RTreeItemId) tid)
{
  if (getChildrenCount(tid, false) != 0)
    return;
  RObject tre = getItemDataObject(tid);
  if (instanceof(tre, SyntaxParseNode) == true)
  {
    RSyntaxNode sn = RSyntaxParseNode(tre)->getSyntaxTree();
    addSyntaxNode(tid, sn);
  }
  if (instanceof(tre, SyntaxNode) == true)
  {
    RSyntaxNode sn = RSyntaxNode(tre);
    for (int i = 0; i < sn->childs->length(); ++i)
    {
      addSyntaxNode(tid, sn->childs[i]);
    }
  }
}

void 
SyntaxTreeCtrl::addSyntaxNode(IN(RTreeItemId) tit, IN(acdk::aci::parser::RSyntaxNode) sn)
{
      
  if (instanceof(sn, SyntaxRule) == true)
  {
    RString rn = RSyntaxRule(sn)->ruleName;
    RParseNode pn = _comp->getParseNode(rn);
    RTreeItemId ctid = appendItem(tit, pn->toString());
    setItemHasChildren(ctid, true);
    setItemDataObject(ctid, &pn);
    return;
  }
  RString s = sn->toString();
  RTreeItemId ctid = appendItem(tit, s);
  setItemHasChildren(ctid, sn->childs->length() > 0);
  setItemDataObject(ctid, &sn);
}

void 
SyntaxTreeCtrl::expandAll(IN(RTreeItemId) tit)
{
}
  
  

} // namespace guidbg
} // namespace aci 
} // namespace acdk 



