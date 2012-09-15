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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/guidbg/CodeTreeCtrl.cpp,v 1.6 2005/02/05 10:44:51 kommer Exp $

#include "CodeTreeCtrl.h"
#include <acdk/io/StringWriter.h>
#include <acdk/aci/vm/OpCodeOp.h>
#include <acdk/aci/ast/Terminal.h>

namespace acdk {
namespace aci {
namespace guidbg {

using namespace acdk::wx;
using namespace acdk::util;
using namespace acdk::aci::ast;

//using namespace acdk::tools::aunit;
USING_CLASS(::acdk::aci::ast::, AstNode);


void 
CodeTreeCtrl::onExpand(IN(RTreeEvent) event)
{
  RTreeItemId tid = event->getItem();  
  expandItems(tid);
}

void 
CodeTreeCtrl::expandItems(IN(RTreeItemId) tid)
{
  if (getChildrenCount(tid, false) != 0)
    return;
  RObject tre = getItemDataObject(tid);
  
  if (instanceof(tre, AstNode) == true)
  {
    RAstNode code = (RAstNode)tre;

    int scl = code->getChildCount(); 
    if (scl > 0)
    {
      for (int i = 0; i < scl; ++i)
      {
        RAstNode sc = code->getChild(i);
        acdk::io::StringWriter sout;
        acdk::io::PrintWriter pout(&sout);
        // ### sc->printThisNode(&pout, "");

        RString label = sc->getNodeName();
        /*
          if (instanceof(sc, OpCodeStm) == true)
          label = sout.getString();
          */
        //RString label = cn + ": " + sc->toString() + "; " + sc->getCodeString();
        //RString label = sout.getString();
        // label = label->replace("\n", ", ");
        RTreeItemId ctid = appendItem(tid, label);
        setItemDataObject(ctid, &sc);
        //if (instanceof(sc, OpCodeStm) == false)
          setItemHasChildren(ctid, true);
        
        
      }
    }
    RSymbolTable st = code->getSymbolTable();
    if (st != Nil)
    {
      RTreeItemId ctid = appendItem(tid, "ST");
      setItemHasChildren(ctid, true);
      setItemDataObject(ctid, &st);
    }
    acdk::lang::reflect::RFieldArray fields = code->getClass()->getFields();
    for (int j = 0; j < fields->length(); ++j)
    {
      acdk::lang::reflect::RField f = fields[j];
      RString fn = f->getName();
      if (fn->equals("_subNodes") == true)
        continue;
      RObject fo = f->get(&code, 0);
      RString label = fn;
      if (fo == Nil)
        continue;

      if (instanceof(fo, SemanticElem) == true)
      {
        RSemanticElem sem(fo);
        label = label + "=" + fo->getClass()->getName() + ", " + sem->getName()->replace("\n", " ");
        RTreeItemId ctid = appendItem(tid, label);
        setItemDataObject(ctid, fo);
      }
      else
      {
        label = label + "=" + fo->toString()->replace("\n", " ");
        RTreeItemId ctid = appendItem(tid, label);
      }
    }
  } 
  else if (instanceof(tre, SymbolTable) == true)
  {
    RSymbolTable st(tre);
    RHashMap tm = st->_typeMap;
    if (tm != Nil && tm->size() > 0)
    {
      RTreeItemId ctid = appendItem(tid, "TypeMap");
      setItemHasChildren(ctid, true);
      RIterator it = tm->iterator();
      while (it->hasNext() == true)
      {
        RString s = (RString)it->next();
        appendItem(ctid, s);
        // ## append clazz
      }
    }
    if (st->_variables != Nil && st->_variables->length() > 0)
    {
      RTreeItemId ctid = appendItem(tid, "Variables");
      setItemHasChildren(ctid, true);
      for (int i = 0; i < st->_variables->length(); ++i)
      {
        RVarDefinition vdef = st->_variables[i];
        RDClazzInfo vtype = vdef->getType();
        if (vtype != Nil)
          appendItem(ctid, vdef->getName() + "=" + vdef->getType()->getName());
        else
          appendItem(ctid, vdef->getName() + "=" + "<unset type>");
        // ## append clazz
      }
    }
	  if (st->_seeAlsoTypes != Nil && st->_seeAlsoTypes->length() > 0)
	  {
	    RTreeItemId ctid = appendItem(tid, "SeeAlsoTypes");
      setItemHasChildren(ctid, true);
      for (int i = 0; i < st->_seeAlsoTypes->length(); ++i)
      {
        acdk::lang::ref::RWeakReference vdef = st->_seeAlsoTypes[i];
        appendItem(ctid, "Symbol Table");
        setItemHasChildren(ctid, true);
        setItemDataObject(ctid, vdef->get());
      }
	  }
    if (st->_seeAlsoVars != Nil && st->_seeAlsoVars->length() > 0)
	  {
	    RTreeItemId ctid = appendItem(tid, "SeeAlsoVars");
      setItemHasChildren(ctid, true);
      for (int i = 0; i < st->_seeAlsoVars->length(); ++i)
      {
        acdk::lang::ref::RWeakReference vdef = st->_seeAlsoVars[i];
        appendItem(ctid, "Symbol Table");
        setItemHasChildren(ctid, true);
        setItemDataObject(ctid, vdef->get());
      }
	  }
  }

}

void 
CodeTreeCtrl::expandAll(IN(RTreeItemId) tid)
{
  expandItems(tid);
  expand(tid);
  if (itemHasChildren(tid) == false)
    return;
  int cockie = 0;
  RTreeItemId cid = getFirstChild(tid, cockie);
  while (cid->isOk() == true)
  {
    expandAll(cid);
    cid = getNextChild(tid, cockie);
  }
}

void 
CodeTreeCtrl::expandAll()
{
  expandAll(getRootItem());
}


} // namespace guidbg
} // namespace aci
} // namespace acdk 




