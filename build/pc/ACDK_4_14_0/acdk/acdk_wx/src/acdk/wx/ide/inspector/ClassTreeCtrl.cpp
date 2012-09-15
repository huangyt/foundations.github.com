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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ide/inspector/ClassTreeCtrl.cpp,v 1.3 2005/03/11 11:11:50 kommer Exp $


#include "ClassTreeCtrl.h"
#include <acdk/lang/reflect/Unit.h>
#include <acdk/lang/dmi/DmiDelegate.h>
#include <acdk/util/Arrays.h>
#include <acdk/util/StringTokenizer.h>


namespace acdk {
namespace wx {
namespace ide {
namespace inspector {

#include "ti_class.xpm"
#include "ti_interface.xpm"
#include "ti_field.xpm"
#include "ti_method.xpm"
#include "ti_enumeration.xpm"
#include "ti_unit.xpm"

using namespace acdk::lang::reflect;

enum Icons
{
  ClassXpm = 0,
  InterfaceXpm,
  FieldXpm,
  MethodXpm,
  EnumerationXpm,
  UnitXpm
};


ClassTreeCtrl::ClassTreeCtrl(IN(RWindow) parent, int id, IN(RPoint) point, IN(RSize) size)
: TreeCtrl(parent, id, point, size)
, _filterFlags(ClassTreeFilterAll)
{
  RTreeItemId rootId = addRoot("Root");
  RUnit rootUnit = Unit::getUnit("");
  _expand(rootId, &rootUnit);
  expand(rootId);
  connect(TreeEvent::EvtCommandTreeItemExpanding, id, (ObjectEventFunction)&ClassTreeCtrl::onListBoxExpanded);
  connect(TreeEvent::EvtCommandTreeSelChanged, id, (ObjectEventFunction)&ClassTreeCtrl::onSelChanged);
  RImageList imageList = new ImageList(16, 16);
  imageList->add(new Bitmap(ti_class_xpm));
  imageList->add(new Bitmap(ti_interface_xpm));
  imageList->add(new Bitmap(ti_field_xpm));
  imageList->add(new Bitmap(ti_method_xpm));
  imageList->add(new Bitmap(ti_enumeration_xpm));
  imageList->add(new Bitmap(ti_unit_xpm));
  assignImageList(imageList);
}

void 
ClassTreeCtrl::reload()
{
  deleteAllItems();
  RTreeItemId rootId = addRoot("Root");
  RUnit rootUnit = Unit::getUnit("");
  _expand(rootId, &rootUnit);
  expand(rootId);
}

void 
ClassTreeCtrl::onListBoxExpanded(IN(RTreeEvent) event)
{
  RTreeItemId tid = event->getItem();
  if (getChildrenCount(tid, false) != 0)
      return;
  RObject o = getItemDataObject(tid);
  _expand(tid, o);
}

void 
ClassTreeCtrl::onSelChanged(IN(RTreeEvent) event)
{
  if (_overViewTextCtrl == Nil)
    return;
  RTreeItemId tid = event->getItem();
  RObject o = getItemDataObject(tid);
  if (o == Nil)
  {
    _overViewTextCtrl->setValue("");
    return;
  }
  _overViewTextCtrl->setValue(getObjectDescription(o));
}

void 
ClassTreeCtrl::_expand(IN(RTreeItemId) tid, IN(RObject) o)
{
  if (instanceof(o, Unit) == true)
  {
    RUnit unit = (RUnit)o;
    RUnitArray sa = unit->getChilds();
    acdk::util::Arrays::sort(sa);
    int i;
    for (i = 0; i < sa->length(); ++i)
    {
      RUnit su = sa[i];
      RTreeItemId sid = appendItem(tid, su->toString());
      setItemDataObject(sid, &su);
      setItemHasChildren(sid, true);
      setItemImage(sid, UnitXpm);
      setItemImage(sid, UnitXpm, TreeitemiconSelected);
    }
    REnumerationArray ea = unit->getEnumerations();
    if (ea->length() > 0)
    {
      acdk::util::Arrays::sort(ea);
      RTreeItemId eid = appendItem(tid, "Enumerations");
      setItemImage(eid, EnumerationXpm);
      setItemImage(eid, EnumerationXpm, TreeitemiconSelected);
      for (i = 0; i < ea->length(); ++i)
      {
        REnumeration e = ea[i];
        RTreeItemId sid = appendItem(eid, e->getName());
        setItemDataObject(sid, &e);
        setItemHasChildren(sid, true);
        setItemImage(sid, EnumerationXpm);
        setItemImage(sid, EnumerationXpm, TreeitemiconSelected);
      }
    }
    RClassArray aca = unit->getClasses();
    RClassArray ia = new ClassArray(0);
    RClassArray ta = new ClassArray(0);
    RClassArray ca = new ClassArray(0);
    for (i = 0; i < aca->length(); ++i)
    {
      RClass c = aca[i];
      if (c->isInterface() == true)
        ia->append(c);
      else if (Throwable::GetClass()->isAssignableFrom(c) == true)
        ta->append(c);
      else
        ca->append(c);
    }
    if (ia->length() > 0)
    {
      acdk::util::Arrays::sort(ia);
      RTreeItemId eid = appendItem(tid, "Interfaces");
      setItemImage(eid, InterfaceXpm);
      setItemImage(eid, InterfaceXpm, TreeitemiconSelected);
      for (i = 0; i < ia->length(); ++i)
      {
        RClass c = ia[i];
        if (c->getClassName()->endsWith("_DmiProxy") == true)
          continue;
        RTreeItemId sid = appendItem(eid, c->getClassName());
        setItemDataObject(sid, &c);
        setItemHasChildren(sid, true);
        setItemImage(sid, InterfaceXpm);
        setItemImage(sid, InterfaceXpm, TreeitemiconSelected);
      }
    }
    if (ta->length() > 0)
    {
      acdk::util::Arrays::sort(ta);
      RTreeItemId eid = appendItem(tid, "Exceptions");
      for (i = 0; i < ta->length(); ++i)
      {
        RClass c = ta[i];
        if (c->getClassName()->endsWith("_DmiProxy") == true)
          continue;
        RTreeItemId sid = appendItem(eid, c->getClassName());
        setItemDataObject(sid, &c);
        setItemHasChildren(sid, true);
        setItemImage(sid, ClassXpm);
        setItemImage(sid, ClassXpm, TreeitemiconSelected);
      }
    }
    if (ca->length() > 0)
    {
      acdk::util::Arrays::sort(ca);
      RTreeItemId eid = appendItem(tid, "Classes");
      for (i = 0; i < ca->length(); ++i)
      {
        RClass c = ca[i];
        if (c->getClassName()->endsWith("_DmiProxy") == true)
          continue;
        RTreeItemId sid = appendItem(eid, c->getClassName());
        setItemDataObject(sid, &c);
        setItemHasChildren(sid, true);
        setItemImage(sid, ClassXpm);
        setItemImage(sid, ClassXpm, TreeitemiconSelected);
      }
    }
  }
  if (instanceof(o, Class) == true)
  {
    RClass cls = (RClass)o;
    RClassArray ca = cls->getInterfaces();
    int i;
    if (ca->length() > 0)
    {
      RTreeItemId eid = appendItem(tid, "Interfaces");
      setItemImage(eid, InterfaceXpm);
      setItemImage(eid, InterfaceXpm, TreeitemiconSelected);
      for (i = 0; i < ca->length(); ++i)
      {
        RClass c = ca[i];
        if (c->getClassName()->endsWith("_DmiProxy") == true)
          continue;
        RTreeItemId sid = appendItem(eid, c->getClassName());
        setItemDataObject(sid, &c);
        setItemHasChildren(sid, true);
        setItemImage(sid, InterfaceXpm);
        setItemImage(sid, InterfaceXpm, TreeitemiconSelected);
      }
    }
    RFieldArray fa = cls->getDeclaredFields();
    if (fa->length() > 0)
    {
      RTreeItemId eid = appendItem(tid, "Fields");
      setItemImage(eid, FieldXpm);
      setItemImage(eid, FieldXpm, TreeitemiconSelected);
      for (i = 0; i < fa->length(); ++i)
      {
        RField f = fa[i];
        RTreeItemId sid = appendItem(eid, f->toString());
        setItemDataObject(sid, &f);
        setItemHasChildren(sid, false);
        setItemImage(sid, FieldXpm);
        setItemImage(sid, FieldXpm, TreeitemiconSelected);
      }
    }
    RMethodArray ma = cls->getDeclaredMethods();
    if (ma->length() > 0)
    {
      
      RTreeItemId eid = appendItem(tid, "Methods");
      setItemImage(eid, MethodXpm);
      setItemImage(eid, MethodXpm, TreeitemiconSelected);
      for (i = 0; i < ma->length(); ++i)
      {
        RMethod m = ma[i];
        RTreeItemId sid = appendItem(eid, m->toString());
        setItemDataObject(sid, &m);
        setItemHasChildren(sid, false);
        setItemImage(sid, MethodXpm);
        setItemImage(sid, MethodXpm, TreeitemiconSelected);
      }
    }
  }
  else if (instanceof(o, Enumeration) == true)
  {
    REnumeration en = (REnumeration)o;
    REnumerationValueArray eva = en->getValues();
    for (int i = 0; i < eva->length(); ++i)
    {
      REnumerationValue ev = eva[i];
      RTreeItemId sid = appendItem(tid, ev->toString() + "=" + ev->getValue());
      setItemDataObject(sid, &ev);
      setItemHasChildren(sid, false);
    }
  }
}

RString 
ClassTreeCtrl::getObjectDescription(IN(RObject) obj)
{
  using namespace acdk::lang::dmi;
  if (instanceof(obj, MetaObject) == true)
  {
    RMetaObject cls = (RMetaObject)obj;
    return cls->toTypeString(TpFtJavaType | TpFtTypeDef| TpFtAttributes);
  }
  return "";
}

void 
ClassTreeCtrl::expandAll()
{
  _expandAll(getRootItem());
}

void 
ClassTreeCtrl::_expandAll(IN(RTreeItemId) id)
{
  RObject obj = getItemDataObject(id);
  if (obj != Nil && (instanceof(obj, Class) == true || instanceof(obj, Enumeration) == true))
    return;
  expand(id);
  jlong cookie;
  RTreeItemId tid = getFirstChild(id, cookie);
  while (tid != Nil && tid->isOk())
  {
    _expandAll(tid);
    tid = getNextChild(id, cookie);
  }
}

void 
ClassTreeCtrl::selectClass(IN(RClass) cls)
{
  RString name = cls->getName();
  RStringArray elements = acdk::util::StringTokenizer(name, "/").allToken();
  RTreeItemId id = getRootItem();
  for (int i = 0; i < elements->length(); ++i)
  {
    expand(id);
    RString el = elements[i];
    jlong cookie;
    RTreeItemId tid = getFirstChild(id, cookie);
    while (tid != Nil && tid->isOk())
    {
      if (elements->length() - 1 == i && getItemText(tid)->equals("Classes") == true)
      {
        tid = getFirstChild(tid, cookie);
        continue;
      }
      
      RObject o = getItemDataObject(tid);
      if (o == Nil)
      {
        tid = getNextChild(id, cookie);
        continue;
      }
      RString os = o->toString();

      if (instanceof(o, Unit) == true)
      {
        RString s = RUnit(o)->toTypeString(acdk::lang::dmi::TpFtLoadableClass);
        if (s->equals(el) == true)
        {
          id = tid;
          selectItem(id);
          break;
        }
      }
      if (instanceof(o, Class) == true)
      {
        //RString s = RClass(o)->toTypeString(acdk::lang::dmi::TpFtLoadableClass);
        //RString s = RClass(o)->getName(acdk::lang::dmi::TpFtName);
        RString s = RClass(o)->toTypeString(acdk::lang::dmi::TpFtTypeName);
        
        if (s->equals(el) == true)
        {
          id = tid;
          selectItem(id);
          return;
        }
      }
      //_expandAll(tid);
      tid = getNextChild(id, cookie);
    }

  }
}

} // inspector
} // ide
} // wx
} // acdk

