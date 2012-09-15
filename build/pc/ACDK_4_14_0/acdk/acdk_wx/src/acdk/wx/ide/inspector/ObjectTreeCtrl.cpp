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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ide/inspector/ObjectTreeCtrl.cpp,v 1.3 2005/04/18 20:40:36 kommer Exp $


#include "ObjectTreeCtrl.h"
#include <acdk/lang/reflect/Unit.h>
#include <acdk/lang/dmi/DmiDelegate.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>

#include <acdk/util/Arrays.h>

namespace acdk {
namespace wx {
namespace ide {
namespace inspector {

using namespace acdk::lang::reflect;

ACDK_DECL_CLASS(ObjectClassComparator);

class ObjectClassComparator
: extends acdk::lang::Object
{
public:
  ObjectClassComparator() {}
  int compare(IN(RObject) o1, IN(RObject) o2)
  {
    return o1->getClass()->getName()->compareTo(o2->getClass()->getName());
  }
};

ObjectTreeCtrl::ObjectTreeCtrl(IN(RWindow) parent, int id, IN(RPoint) point, IN(RSize) size)
: TreeCtrl(parent, id, point, size)
, _filterFlags(ObjectFilterRootsOnly)
{
  ObjectEventFunction oef = (ObjectEventFunction)&ObjectTreeCtrl::onListBoxExpanded;
  connect(TreeEvent::EvtCommandTreeItemExpanding, id, oef);
  connect(TreeEvent::EvtCommandTreeSelChanged, id, (ObjectEventFunction)&ObjectTreeCtrl::onSelChanged);
  fillRoot();
}

void 
ObjectTreeCtrl::reload(ObjectFilterFlags flags)
{
  if (flags != ObjectFilterCurrent)
    _filterFlags = flags;
  deleteAllItems();
  fillRoot();
}

void 
ObjectTreeCtrl::fillRoot()
{
  RTreeItemId rootId;
  RObjectArray oa;
  if (_filterFlags == ObjectFilterRootsOnly)
  {
    rootId = addRoot("Root Objects");
    oa = acdk::lang::System::getRootObjects();
  }
  else if (_filterFlags == ObjectFilterAllObjects)
  {
    rootId = addRoot("All Objects");
    oa = acdk::lang::System::getObjectList(acdk::lang::sys::ObjectMem);
  }
  else if (_filterFlags == ObjectFilterGcAble)
  {
    rootId = addRoot("GcAble Objects");
    oa = acdk::lang::System::getObjectList(acdk::lang::sys::ListGcAble | acdk::lang::sys::ObjectMem);
  }
  RObjectClassComparator comparator = new ObjectClassComparator();
  acdk::util::Arrays::sort(oa, comparator);
  RString lstr = "";
  RTreeItemId lt = Nil;
  for (int i = 0; i < oa->length(); ++i)
  {
    RObject o = oa[i];
    RString clsstr = o->getClass()->getName();
    if (lstr->equals(clsstr) == false)
    {
      lt = appendItem(rootId, clsstr);
      lstr = clsstr;
    }
    if (instanceof(o, String) == true)
    {
       RTreeItemId sid = appendItem(lt, o->toString());
       setItemHasChildren(sid, false);
    }
    else
    {
      RTreeItemId sid = appendItem(lt, clsstr);
      setItemDataObject(sid, &o);
      setItemHasChildren(sid, true);
    }
  }
  expand(rootId);
}

void 
ObjectTreeCtrl::onListBoxExpanded(IN(RTreeEvent) event)
{
  RTreeItemId tid = event->getItem();
  if (getChildrenCount(tid, false) != 0)
      return;
  RObject o = getItemDataObject(tid);
  _expand(tid, o);
}

void 
ObjectTreeCtrl::_expand(IN(RTreeItemId) tid, IN(RObject) o)
{
  int flags = 0;
  acdk::lang::dmi::SysFields sf = o->getInternalFields(flags);
  acdk::lang::dmi::SysFields::iterator it = sf.begin();
  acdk::lang::dmi::SysFields::iterator end = sf.end();
  for (; it != end; ++it)
  {
    dmi::ScriptVar sv = it->getScriptVar(0);
    if (it->fieldInfo == 0)
      continue;
    RString fname = it->fieldInfo->name;
    if (sv.isObjectType() == true && sv.isStringType() == false)
    {
      RObject o = sv.getObjectVar();
      if (o == Nil)
      {
        RTreeItemId sid = appendItem(tid, SBSTR(fname << "=Nil"));
        setItemHasChildren(sid, false);
      }
      else
      {
        RTreeItemId sid = appendItem(tid, fname);
        setItemDataObject(sid, &o);
        setItemHasChildren(sid, true);
      }
    }
    else
    {
      RTreeItemId sid = appendItem(tid, SBSTR(fname << "=" << sv.toCode()));
      setItemHasChildren(sid, false);
    }
  }
}

void 
ObjectTreeCtrl::setFilter(IN(RString) filter, int flags)
{
  _filter = filter;
  _filterFlags = flags;

}

void 
ObjectTreeCtrl::onSelChanged(IN(RTreeEvent) event)
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

RString 
ObjectTreeCtrl::getObjectDescription(IN(RObject) obj)
{
  StringBuffer sb;
  sb << "Object:\n"
    << "Address: 0x" << acdk::lang::Integer::toHexString((int)obj.impl()) << "\n"
     << "RefCount: " << obj->refCount() - 2 << "\n"
     << "Class: " << obj->getClass()->getName() << "\n";
  return sb.toString();
}

} // inspector
} // ide
} // wx
} // acdk

