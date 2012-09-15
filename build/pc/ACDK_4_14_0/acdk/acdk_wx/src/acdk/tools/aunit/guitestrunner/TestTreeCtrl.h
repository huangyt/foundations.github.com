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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/tools/aunit/guitestrunner/TestTreeCtrl.h,v 1.6 2005/03/11 11:11:50 kommer Exp $

#ifndef acdk_tools_aunit_guitestrunner_TestTreeCtrl_h
#define acdk_tools_aunit_guitestrunner_TestTreeCtrl_h

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

namespace acdk {
namespace tools {
namespace aunit {
namespace guitestrunner {

using namespace acdk::wx;
//using namespace acdk::tools::aunit;

#include "folder_untested.xpm"
#include "folder_fail.xpm"
#include "folder_ok.xpm"

enum TestState
{
  TestUnknown = 0,
  TestFailed = 1,
  TestOk = 2
};

ACDK_DECL_CLASS(TestTreeCtrl);

class TestTreeCtrl
: extends TreeCtrl
{
public:
  acdk::util::RTreeMap _testResultToTreeItemIdMap;
  acdk::cfgscript::RProps testCfg;
  TestTreeCtrl(IN(RWindow) parent, int id, IN(RPoint) pos, IN(RSize) size, IN(acdk::cfgscript::RProps) testcfg)
    : TreeCtrl(&parent, id, pos, size)
    , _testResultToTreeItemIdMap(new acdk::util::TreeMap())
    , testCfg(testcfg)
    //, _parent(parent)
  {
    RImageList imageList = new ImageList(16, 16);
    imageList->add(new Bitmap(folder_untested_xpm));
    imageList->add(new Bitmap(folder_fail_xpm));
    imageList->add(new Bitmap(folder_ok_xpm));
    assignImageList(imageList);

    connect(TreeEvent::EvtCommandTreeItemExpanding, id, (ObjectEventFunction)&TestTreeCtrl::onExpand);
    
    fillWithTests();
  }
  void fillWithTests()
  {
    //RString dir = acdk::lang::System::getAcdkHome() + "/bin"; //testCfg->testDir; //acdk->lang->System->getAcdkHome() + "/bin";
    RString dir = testCfg->getStringVal("testDir");
    //RString filter = "*_Test_d.exe"; //testCfg->testPattern;
    RString filter = testCfg->getStringVal("testPattern");
    RStringArray testexecs = (new acdk::io::File(dir))->list(new acdk::io::GlobFilenameFilter(filter));
    RTreeItemId root = addRoot("Test Executables");
    setItemImage(root, TestUnknown);
    setItemImage(root, TestUnknown, TreeitemiconSelected);
    int i;
    for (i = 0; i < testexecs->length(); ++i)
    {
      RString execn = testexecs[i];
      bool ignoreTest = false;
      RStringArray keys = testCfg->getProps("tests")->getKeys();
      for (int j = 0; j < keys->length(); ++j)
      {
        RString t = keys[j];
        if (execn->indexOf(t) != -1)
        {
          if (testCfg->getProps("tests")->getProps(t)->getBoolVal("ignore") == true)
            ignoreTest = true;
          break;
        }
      }
      if (ignoreTest == true)
        continue;
      RTreeItemId tid = appendItem(root, execn);
      //RTestResultEntry tre = new TestResultEntry(new ProcessTestSuite(dir + "/" + execn, Nil, ConfigTools->getTestEnv()));
      RTestResultEntry tre = new TestResultEntry(new ProcessTestSuite(dir + "/" + execn, Nil, (acdk::util::RProperties)testCfg->getObjectVal("testEnviron")));
      setItemDataObject(tid, &tre);
      _testResultToTreeItemIdMap->put(&tre, &tid);
      setItemHasChildren(tid, true);
      expand(root);
    }
  }
  void onSelChanged(IN(RTreeEvent) event)
  {
  }
  int setIconsToUnknown(IN(RTreeItemId) id)
  {
    setItemImage(id, TestUnknown);
    setItemImage(id, TestUnknown, TreeitemiconSelected);
    jlong cookie = 0;
    RTreeItemId c = getFirstChild(id, cookie);
    int count = 0;
    if (c->isOk() == false)
      count = 1;
    while (c->isOk() == true)
    {
      count = count + setIconsToUnknown(c);
      c = getNextChild(id, cookie);
    }
    return count;
  }
  void onExpand(IN(RTreeEvent) event)
  {
    RTreeItemId tid = event->getItem();
    if (getChildrenCount(tid, false) != 0)
      return;
    RTestResultEntry tre = (RTestResultEntry)getItemDataObject(tid);
    if (tre == Nil)
        messageBox("No Suit available");
    RProcessTestSuite psuit = (RProcessTestSuite)tre->test;
    RTestArray ta = psuit->tests();
    int i;
    for (i = 0; i < ta->length(); ++i)
    {
      RTest t = ta[i];
      RString tn = t->getName();
      int idx;
      if ((idx = tn->indexOf(";")) != -1)
        tn = tn->substr(idx + 1);
      RTreeItemId ctid = appendItem(tid, tn);
      RTestResultEntry tre = new TestResultEntry(t);
      setItemDataObject(ctid, &tre);
      _testResultToTreeItemIdMap->put(&tre, &ctid);
      //out->println("Test Class: " + t->GetClass()->toString());
      if (instanceof(t, ProcessTestSuite) == true)
          setItemHasChildren(ctid, true);
      setItemImage(ctid, getItemImage(tid));
      setItemImage(ctid, getItemImage(tid, TreeitemiconSelected), TreeitemiconSelected);
    }
  }
  void expandAllTests(IN(RTreeItemId) tid)
  {
    if (getChildrenCount(tid, false) != 0)
    {
      jlong cookie = 0;
      RTreeItemId c = getFirstChild(tid, cookie);
      while (c->isOk() == true)
      {
        expandAllTests(c);
        c = getNextChild(tid, cookie);
      }      
      return;
    }
    RTest test = RTestResultEntry(getItemDataObject(tid))->test;
    if (instanceof(test, ProcessTestSuite) == false)
      return;
    RProcessTestSuite psuit = (RProcessTestSuite)test;
    if (psuit == Nil)
      return;
    RTestArray ta = psuit->tests();
    RString tests = psuit->toString();
    int i;
    for (i = 0; i < ta->length(); ++i)
    {
      RTest t = ta[i];
      RString tn = t->getName();
      int idx;
      if ((idx = tn->indexOf(";")) != -1)
        tn = tn->substr(idx + 1);
      RTreeItemId ctid = appendItem(tid, tn);
      RTestResultEntry tre = new TestResultEntry(t);
      setItemDataObject(ctid, &tre);
      
      _testResultToTreeItemIdMap->put(&tre, &ctid);
      setItemImage(ctid, getItemImage(tid));
      setItemImage(ctid, getItemImage(tid, TreeitemiconSelected), TreeitemiconSelected);
      if (instanceof(t, ProcessTestSuite) == true)
      {
        setItemHasChildren(ctid, true);
        expandAllTests(ctid);
      }
    }
  }
  void setTestOk(RTreeItemId p)
  {
    //out.println("SetOK: " + _treectrl.getItemText(p));
    setItemImage(p, TestOk);
    setItemImage(p, TestOk, TreeitemiconSelected);
    setTestOkInChilds(p);
    while (p->isOk() == true)
    {
      p = getItemParent(p);
      if (p->isOk() == false)
        break;
      if (getItemImage(p) == TestUnknown)
      {
        setItemImage(p, TestOk);
        setItemImage(p, TestOk, TreeitemiconSelected);
      }
    }
  }
  void setTestOkInChilds(IN(RTreeItemId) p)
  {
    jlong cookie = 0;
    RTreeItemId c = getFirstChild(p, cookie);
    
    while (c->isOk() == true)
    {
      setItemImage(c, TestOk);
      setItemImage(c, TestOk, TreeitemiconSelected);
      setTestOkInChilds(c);
      c = getNextChild(p, cookie);
    }
  }
  void setTestFailed(RTreeItemId p)
  {
    setItemImage(p, TestFailed);
    setItemImage(p, TestFailed, TreeitemiconSelected);
    
    while (p->isOk() == true)
    {
      p = getItemParent(p);
      if (p->isOk() == false)
        break;
      setItemImage(p, TestFailed);
      setItemImage(p, TestFailed, TreeitemiconSelected);
    }
  }
};

} // namespace guitestrunner 
} // namespace aunit
} // namespace tools 
} // namespace acdk 


#endif //acdk_tools_aunit_guitestrunner_TestTreeCtrl_h


