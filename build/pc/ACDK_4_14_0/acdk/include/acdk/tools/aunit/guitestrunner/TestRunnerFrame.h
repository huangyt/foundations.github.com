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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/tools/aunit/guitestrunner/TestRunnerFrame.h,v 1.13 2005/04/10 12:52:40 kommer Exp $

#ifndef acdk_tools_aunit_guitestrunner_TestRunnerFrame_h
#define acdk_tools_aunit_guitestrunner_TestRunnerFrame_h

#include "TestTreeCtrl.h"
#include "GuiTestListener.h"

#include <acdk/io/TeeCharWriter.h>
#include <acdk/cfgscript/Script.h>
#include <acdk/wx/TextCtrlCharWriter.h>
#include <acdk/wx/Gauge.h>
#include <acdk/wx/Timer.h>
#include <acdk/wx/Frame.h>
#include <acdk/wx/TextCtrl.h>
#include <acdk/wx/StaticText.h>
#include <acdk/wx/Button.h>
#include <acdk/wx/ToolBar.h>
#include <acdk/wx/Notebook.h>
#include <acdk/wx/HtmlWindow.h>
#include <acdk/wx/SplitterWindow.h>

namespace acdk {
namespace tools {
namespace aunit {
/**
  A GUI application for running ACDK Unit Tests
*/
namespace guitestrunner {

//using namespace acdk::wx;
//using namespace acdk::tools::aunit;

#include "help.xpm"
#include "run.xpm"

enum Ids
{
  ClearLogId = 1001,
  MenuQuitId,
  MenuAboutId,
  TbHelpId,
  TestTreeId,
  TestRepId,
  TestLogId,
  RunTestId,
  CurrentTestId,
  StResultId,
  RunTimerId,
  RunBtnId,
  TbRunId
};

ACDK_DECL_CLASS(BufferedCharWriter);
class BufferedCharWriter
: extends acdk::lang::Object
, implements acdk::io::CharWriter
{
public:
  StringBuffer _buffer;
  BufferedCharWriter() 
  {
  }
  void writeChar(char c) 
  {
    SYNCTHIS();
    _buffer.append(c);
    
  }
  void writeChar(ucchar c)
  {
    SYNCTHIS();
    _buffer.append(c);
  }
  void writeString(IN(RString) str)
  {
    SYNCTHIS();
    _buffer.append(str);
  }
  RString fetchBuffer()
  {
    SYNCTHIS();
    RString ret = _buffer.toString();
    _buffer.set("");
    return ret;
  }
  void flush() {}
  void close() {}
};

class TestRunnerFrame
: extends Frame
{
  RTestTreeCtrl _treectrl;
  RTextCtrl _console;
  RMenu _treeMenu;
  RTimer _testOutputTimer;
  RBufferedCharWriter _consoleWriter;
  RTestRunThread _testRunThread;
  RStaticText _stSelectedTest;
  RStaticText _stResult;
  RButton _runButton;
  RTestResult _testResult;
  RGuiTestListener _guiTestListener;
  //RHtmlWindow _htmlLogWindow;
  RTreeItemId _runnedTreeItemId;
  RGauge _progress;
  bool _runNextSibling;// @todo = false doesn't working!
  int _curErrsAndFailures;
  RTestResultEntryArray _allTestResults;
  bool _stopTests;
  bool _abortTest;
  RTextCtrl _testRep;
  RTextCtrl _testLog;
  acdk::cfgscript::RProps testCfg;
  acdk::util::RProperties _testEnviron;
  RTextCtrl _cfgTextCtrl;
  RNotebook  _notebook;
public:
  TestRunnerFrame(IN(RString) title) 
  : Frame(Nil, -1, title, new Point(100, 100), new Size(800, 600))
  {
    _testEnviron = (acdk::util::RProperties)System::getEnvironment()->clone();
    testCfg = new acdk::cfgscript::Props();
    loadCfg();
    _runNextSibling = false;
    _stopTests = false;
    _abortTest = false;
    _curErrsAndFailures = 0;
    RMenuBar menuBar = new MenuBar();
    
    RMenu menuFile = new Menu();
    menuFile->append(ClearLogId, "Clear Log", "Clear Log Window"); 
    menuFile->appendSeparator();
    menuFile->append(MenuQuitId, "E&xit\tAlt-X", "Quit this program");
    menuBar->append(menuFile, "&File");
    RMenu helpMenu = new Menu();
    helpMenu->append(MenuAboutId, "&About...\tCtrl-A", "Show about dialog");
    menuBar->append(helpMenu, "&Help");
    setMenuBar(menuBar);
    connect(CommandEvent::EvtCommandMenuSelected, MenuQuitId, (ObjectEventFunction)&TestRunnerFrame::onMenuQuit);
    connect(CommandEvent::EvtCommandMenuSelected, MenuAboutId, (ObjectEventFunction)&TestRunnerFrame::onMenuAbout);
    connect(CommandEvent::EvtCommandMenuSelected, ClearLogId,  (ObjectEventFunction)&TestRunnerFrame::onMenuClear);
    connect(CommandEvent::EvtCommandMenuSelected, RunTestId, (ObjectEventFunction)&TestRunnerFrame::onTestRun);

    RToolBar tb = createToolBar();
    tb->setWindowStyle(TbText | Tb3dbuttons);
    RBitmap helpbm = new Bitmap(help_xpm);
    RToolBarToolBase tbb = tb->addTool(TbHelpId, "Help", helpbm, "Display short help");
    RBitmap runpbm = new Bitmap(run_xpm);
    tbb = tb->addTool(TbRunId, "Run", runpbm, "Run selected Test");
    
    connect(CommandEvent::EvtCommandMenuSelected, TbHelpId, (ObjectEventFunction)&TestRunnerFrame::onMenuAbout);
    connect(CommandEvent::EvtCommandMenuSelected, TbRunId, (ObjectEventFunction)&TestRunnerFrame::onTestRun);
    tb->realize();
    RSplitterWindow splitter = new SplitterWindow(this);
      
    _treectrl = new TestTreeCtrl(&splitter, TestTreeId, new Point(0, 0), new Size(200, 200), testCfg);
    
    _notebook = new Notebook(&splitter, -1);
    RSplitterWindow rsplt = new SplitterWindow(&_notebook);
    _notebook->addPage(&rsplt,  "Execute");
    
    RSplitterWindow testlogsplitter = new SplitterWindow(&_notebook);
    
    _testRep = new TextCtrl(&testlogsplitter, TestRepId, "", Point::defaultPosition(), Size::defaultSize(), 
                            TeMultiline | TeAutoScroll | TeDontwrap);
    
    _testLog = new TextCtrl(&testlogsplitter, TestLogId, "", Point::defaultPosition(), Size::defaultSize(), 
                            TeMultiline | TeAutoScroll | TeDontwrap);
    
    _notebook->addPage(&testlogsplitter,  "Test Logs");
    
    testlogsplitter->splitHorizontally(&_testRep, &_testLog, 130);
    
    //_htmlLogWindow = new HtmlWindow(&_notebook, -1);
    //_notebook->addPage(&_htmlLogWindow, "HTML Log");
    
    
    
    
    RPanel testResultPanel = new Panel(&rsplt, 0, 0, 400, 100);
    
    _console = new TextCtrl(&rsplt, -1, "", Point::defaultPosition(), Size::defaultSize(), 
                            TeMultiline | TeAutoScroll | TeDontwrap | TeRich2);
            
    _consoleWriter = new BufferedCharWriter();
    splitter->splitVertically(&_treectrl, &_notebook, 300);
    rsplt->splitHorizontally(&testResultPanel, &_console, 130);
    _runButton = new Button(&testResultPanel, RunBtnId, "Run Test", new Point(10, 10), new Size(100, 40));
   connect(CommandEvent::EvtCommandButtonClicked, RunBtnId, (ObjectEventFunction)&TestRunnerFrame::onTestRun);
    
    _progress = new Gauge(&testResultPanel, -1, 100, new Point(10,60), new Size(400, 40), GaSmooth | BorderNone );
    
    _stSelectedTest = new StaticText(&testResultPanel, CurrentTestId, "<selected Test>", new Point(120, 25));
    _stResult = new StaticText(&testResultPanel, StResultId, "", new Point(10, 100));
    _treeMenu = new Menu();
    _treeMenu->append(RunTestId, "Run Test");
    
    _testOutputTimer = new Timer(this, RunTimerId);
    connect(TimerEvent::EvtTimer, RunTimerId, (ObjectEventFunction)&TestRunnerFrame::onReadTestBuffer);

    createStatusBar(2);
    _stResult->setBackgroundColour(new Colour(0, 0, 255));
    _stResult->setLabel("asdfasdf");
    
    connect(TreeEvent::EvtCommandTreeSelChanged, TestTreeId, (ObjectEventFunction)&TestRunnerFrame::onTestSelChanged);
    connect(TreeEvent::EvtCommandTreeItemActivated, TestTreeId, (ObjectEventFunction)&TestRunnerFrame::onItemActivated);
    _cfgTextCtrl = new TextCtrl(&_notebook, -1, "", Point::defaultPosition(), Size::defaultSize(), 
                            TeMultiline | TeAutoScroll | TeDontwrap);
                            
    acdk::io::File f(getCfgFile());
    if (f.exists() == true)
    {
      _cfgTextCtrl->setValue(f.getReader()->getCharReader()->readString());
    }
    
    _notebook->addPage(&_cfgTextCtrl, "Configuration");
    
  }
  
  void onMenuQuit(IN(REvent) event)
  {
    close(true);
  }
  void onMenuAbout(IN(REvent) event)
  {
    Window::messageBox("ACDK Unit Testrunner written with acdk_wx\n By Roger Rene Kommer (http://acdk->sourceforge.net)");
  }
  void onMenuClear(IN(REvent) event)
  {
    _console->clear(); 
  }
  void onTestRun(IN(REvent) event)
  {
    
    if (_runButton->getLabel()->equals("Stop") == true)
    {
      _stopTests = true;  
      _runButton->setLabel("Abort");
      _guiTestListener->doStop();
      return;
    }
    if (_runButton->getLabel()->equals("Abort") == true)
    {
      _abortTest = true;
      return;
    }
  
    RTreeItemId tid = _treectrl->getSelection();
    setCursor(Cursor::getHourglassCursor());
    setStatusText("Prepare Test Execution", 0);    
    _runButton->enable(false);
    
    _treectrl->expandAllTests(tid);
    /*
    int allTestCount = setIconsToUnknown(tid);
    _progress.setRange(allTestCount);
    _progress.setValue(0);
    */
    _runButton->enable(true);
    setCursor(Cursor::getNullCursor());
    _curErrsAndFailures = 0;
    _stopTests = false;
    _abortTest = false;
    _allTestResults = new TestResultEntryArray(0);
    RTestResultEntry tre = (RTestResultEntry)_treectrl->getItemDataObject(tid);
    if (tre == Nil)
    {
      if (_treectrl->getItemText(tid)->equals("Test Executables") == true)
      {
        jlong cookie = 0;
        tid = _treectrl->getFirstChild(tid, cookie);
        if (tid->isOk() == false)
          return;
        _runNextSibling = true;
        tre = (RTestResultEntry)_treectrl->getItemDataObject(tid);
      }
    }
    if (tre == Nil)
    {
      messageBox("No Test defined");
      return;
    }
    _runButton->setLabel("Stop");
    runTest(tre->test, tid);
  }
  void runTest(IN(RTest) t, IN(RTreeItemId) tid, bool nextRun = false)
  {
    _runnedTreeItemId = tid;
    acdk::io::RStringWriter sout = new acdk::io::StringWriter();
    acdk::io::RStringWriter serr = new acdk::io::StringWriter();
    if (instanceof(t, ProcessTestCase) == true)
    {
      RProcessTestCase ptc(t);
      ptc->setOutWriter(new acdk::io::TeeCharWriter(&_consoleWriter, &sout));
      ptc->setErrWriter(new acdk::io::TeeCharWriter(&_consoleWriter, &serr));
    }
    else if (instanceof(t, ProcessTestSuite) == true)
    {
      RProcessTestSuite pts(t);
      pts->setOutWriter(new acdk::io::TeeCharWriter(&_consoleWriter, &sout));
      pts->setErrWriter(new acdk::io::TeeCharWriter(&_consoleWriter, &serr));
    }

    _testResult = new TestResult(sout, serr);
    _guiTestListener = new GuiTestListener();
    
    int allTestCount = _treectrl->setIconsToUnknown(tid);
    _progress->setRange(allTestCount);
    _progress->setValue(0);
    
    _testResult->addTestListener(&_guiTestListener);
    _testRunThread = new TestRunThread(t, _testResult);
    _testRunThread->start();
    _testOutputTimer->start(500, false);
    //_runButton.enable(false);
    Thread::sleep(200);
    onReadTestBuffer(new TimerEvent());
  }
  void onTestSelChanged(IN(RTreeEvent) event)
  {
    RTreeItemId id = _treectrl->getSelection();
    RString t = _treectrl->getItemText(id);
    _stSelectedTest->setLabel(t);
    setStatusText(t, 1);
    RTestResultEntry tre = (RTestResultEntry)_treectrl->getItemDataObject(id);
    if (tre == Nil)
    {
      _testLog->setValue("");
      _testRep->setValue("not runned");
      return;
    }
    StringBuffer sb;
    sb << tre->toString() << ":\n";
    if (tre->errors != Nil && tre->errors->length() > 0)
    {
      sb << tre->errors->length() << " ERRORS:\n";
      for (int i = 0; i < tre->errors->length(); ++i)
      {
        RThrowable ex = tre->errors[i];
        if (ex != Nil)
          sb << ex->getMessage() << "\n";
      }
    }
    else
      sb << "0 Errors\n";
    if (tre->failures != Nil && tre->failures->length() > 0)
    {
      sb << tre->failures->length() << " ERRORS:\n";
      for (int i = 0; i < tre->failures->length(); ++i)
      {
        RThrowable ex = tre->failures[i];
        if (ex != Nil)
          sb << ex->getMessage() << "\n";
      }
    }
    else
      sb << "0 Failures\n";
    _testRep->setValue(sb.toString());
    _testLog->setValue("STDOUT:\n" + tre->output + "\n==========================================\nSTDERR:\n" + tre->errput + "\n");
    RTest test = tre->test;
    RString tn = test->getName();
    if (tn->endsWith("_Test") == true || tn->endsWith("_Test.exe") == true  || tn->endsWith("_Test_d.exe") == true || tn->endsWith("_Test_r.exe") == true)
    {
      RString fn;
      if (t->endsWith(".exe") == true)
        fn = System::getAcdkHome() + "/testreports/" + t->substr(0, t->length()  - 4) + ".html";
      else
        fn = System::getAcdkHome() + "/testreports/" + t + ".html";
      /*if ((new acdk::io::File(fn))->exists() == true)
        _htmlLogWindow->loadFile(fn);
      else
        setStatusText("File doesn't exists: " + fn, 0);
      */
    }
    
  }
  void onItemActivated(IN(RTreeEvent) event)
  {
    //messageBox("Item Activated");
    _notebook->setSelection(1);
  }

  void onReadTestBuffer(IN(REvent) event)
  {
    RString s = _consoleWriter->fetchBuffer();
    //out.println("Runned: " + _guiTestListener.runnedTest);
    _progress->setValue(_guiTestListener->runnedTest);
    if (s->length() > 0)
    {
      _console->appendText(s);
    }
    if (_testRunThread->isAlive() == false)
    {
      _testOutputTimer->stop();
      _testRunThread = Nil;
      if (_stopTests == true)
        _runNextSibling = false;
        
      if (_runNextSibling == true)
      {
        RTreeItemId nid = _treectrl->getNextSibling(_runnedTreeItemId);
        if (nid->isOk() == true)
        {
          RTestResultEntry tre = (RTestResultEntry)_treectrl->getItemDataObject(nid);
          if (tre != Nil)
          {
            writeTestResult();
            runTest(tre->test, nid, true);
            return;
          }
        }
        else
        {
          _runNextSibling = false;
        }
      }
      _runButton->setLabel("Run");
      _console->appendText("Test Finished\r\n");
      _runButton->enable(true);
      setStatusText("Postprocess tests results");
      setCursor(Cursor::getHourglassCursor());
      writeTestResult();
      setCursor(Cursor::getNullCursor());
    }
    else
    {
      setStatusText(_guiTestListener->getLastMessage(), 0);
    }
  }
  void setTestResult(IN(RTreeItemId) id, IN(RTestResultEntry) tre, bool derived = false)
  {
    // set parents on fail and icon
    _treectrl->setItemDataObject(id, &tre);
    if ((tre->errors->length() > 0) || (tre->failures->length() > 0))
    {
      _treectrl->setTestFailed(id);
    }
    else
    {
      _treectrl->setTestOk(id);
      /*
      if (derived == false || _treectrl.getItemImage(id) == TestUnknown)
      {
        _treectrl.setItemImage(id, TestOk);
        _treectrl.setItemImage(id, TestOk, TreeitemiconSelected);
      }
      */
    }
  }
  void writeTestResult()
  {
    RTestResultEntryArray results = _testResult->getResults();
    StringBuffer sb;
    sb << "Runned Test: " << results->length() << "\n";
    int allErrors = 0;
    int allFailures = 0;
    for (int i = 0; i < results->length(); ++i)
    {
      RTestResultEntry tre = results[i];
      allErrors = allErrors + tre->errors->length();
      allFailures = allFailures + tre->failures->length();
      //if (findSetTestResult(_treectrl.getRootItem(), tre) == false)
      if (findSetTestResult(_runnedTreeItemId, &tre) == false)
      {
        //out.println("Test was not found: " + tre.test.toString());
      }
    }
    if ((allErrors + allFailures) != 0)
    {
      sb << "      TEST FAILED\n"
         << "Errors: " << allErrors << "\n"
         << "Failures: " <<  allFailures << "\n";
     _stResult->setBackgroundColour(new Colour(255, 0, 0));
     //setTestFailed();
    }
    else
    {
      sb << "        Test ok\n";
      _stResult->setBackgroundColour(new Colour(0, 255, 0));
      _treectrl->setTestOk(_runnedTreeItemId);
    }
    //out.println(sb.toString());
    _stResult->setLabel(sb.toString());
    
  }
  bool findSetTestResult(IN(RTreeItemId) id, IN(RTestResultEntry) tre)
  {
    RTreeItemId fid = (RTreeItemId)_treectrl->_testResultToTreeItemIdMap->get(&tre);
    if (fid != Nil)
    {
      setTestResult(fid, tre);
      return true;
    }
    //out.println("TEST not found: " + tre.test.toString() + " in:\n" + testResultToTreeItemIdMap.toString());
    RTestResultEntry itre = (RTestResultEntry)_treectrl->getItemDataObject(id);
    if (itre != Nil)
    {
      //out.println(itre.test.toString() + " == " + tre.test.toString());
      if (tre->test->getName()->equals(itre->test->getName()) == true)
      {
        setTestResult(id, tre);
        return true;
      }  
    }

    jlong cookie = 0;
    RTreeItemId c = _treectrl->getFirstChild(id, cookie);
    while (c->isOk() == true)
    {
      if (findSetTestResult(c, tre) == true)
      {
        setTestResult(id, tre, true);
        return true;
      }
      c = _treectrl->getNextChild(id, cookie);
    }
    return false;
  }
  RString getCfgFile()
  {
    return SBSTR(System::getAcdkToolsHome() << acdk::io::File::separator() << "cfg" 
                      << acdk::io::File::separator() << "guitestrunner.csf");
  }
  bool loadCfg()
  {
    RString cfgfile;
    try {
      cfgfile = getCfgFile();
      acdk::io::File f(cfgfile);
      if (f.exists() == false)
      {
        messageBox("Configuration file missing: " + cfgfile);
        return false;
      }
      acdk::cfgscript::RScript script = new acdk::cfgscript::Script(cfgfile);
      acdk::cfgscript::RProps globals = new acdk::cfgscript::Props();
      script->initAsEnvProps(globals);
      globals->setProps("testCfg", testCfg);
      script->readEval(globals, acdk::cfgscript::PropsParentRead | acdk::cfgscript::PropsParentWrite);
      initCfg();
    } catch (RThrowable ex) {
      messageBox("Error while reading configuration file " + cfgfile + ":\n" + ex->getMessage());
    }
    return true;
  }
  void initCfg()
  {
    RStringArray pathlist = testCfg->getStringArrayVal("path", Nil);
    if (pathlist != Nil)
    {
      RString path = _testEnviron->getProperty("PATH");
      if (path == Nil)
        path = _testEnviron->getProperty("Path");
      StringBuffer np(path);
      
      for (int i = 0; i < pathlist->length(); ++i)
      {
        RString pe = pathlist[i];
        //out.println(pe);
        np << acdk::io::File::pathSeparator() << pe;
      }
      _testEnviron->setProperty("PATH", np.toString());
      _testEnviron->setProperty("Path", np.toString());
      //out.println("New Path: " + np.toString());
    }
    acdk::cfgscript::RProps envprops = testCfg->getProps("environ", Nil);
    if (envprops != Nil)
    {
      RStringArray keys = envprops->getKeys();
      for (int i = 0; i < keys->length(); ++i)
      {
        RString k = keys[i];
        _testEnviron->setProperty(k, envprops->getStringVal(k));
      }
    }
    testCfg->setObjectVal("testEnviron", &_testEnviron);
  }
};

} // namespace guitestrunner 
} // namespace aunit
} // namespace tools 
} // namespace acdk 


#endif //acdk_tools_aunit_guitestrunner_TestRunnerFrame_h
