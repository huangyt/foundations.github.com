
#include "Main.h"

#include "CsfDebugger.h"
#include <acdk/wx/ide/TextOutputCtrl.h>

namespace acdk {
namespace tools {
namespace csfide {

using namespace acdk::cfgscript;
using namespace acdk::wx;
using namespace acdk::wx::ide;

#define InternalId 1
#define InternalEventTypeId CommandEvent::EvtCommandButtonClicked
  
struct WrappedInObject
: extends wxObject
{
  RObject wrapped;
  WrappedInObject(IN(RObject) obj) 
  : wrapped(obj)
  {
  }
};


IdeScriptDebugger::IdeScriptDebugger()
: _state(Sleeping)
, _nextState(DoContinue)
, _guiThread(Thread::currentThread()->threadID())
{
  connect(InternalEventTypeId, InternalId, (ObjectEventFunction)&IdeScriptDebugger::onCommand);
}

void 
IdeScriptDebugger::uiIsReady()
{
  DebugBreakPoints::get()->setDebugger(this);
}

//static 
RIdeScriptDebugger 
IdeScriptDebugger::get()
{
  return MainFrame::get()->_debugger;
}

bool 
IdeScriptDebugger::doBreak(int action, acdk::cfgscript::PEStack& stack)
{
  if (_nextState == DoBreak || _nextState == DoTerminate)
    return true;
  return DebugBreakPoints::checkBreakPoints(action, stack);
}

void 
IdeScriptDebugger::_branchToGui(DebuggerNextState cmd, bool doWait)
{
  RCommandEvent evt = new CommandEvent();
  evt->setEventType(InternalEventTypeId);
  evt->setInt(cmd);
  evt->setId(InternalId);
  if (cmd == DoBreak)
    evt->getWx()->SetEventObject(new WrappedInObject(&ExecutionStack::get()));
  IdeScriptDebugger::get()->addPendingEvent(&evt);
  if (doWait == false)
    return;
  SYNCHRONIZETHIS();
  wait();
  System::out->println("here again in Script");
}



DebugNextAction 
IdeScriptDebugger::onBreak(int action, PEStack& stack, IN(RExecutionStackFrame) frame)
{
  RDebugBreakPoints dbg = DebugBreakPoints::get();
  RIdeScriptDebugger ideDbg = IdeScriptDebugger::get();
  if (_nextState == DoTerminate)
  {
    IdeScriptDebugger::get()->_branchToGui(DoTerminate, false);
    return DbgTerminate;
  }

  _branchToGui(DoBreak);

  switch(_nextState)
  {
  case DoContinue:
    ideDbg->_branchToGui(DoContinue, false);
    return DbgNAContinue;
  case DoNext:
    ideDbg->_branchToGui(DoContinue, false);
    return DbgNAStepOver;
  case DoStep:
    ideDbg->_branchToGui(DoContinue, false);
    return DbgNAStepInto;
  case DoReturn:
    ideDbg->_branchToGui(DoContinue, false);
    return DbgNAUntilReturn;
  default:
    return DbgNAContinue;
  }
  return DbgNAContinue;
}



class ScriptRunThread
: extends acdk::lang::Thread
{
public:
  RString _fileName;
  ScriptRunThread(IN(RString) fileName)
    : _fileName(fileName)
  {
  }
  void run()
  {
    RTextOutputCtrl outPut = (RTextOutputCtrl)Window::findWindowById(ID_TEXTCTRL_OUTPUT);
    acdk::io::RPrintWriter out = new acdk::io::PrintWriter(&outPut);
      
    RScript script;
    try {
      script = new Script(_fileName);
      RProps props = new Props();
      acdk::lang::dmi::RDmiObject dmiOut = new acdk::lang::dmi::DmiObject(inOf(out));
      props->set("out", dmiOut);
      props->set("err", dmiOut);
      int ret = script->readEval(props, ScriptRunIsolated | PropsParentRead | PropsNoParentWrite);
      out->println(SBSTR("Script exit with: " << ret));
    }
    catch (acdk::io::RThrowable ex)
    {
      out->println("unhandled exception:\n" + ex->getMessage() + "\nIn:");
      out->println(Script::getScriptBackTrace());
    }
    Script::getExecutionStack()->rollbackMetaInfo();
    IdeScriptDebugger::get()->_branchToGui(DoTerminate, false);
  }

};

void
IdeScriptDebugger::_startScript(IN(RMainFrame) frame, bool debug)
{
  if (_state == Breaked)
  {
    onContinue(frame);
    return;
  }
  RString fileName = "";
  REditor editor = frame->getActiveEditWindow();
  if (editor == Nil)
  {
    frame->messageBox("no script to run");
    return;
  }
  fileName = editor->getFileName();
  RThread threadRun = new ScriptRunThread(fileName);
  if (debug == true)
  {
    RIdeScriptDebugger ideDbg = IdeScriptDebugger::get();
    _nextState = DoBreak;
  }
  threadRun->start();
  MainFrame::get()->_debugger->setNewState(Running);
}

void 
IdeScriptDebugger::onRun(IN(RMainFrame) frame)
{
  _startScript(frame, false);
}

void 
IdeScriptDebugger::onDebug(IN(RMainFrame) frame)
{
  _startScript(frame, true);
}



void 
IdeScriptDebugger::onCommand(IN(acdk::wx::RCommandEvent) event)
{
  //MainFrame::get()->messageBox("Event received");
  DebuggerNextState cmd = (DebuggerNextState)event->getInt();
  switch (cmd)
  {
  case DoBreak:
  {
    setNewState(Breaked);
    WrappedInObject* wrapp = (WrappedInObject*)event->getWx()->GetEventObject();
    currentStack = (RExecutionStack)wrapp->wrapped;
    delete wrapp;

    OUT(RExecutionStackFrame) sf = currentStack->top();

    RString fname = sf->_script->getFileName();
    int lineNo = sf->getSourceLine();
    MainFrame::get()->openOrActiveFileAtLine(fname, lineNo - 1);
    MainFrame::get()->backTraceCtrl->updateBacktrace(currentStack);
    MainFrame::get()->localVarsCtrl->updateStackFrame(currentStack->getFrameFromTop(0));
    break;
  }
  case DoTerminate:
    setNewState(Sleeping);
    _nextState = DoContinue;
    break;
  case DoContinue:
    setNewState(RunningDebug);
    break;
  default:
    MainFrame::get()->messageBox(RString("unknown cmd in IdeScriptDebugger::onCommand: ") + cmd);
    break;
  }
}



void 
IdeScriptDebugger::onContinue(IN(RMainFrame) frame)
{
  if (_state != Breaked)
    return;

  SYNCHRONIZETHIS();
  _nextState = DoContinue;
  setNewState(RunningDebug);
  notify();
}

void 
IdeScriptDebugger::onStep(IN(RMainFrame) frame)
{
  if (_state != Breaked)
    return;
  SYNCHRONIZETHIS();
  _nextState = DoStep;
  notify();
}

void 
IdeScriptDebugger::onNext(IN(RMainFrame) frame)
{
  if (_state != Breaked)
    return;
  SYNCHRONIZETHIS();
  _nextState = DoNext;
  notify();
}

void 
IdeScriptDebugger::onTerminate(IN(RMainFrame) frame)
{
  _nextState = DoTerminate;
  if (_state == Breaked)
  {
    SYNCHRONIZETHIS();
    notify();
  }
}

void 
IdeScriptDebugger::onBreak(IN(RMainFrame) frame)
{
  _nextState = DoBreak;
}

void 
IdeScriptDebugger::onReturn(IN(RMainFrame) frame)
{
   if (_state != Breaked)
    return;
  SYNCHRONIZETHIS();
  _nextState = DoReturn;
  notify();
}

void 
IdeScriptDebugger::setNewState(DebuggerState newState)
{
  if (newState == _state)
    return;
  _state = newState;
  updateUI();
}

void 
IdeScriptDebugger::reset()
{
  RMainFrame mf = MainFrame::get();
  mf->backTraceCtrl->reset();
  mf->localVarsCtrl->reset();
  currentStack = Nil;
}

void 
IdeScriptDebugger::gotoFrame(int idx)
{
  if (currentStack == Nil)
    return;

  RExecutionStackFrame fr = currentStack->getFrameFromTop(idx);
  if (fr == Nil)
    return;
  RMainFrame mf = MainFrame::get();
  mf->localVarsCtrl->updateStackFrame(fr);
}


void 
IdeScriptDebugger::updateUI()
{
  RMainFrame mf = MainFrame::get();
  if (mf == Nil)
    return;
  mf->debugMenu->enable(ID_MENU_DEBUG_RUN, _state == Sleeping || _state == Breaked);
  mf->debugBar->enableTool(ID_MENU_DEBUG_RUN, _state == Sleeping || _state == Breaked);

  mf->debugMenu->enable(ID_MENU_DEBUG_DEBUG, _state == Sleeping);
  mf->debugBar->enableTool(ID_MENU_DEBUG_DEBUG, _state == Sleeping);
  mf->debugMenu->enable(ID_MENU_DEBUG_TERM, _state != Sleeping);
  mf->debugBar->enableTool(ID_MENU_DEBUG_TERM, _state != Sleeping);
  mf->debugMenu->enable(ID_MENU_DEBUG_STEP, _state == Breaked);
  mf->debugBar->enableTool(ID_MENU_DEBUG_STEP, _state == Breaked);
  mf->debugMenu->enable(ID_MENU_DEBUG_NEXT, _state == Breaked);
  mf->debugBar->enableTool(ID_MENU_DEBUG_NEXT, _state == Breaked);
  
  mf->debugMenu->enable(ID_MENU_DEBUG_RETURN, _state == Breaked);
  mf->debugBar->enableTool(ID_MENU_DEBUG_RETURN, _state == Breaked);

  mf->debugMenu->enable(ID_MENU_DEBUG_BREAK, _state == Running || _state == RunningDebug);
  mf->debugBar->enableTool(ID_MENU_DEBUG_BREAK, _state == Running || _state == RunningDebug);
  mf->showDebugCtrls(_state != Sleeping);
  if (_state == Sleeping)
  {
    reset();
  }
  else
  {
    mf->backTraceCtrl->enable(_state == Breaked);
    mf->localVarsCtrl->enable(_state == Breaked);
  }
  
}

void 
IdeScriptDebugger::insertBreakPoint(IN(RString) fname, int line)
{
  DebugBreakPoints::get()->addBreakPoint(new SourceLineDebugPoint(fname, line + 1));
  
}

void 
IdeScriptDebugger::removeBreakPoint(IN(RString) fname, int line)
{
  DebugBreakPoints::get()->removeBreakpoint(SBSTR(fname << ":" << (line + 1)));
}

void 
BackTraceCtrl::_init()
{
  connect(CommandEvent::EvtCommandListboxDoubleclicked, -1, (ObjectEventFunction)&BackTraceCtrl::onSelFrame);
}

void 
BackTraceCtrl::onSelFrame(IN(acdk::wx::RCommandEvent) event)
{
  int idx = getSelection();
  IdeScriptDebugger::get()->gotoFrame(idx);
}


void 
BackTraceCtrl::updateBacktrace(IN(acdk::cfgscript::RExecutionStack) stack)
{
  if (_lastFrame == stack->getFrameFromTop(0))
    return;
  clear();
  RExecutionStackFrame frame;
  for (int i = 0; (frame = stack->getFrameFromTop(i)) != Nil; ++i)
  {
    RString bstr = frame->getScriptBackTrace(false, false);
    append(bstr);
  }
}

void 
BackTraceCtrl::reset()
{
  clear();
  _lastFrame = Nil;
}


LocalVarsCtrl::LocalVarsCtrl(IN(acdk::wx::RWindow) parent, int id)
: TreeCtrl(parent, id, acdk::wx::Point::defaultPosition(), acdk::wx::Size::defaultSize(), TrRowLines | TrSingle | TrHasButtons | TrHideRoot | TrFullRowHighlight)
{
  connect(TreeEvent::EvtCommandTreeItemExpanding, id, (ObjectEventFunction)&LocalVarsCtrl::onListBoxExpanded);
}

using namespace acdk::lang::dmi;

void
LocalVarsCtrl::insertItem(IN(RTreeItemId) parent, IN(RString) label, IN(RDmiObject) obj)
{
  if (obj->isStringType() == false)
  {
    RTreeItemId sid = appendItem(parent, SBSTR(label << ": " << obj->toCode()));
    setItemDataObject(sid, obj->getObjectVar());
    setItemHasChildren(sid, true);
  }
  else
  {
    appendItem(parent, SBSTR(label << ": " << obj->toCode()));
  }
}

void 
LocalVarsCtrl::fillProps(IN(acdk::cfgscript::RProps) scopeProps, IN(acdk::cfgscript::RProps) frameProps)
{
  deleteAllItems();
  RTreeItemId root = addRoot("TheRoot");
  RProps props = scopeProps;
  bool breakNext = false;
  while (props != Nil)
  {
    RStringArray sa = props->getKeys(PropsNoParentRead);
    for (int i = 0; i < sa->length(); ++i)
    {
      RString key = sa[i];
      if (key->startsWith(".") == true || key->startsWith("__") == true)
        continue;
      insertItem(root, sa[i], props->get(sa[i]));
    }

    if (breakNext == true)
      break;
    props = props->getParentProps();
    if (props == frameProps)
      breakNext = true;
  } 
}

void 
LocalVarsCtrl::refreshProps(IN(acdk::cfgscript::RProps) props)
{
}

void 
LocalVarsCtrl::refreshItem(IN(RTreeItemId) sid, IN(acdk::cfgscript::RProps) props)
{
}

void 
LocalVarsCtrl::updateStackFrame(IN(acdk::cfgscript::RExecutionStackFrame) frame)
{
  if (frame->_scopeProps == _lastProps)
    ;
  
  _lastProps = frame->_scopeProps;
  fillProps(_lastProps, frame->_frameProps);
  /*
  RTreeItemId root = getRootItem();
  if (root != Nil && root->isOk())
    return;
  root = addRoot("TheRoot");
  appendItem(root, "First\tBla");
  appendItem(root, "First\tBla");
  appendItem(root, "Second\tBla");
  appendItem(root, "Third\tBla");
  */
}


void 
LocalVarsCtrl::onListBoxExpanded(IN(RTreeEvent) event)
{
  RTreeItemId tid = event->getItem();
  if (getChildrenCount(tid, false) != 0)
      return;
  RObject o = getItemDataObject(tid);
  _expand(tid, o);
}

void 
LocalVarsCtrl::_expand(IN(RTreeItemId) tid, IN(RObject) o)
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
LocalVarsCtrl::reset()
{
  deleteAllItems();
  _lastProps = Nil;
}

} // csfide
} // tools
} // acdk

