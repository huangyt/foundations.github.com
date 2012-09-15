#ifndef acdk_tools_csfide_CsfDebugger_h
#define acdk_tools_csfide_CsfDebugger_h


#include <acdk/wx/wx.h>
#include <acdk/cfgscript/Script.h>
#include <acdk/cfgscript/ScriptDebug.h>
#include <acdk/lang/Thread.h>

namespace acdk {
namespace tools {
/** experimental IDE for CfgScript */
namespace csfide {

ACDK_DECL_CLASS(MainFrame);


enum DebuggerState
{
  /**
    no program running
  */
  Sleeping,
  Running,
  RunningDebug,
  Breaked
};

enum DebuggerNextState
{
  DoContinue,
  DoBreak,
  DoNext,
  DoStep,
  DoReturn,
  DoTerminate
};

ACDK_DECL_CLASS(IdeScriptDebugger);

class IdeScriptDebugger
: extends acdk::wx::EvtHandler
, implements acdk::cfgscript::Debugger
{
  DebuggerState _state;
  DebuggerNextState _nextState;
  ThreadID _guiThread;

  acdk::cfgscript::RExecutionStack currentStack;
public:
  IdeScriptDebugger();
  /// called when ui is ready
  void uiIsReady();
  void reset();
  static RIdeScriptDebugger get();
  foreign virtual bool doBreak(int action, acdk::cfgscript::PEStack& stack);
  foreign virtual acdk::cfgscript::DebugNextAction onBreak(int action, acdk::cfgscript::PEStack& stack, IN(acdk::cfgscript::RExecutionStackFrame) frame);
  void onRun(IN(RMainFrame) frame);
  void onDebug(IN(RMainFrame) frame);
  void onStep(IN(RMainFrame) frame);
  void onNext(IN(RMainFrame) frame);
  void onContinue(IN(RMainFrame) frame);
  void onTerminate(IN(RMainFrame) frame);
  void onBreak(IN(RMainFrame) frame);
  void onReturn(IN(RMainFrame) frame);
  
  // if change 
  void setNewState(DebuggerState newState);
  void updateUI();
  void onCommand(IN(acdk::wx::RCommandEvent) event);
  
  void _branchToGui(DebuggerNextState cmd, bool wait = true);
  void _startScript(IN(RMainFrame) frame, bool debug);

  void insertBreakPoint(IN(RString) fname, int line);
  void removeBreakPoint(IN(RString) fname, int line);
  void gotoFrame(int idx);

};

} // csfide
} // tools
} // acdk

#endif //acdk_tools_csfide_CsfDebugger_h
