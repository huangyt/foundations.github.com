

#ifndef acdk_tools_csfide_Main_h
#define acdk_tools_csfide_Main_h

#include <acdk/wx/wx.h>
#include <acdk/wx/MDIParentFrame.h>
#include <acdk/wx/TextCtrl.h>
#include <acdk/wx/FileDialog.h>
#include <acdk/wx/ide/FrameLayout.h>
#include <acdk/wx/ide/TextOutputCtrl.h>
#include <acdk/wx/Panel.h>
#include <acdk/wx/TreeCtrl.h>
#include <acdk/wx/Notebook.h>
#include "Editor.h"
#include "CsfDebugger.h"
#include "BackTraceCtrl.h"
#include "LocalVarsCtrl.h"

namespace acdk {
namespace tools {
namespace csfide {

enum MenuIds
{
  ID_MENUBase = acdk::wx::IdHighest + 1,
  ID_MENU_CLOSE,
  ID_MENU_FILE_NEW,
  ID_MENU_FILE_OPENFILE,
  ID_MENU_FILE_SAVE,
  ID_MENU_FILE_SAVEAS,
  ID_MENU_VIEW_LINEENDINGS,
  ID_MENU_VIEW_ZOOMIN,
  ID_MENU_VIEW_ZOOMOUT,
  ID_MENU_EDIT_UNDO,
  ID_MENU_EDIT_REDO,
  ID_MENU_EDIT_CUT,
  ID_MENU_EDIT_COPY,
  ID_MENU_EDIT_PASTE,

  ID_MENU_EDIT_TONGLE_BREAKPOINT,
  
  ID_TEXTCTRL_OUTPUT,

  ID_MENU_DEBUG_RUN,
  ID_MENU_DEBUG_DEBUG,
  ID_MENU_DEBUG_TERM,
  ID_MENU_DEBUG_STEP,
  ID_MENU_DEBUG_NEXT,
  ID_MENU_DEBUG_BREAK,
  ID_MENU_DEBUG_RETURN


};

ACDK_DECL_CLASS(MainFrame);

class MainFrame
: extends acdk::wx::MDIParentFrame
{
  acdk::wx::ide::RFrameLayout _layout;
  //RStyledTextCtrl _editWindow;
  static MainFrame* _mainFrame;
  
public:
  RIdeScriptDebugger _debugger;
  acdk::wx::RMenu debugMenu;
  acdk::wx::RToolBarBase debugBar;
  RBackTraceCtrl backTraceCtrl;
  RLocalVarsCtrl localVarsCtrl;
  MainFrame();
  void onMenuQuit(IN(acdk::wx::REvent) event)
  {
    close(true);
  }
  void onFileNew(IN(acdk::wx::REvent) event)
  {
    messageBox("currently not implemented");
  }
  
  void onFileOpen(IN(acdk::wx::REvent) event)
  {
    acdk::wx::FileDialog dlg (this, "Open file", "", "",
                          "CfgFiles (*.csf)|*.csf", acdk::wx::FDFOpen | acdk::wx::FDFFileMustExist| acdk::wx::FDFChangeDir);
    if (dlg.showModal() != acdk::wx::IdOk) 
      return;
    openFile(dlg.getPath());
  }
  void onViewLineEndings(IN(acdk::wx::RCommandEvent) event)
  {
    REditor edit = getActiveEditWindow(); 
    if (edit == Nil)
      return;
    edit->setViewEOL(event->isChecked() == true);
  }

  REditor getActiveEditWindow();
  REditorArray  getEditors();
  void onFocus(IN(acdk::wx::RFocusEvent) focusEvent)
  {
    System::out->println("onFocus with id: " + focusEvent->getId());
  }
  void onIdle(IN(acdk::wx::RIdleEvent) event)
  {
    System::out->print(".");
    System::out->flush();
    event->skip();
  }
  REditor openFile(IN(RString) path)
  {
    REditor edit = createEditWindow(path);
    edit->openFile(path);
    return edit;
  }
  REditor createEditWindow(IN(RString) title);
  void forwardEventToChild(IN(acdk::wx::REvent) event)
  {
    REditor child = getActiveEditWindow();
    acdk::wx::RWindow w = acdk::wx::Window::FindFocus();
    if (w == child)
      child->processEvent(event);
    //if (child != Nil)
    //  child->processEvent(event);
  }
  void onFileSaveUI(IN(acdk::wx::RUpdateUIEvent) event)
  {
    System::out->println("onFileSaveUI");
    REditor txt = getActiveEditWindow();
    if (txt != Nil && txt->getModify() == true)
      event->enable(true);
    else
      event->enable(false);
  }
  /**
    goto opened window, which has open the file, or try to open a new editor with file
    or return Nil
  */
  REditor  findOrOpenFile(IN(RString) fileName);
  void openFileAtErrorLine(IN(RString) line);
  void openOrActiveFileAtLine(IN(RString) file, int line, int column = -1);
  static RMainFrame get() { return _mainFrame; }

  void onRun(IN(acdk::wx::RCommandEvent) event) { _debugger->onRun(this); }
  void onDebug(IN(acdk::wx::RCommandEvent) event) { _debugger->onDebug(this); }
  void onStep(IN(acdk::wx::RCommandEvent) event) { _debugger->onStep(this); }
  void onNext(IN(acdk::wx::RCommandEvent) event) { _debugger->onNext(this); }
  void onTerminate(IN(acdk::wx::RCommandEvent) event) { _debugger->onTerminate(this); }
  void onBreak(IN(acdk::wx::RCommandEvent) event) { _debugger->onBreak(this); }
  void onReturn(IN(acdk::wx::RCommandEvent) event) { _debugger->onReturn(this); }
  
  void onFileClose(IN(acdk::wx::RCommandEvent) event);
  void onClose(IN(acdk::wx::RCloseEvent) event);
  void showDebugCtrls(bool show);
private:
  void _createEditMenus(IN(acdk::wx::RMenuBar) menu_bar);
  void _createDebugMenus(IN(acdk::wx::RMenuBar) menu_bar);
  void _createEvents();
  void _createLeftDebugView(IN(acdk::wx::RWindow) parent, IN(acdk::wx::ide::RFrameLayout) layout);
  void _createCsfDebugBars(IN(acdk::wx::RWindow) parent, IN(acdk::wx::ide::RFrameLayout) layout);
  void _createEditBars(IN(acdk::wx::RWindow) parent, IN(acdk::wx::ide::RFrameLayout) layout);
  void _createOutputCtrl(IN(acdk::wx::RWindow) parent, IN(acdk::wx::ide::RFrameLayout) layout);
  void  _createCsfDebugCtrls(IN(acdk::wx::RWindow) parent, IN(acdk::wx::ide::RFrameLayout) layout);
};


} // csfide
} // tools
} // acdk

#endif //acdk_tools_csfide_Main_h
