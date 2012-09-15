

#include "Main.h"
#include "Editor.h"
#include "OutputWindow.h"
#include <acdk/text/RegExp.h>
#include <acdk/lang/Number.h>
#include <acdk/wx/Timer.h>
#include <acdk/wx/FileDropTarget.h>
#include <acdk/wx/ide/inspector/ClassTreeCtrl.h>
#include <acdk/wx/ide/inspector/ObjectTreeCtrl.h>
#include <acdk/wx/ide/inspector/CfgScriptConsole.h>

namespace acdk {
namespace tools {
namespace csfide {

using namespace acdk::wx;
using namespace acdk::wx::ide;
using namespace acdk::wx::ide::inspector;

#include <acdk/wx/xpms/text_GO_1616.xpm>
#include <acdk/wx/xpms/text_DB_1616.xpm>
#include <acdk/wx/xpms/text_ST_1616.xpm>
#include <acdk/wx/xpms/text_NE_1616.xpm>
#include <acdk/wx/xpms/text_TE_1616.xpm>
#include <acdk/wx/xpms/text_BR_1616.xpm>
#include <acdk/wx/xpms/text_RT_1616.xpm>

#include <acdk/wx/xpms/toolbar/copy.xpm>
#include <acdk/wx/xpms/toolbar/cut.xpm>
#include <acdk/wx/xpms/toolbar/new.xpm>
#include <acdk/wx/xpms/toolbar/open.xpm>
#include <acdk/wx/xpms/toolbar/paste.xpm>
#include <acdk/wx/xpms/toolbar/redo.xpm>
#include <acdk/wx/xpms/toolbar/save.xpm>
#include <acdk/wx/xpms/toolbar/undo.xpm>

//static 
MainFrame* MainFrame::_mainFrame = 0;

ACDK_DECL_CLASS(MyFileDropTarget);

class MyFileDropTarget
: extends FileDropTarget
{
public:
  MyFileDropTarget()
  {
  }
  virtual DragResult onEnter(int x, int y, DragResult def) 
  { 
    return DragCopy;
  }
  
  virtual DragResult onDragOver(int x, int y, DragResult def) 
  { 
    return DragCopy;
  }
  virtual void onLeave() 
  { 
  }
  virtual bool onDropFiles(int x, int y, IN(RStringArray) filenames) 
  {
    //MainFrame::get()->messageBox("Files droped: " + filenames->toString());
    for (int i = 0; i < filenames->length(); ++i)
    {
      MainFrame::get()->openOrActiveFileAtLine(filenames[i], -1);
    }
    return true;
  }
};

class EditorChildFrame
: extends MDIChildFrame
{
public:
  EditorChildFrame(IN(RMDIParentFrame) parent, int id, IN(RString) title, IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), int style =  DefaultFrameStyle,
            IN(RString) name = "frame")
  : MDIChildFrame(parent, id, title, pos, size, style, name)
  {
    ACDK_SAFE_CONSTRUCTOR();

    connect(CloseEvent::EvtCloseWindow, -1, (ObjectEventFunction)&EditorChildFrame::onClose);
    
    
    RTimer timer = new Timer(this, 1);
    connect(TimerEvent::EvtTimer, 1, (ObjectEventFunction)&EditorChildFrame::onDoMaximize);
    timer->start(300, true);
  }
  void onClose(IN(acdk::wx::RCloseEvent) event)
  {
    if (REditor(getChildren()[0])->canClose() == false)
    {
      event->veto();
      return;
    }
    event->skip();
  }
  void onDoMaximize(IN(RTimerEvent) event)
  {
    maximize();
  }
  
};


void showBarByName(IN(RFrameLayout) layout, IN(RString) name, bool show)
{
  RBarInfo outpInfo = layout->findBarByName(name);
  if (outpInfo == Nil)
    return;
  if (show == true)
    layout->setBarState(outpInfo, outpInfo->getState() & ~CbarHidden, true);
  else
    layout->setBarState(outpInfo, outpInfo->getState() | CbarHidden, true);
}


MainFrame::MainFrame()
: MDIParentFrame(Nil, -1, "CfgScript IDE", new Point(100, 100), new Size(1200, 1000))
, _debugger(new IdeScriptDebugger())
{
  ACDK_SAFE_CONSTRUCTOR();
  MainFrame::_mainFrame = this;
  StyledTextCtrl::loadConfig("C:\\d\\artefaktur\\acdk\\acdk_wx\\cfg\\csf\\acdk\\wx\\ide\\StyledTextCtrConfig.csf");
  
  RMDIClientWindow client = getClientWindow(); 
  _layout = new acdk::wx::ide::FrameLayout(this, &client); 
  _layout->pushDefaultPlugins(); 
  _layout->addPlugins(SimpleCustomizationPlugin | AntiflickerPlugin | BarHintsPlugin /*| RowDragPlugin*/); //BarHintsPlugin | 

  RMenuBar menu_bar = new MenuBar();
  
  _createEditMenus(menu_bar);
  _createDebugMenus(menu_bar);
  setMenuBar(menu_bar); 

  _createCsfDebugBars(this, _layout);
  _createEditBars(this, _layout);

  _createLeftDebugView(this, _layout);
  
  _createCsfDebugCtrls(this, _layout);
  _createOutputCtrl(this, _layout);
  
  
  _createEvents();
  
  _debugger->updateUI();
  RMyFileDropTarget fileDropTarget = new MyFileDropTarget();
  setDropTarget(&fileDropTarget);

  //openFile("C:\\d\\artefaktur\\acdk\\acdk_core\\cfg\\csf\\tests\\scribble.csf");
  _debugger->uiIsReady();
  //openFile("C:\\d\\artefaktur\\acdk\\acdk_boot\\acdk_boot.linux");
}

void 
MainFrame::_createOutputCtrl(IN(RWindow) parent, IN(acdk::wx::ide::RFrameLayout) layout)
{
  RNotebook notebook = new Notebook(parent, -1);
  RTextOutputCtrl outPutWindow = new OutputWindow(&notebook, ID_TEXTCTRL_OUTPUT);
  notebook->addPage(&outPutWindow, "Output");

  RCfgScriptConsole console = new CfgScriptConsole(&notebook, -1);
  notebook->addPage(&console, "CsfConsole");

  layout->addBar(&notebook, new DimInfo( 
                      450,150, // when docked horizontally      
                      250,150, // when docked vertically        
                      475,150, // when floated                  
                      false,   // fixed-size?
                      4,      // vertical gap (bar border)
                      4,       // horizontal gap (bar border)
                      new DynToolBarDimHandler()
                    ), 
                    FlAlignBottom, 0, //CbarDockedHorizontally, // CbarDockedVertically, 
                    1, 0, "Output");
}

void 
MainFrame::_createCsfDebugCtrls(IN(RWindow) parent, IN(acdk::wx::ide::RFrameLayout) layout)
{
  //RPanel dbgPanel = new Panel(&notebook, -1);
  //dbgPanel->show(false);

  acdk::wx::ide::RFrameLayout debugLayout = layout; //new acdk::wx::ide::FrameLayout(&dbgPanel, &notebook); 
  backTraceCtrl = new BackTraceCtrl(parent, -1);

  localVarsCtrl = new LocalVarsCtrl(parent, -1);
  //RTextCtrl dummyLocalVars2 = new TextCtrl(parent, -1);

  //toolBar->addControl(dbgPanel);

  RRect r = getRect();
  RDimInfo dimInfo = new DimInfo( 
                      r->width() / 3,250, // when docked horizontally      
                      250,150, // when docked vertically        
                      r->width() / 3, 250, // when floated                  
                      false,   // fixed-size?
                      4,      // vertical gap (bar border)
                      4,       // horizontal gap (bar border)
                      new DynToolBarDimHandler()
                    );


  debugLayout->addBar(&backTraceCtrl, dimInfo, 
                    FlAlignBottom, 0,//CbarHidden, //CbarDockedHorizontally | 
                    0, 1, "DbgBacktrace");

  debugLayout->addBar(&localVarsCtrl, dimInfo, 
                    FlAlignBottom,  0, //CbarHidden, //CbarDockedVertically |
                    0, 2, "DbgScopeVars");
  /*debugLayout->addBar(&dummyLocalVars2, dimInfo, 
                    FlAlignBottom, 0, //CbarHidden, //CbarDockedVertically | 
                    0, 3, 
                    "DbgInspector");
                    */
}

void
MainFrame::_createLeftDebugView(IN(RWindow) parent, IN(acdk::wx::ide::RFrameLayout) layout)
{
  RNotebook leftNotebook = new Notebook(parent, -1);
  
  RTreeCtrl classView = new ClassTreeCtrl(&leftNotebook, -1, Point::defaultPosition(), Size::defaultSize());
  leftNotebook->addPage(&classView, "Classes");
  
  RTreeCtrl objectView = new ObjectTreeCtrl(&leftNotebook, -1, Point::defaultPosition(), Size::defaultSize());
  leftNotebook->addPage(&objectView, "Objects");
  
  layout->addBar(&leftNotebook, new DimInfo( 
                      250,450, // when docked horizontally      
                      250,250, // when docked vertically        
                      275,450, // when floated                  
                      false,   // fixed-size?
                      4,      // vertical gap (bar border)
                      4,       // horizontal gap (bar border)
                      new DynToolBarDimHandler()
                    ), FlAlignLeft, CbarDockedVertically, 
                    0, 0, "Project Workplace");

}

void 
MainFrame::showDebugCtrls(bool show)
{
  return;


  RBarInfo stackTraceInfo = _layout->findBarByName("DbgBacktrace");
  if (stackTraceInfo == Nil)
    return;

  if (show == true)
  {
    showBarByName(_layout, "Output", false);
    showBarByName(_layout, "DbgBacktrace", true);
    showBarByName(_layout, "DbgScopeVars", true);
    showBarByName(_layout, "DbgInspector", true);
    //showBarByName(_layout, "Output", true);
  }
  else
  {
    showBarByName(_layout, "DbgBacktrace", false);
    showBarByName(_layout, "DbgScopeVars", false);
    showBarByName(_layout, "DbgInspector", false);
    showBarByName(_layout, "Output", true);
  }
  //_layout->inverseVisibility(debugLayout->findBarByName("DbgInspector"));
  //_layout->->inverseVisibility(debugLayout->findBarByName("DbgScopeVars"));
  //_layout->->inverseVisibility(debugLayout->findBarByName("DbgBacktrace"));
}

void 
MainFrame::_createCsfDebugBars(IN(RWindow) parent, IN(acdk::wx::ide::RFrameLayout) layout)
{
  //acdk::wx::ide::RDynamicToolBar toolBar = new acdk::wx::ide::DynamicToolBar(this, -1);
  RToolBar toolBar = new ToolBar(parent, -1, Point::defaultPosition(), Size::defaultSize(), BorderNone | TbHorizontal | TbNodivider);
  //RToolBar toolBar = new ToolBar(this, -1);
  
  toolBar->addTool(ID_MENU_DEBUG_RUN, "Run", new Bitmap(text_go_1616_xpm), new Bitmap(text_go_1616_xpm), ItemNormal,
                                 "Run the current script");

  toolBar->addTool(ID_MENU_DEBUG_DEBUG, "Debug", new Bitmap(text_db_1616_xpm), 
                                 "Debug the current script");
  toolBar->addTool(ID_MENU_DEBUG_TERM, "Terminate", new Bitmap(text_te_1616_xpm),
                                 "Terminate the current running script");

  toolBar->addTool(ID_MENU_DEBUG_STEP, "Step", new Bitmap(text_st_1616_xpm),
                                 "Steps through Script");
  toolBar->addTool(ID_MENU_DEBUG_NEXT, "Next", new Bitmap(text_ne_1616_xpm),
                                 "Steps through Script until next line");

  toolBar->addTool(ID_MENU_DEBUG_RETURN, "Return", new Bitmap(text_rt_1616_xpm),
                                 "Run until return");

  
  toolBar->realize();
  debugBar = &toolBar;
  layout->addBar(&toolBar,             // bar window (can be NULL)
                      new DimInfo( 
                      200,35, // when docked horizontally      
                      200,35, // when docked vertically        
                      200,35, // when floated                  
                      true,   // the bar is not fixed-size
                      4,      // vertical gap (bar border)
                      4       // horizontal gap (bar border)
                    ), 
                    FlAlignTop, // alignment ( 0-top,1-bottom, etc)
                    CbarDockedHorizontally, //CbarDockedVertically CbarHidden,
                      0,                    // insert into 0th row (vert. position)
                      1,                    // offset from the start of row (in pixels)
                      "DebugToolbar",           // name for reference in customization pop-ups
                      false
                    );
}

void 
MainFrame::_createEditBars(IN(RWindow) parent, IN(acdk::wx::ide::RFrameLayout) layout)
{
  RToolBar toolBar = new ToolBar(parent, -1, Point::defaultPosition(), Size::defaultSize(), BorderNone | TbHorizontal | TbNodivider);
  //RToolBar toolBar = new ToolBar(this, -1);
  toolBar->addTool(ID_MENU_FILE_NEW, "New", new Bitmap(new_xpm),  "Open a new File");
  toolBar->addTool(ID_MENU_FILE_SAVE, "Save", new Bitmap(save_xpm), 
                                      "The the current file");
  

  toolBar->addTool(ID_MENU_FILE_SAVE, "Open", new Bitmap(open_xpm), 
                                      "Open an existand file");
  toolBar->addTool(ID_MENU_FILE_SAVEAS, "Save as ...", new Bitmap(open_xpm),
                                        "Save file under different file name");
  toolBar->addSeparator();
  toolBar->addTool(ID_MENU_EDIT_CUT, "Cut", new Bitmap(cut_xpm),
                                        "Cut selected text");
  
  toolBar->addTool(ID_MENU_EDIT_COPY, "Copy", new Bitmap(copy_xpm),
                                        "Copy selected text");
  
  toolBar->addTool(ID_MENU_EDIT_PASTE, "Paste", new Bitmap(paste_xpm),
                                        "Past text");
  
  toolBar->addSeparator();
  toolBar->addTool(ID_MENU_EDIT_UNDO, "Undo", new Bitmap(undo_xpm),
                                        "Undo last operation");
  toolBar->addTool(ID_MENU_EDIT_REDO, "Redo", new Bitmap(redo_xpm),
                                        "Redo last operation");
  
  toolBar->realize();

  layout->addBar(&toolBar,             // bar window (can be NULL)
                      new DimInfo( 
                      255,35, // when docked horizontally      
                      255,35, // when docked vertically        
                      255,35, // when floated                  
                      true,   // the bar is not fixed-size
                      4,      // vertical gap (bar border)
                      4       // horizontal gap (bar border)
                    ), 
                    FlAlignTop, // alignment ( 0-top,1-bottom, etc)
                    CbarDockedVertically,
                      0,                    // insert into 0th row (vert. position)
                      1,                    // offset from the start of row (in pixels)
                      "FileToolbar",           // name for reference in customization pop-ups
                      false
                    );
}
  

void
MainFrame::_createDebugMenus(IN(RMenuBar) menu_bar)
{

  RMenu debug_menu = new Menu();
  debug_menu->append(ID_MENU_DEBUG_RUN, "Run\tCtrl+F5");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_DEBUG_RUN, (ObjectEventFunction)&MainFrame::onRun);
  
  debug_menu->append(ID_MENU_DEBUG_DEBUG, "Debug\tF5");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_DEBUG_DEBUG, (ObjectEventFunction)&MainFrame::onDebug);
  
  debug_menu->append(ID_MENU_DEBUG_TERM, "Terminate");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_DEBUG_TERM, (ObjectEventFunction)&MainFrame::onTerminate);

  debug_menu->append(ID_MENU_DEBUG_NEXT, "Next\tF10");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_DEBUG_NEXT, (ObjectEventFunction)&MainFrame::onNext);

  debug_menu->append(ID_MENU_DEBUG_STEP, "Step\tF11");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_DEBUG_STEP, (ObjectEventFunction)&MainFrame::onStep);
  
  debug_menu->append(ID_MENU_DEBUG_BREAK, "Break\tESC");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_DEBUG_BREAK, (ObjectEventFunction)&MainFrame::onBreak);
  
  debug_menu->append(ID_MENU_DEBUG_RETURN, "Return");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_DEBUG_RETURN, (ObjectEventFunction)&MainFrame::onReturn);
  
  menu_bar->append(debug_menu, "Debug");
  debugMenu = debug_menu;
}

void 
MainFrame::_createEditMenus(IN(RMenuBar) menu_bar)
{
  RMenu file_menu = new Menu(); 
  file_menu->append(ID_MENU_FILE_NEW, "&New\tCtrl+N");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_FILE_NEW, (ObjectEventFunction)&MainFrame::onFileNew);

  file_menu->append(ID_MENU_FILE_OPENFILE, "&Open ...\tCtrl+O");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_FILE_OPENFILE, (ObjectEventFunction)&MainFrame::onFileOpen);
  
  file_menu->append(ID_MENU_FILE_SAVE, "&Save\tCtrl+S");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_FILE_SAVE, (ObjectEventFunction)&MainFrame::forwardEventToChild);
  file_menu->append(ID_MENU_FILE_SAVEAS, "Save &as ...\tCtrl+Shift+S");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_FILE_SAVEAS, (ObjectEventFunction)&MainFrame::forwardEventToChild);

  file_menu->append(ID_MENU_CLOSE, "&Quit\tCtrl+Q"); 
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_CLOSE, (ObjectEventFunction)&MainFrame::onMenuQuit);
  menu_bar->append(file_menu, "&File"); 

  RMenu edit_menu = new Menu();
  
  edit_menu->append(ID_MENU_EDIT_UNDO, "&Undo\tCtrl+Z");
  edit_menu->append(ID_MENU_EDIT_REDO, "&Redo\tCtrl+Shift+Z");
  edit_menu->appendSeparator();
  edit_menu->append(ID_MENU_EDIT_CUT, "Cu&t\tCtrl+X");
  edit_menu->append(ID_MENU_EDIT_COPY, "&Copy\tCtrl+C");
  edit_menu->append(ID_MENU_EDIT_PASTE, "&Paste\tCtrl+V");
  // ### delete
  menu_bar->append(edit_menu, "&Edit"); 

  // File Menu
  
  

  RMenu view_menu = new Menu();
  view_menu->append(ID_MENU_VIEW_LINEENDINGS, "View Line Endigs", "", ItemCheck);
  view_menu->appendSeparator();
  view_menu->append(ID_MENU_VIEW_ZOOMIN, "Zoom in\tCTRL+");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_VIEW_ZOOMIN, (ObjectEventFunction)&MainFrame::forwardEventToChild);
  view_menu->append(ID_MENU_VIEW_ZOOMOUT, "Zoom out\tCTRL-");
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_VIEW_ZOOMOUT, (ObjectEventFunction)&MainFrame::forwardEventToChild);

  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_VIEW_LINEENDINGS, (ObjectEventFunction)&MainFrame::onViewLineEndings);
  menu_bar->append(view_menu, "&View"); 
  createStatusBar(); 
}

void 
MainFrame::_createEvents()
{
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_UNDO, (ObjectEventFunction)&MainFrame::forwardEventToChild);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_REDO, (ObjectEventFunction)&MainFrame::forwardEventToChild);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_CUT, (ObjectEventFunction)&MainFrame::forwardEventToChild);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_COPY, (ObjectEventFunction)&MainFrame::forwardEventToChild);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_PASTE, (ObjectEventFunction)&MainFrame::forwardEventToChild);
  connect(CloseEvent::EvtCloseWindow, -1, (ObjectEventFunction)&MainFrame::onClose);
  
  // geht nicht? connect(IdleEvent::EvtIdle, -1, (ObjectEventFunction)&MainFrame::onIdle);
  //wxIdleEvent::SetMode(wxIDLE_PROCESS_ALL);// ### hack/test
  System::out->println(RString("UpdateUIEvent::getUpdateInterval: ") + UpdateUIEvent::getUpdateInterval());
  UpdateUIEvent::setUpdateInterval(300);
  //connect(UpdateUIEvent::EvtUpdateUi, ID_MENU_FILE_SAVE, (ObjectEventFunction)&MainFrame::onFileSaveUI);
  
  
}

void 
MainFrame::onFileClose(IN(acdk::wx::RCommandEvent) event)
{
  REditor editor = getActiveEditWindow();
  if (editor == Nil)
    return;
  editor->canClose();
}

REditorArray 
MainFrame::getEditors()
{
  REditorArray erg = new EditorArray(0);
  RWindowArray childs = getChildren();
  for (int i = 0; i < childs->length(); ++i)
  {
    RWindow c = childs[i];
    if (instanceof(c, MDIChildFrame) == true)
    {
      acdk::wx::RWindowArray wa = c->getChildren();
      if (wa->length() > 0)
      {
        acdk::wx::RWindow w = wa[0];
        if (instanceof(w, Editor) == true)
          erg->append((REditor)w);
      }
    }
  }
  return erg;
}

REditor 
MainFrame::getActiveEditWindow()
{
  acdk::wx::RMDIChildFrame child = getActiveChild();
  if (child == Nil)
    return Nil;
  acdk::wx::RWindowArray wa = child->getChildren();
  acdk::wx::RWindow w = wa[0];
  if (w == Nil)
    return Nil;
  return (REditor)w;
}

void 
MainFrame::onClose(IN(acdk::wx::RCloseEvent) event)
{
  REditorArray editors = getEditors();
  for (int i = 0; i < editors->length(); ++i)
  {
    REditor editor = editors[i];
    if (editor->canClose() == false)
    {
      if (event->canVeto() == true) 
        event->veto(true);
      return;
    }
  }
  destroy();
}

REditor 
MainFrame::createEditWindow(IN(RString) title)
{
  RWindow cf = new EditorChildFrame(this, -1, title, Point::defaultPosition(), Size::defaultSize(), DefaultFrameStyle);
  REditor edit = new Editor(&cf, -1);
  //edit->connect(FocusEvent::EvtSetFocus, -1, (ObjectEventFunction)&MainFrame::onFocus);
  return edit;
}

REditor 
MainFrame::findOrOpenFile(IN(RString) fileName)
{
  REditor editor = getActiveEditWindow();
  if (editor != Nil && fileName->equals(editor->getFileName()) == true)
    return editor;
  REditorArray editors = getEditors();
  for (int i = 0; i < editors->length(); ++i)
  {
    editor = editors[i];
    if (fileName->equals(editor->getFileName()) == true)
      return editor;
  }
  acdk::io::File f(fileName);
  if (f.exists() == false)
    return Nil;
  return openFile(fileName);
}

void 
MainFrame::openOrActiveFileAtLine(IN(RString) file, int line, int column)
{
  REditor editor = findOrOpenFile(file);
  if (editor == Nil)
  {
    messageBox("Cannot open editor for " + file);
    return;
  }
  
  if (line != -1)
  {
    editor->gotoLine(line);
    if (column != -1)
    {
      editor->setCurrentPos(editor->getCurrentPos() + column);
    }
  }
  RMDIChildFrame(editor->getParent())->activate();
  editor->setFocus();
}

void 
MainFrame::openFileAtErrorLine(IN(RString) line)
{
  RString pattern = "^(.*)\\((.*)\\)\\:(.*)$";
  acdk::text::RegExp reg(pattern);
  RStringArray matches = reg.match(line);
  if (matches == Nil)
    return;
  RString file = matches[1];
  RString sourceLine = matches[2];
  int cpos = sourceLine->indexOf(",");
  RNumber xpos = Nil;
  RNumber ypos = Nil;
  if (cpos != -1)
  {
    ypos = Number::decodeToNumber(sourceLine->substr(0, cpos), true);
    xpos = Number::decodeToNumber(sourceLine->substr(cpos + 1), true);
  }
  else
  {
    ypos = Number::decodeToNumber(sourceLine, true);
  }
  int x = -1;
  int y = -1;
  if (xpos != Nil)
    x = xpos->intValue();
  if (ypos != Nil)
    y = ypos->intValue();

  openOrActiveFileAtLine(file, y, x);
  /*
  else
    messageBox("not a number: " + sourceLine);
    */

}

class CsfIdeApp
: extends acdk::wx::App
{
public:
  bool onInit()
  {
    acdk::wx::RFrame win = new MainFrame();
    win->show(true);
    return true;
  }
  static int acdkMain(RStringArray args)
  {
    return acdk::wx::App::createGui(new CsfIdeApp(), args);
  }
};

} // csfide
} // tools
} // acdk

int
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(::acdk::tools::csfide::CsfIdeApp::acdkMain, argc, argv, envptr);
}
