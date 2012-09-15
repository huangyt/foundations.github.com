
#include "Main.h"
#include "Editor.h"
#include <acdk/io/CharToByteWriter.h>
#include <acdk/cfgscript/Script.h>
#include <acdk/wx/ide/TextOutputCtrl.h>

namespace acdk {
namespace tools {
namespace csfide {

using namespace acdk::wx;
using namespace acdk::wx::ide;
using namespace acdk::cfgscript;

enum MarginIds
{
  MarginBreakPoints = 1
};

void 
Editor::_init()
{
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_FILE_SAVE, (ObjectEventFunction)&Editor::onSave);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_FILE_SAVEAS, (ObjectEventFunction)&Editor::onSaveAs);

  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_COPY, (ObjectEventFunction)&Editor::onCopyText);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_CUT, (ObjectEventFunction)&Editor::onCutText);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_PASTE, (ObjectEventFunction)&Editor::onPasteText);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_REDO, (ObjectEventFunction)&Editor::onRedo);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_UNDO, (ObjectEventFunction)&Editor::onUndo);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_DEBUG_RUN, (ObjectEventFunction)&Editor::onRun);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_VIEW_ZOOMIN, (ObjectEventFunction)&Editor::onZoomIn);
  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_VIEW_ZOOMOUT, (ObjectEventFunction)&Editor::onZoomOut);

  connect(MouseEvent::EvtRightDown, -1, (ObjectEventFunction)&Editor::onContextMenu);
  
  setMarginType(MarginBreakPoints, StcMarginSymbol);
  setMarginSensitive(MarginBreakPoints, true);
  setMarginWidth(MarginBreakPoints, 12);
  connect(StyledTextEvent::EvtSciMarginclick, -1, (ObjectEventFunction)&Editor::onMarginClick);
  

  connect(CommandEvent::EvtCommandMenuSelected, ID_MENU_EDIT_TONGLE_BREAKPOINT, (ObjectEventFunction)&Editor::onToggleBreakPoint);
  connect(StyledTextEvent::EvtSciCharadded, -1, (ObjectEventFunction)&Editor::onCharAdded);
  
  connect(CloseEvent::EvtCloseWindow, -1, (ObjectEventFunction)&Editor::onClose);

  setLayoutCache(StcCachePage);
  setModEventMask(StcModBeforedelete | StcModDeletetext | StcModInserttext | StcPerformedUser |
                   StcPerformedUndo | StcPerformedRedo | StcModChangestyle);

  /*
  // set default language
  styleClearAll();

  setLexer(SciLexCpp);
  RString keywords = "using with delegate enum abstract assert boolean break byte case catch char class"
   "const continue default do double else extends final finally float for future"
    "generic goto if implements import inner instanceof int interface long"
    "native new null outer package private protected public rest"
    "return short static super switch synchronized this throw throws"
    "transient try var void volatile while";
  setLexerLanguage("cpp");
  setKeyWords(0, keywords);

  styleSetForeground(SciCNumber, new Colour(0, 0, 0xFF));
  styleSetForeground(SciCString, new Colour(0, 0, 0xFF));
  styleSetForeground(SciCCharacter, new Colour(0, 0, 0xFF));
    
  styleSetForeground(SciCWord, new Colour(222, 0, 0));
  styleSetForeground(SciCWord2, new Colour(222, 0, 0));
    
  styleSetForeground(SciCCommentline, new Colour(0, 222, 0));
  setUseTabs(false);
  setTabWidth(2);
  setIndent(2);
  usePopUp(0); // we create an own
  setCaretLineVisible(true);
 
  setTabWidth(2);
  setUseTabs(false);
  setTabIndents(true);
  setBackSpaceUnIndents(true);
  setIndentationGuides(true);
  setIndent(2);
   */
}

void 
Editor::createContextMenu(IN(acdk::wx::RMenu) menu)
{
  menu->append(ID_MENU_EDIT_CUT, "Cut");
  menu->append(ID_MENU_EDIT_COPY, "Copy");
  menu->append(ID_MENU_EDIT_PASTE, "Paste");
  menu->appendSeparator();
  menu->append(ID_MENU_EDIT_TONGLE_BREAKPOINT, "Toggle breakpoint");
}

void 
Editor::onContextMenu(IN(RMouseEvent) event)
{
  /*
  wxEvent* wxevent = event->getWx();
  if (event->leftDown() == true)
  {
    return;
  }
  
  RMouseEvent lmouseEvent = event->clone();
  lmouseEvent->setRightDown(false);
  lmouseEvent->setLeftDown(true);
  lmouseEvent->setEventType(MouseEvent::EvtLeftDown);
  processEvent(&lmouseEvent);
  lmouseEvent = (RMouseEvent)event->clone();
  lmouseEvent->setRightDown(false);
  lmouseEvent->setLeftDown(false);
  lmouseEvent->setEventType(MouseEvent::EvtLeftUp);
  processEvent(&lmouseEvent);
  
  */
  if (contextMenu == Nil)
  {
    contextMenu = new Menu();
    createContextMenu(contextMenu);
  }
  popupMenu(contextMenu, event->getX(), event->getY());
  
}

void 
Editor::toggleBreakPoint(int curline)
{
   int markerMask = markerGet(curline);
  if (markerMask & (int(MarginBreakPoints) * 2))
  {
    markerDelete(curline, MarginBreakPoints);
    IdeScriptDebugger::get()->removeBreakPoint(getFileName(), curline);
  }
  else
  {
    markerAdd(curline, MarginBreakPoints);
    IdeScriptDebugger::get()->insertBreakPoint(getFileName(), curline);
  }

}

void 
Editor::onToggleBreakPoint(IN(RCommandEvent) event)
{
  int curline = getCurrentLine();
  toggleBreakPoint(curline);
}

void 
Editor::onMarginClick(IN(RStyledTextEvent) event)
{
  if (event->getMargin() == MarginBreakPoints)
  {
    int lineClick = lineFromPosition(event->getPosition());
    toggleBreakPoint(lineClick);
  }
}

void 
Editor::onCopyText(IN(RCommandEvent) event)
{
  if (getSelectionEnd() - getSelectionStart() <= 0)
    return;
  copy();
}

void 
Editor::onCutText(IN(RCommandEvent) event)
{
  cut();
}

void 
Editor::onPasteText(IN(RCommandEvent) event)
{
  if (canPaste() == false) 
    return;
  paste();
}

void 
Editor::onRedo(IN(RCommandEvent) event)
{
  if (canRedo() == false)
    return;
  redo();
}

void 
Editor::onUndo(IN(RCommandEvent) event)
{
  if (canUndo() == false)
    return;
  undo();
}

void 
Editor::onSave(IN(RCommandEvent) event)
{
  if (getModify() == false) 
    return;
  saveInFile(_fileName);
}


bool 
Editor::canClose()
{
  if (getModify() == false)
    return true;
  int erg = messageBox("Save changes in " + getFileName() + "?", "Unsaved changes", MbIconQuestion| MbYesNo | MbCancel);
  if (erg == MbCancel)
    return false;
  if (erg == MbNo)
    return true;
  saveInFile(_fileName);
  return true;
}
void 
Editor::onClose(IN(acdk::wx::RCloseEvent) event) // ### todo never called
{
  messageBox("Unsafed changes");
  if (getModify() == false)
    return;
  setFocus();
  
}

void 
Editor::onZoomIn(IN(acdk::wx::RCommandEvent) event)
{
  zoomIn();
}

void 
Editor::onZoomOut(IN(acdk::wx::RCommandEvent) event)
{
  zoomOut();
}
  

void 
Editor::saveInFile(IN(RString) fileName)
{

  RString txt = getText();
  acdk::io::FileWriter f(fileName);
  acdk::io::CharToByteWriter fc(&f);
  fc.writeString(txt);
  fc.close();
  setSavePoint();
}

void 
Editor::onSaveAs(IN(RCommandEvent) event)
{
  messageBox("not implemented yet");
}


void Editor::onCharAdded(IN(RStyledTextEvent) event) 
{
  char chr = event->getKey();
  if (chr != '\n')
    return;
  int curLine = getCurrentLine();
  if (curLine == 0)
    return;
  int indent = getLineIndentation(curLine - 1);
  if (indent == 0)
    return;
  int curPos = getCurrentPos();
  for (int i = 0; i < indent; ++i)
    insertText(curPos, " ");
  gotoPos(curPos + indent);
  
}

void 
Editor::onRun(IN(RCommandEvent) event)
{
}

void 
Editor::onDebug(IN(RCommandEvent) event)
{

}

} // csfide
} // tools
} // acdk
