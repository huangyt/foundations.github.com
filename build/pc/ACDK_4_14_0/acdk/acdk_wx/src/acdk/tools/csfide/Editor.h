#ifndef acdk_tools_csfide_Editor_h
#define acdk_tools_csfide_Editor_h


#include <acdk/wx/wx.h>
#include <acdk/wx/ide/StyledTextCtrl.h>


namespace acdk {
namespace tools {
namespace csfide {

//using namespace acdk::wx;
//using namespace acdk::wx::ide;

USING_CLASS(acdk::wx::ide::, StyledTextCtrl);

ACDK_DECL_CLASS(Editor);

class Editor
: extends acdk::wx::ide::StyledTextCtrl
{
public:
  RString _fileName;
  acdk::wx::RMenu contextMenu;
  Editor(IN(acdk::wx::RWindow) parent, int id)
    : StyledTextCtrl(parent, id, acdk::wx::Point::defaultPosition(), acdk::wx::Size::defaultSize(), acdk::wx::Maximize)
  {
    _init();
  }
  void openFile(IN(RString) fileName)
  {
    _fileName = fileName;
    initFromTextFile(fileName);
    loadFile(fileName);
  }
  virtual void createContextMenu(IN(acdk::wx::RMenu) menu);
  RString getFileName() { return _fileName; }
  void onCopyText(IN(acdk::wx::RCommandEvent) event);
  void onCutText(IN(acdk::wx::RCommandEvent) event);
  void onPasteText(IN(acdk::wx::RCommandEvent) event);
  void onRedo(IN(acdk::wx::RCommandEvent) event);
  void onUndo(IN(acdk::wx::RCommandEvent) event);
  void onContextMenu(IN(acdk::wx::RMouseEvent) event);
  void onToggleBreakPoint(IN(acdk::wx::RCommandEvent) event);
  void onMarginClick(IN(acdk::wx::ide::RStyledTextEvent) event);
  void onRun(IN(acdk::wx::RCommandEvent) event);
  void onDebug(IN(acdk::wx::RCommandEvent) event);
  void onSave(IN(acdk::wx::RCommandEvent) event);
  void onSaveAs(IN(acdk::wx::RCommandEvent) event);
  void onCharAdded(IN(acdk::wx::ide::RStyledTextEvent) event);
  void onClose(IN(acdk::wx::RCloseEvent) event);
  void onZoomIn(IN(acdk::wx::RCommandEvent) event);
  void onZoomOut(IN(acdk::wx::RCommandEvent) event);
  bool canClose();
  void toggleBreakPoint(int lineNo);
  void saveInFile(IN(RString) fileName);
private:
  void _init();
  
};


} // csfide
} // tools
} // acdk

#endif //acdk_tools_csfide_Editor_h

