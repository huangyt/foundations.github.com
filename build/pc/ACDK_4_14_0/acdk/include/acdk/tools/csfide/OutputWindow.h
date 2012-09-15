#ifndef acdk_tools_csfide_OutputWindow_h
#define acdk_tools_csfide_OutputWindow_h

#include "Main.h"
#include <acdk/wx/ide/TextOutputCtrl.h>


namespace acdk {
namespace tools {
namespace csfide {

//using namespace acdk::wx;
//using namespace acdk::wx::ide;
  
//USING_CLASS(acdk::wx::ide::, TextOutputCtrl);

ACDK_DECL_CLASS(OutputWindow);

class OutputWindow
//: extends acdk::wx::ide::StyledTextCtrl
: extends acdk::wx::ide::TextOutputCtrl
{
public:
  RString _fileName;
  OutputWindow(IN(acdk::wx::RWindow) parent, int id)
    : ACDK_FQ_SUPER_QUALIFIER(acdk::wx::ide::, TextOutputCtrl)(parent, id, "", acdk::wx::Point::defaultPosition(), acdk::wx::Size::defaultSize(), 
                                        acdk::wx::TeAutoScroll | acdk::wx::TeMultiline | acdk::wx::TeLinewrap)
  {
    connect(acdk::wx::MouseEvent::EvtLeftDclick, -1, (acdk::wx::ObjectEventFunction)&OutputWindow::onDoubleClick);
  }
  void onDoubleClick(IN(acdk::wx::RMouseEvent) event)
  {
    acdk::wx::RMouseEvent evt = (acdk::wx::RMouseEvent)event->clone();
    evt->setEventType(acdk::wx::MouseEvent::EvtLeftDown);
    processEvent(&evt);
    int x, y;
    positionToXY(getInsertionPoint(), x, y);
    RString text = getLineText(y);
    MainFrame::get()->openFileAtErrorLine(text);
    //messageBox("doubleclick: " + text);
  }
  
};


} // csfide
} // tools
} // acdk

#endif //acdk_tools_csfide_OutputWindow_h

