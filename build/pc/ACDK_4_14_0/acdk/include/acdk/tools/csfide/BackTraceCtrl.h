#ifndef acdk_tools_csfide_BackTraceCtrl_h
#define acdk_tools_csfide_BackTraceCtrl_h


#include <acdk/wx/wx.h>
#include <acdk/wx/ListBox.h>
#include <acdk/cfgscript/Script.h>

namespace acdk {
namespace tools {
namespace csfide {

USING_CLASS(acdk::wx::, ListBox);

ACDK_DECL_CLASS(BackTraceCtrl);

class BackTraceCtrl
: extends ListBox
{
public:
  acdk::cfgscript::RExecutionStackFrame _lastFrame;
  BackTraceCtrl(IN(acdk::wx::RWindow) parent, int id)
    : ListBox(parent, id, acdk::wx::Point::defaultPosition(), acdk::wx::Size::defaultSize())
  {
    _init();
  }
  
  void updateBacktrace(IN(acdk::cfgscript::RExecutionStack) stack);
  void reset();
private:
  void _init();
  void onSelFrame(IN(acdk::wx::RCommandEvent) event);
};


} // csfide
} // tools
} // acdk

#endif //acdk_tools_csfide_BackTraceCtrl_h
