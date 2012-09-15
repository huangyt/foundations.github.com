#ifndef acdk_tools_csfide_LocalVarsCtrl_h
#define acdk_tools_csfide_LocalVarsCtrl_h


#include <acdk/wx/wx.h>
#include <acdk/wx/TreeCtrl.h>
#include <acdk/cfgscript/Script.h>

namespace acdk {
namespace tools {
namespace csfide {

USING_CLASS(acdk::wx::, TreeCtrl);

ACDK_DECL_CLASS(LocalVarsCtrl);

class LocalVarsCtrl
: extends TreeCtrl
{
public:
  acdk::cfgscript::RProps  _lastProps;
  LocalVarsCtrl(IN(acdk::wx::RWindow) parent, int id);
  void updateStackFrame(IN(acdk::cfgscript::RExecutionStackFrame) frame);
  

  void  onListBoxExpanded(IN(acdk::wx::RTreeEvent) event);
  void fillProps(IN(acdk::cfgscript::RProps) scopeProps, IN(acdk::cfgscript::RProps) frameProps);
  void insertItem(IN(acdk::wx::RTreeItemId) parent, IN(RString) label, IN(acdk::lang::dmi::RDmiObject) obj);
  void refreshProps(IN(acdk::cfgscript::RProps) props);
  void refreshItem(IN(acdk::wx::RTreeItemId) sid, IN(acdk::cfgscript::RProps) props);
  void reset();
private:
  void  _expand(IN(acdk::wx::RTreeItemId) tid, IN(RObject) o);
  
};


} // csfide
} // tools
} // acdk

#endif //acdk_tools_csfide_LocalVarsCtrl_h
