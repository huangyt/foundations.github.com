
#ifndef acdk_wx_ObjectExplorer_h
#define acdk_wx_ObjectExplorer_h

#include <acdk/wx/wx.h>

#include <acdk/wx/Frame.h>
#include <acdk/wx/Dialog.h>
#include <acdk/wx/TextCtrl.h>
#include <acdk/wx/StaticText.h>
#include <acdk/wx/LayoutConstraints.h>
#include <acdk/wx/Sizer.h>
#include <acdk/wx/Button.h>
#include <acdk/wx/TreeCtrl.h>

namespace acdk {
namespace wx {

ACDK_DECL_CLASS(ObjectExplorer);
class ObjectExplorer
: extends Dialog
{
  RButton _okButton;
   RButton _updateBtn;
  RStaticText _label;
public:
  ObjectExplorer(IN(RWindow) parent);
  ~ObjectExplorer();
  void onUpdate(IN(RCommandEvent) event);
};

} // namespace wx
} // namespace acdk

#endif //acdk_wx_ObjectExplorer_h
