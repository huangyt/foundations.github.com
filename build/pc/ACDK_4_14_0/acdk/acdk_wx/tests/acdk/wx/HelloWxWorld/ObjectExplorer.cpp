
#include "ObjectExplorer.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace wx {

enum 
{
  OnUpdate
};

ObjectExplorer::ObjectExplorer(IN(RWindow) parent)
: Dialog(parent, -1, "Object Explorer", Point::defaultPosition(), Size::defaultSize()/*, wxDefaultFrameStyle*/)
{
  ACDK_SAFE_CONSTRUCTOR();
  setAutoLayout(true);

  RLayoutConstraints layout = new LayoutConstraints();

  layout->top()->sameAs(this, Top, 10);
  layout->left()->sameAs(this, Left);
  layout->width()->asIs();
  layout->height()->asIs();
  _updateBtn = new Button(this, OnUpdate, "Update");
  _updateBtn->setConstraints(layout);
  connect(CommandEvent::EvtCommandButtonClicked, OnUpdate, (ObjectEventFunction)&ObjectExplorer::onUpdate);

  layout = new LayoutConstraints();
  layout->top()->sameAs(this, Top, 10);
  layout->left()->leftOf(&_updateBtn, Right);
  layout->width()->asIs();
  layout->height()->asIs();
  _okButton = new Button(this, wxID_OK, "Close");
  _okButton->setConstraints(layout);
  

  RTreeCtrl treectrl = new TreeCtrl(this);
  //RTreeItemId rid = treectrl->addRoot("RootItem");
  //treectrl->appendItem(rid, "First");
  
  layout = new LayoutConstraints();
  layout->top()->below(&_okButton);
  layout->centreX()->sameAs(this, CentreX);
  layout->width()->sameAs(this, Width);
  layout->bottom()->sameAs(this, Bottom);
  treectrl->setConstraints(layout);
  
}

ObjectExplorer::~ObjectExplorer()
{
}

void 
ObjectExplorer::onUpdate(IN(RCommandEvent) event)
{
  System::out->println("Updating...");
}

} // namespace wx
} // namespace acdk

