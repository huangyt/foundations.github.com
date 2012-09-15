// Generated by ACDK Metacompiler, 
// Copyrighted by Roger Rene Kommer, artefaktur
// 
// Dont edit this file manually
// 
#include <acdk.h>


#include "../wx.h"
#include "../App.h"
#include "../ArtProvider.h"
#include "../Bitmap.h"
#include "../BitmapButton.h"
#include "../BitmapDataObject.h"
#include "../BoxSizer.h"
#include "../Button.h"
#include "../Caret.h"
#include "../CheckBox.h"
#include "../Choice.h"
#include "../ClientData.h"
#include "../ClientDC.h"
#include "../Clipboard.h"
#include "../Colour.h"
#include "../ColourData.h"
#include "../ColourDialog.h"
#include "../ComboBox.h"
#include "../Config.h"
#include "../Control.h"
#include "../ControlWithItems.h"
#include "../Cursor.h"
#include "../DataFormat.h"
#include "../DataObject.h"
#include "../DataObjectComposite.h"
#include "../DataObjectSimple.h"
#include "../DC.h"
#include "../Dialog.h"
#include "../DirDialog.h"
#include "../DropSource.h"
#include "../DropTarget.h"
#include "../Event.h"
#include "../FileDataObject.h"
#include "../FileDialog.h"
#include "../FileDropTarget.h"
#include "../Font.h"
#include "../FontData.h"
#include "../FontDialog.h"
#include "../Frame.h"
#include "../Gauge.h"
#include "../GDIImage.h"
#include "../GDIObject.h"
#include "../HtmlWindow.h"
#include "../Icon.h"
#include "../LayoutConstraints.h"
#include "../ListBox.h"
#include "../MDIChildFrame.h"
#include "../MDIClientWindow.h"
#include "../MDIParentFrame.h"
#include "../MemoryDC.h"
#include "../Menu.h"
#include "../MenuBar.h"
#include "../MenuItem.h"
#include "../Notebook.h"
#include "../PaintDC.h"
#include "../Panel.h"
#include "../Pen.h"
#include "../PostScriptDC.h"
#include "../ProgressDialog.h"
#include "../RadioBox.h"
#include "../RadioButton.h"
#include "../Region.h"
#include "../ScreenDC.h"
#include "../ScrolledWindow.h"
#include "../SingleChoiceDialog.h"
#include "../Sizer.h"
#include "../Slider.h"
#include "../SpinButton.h"
#include "../SpinCtrl.h"
#include "../SplitterWindow.h"
#include "../StaticBitmap.h"
#include "../StaticBox.h"
#include "../StaticText.h"
#include "../StatusBar.h"
#include "../Structs.h"
#include "../TextCtrl.h"
#include "../TextCtrlCharWriter.h"
#include "../TextDataObject.h"
#include "../TextDropTarget.h"
#include "../TextEntryDialog.h"
#include "../Timer.h"
#include "../ToggleButton.h"
#include "../ToolBar.h"
#include "../ToolTip.h"
#include "../TreeCtrl.h"
#include "../Validator.h"
#include "../Window.h"
#include "../WindowDC.h"
#include "../WindowStyle.h"
#include "../Wizard.h"
#include "../WizardPage.h"
#include "../wx.h"
#include "../WxObject.h"
#include "../XmlResource.h"
#include <acdk/lang/dmi/ClazzInfoInternals.h>

namespace acdk { 
namespace wx { 

class WizardPage_DmiProxy
: extends WizardPage
, implements ::acdk::lang::dmi::DmiProxyBase
{
  ACDK_PROXY_WITH_METAINFO(WizardPage)
public:
  ::acdk::lang::Object* _cast(const ::acdk::lang::dmi::ClazzInfo* ci)
  {
    ::acdk::lang::Object* ret = _dmiProxyCast(ci);
    if (ret != 0)
      return ret;
    ret =  WizardPage::_cast(ci);
    return ret;
  }
  virtual void getCollectableFields(FieldReferences& fields)
  {
    ACDK_FQ_SUPER_QUALIFIER(acdk::wx::, WizardPage)::getCollectableFields(fields);
    fields.push_back((::acdk::lang::RObject*)_dmiTarget._ref_this());
  }
  virtual bool _gc_releaseRef(bool force = false) const { return ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::_gc_releaseRef(this); }
  ::acdk::lang::Object* getDmiTarget(bool& forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci) { return _dmiProxygetDmiTarget(forwarded, ci); }
  WizardPage_DmiProxy(IN(::acdk::wx::RWizard) parent, IN(::acdk::wx::RBitmap) bitmap, IN(::acdk::lang::RString) resource)
  : WizardPage(parent, bitmap, resource)
  {
     clazzInfo()->_resolveSupers(true, false);
     ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::_initThis(this);
  }
  WizardPage_DmiProxy(IN(::acdk::wx::RWizard) parent, IN(::acdk::wx::RBitmap) bitmap)
  : WizardPage(parent, bitmap)
  {
     clazzInfo()->_resolveSupers(true, false);
     ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::_initThis(this);
  }
  void SetFocusFromKbd()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, Window)::clazzInfo()->methods[2]) == false)
    {
      ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::SetFocusFromKbd();
      return;
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("SetFocusFromKbd", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
  }
  int getWindowStyleFlag()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, Window)::clazzInfo()->methods[86]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::getWindowStyleFlag();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("getWindowStyleFlag", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  bool setCursor(IN(::acdk::wx::RCursor) cursor)
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, Window)::clazzInfo()->methods[148]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::setCursor(cursor);
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(1);
    __acdk_args[0] = ::acdk::lang::inOf(cursor);
    _dmiProxyGetTarget()->standardDispatch("setCursor", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  bool processEvent(IN(::acdk::wx::REvent) event)
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, EvtHandler)::clazzInfo()->methods[12]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::processEvent(event);
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(1);
    __acdk_args[0] = ::acdk::lang::inOf(event);
    _dmiProxyGetTarget()->standardDispatch("processEvent", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  ::acdk::lang::RObject clone()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[3]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::clone();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("clone", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return (::acdk::lang::RObject)(::acdk::lang::RObject)__acdk_retval;
  }
  int compareTo(IN(::acdk::lang::RObject) o)
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[4]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::compareTo(o);
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(1);
    __acdk_args[0] = ::acdk::lang::inOf(o);
    _dmiProxyGetTarget()->standardDispatch("compareTo", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  bool equals(IN(::acdk::lang::RObject) o)
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[6]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::equals(o);
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(1);
    __acdk_args[0] = ::acdk::lang::inOf(o);
    _dmiProxyGetTarget()->standardDispatch("equals", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  void finalize()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[7]) == false)
    {
      ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::finalize();
      return;
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("finalize", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
  }
  ::acdk::lang::RClass getClass()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[8]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::getClass();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("getClass", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return (::acdk::lang::RClass)(::acdk::lang::RObject)__acdk_retval;
  }
  int hashCode()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[9]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::hashCode();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("hashCode", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  ::acdk::lang::RString toString()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[15]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WizardPage)::toString();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("toString", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return (::acdk::lang::RString)(::acdk::lang::RObject)__acdk_retval;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo*
  _WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String_dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject) new WizardPage_DmiProxy((::acdk::wx::RWizard)args[0].getObjectVar(), (::acdk::wx::RBitmap)args[1].getObjectVar(), (::acdk::lang::RString)args[2].getObjectVar());
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo*
  _WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject) new WizardPage_DmiProxy((::acdk::wx::RWizard)args[0].getObjectVar(), (::acdk::wx::RBitmap)args[1].getObjectVar());
    return methinf;
  }
};

::acdk::lang::dmi::ClazzSuperInfo WizardPage_DmiProxy_super =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  WizardPage::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _WizardPage_DmiProxy_interfaces[] =
{
  &WizardPage_DmiProxy_super,
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String_arg_parent =
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiIn,
  0, //AttributesRes
  "parent",
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::wx::Wizard::clazzInfo()
};

::acdk::lang::dmi::ClazzMethodArgInfo WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String_arg_bitmap =
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiIn,
  0, //AttributesRes
  "bitmap",
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::wx::Bitmap::clazzInfo()
};

::acdk::lang::dmi::ClazzMethodArgInfo WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String_arg_resource =
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiIn | ::acdk::lang::dmi::MiAiHasDefaultInit,
  0, //AttributesRes
  "resource",
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::String::clazzInfo()
};

::acdk::lang::dmi::ClazzMethodArgInfo* WizardPage_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String_args[] = 
{
  &WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String_arg_parent,
  &WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String_arg_bitmap,
  &WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String_arg_resource,
  0
};

::acdk::lang::dmi::ClazzMethodInfo WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiConstructor | ::acdk::lang::dmi::MiMiOrgPoly,
  0, //AttributesRes
  "WizardPage_DmiProxy",
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  WizardPage::clazzInfo(), // returnType
  "_0_WizardPage_DmiProxy", // altname
  -1, // altnamehashCode
  WizardPage_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String_args,
  0, // argumentCount
  0, // excpetions,
  WizardPage_DmiProxy::_WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String_dispatch,
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, 
  0 // cached methodhash
};

::acdk::lang::dmi::ClazzMethodArgInfo WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_arg_parent =
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiIn,
  0, //AttributesRes
  "parent",
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::wx::Wizard::clazzInfo()
};

::acdk::lang::dmi::ClazzMethodArgInfo WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_arg_bitmap =
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiIn,
  0, //AttributesRes
  "bitmap",
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::wx::Bitmap::clazzInfo()
};

::acdk::lang::dmi::ClazzMethodArgInfo* WizardPage_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_args[] = 
{
  &WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_arg_parent,
  &WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_arg_bitmap,
  0
};

::acdk::lang::dmi::ClazzMethodInfo WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiConstructor,
  0, //AttributesRes
  "WizardPage_DmiProxy",
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  WizardPage::clazzInfo(), // returnType
  "_1_WizardPage_DmiProxy", // altname
  -1, // altnamehashCode
  WizardPage_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_args,
  0, // argumentCount
  0, // excpetions,
  WizardPage_DmiProxy::_WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_dispatch,
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, 
  0 // cached methodhash
};

::acdk::lang::dmi::ClazzMethodInfo* WizardPage_methods[] = 
{
  &WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap_in_acdk_lang_String,
  &WizardPage_DmiProxy_methods__WizardPage_in_acdk_wx_Wizard_in_acdk_wx_Bitmap,
  0
};

::acdk::lang::dmi::ClazzInfo* WizardPage_DmiProxy::clazzInfo()
{
static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
  {
    ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiResolved, // clazz-flags
    0, //AttributesRes
    "WizardPage_DmiProxy", // name of class
  -1, // hashCode
    "acdk/wx", // the namespace
     0, // _scopeParent
     0, // _nextSibling
     0, // type
     0, // _firstChild
     _WizardPage_DmiProxy_interfaces, // pointer to Array of ClazzInfo references
     0, // count of Super / Interfaces
     0, // pointer to Array of fields
     0, // count of Fields
     WizardPage_methods, // pointer to Array of Methods
     0, // count of Methods
     0, // create-function for cloning/serializing
     0, // create-function for cloning/serializing arrays
     0, // create-function for cloning/serializing arrays
     0, // Class* thisClass; chaching instance
     0, // jlong serialVersionUID; for serialization
     ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
     ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
     0, // count off all collectable members in this class
     0, // user defined info
     0 // next ClazzInfo in chain
  };
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};
static ::acdk::lang::dmi::RegisterClazzInfo _register_WizardPage_DmiProxy(WizardPage_DmiProxy::clazzInfo());


} // namespace acdk
} // namespace wx


namespace acdk { 
namespace wx { 

class WxObject_DmiProxy
: extends WxObject
, implements ::acdk::lang::dmi::DmiProxyBase
{
  ACDK_PROXY_WITH_METAINFO(WxObject)
public:
  ::acdk::lang::Object* _cast(const ::acdk::lang::dmi::ClazzInfo* ci)
  {
    ::acdk::lang::Object* ret = _dmiProxyCast(ci);
    if (ret != 0)
      return ret;
    ret =  WxObject::_cast(ci);
    return ret;
  }
  virtual void getCollectableFields(FieldReferences& fields)
  {
    ACDK_FQ_SUPER_QUALIFIER(acdk::wx::, WxObject)::getCollectableFields(fields);
    fields.push_back((::acdk::lang::RObject*)_dmiTarget._ref_this());
  }
  virtual bool _gc_releaseRef(bool force = false) const { return ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::_gc_releaseRef(this); }
  ::acdk::lang::Object* getDmiTarget(bool& forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci) { return _dmiProxygetDmiTarget(forwarded, ci); }
  WxObject_DmiProxy()
  : WxObject()
  {
     clazzInfo()->_resolveSupers(true, false);
     ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::_initThis(this);
  }
  ::acdk::lang::RObject clone()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[3]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WxObject)::clone();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("clone", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return (::acdk::lang::RObject)(::acdk::lang::RObject)__acdk_retval;
  }
  int compareTo(IN(::acdk::lang::RObject) o)
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[4]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WxObject)::compareTo(o);
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(1);
    __acdk_args[0] = ::acdk::lang::inOf(o);
    _dmiProxyGetTarget()->standardDispatch("compareTo", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  bool equals(IN(::acdk::lang::RObject) o)
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[6]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WxObject)::equals(o);
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(1);
    __acdk_args[0] = ::acdk::lang::inOf(o);
    _dmiProxyGetTarget()->standardDispatch("equals", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  void finalize()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[7]) == false)
    {
      ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WxObject)::finalize();
      return;
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("finalize", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
  }
  ::acdk::lang::RClass getClass()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[8]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WxObject)::getClass();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("getClass", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return (::acdk::lang::RClass)(::acdk::lang::RObject)__acdk_retval;
  }
  int hashCode()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[9]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WxObject)::hashCode();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("hashCode", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  ::acdk::lang::RString toString()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[15]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, WxObject)::toString();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("toString", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return (::acdk::lang::RString)(::acdk::lang::RObject)__acdk_retval;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo*
  _WxObject_dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject) new WxObject_DmiProxy();
    return methinf;
  }
};

::acdk::lang::dmi::ClazzSuperInfo WxObject_DmiProxy_super =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  WxObject::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _WxObject_DmiProxy_interfaces[] =
{
  &WxObject_DmiProxy_super,
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo* WxObject_methods__WxObject_args[] = 
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WxObject_DmiProxy_methods__WxObject = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiConstructor | ::acdk::lang::dmi::MiMiOrgPoly,
  0, //AttributesRes
  "WxObject_DmiProxy",
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  WxObject::clazzInfo(), // returnType
  "_0_WxObject_DmiProxy", // altname
  -1, // altnamehashCode
  WxObject_methods__WxObject_args,
  0, // argumentCount
  0, // excpetions,
  WxObject_DmiProxy::_WxObject_dispatch,
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, 
  0 // cached methodhash
};

::acdk::lang::dmi::ClazzMethodInfo* WxObject_methods[] = 
{
  &WxObject_DmiProxy_methods__WxObject,
  0
};

::acdk::lang::dmi::ClazzInfo* WxObject_DmiProxy::clazzInfo()
{
static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
  {
    ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiResolved, // clazz-flags
    0, //AttributesRes
    "WxObject_DmiProxy", // name of class
  -1, // hashCode
    "acdk/wx", // the namespace
     0, // _scopeParent
     0, // _nextSibling
     0, // type
     0, // _firstChild
     _WxObject_DmiProxy_interfaces, // pointer to Array of ClazzInfo references
     0, // count of Super / Interfaces
     0, // pointer to Array of fields
     0, // count of Fields
     WxObject_methods, // pointer to Array of Methods
     0, // count of Methods
     0, // create-function for cloning/serializing
     0, // create-function for cloning/serializing arrays
     0, // create-function for cloning/serializing arrays
     0, // Class* thisClass; chaching instance
     0, // jlong serialVersionUID; for serialization
     ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
     ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
     0, // count off all collectable members in this class
     0, // user defined info
     0 // next ClazzInfo in chain
  };
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};
static ::acdk::lang::dmi::RegisterClazzInfo _register_WxObject_DmiProxy(WxObject_DmiProxy::clazzInfo());


} // namespace acdk
} // namespace wx


namespace acdk { 
namespace wx { 

class XmlResource_DmiProxy
: extends XmlResource
, implements ::acdk::lang::dmi::DmiProxyBase
{
  ACDK_PROXY_WITH_METAINFO(XmlResource)
public:
  ::acdk::lang::Object* _cast(const ::acdk::lang::dmi::ClazzInfo* ci)
  {
    ::acdk::lang::Object* ret = _dmiProxyCast(ci);
    if (ret != 0)
      return ret;
    ret =  XmlResource::_cast(ci);
    return ret;
  }
  virtual void getCollectableFields(FieldReferences& fields)
  {
    ACDK_FQ_SUPER_QUALIFIER(acdk::wx::, XmlResource)::getCollectableFields(fields);
    fields.push_back((::acdk::lang::RObject*)_dmiTarget._ref_this());
  }
  virtual bool _gc_releaseRef(bool force = false) const { return ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::_gc_releaseRef(this); }
  ::acdk::lang::Object* getDmiTarget(bool& forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci) { return _dmiProxygetDmiTarget(forwarded, ci); }
  XmlResource_DmiProxy()
  : XmlResource()
  {
     clazzInfo()->_resolveSupers(true, false);
     ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::_initThis(this);
  }
  ::acdk::lang::RObject clone()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[3]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, XmlResource)::clone();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("clone", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return (::acdk::lang::RObject)(::acdk::lang::RObject)__acdk_retval;
  }
  int compareTo(IN(::acdk::lang::RObject) o)
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[4]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, XmlResource)::compareTo(o);
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(1);
    __acdk_args[0] = ::acdk::lang::inOf(o);
    _dmiProxyGetTarget()->standardDispatch("compareTo", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  bool equals(IN(::acdk::lang::RObject) o)
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[6]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, XmlResource)::equals(o);
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(1);
    __acdk_args[0] = ::acdk::lang::inOf(o);
    _dmiProxyGetTarget()->standardDispatch("equals", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  void finalize()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[7]) == false)
    {
      ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, XmlResource)::finalize();
      return;
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("finalize", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
  }
  ::acdk::lang::RClass getClass()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[8]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, XmlResource)::getClass();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("getClass", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return (::acdk::lang::RClass)(::acdk::lang::RObject)__acdk_retval;
  }
  int hashCode()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[9]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, XmlResource)::hashCode();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("hashCode", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return __acdk_retval;
  }
  ::acdk::lang::RString toString()
  {
    if (_dmiProxyIsOverloaded(getClazzInfo(), ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::clazzInfo()->methods[15]) == false)
    {
      return ACDK_FQ_SUPER_QUALIFIER(::acdk::wx::, XmlResource)::toString();
    }
    ::acdk::lang::dmi::ScriptVar __acdk_retval;
    ::acdk::lang::dmi::ScriptVarArray __acdk_args(0);
    _dmiProxyGetTarget()->standardDispatch("toString", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), 0);
    return (::acdk::lang::RString)(::acdk::lang::RObject)__acdk_retval;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo*
  _XmlResource_dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject) new XmlResource_DmiProxy();
    return methinf;
  }
};

::acdk::lang::dmi::ClazzSuperInfo XmlResource_DmiProxy_super =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  XmlResource::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _XmlResource_DmiProxy_interfaces[] =
{
  &XmlResource_DmiProxy_super,
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo* XmlResource_methods__XmlResource_args[] = 
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo XmlResource_DmiProxy_methods__XmlResource = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiConstructor | ::acdk::lang::dmi::MiMiOrgPoly,
  0, //AttributesRes
  "XmlResource_DmiProxy",
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  XmlResource::clazzInfo(), // returnType
  "_0_XmlResource_DmiProxy", // altname
  -1, // altnamehashCode
  XmlResource_methods__XmlResource_args,
  0, // argumentCount
  0, // excpetions,
  XmlResource_DmiProxy::_XmlResource_dispatch,
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, 
  0 // cached methodhash
};

::acdk::lang::dmi::ClazzMethodInfo* XmlResource_methods[] = 
{
  &XmlResource_DmiProxy_methods__XmlResource,
  0
};

::acdk::lang::dmi::ClazzInfo* XmlResource_DmiProxy::clazzInfo()
{
static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
  {
    ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiResolved, // clazz-flags
    0, //AttributesRes
    "XmlResource_DmiProxy", // name of class
  -1, // hashCode
    "acdk/wx", // the namespace
     0, // _scopeParent
     0, // _nextSibling
     0, // type
     0, // _firstChild
     _XmlResource_DmiProxy_interfaces, // pointer to Array of ClazzInfo references
     0, // count of Super / Interfaces
     0, // pointer to Array of fields
     0, // count of Fields
     XmlResource_methods, // pointer to Array of Methods
     0, // count of Methods
     0, // create-function for cloning/serializing
     0, // create-function for cloning/serializing arrays
     0, // create-function for cloning/serializing arrays
     0, // Class* thisClass; chaching instance
     0, // jlong serialVersionUID; for serialization
     ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
     ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
     0, // count off all collectable members in this class
     0, // user defined info
     0 // next ClazzInfo in chain
  };
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};
static ::acdk::lang::dmi::RegisterClazzInfo _register_XmlResource_DmiProxy(XmlResource_DmiProxy::clazzInfo());


} // namespace acdk
} // namespace wx

