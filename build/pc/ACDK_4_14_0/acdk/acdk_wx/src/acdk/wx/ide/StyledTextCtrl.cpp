
#include "StyledTextCtrl.h"
#include <acdk/io/File.h>
#include <acdk/cfgscript/Script.h>

namespace acdk {
namespace wx {
namespace ide {



ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciChange, wxEVT_STC_CHANGE);

ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciStyleneeded, wxEVT_STC_STYLENEEDED);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciCharadded, wxEVT_STC_CHARADDED);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciSavepointreached, wxEVT_STC_SAVEPOINTREACHED);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciSavepointleft, wxEVT_STC_SAVEPOINTLEFT);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciRomodifyattempt, wxEVT_STC_ROMODIFYATTEMPT);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciKey, wxEVT_STC_KEY);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciDoubleclick, wxEVT_STC_DOUBLECLICK);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciUpdateui, wxEVT_STC_UPDATEUI);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciModified, wxEVT_STC_MODIFIED);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciMacrorecord, wxEVT_STC_MACRORECORD);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciMarginclick, wxEVT_STC_MARGINCLICK);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciNeedshown, wxEVT_STC_NEEDSHOWN);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciPainted, wxEVT_STC_PAINTED);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciUserlistselection, wxEVT_STC_USERLISTSELECTION);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciUridropped, wxEVT_STC_URIDROPPED);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciDwellstart, wxEVT_STC_DWELLSTART);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciDwellend, wxEVT_STC_DWELLEND);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciStartDrag, wxEVT_STC_START_DRAG);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciDragOver, wxEVT_STC_DRAG_OVER);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciDoDrop, wxEVT_STC_DO_DROP);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciZoom, wxEVT_STC_ZOOM);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciHotspotClick, wxEVT_STC_HOTSPOT_CLICK);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciHotspotDclick, wxEVT_STC_HOTSPOT_DCLICK);
ACDK_DEFINE_WX_EVENT(StyledTextEvent, EvtSciCalltipClick, wxEVT_STC_CALLTIP_CLICK);

//static 
acdk::cfgscript::RProps 
StyledTextCtrl::_globalProps = Nil;

//static 
void 
StyledTextCtrl::loadStdConfig()
{
  loadConfig(System::getAcdkToolsHome() + acdk::io::File::separator() + "cfg/csf/acdk/wx/ide/StyledTextCtrConfig.csf");
}

using namespace acdk::cfgscript;

//static 
void 
StyledTextCtrl::loadConfig(IN(RString) csfFile)
{
  acdk::io::File f(csfFile);
  if (f.exists() == false)
    return;
  RScript script = new Script(csfFile);
  RProps sysProps = new Props(PropsNoFlags, Nil, false);//init as enviroment
  Script::initAsEnvProps(sysProps);
  _globalProps = new Props(PropsParentRead | PropsNoParentWrite, sysProps);
  sysProps->setObjectVal("StyledTextCtrConfig", &_globalProps);
  //sysProps->setObjectVal("editor", this);
  
  try {
    script->readEval(_globalProps, PropsParentRead | PropsParentWrite);
    //_globalProps = (RProps)_globalProps->getObjectVal("StyledTextCtrConfig");
    System::out->println("Initialized: " + _globalProps->asCfgScriptLiteral("StyledTextCtrConfig", "", PropsNoParentRead));

    //System::out->println("Initialized: " + _globalProps->toString());
  } catch (RThrowable ex) {
    System::out->println("Cannot initialize configuration: " + csfFile + "\n" + ex->getMessage());
    System::out->println(Script::getScriptBackTrace());
  }
}

void 
StyledTextCtrl::initFromProps(IN(::acdk::cfgscript::RProps) props)
{
  if (props->hasValue("BufferedDraw") == true)
    setBufferedDraw(props->getBoolVal("BufferedDraw"));
  if (props->hasValue("Indent") == true)
    setIndent(props->getIntVal("Indent"));
  setBackSpaceUnIndents(true);
  setTabIndents(true);
  if (props->hasValue("TabWidth") == true)
    setTabWidth(props->getIntVal("TabWidth"));
  
  if (props->hasValue("UseTabs") == true)
    setUseTabs(props->getBoolVal("UseTabs"));
  //if (props->hasValue("TabIndents") == true)
   // setTabIndents(props->getBoolVal("TabIndents"));

  if (props->hasValue("LayoutCache") == true)
    setLayoutCache(props->getIntVal("LayoutCache"));
  if (props->hasValue("CaretLineVisible") == true)
    setCaretLineVisible(props->getBoolVal("CaretLineVisible"));
  if (props->hasValue("KeyMapping") == true)
  {
    typedef ObjectArrayImpl<RintArray> intArrayArray;
    typedef RObjectArrayImpl<RintArray> RintArrayArray;

    // currently casting not supported 
    //RintArrayArray mapping = (RintArrayArray)props->getObjectVal("KeyMapping");
    RDmiObjectArray oa = (RDmiObjectArray)props->getObjectVal("KeyMapping");
    for (int i = 0; i < oa->length(); ++i)
    {
      RDmiObjectArray ia = (RDmiObjectArray)oa[i]->getObjectVar();
      if (ia->length() == 3)
      {
        int keyCode = ia[0]->getIntVar();
        int mod = ia[1]->getIntVar();
        int cmd = ia[2]->getIntVar();
        cmdKeyAssign(keyCode, mod, cmd);
      }
    }


  }
}


int 
StyledTextCtrl::_initState(IN(::acdk::cfgscript::RProps) statepprops, int state)
{
  if (state == -1)
  {
    if (statepprops->hasValue("state") == false)
      return -1;
    state = statepprops->getIntVal("state");
  }
  if (statepprops->hasValue("colorfg") == true)
    styleSetForeground(state, new Colour(statepprops->getIntVal("colorfg")));
  if (statepprops->hasValue("colorbg") == true)
    styleSetForeground(state, new Colour(statepprops->getIntVal("colorbg")));
  if (statepprops->hasValue("font") == true)
  {
    RProps fontProps = (RProps)statepprops->getObjectVal("font");
    int size = fontProps->getIntVal("size", PropsNoWarnRead);
    RString name = fontProps->getStringVal("name", PropsNoWarnRead);
    RFont f = new Font(size, GdiDefault, GdiNormal, GdiBold, false, name);
    styleSetFont(state, f);
  }
  return state;
}

void 
StyledTextCtrl::initFromTextFile(IN(RString) fileName)
{
  if (_globalProps == Nil)
    return;
  initFromProps(_globalProps);
  if (_globalProps->hasValue("fileExtions") == false)
    return;
  int idx = fileName->lastIndexOf('.');
  if (idx == -1)
    return;
  RString ext = fileName->substr(idx + 1);
  RProps langprops = (RProps)_globalProps->getObjectVal("fileExtions");
  if (langprops->hasValue(ext) == false)
    return;
  langprops = (RProps)langprops->getObjectVal(ext);
  
  styleClearAll();
  setLexer(langprops->getIntVal("lexer"));
  if (langprops->hasValue("keywords") == true)
    setKeyWords(0, langprops->getStringVal("keywords"));
  if (langprops->hasValue("syntax") == false)
    return;
  RProps syntax = (RProps)langprops->getObjectVal("syntax");
  RStringArray skeys = syntax->getKeys();
  int maxStyles = langprops->getIntVal("maxStyles");
  boolArray isSet(maxStyles);
  {
    for (int i = 0; i < isSet.length(); ++i)
      isSet[i] = false;
  }
  for (int i = 0; i < skeys->length(); ++i)
  {
    RProps statepprops = (RProps)syntax->getObjectVal(skeys[i]);
    int state = _initState(statepprops, -1);
    if (state == -1)
      continue;
    isSet[state] = true;
    /*
    if (statepprops->hasValue("state") == false)
      continue;
    int state = statepprops->getIntVal("state");
    isSet[state] = true;
    if (statepprops->hasValue("colorfg") == true)
      styleSetForeground(state, new Colour(statepprops->getIntVal("colorfg")));
    if (statepprops->hasValue("colorbg") == true)
      styleSetForeground(state, new Colour(statepprops->getIntVal("colorbg")));
    if (statepprops->hasValue("font") == true)
    {
      RProps fontProps = (RProps)statepprops->getObjectVal("font");
      int size = fontProps->getIntVal("size", PropsNoWarnRead);
      RString name = fontProps->getStringVal("name", PropsNoWarnRead);
      RFont f = new Font(size, GdiDefault, GdiNormal, GdiBold, false, name);
      styleSetFont(state, f);
    }
    */
    /*
    styleSetForeground(SciCString, new Colour(0, 0, 0xFF));
    styleSetForeground(SciCCharacter, new Colour(0, 0, 0xFF));
    
    styleSetForeground(SciCWord, new Colour(222, 0, 0));
    styleSetForeground(SciCWord2, new Colour(222, 0, 0));
    */
  }
  RProps defaultStyle = (RProps)langprops->getObjectVal("defaultStyle");
  if (defaultStyle != Nil)
  {
    for (int i = 0; i < isSet.length(); ++i)
    {
      if (isSet[i] == false)
        _initState(defaultStyle, i);
    }
  }
  
}


} // namespace ide
} //namespace wx
} // namespace acdk

