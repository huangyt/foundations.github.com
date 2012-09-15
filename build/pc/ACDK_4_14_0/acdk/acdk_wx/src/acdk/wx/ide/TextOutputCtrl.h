
#ifndef acdk_wx_ide_TextOutputCtrl_h
#define acdk_wx_ide_TextOutputCtrl_h

#include "ide.h"
#include <acdk/wx/TextCtrl.h>

namespace acdk {
namespace wx {
namespace ide {

using namespace acdk::wx;

ACDK_DECL_CLASS(TextOutputCtrl);
/**
  TextCtrl which implements the acdk::io::CharWriter
  interface
*/
class ACDK_WX_IDE_PUBLIC TextOutputCtrl
: extends acdk::wx::TextCtrl
, implements acdk::io::CharWriter
{
  ACDK_WITH_METAINFO(TextOutputCtrl)
private:
  RStringBuffer _buffer;
  int _internalId;
public:

  TextOutputCtrl(IN(RWindow) parent, int id, IN(RString) value = "", IN(RPoint) pos = Point::defaultPosition(),
           IN(RSize) size = Size::defaultSize(), int style = 0)
  : TextCtrl(parent, id, value, pos, size, style)
  , _buffer(new StringBuffer())
  {
    _internalId = getFreeId();
    connect(CommandEvent::EvtCommandMenuSelected, _internalId, (ObjectEventFunction)&TextOutputCtrl::onUpdate);
  }
  virtual void writeChar(char c)
  {
    SYNCHRONIZETHIS();
    _buffer->append(c);
    if (c == '\n')
      flush();
  }

  virtual void writeChar(ucchar c)
  {
    SYNCHRONIZETHIS();
    _buffer->append(c);
    if (c == '\n')
      flush();
  }
  foreign virtual void writeString(const char* cstr) 
  {
    SYNCHRONIZETHIS();
    _buffer->append(cstr);
    if (strchr(cstr, '\n') != 0)
      flush();
  }
  foreign virtual void writeString(const ucchar* cstr)
  {
    writeString(RString(cstr));
  }
  virtual void writeString(IN(RString) str) 
  {
    SYNCHRONIZETHIS();
    _buffer->append(str);
    if (str->indexOf('\n') != -1)
      flush();
  }
  virtual void flush() 
  {
    addPendingEvent(new CommandEvent(CommandEvent::EvtCommandMenuSelected, _internalId));
  }
  void onUpdate(IN(REvent) event)
  {
    SYNCHRONIZETHIS();
     RString text = _buffer->toString();
#if defined(ACDK_OS_WIN32)
     //text = text->replace("\n", "\r\n");
#endif
    _buffer->reset();
    appendText(text);
  }
  virtual void close()
  {
    flush();
  }
  
};

} // namespace ide
} //namespace wx
} // namespace acdk

#endif //acdk_wx_ide_TextOutputCtrl_h
