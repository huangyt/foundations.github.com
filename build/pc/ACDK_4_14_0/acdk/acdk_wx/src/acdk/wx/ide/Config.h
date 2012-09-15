
#ifndef acdk_wx_ide_Config_h
#define acdk_wx_ide_Config_h

#include <acdk/wx/wx.h>

#include <acdk.h>

#if defined(ACDK_NEED_DLLEXPORT)
# if defined(IN_ACDK_WX_IDE_LIB)
#   define ACDK_WX_IDE_PUBLIC __declspec(dllexport)
# elif defined(ACDK_STATIC_LIB)
# 	define ACDK_WX_IDE_PUBLIC
# else
#   define ACDK_WX_IDE_PUBLIC __declspec(dllimport)
# endif
#else
# define ACDK_WX_IDE_PUBLIC
#endif


namespace acdk {
namespace wx {
/**
  contains gui classes for building IDE's and other complex 
  GUI applications
*/
namespace ide {

} // namespace ide
} //namespace wx
} // namespace acdk

#endif //acdk_wx_ide_Config_h
