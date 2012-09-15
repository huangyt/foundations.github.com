#ifndef libxml2_config_h
#define libxml2_config_h

#include <acdk/Compiler.h>
#include <acdk/Platform.h>

#if defined(ACDK_OS_WIN32)
# include "win32config.h"
#else
# include <acdk/net/TCPSocket.h>
#endif


/*
#if defined(WITH_TRIO)
# undef WITH_TRIO
#endif
#define WITHOUT_TRIO

#include <stdio.h>
*/

#endif

