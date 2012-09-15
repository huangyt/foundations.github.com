
REM Settings for acdk_wx module



goto use_wx_25

set ACDK_WX_PATH=C:\programr\lang\c++\lib\gui\wxWindows-2.4.2
set ACDK_WX_LIB_PATH=%ACDK_WX_PATH%\lib
set ACDK_WX_INCLUDE=%ACDK_WX_PATH%\include
set ACDK_WX_INCLUDE_PLATTFORM=%ACDK_WX_PATH%\include\lib\mswdllud
set ACDK_WX_CORELIB_RELEASE=wxmsw24u
set ACDK_WX_CORELIB_DEBUG=wxmsw24ud
set ACDK_WX_XRCLIB_RELEASE=wxxrcu
set ACDK_WX_XRCLIB_DEBUG=wxxrcud
set ACDK_WX_FLLIB_RELEASE=fldllu
set ACDK_WX_FLLIB_DEBUG=fldllud
set ACDK_WX_STCLIB_RELEASE=
set ACDK_WX_STCLIB_DEBUG=
goto end_of_wx


:use_wx_25

set ACDK_WX_PATH=c:\d\artefaktur\extsrc\wxWidgets-2.6.0
set ACDK_WX_VERS=26

set WXSTABLE=%ACDK_WX_PATH%
set WXDEVEL=%ACDK_WX_PATH%
set WXWIN=%ACDK_WX_PATH%
REM used to build wyoEditor
set WXCODE=C:\d\artefaktur\extsrc\wxCode\components
set ACDK_WX_WXLIBS_RELEASE=wxmsw%ACDK_WX_VERS%u_xrc.lib wxmsw%ACDK_WX_VERS%u_html.lib wxmsw%ACDK_WX_VERS%u_adv.lib wxregexu.lib wxmsw%ACDK_WX_VERS%u_core.lib wxbase%ACDK_WX_VERS%u.lib 
REM standard
set ACDK_WX_WXLIBS_DEBUG=wxmsw%ACDK_WX_VERS%ud_xrc.lib wxmsw%ACDK_WX_VERS%ud_html.lib wxmsw%ACDK_WX_VERS%ud_adv.lib  wxregexud.lib wxmsw%ACDK_WX_VERS%ud_core.lib wxbase%ACDK_WX_VERS%ud.lib 
REM universal
REM set ACDK_WX_WXLIBS_DEBUG=wxmswuniv%ACDK_WX_VERS%ud_xrc.lib wxmswuniv%ACDK_WX_VERS%ud_html.lib wxmswuniv%ACDK_WX_VERS%ud_adv.lib  wxregexud.lib wxmswuniv%ACDK_WX_VERS%ud_core.lib wxbase%ACDK_WX_VERS%ud.lib 
REM wxtiffd.lib wxjpegd.lib wxpngd.lib wxzlibd.lib wxmsw%ACDK_WX_VERS%u_adv.lib 

set ACDK_WX_IDE_WXLIBS_RELEASE=%ACDK_WX_WXLIBS_DEBUG% wxmsw%ACDK_WX_VERS%u_fl.lib wxmsw%ACDK_WX_VERS%u_stc.lib
set ACDK_WX_IDE_WXLIBS_DEBUG=%ACDK_WX_WXLIBS_DEBUG% wxmsw%ACDK_WX_VERS%ud_stc.lib wxmsw%ACDK_WX_VERS%ud_fl.lib

set ACDK_WX_LIB_PATH=%ACDK_WX_PATH%\lib\vc_dll
set ACDK_WX_INCLUDE=%ACDK_WX_PATH%\include

REM standard
set ACDK_WX_INCLUDE_PLATTFORM=%ACDK_WX_PATH%\lib\vc_dll\mswud
REM universal
REM set ACDK_WX_INCLUDE_PLATTFORM=%ACDK_WX_PATH%\lib\vc_dll\mswunivud

set ACDK_WX_INCLUDE_CONTRIB=%ACDK_WX_PATH%\contrib\include
SET PATH=%PATH%;%ACDK_WX_LIB_PATH%
:end_of_wx

REM =================================================================
REM Python settings for the acdk_python module
REM Normally only ajust ACDK_PYTHON_PATH variable

set ACDK_PYTHON_PATH=C:\programr\lang\python\Python-2.4
set ACDK_PYTHON_INCLUDE=%ACDK_PYTHON_PATH%\include
set ACDK_PYTHON_INCLUDE_PLATFROM=%ACDK_PYTHON_PATH%\PC

REM If compile at yourself
set ACDK_PYTHON_LIB_PATH=%ACDK_PYTHON_PATH%\PC\VC6
REM else
REM set ACDK_PYTHON_LIB_PATH=%ACDK_PYTHON_PATH%\libs

REM If compile at yourself
set ACDK_PYTHON_PYLIBS_DEBUG=python24_d.lib
set ACDK_PYTHON_PYLIBS_RELEASE=python24.lib
REM else
REM set ACDK_PYTHON_PYLIBS_DEBUG=python24.lib
REM set ACDK_PYTHON_PYLIBS_RELEASE=python24.lib

set PATH=%PATH%;%ACDK_PYTHON_LIB_PATH%


REM =================================================================
REM OpenSSL settings
REM 
set ACDK_OPENSSL_PATH=C:\d\artefaktur\extsrc\openssl-0.9.7d
set ACDK_OPENSSL_INCLUDE=%ACDK_OPENSSL_PATH%\include
set ACDK_OPENSSL_LIBPATH=%ACDK_OPENSSL_PATH%\out32dll\Debug
set ACDK_OPENSSL_LIBS=libeay32.lib SSLeay32.lib
set PATH=%PATH%;%ACDK_OPENSSL_LIB%


REM =================================================================
REM Perl settings
REM 
set ACDK_PERL_INCLUDE=C:/programr/lang/perl/interpreter/perl-5.6.0/lib/CORE
set ACDK_PERL_LIBPATH=C:/programr/lang/perl/interpreter/perl-5.6.0/lib/CORE
set ACDK_PERL_LIB=perl56

REM =================================================================
REM Java settings
REM 
set ACDK_JAVA_PATH=C:\Programme\Java\j2sdk1.4.1_06
REM set ACDK_JAVA_PATH=c:\programr\lang\java\jdk\jdk1.3.1

set ACDK_JAVA_INCLUDE=%ACDK_JAVA_PATH%\include
set ACDK_JAVA_INCLUDE_WIN32=%ACDK_JAVA_INCLUDE%\win32
set ACDK_JAVA_LIBDIR=%ACDK_JAVA_PATH%\lib
set ACDK_JAVA_JVM_LIB=jvm

set ACDK_JAVA_JVM_PATH=%ACDK_JAVA_PATH%\bin;%ACDK_JAVA_PATH%\jre\bin;%ACDK_JAVA_PATH%\jre\bin\client
set PATH=%PATH%;%ACDK_JAVA_JVM_PATH%



REM =================================================================
REM Tcl settings
REM 
set ACDK_TCL_PATH=C:\programr\lang\tcl\Tcl\Tcl_8.3
set ACDK_TCL_LIB=tcl83
set ACDK_TCL_INCLUDE=%ACDK_TCL_PATH%\include

set ACDK_TCL_LIBDIR=%ACDK_TCL_PATH%\lib
set ACDK_TCL_BINDIR=%ACDK_TCL_PATH%\bin
set PATH=%PATH%;%ACDK_TCL_BINDIR%


