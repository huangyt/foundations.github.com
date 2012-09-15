call acdkenv.cmd
PATH=C:\Programme\VisualStudio.NET2003\Vc7\bin;C:\Programme\VisualStudio.NET2003\Common7\IDE;%PATH%
set ACDKHOME=c:\d\artefaktur\vc7acdk
set INCLUDE=C:\Programme\VisualStudio.NET2003\Vc7\include;C:\Programme\VisualStudio.NET2003\Vc7\PlatformSDK\Include;C:\Programme\VisualStudio.NET2003\Vc7\FrameworkSDK\include
set LIB=C:\Programme\VisualStudio.NET2003\Vc7\lib;C:\Programme\VisualStudio.NET2003\Vc7\PlatformSDK\lib;C:\VStudio.NET\FrameworkSDK\Lib

start devenv.exe %1
