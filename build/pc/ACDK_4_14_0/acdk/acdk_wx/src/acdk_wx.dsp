# Microsoft Developer Studio Project File - Name="acdk_wx" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=acdk_wx - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "acdk_wx.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "acdk_wx.mak" CFG="acdk_wx - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "acdk_wx - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE "acdk_wx - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library" 
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "acdk_wx - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_wx_r"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_wx_r"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /Gi- /GR /EHsc /O2  /D "OS_WIN32" /D "IN_ACDK_WX_LIB" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MT" /D "_USRDLL" /I "."  /I "$(ACDK_WX_INCLUDE)"  /I "$(ACDK_WX_INCLUDE_PLATTFORM)"  /I "$(ACDK_WX_INCLUDE_CONTRIB)"  /I "../../include"  /Zm400 /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_r.lib /nologo /debug /debugtype:coff /machine:I386  /dll  /out:"..\..\bin\acdk_wx_r.dll"  /libpath:"$(ACDK_WX_LIB_PATH)" $(ACDK_WX_WXLIBS_RELEASE)
# SUBTRACT LINK32 /pdbtype:<none>

!ELSEIF  "$(CFG)" == "acdk_wx - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\..\bin"
# PROP BASE Intermediate_Dir "..\tobj\acdk_wx_d"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\bin"
# PROP Intermediate_Dir "..\tobj\acdk_wx_d"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GR /EHsc /Zi /Od  /D "OS_WIN32" /D "IN_ACDK_WX_LIB" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MT" /D "ACDK_DEBUG" /D "_USRDLL" /I "."  /I "$(ACDK_WX_INCLUDE)"  /I "$(ACDK_WX_INCLUDE_PLATTFORM)"  /I "$(ACDK_WX_INCLUDE_CONTRIB)"  /I "../../include"  /FD /Zm400 /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib ..\..\bin\acdk_core_d.lib /nologo /debug /pdbtype:sept /machine:I386  /dll  /out:"..\..\bin\acdk_wx_d.dll"  /libpath:"$(ACDK_WX_LIB_PATH)" $(ACDK_WX_WXLIBS_DEBUG)

!ENDIF 

# Begin Target

# Name "acdk_wx - Win32 Release"
# Name "acdk_wx - Win32 Debug"


# Begin Group "acdk"
# PROP Default_Filter ""
# Begin Group "wx"
# PROP Default_Filter ""

# Begin Source File
SOURCE=acdk/wx\App.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ArtProvider.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Bitmap.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\BitmapButton.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\BitmapDataObject.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\BoxSizer.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Button.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Caret.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\CheckBox.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Choice.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ClientData.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ClientDC.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Clipboard.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Colour.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ColourData.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ColourDialog.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ComboBox.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Config.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Control.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ControlWithItems.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Cursor.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\DataFormat.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\DataObject.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\DataObjectComposite.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\DataObjectSimple.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\DC.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Dialog.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\DirDialog.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\DropSource.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\DropTarget.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Event.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\FileDataObject.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\FileDialog.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\FileDropTarget.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Font.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\FontData.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\FontDialog.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Frame.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Gauge.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\GDIImage.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\GDIObject.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\HtmlWindow.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Icon.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\LayoutConstraints.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ListBox.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\MDIChildFrame.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\MDIClientWindow.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\MDIParentFrame.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\MemoryDC.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Menu.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\MenuBar.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\MenuItem.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Notebook.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\PaintDC.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Panel.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Pen.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\PostScriptDC.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ProgressDialog.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\RadioBox.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\RadioButton.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Region.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ScreenDC.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ScrolledWindow.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\SingleChoiceDialog.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Sizer.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Slider.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\SpinButton.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\SpinCtrl.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\SplitterWindow.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\StaticBitmap.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\StaticBox.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\StaticText.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\StatusBar.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Structs.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\TextCtrl.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\TextCtrlCharWriter.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\TextDataObject.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\TextDropTarget.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\TextEntryDialog.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Timer.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ToggleButton.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ToolBar.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\ToolTip.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\TreeCtrl.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Validator.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Window.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\WindowDC.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\WindowStyle.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\Wizard.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\WizardPage.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\wx.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\WxObject.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\XmlResource.h
# End Source File

# Begin Source File
SOURCE=acdk/wx\App.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx\Event.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx\Frame.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx\Notebook.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx\TreeCtrl.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx\Window.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx\WxObject.cpp
# End Source File

# Begin Source File
SOURCE=acdk/wx\wx_metainf_base.cpp
# End Source File
# end Group
# end Group


# End Target
# End Project
