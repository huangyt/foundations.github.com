# ---------------------------------------------------------------------------
!if !$d(BCB)
BCB = $(MAKEDIR)\..
!endif

# ---------------------------------------------------------------------------
# IDE-ABSCHNITT
# ---------------------------------------------------------------------------
# Der folgende Abschnitt der Make-Datei des Projekts wird von der IDE verwaltet.
# Wir empfehlen, Änderungen an diesem Abschnitt nur über die IDE
# vorzunehmen.
# ---------------------------------------------------------------------------

VERSION = BCB.05.03
# ---------------------------------------------------------------------------
PROJECT = acdk_core_dll.dll
OBJFILES = d:\programr\lang\c++\ide\BCB5\CBuilder5\Projects\Unit1.obj \
    acdk\lang\Void.obj acdk\lang\Boolean.obj acdk\lang\Byte.obj \
    acdk\lang\Character.obj acdk\lang\Class.obj acdk\lang\ClassLoader.obj \
    acdk\lang\ClazzInfo.obj acdk\lang\Double.obj acdk\lang\Float.obj \
    acdk\lang\Integer.obj acdk\lang\Lang.obj acdk\lang\lang_clazzinfo.obj \
    acdk\lang\Long.obj acdk\lang\Math.obj acdk\lang\Object.obj \
    acdk\lang\ObjectDebug.obj acdk\lang\Package.obj acdk\lang\Process.obj \
    acdk\lang\Runtime.obj acdk\lang\SharedLibrary.obj acdk\lang\Short.obj \
    acdk\lang\String.obj acdk\lang\StringBuffer.obj acdk\lang\System.obj \
    acdk\lang\Thread.obj acdk\lang\ThreadGroup.obj acdk\lang\Throwable.obj \
    acdk\lang\acdk_lang_Package.obj acdk\lang\ref\ReferenceQueue.obj \
    acdk\lang\ref\NotifyObjectEvent.obj acdk\lang\ref\ref_clazzinfo.obj \
    acdk\lang\ref\Reference.obj acdk\lang\ref\acdk_lang_ref_Package.obj \
    acdk\lang\sys\TracedRawAllocator.obj acdk\lang\sys\BasicArray.obj \
    acdk\lang\sys\BitmapPagedAllocator.obj acdk\lang\sys\core_condition.obj \
    acdk\lang\sys\core_fastmutex.obj acdk\lang\sys\core_mutex.obj \
    acdk\lang\sys\Fields.obj acdk\lang\sys\Garbage_Heap.obj \
    acdk\lang\sys\ObjectArrayImpl.obj acdk\lang\sys\ObjectHeap.obj \
    acdk\lang\sys\ObjectLockPool.obj acdk\lang\sys\PagedAllocator.obj \
    acdk\lang\sys\PagedHeap.obj acdk\lang\sys\RC_GC_Heap.obj \
    acdk\lang\sys\RC_Heap.obj acdk\lang\sys\ScriptVar.obj \
    acdk\lang\sys\sys_clazzinfo.obj acdk\lang\sys\TraceCallStack.obj \
    acdk\lang\sys\Allocator.obj
RESFILES = 
MAINSOURCE = acdk_core_dll.bpf
RESDEPEN = $(RESFILES)
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = Vcl50.lib
PACKAGES = Vcl50.bpi Vclx50.bpi bcbsmp50.bpi Vcldb50.bpi ibsmp50.bpi VCLBDE50.bpi \
    vcldbx50.bpi Qrpt50.bpi TeeUI50.bpi TeeDB50.bpi Tee50.bpi Dss50.bpi \
    TeeQR50.bpi VCLIB50.bpi Vclmid50.bpi vclie50.bpi Inetdb50.bpi Inet50.bpi \
    NMFast50.bpi webmid50.bpi bcbie50.bpi dclocx50.bpi bcb2kaxserver50.bpi
SPARELIBS = Vcl50.lib
DEFFILE = 
# ---------------------------------------------------------------------------
PATHCPP = .;d:\programr\lang\c++\ide\BCB5\CBuilder5\Projects\;acdk\lang;acdk\lang\ref;acdk\lang\sys
PATHASM = .;
PATHPAS = .;
PATHRC = .;
DEBUGLIBPATH = $(BCB)\lib\debug
RELEASELIBPATH = $(BCB)\lib\release
USERDEFINES = OS_WIN32,ACDK_IN_ACDK_LIB,WIN32,_DEBUG,_WINDOWS,_MT,ACDK_USE_GC,ACDK_DEBUG,_USRDLL
SYSDEFINES = NO_STRICT;_NO_VCL
INCLUDEPATH = .;d:\programr\lang\c++\ide\BCB5\CBuilder5\Projects\;$(BCB)\include;$(BCB)\include\vcl
LIBPATH = .;d:\programr\lang\c++\ide\BCB5\CBuilder5\Projects\;$(BCB)\lib\obj;$(BCB)\lib
WARNINGS= -w-par
# ---------------------------------------------------------------------------
CFLAG1 = -WD -Od -H=$(BCB)\lib\vcl50.csm -Hc -Vx -Ve -X- -A -r- -a8 -6 -b- -k -y -v \
    -vi- -tWD -tWM -c
IDLCFLAGS = -Iacdk\lang\sys -Iacdk\lang\ref -Iacdk\lang -I. \
    -Id:\programr\lang\c++\ide\BCB5\CBuilder5\Projects\. -I$(BCB)\include \
    -I$(BCB)\include\vcl -src_suffix cpp \
    -DOS_WIN32,ACDK_IN_ACDK_LIB,WIN32,_DEBUG,_WINDOWS,_MT,ACDK_USE_GC,ACDK_DEBUG,_USRDLL \
    -boa
PFLAGS = -$YD -$W -$O- -v -JPHNE -M
RFLAGS = 
AFLAGS = /mx /w2 /zd
LFLAGS = -D"" -aa -Tpd -x -Gn -Gi -v
# ---------------------------------------------------------------------------
ALLOBJ = c0d32.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cw32mt.lib
# ---------------------------------------------------------------------------
!ifdef IDEOPTIONS

[Version Info]
IncludeVerInfo=0
AutoIncBuild=0
MajorVer=1
MinorVer=0
Release=0
Build=0
Debug=0
PreRelease=0
Special=0
Private=0
DLL=0

[Version Info Keys]
CompanyName=
FileDescription=
FileVersion=1.0.0.0
InternalName=
LegalCopyright=
LegalTrademarks=
OriginalFilename=
ProductName=
ProductVersion=1.0.0.0
Comments=

[Debugging]
DebugSourceDirs=$(BCB)\source\vcl

!endif





# ---------------------------------------------------------------------------
# MAKE-ABSCHNITT
# ---------------------------------------------------------------------------
# Der folgende Abschnitt der Datei des Projekts wird nicht von der IDE verwendet. Er ist
# für das Erstellen von der Befehlszeile mit dem Programm MAKE gedacht.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------
!if "$(USERDEFINES)" != ""
AUSERDEFINES = -d$(USERDEFINES:;= -d)
!else
AUSERDEFINES =
!endif

!if !$d(BCC32)
BCC32 = bcc32
!endif

!if !$d(CPP32)
CPP32 = cpp32
!endif

!if !$d(DCC32)
DCC32 = dcc32
!endif

!if !$d(TASM32)
TASM32 = tasm32
!endif

!if !$d(LINKER)
LINKER = ilink32
!endif

!if !$d(BRCC32)
BRCC32 = brcc32
!endif


# ---------------------------------------------------------------------------
!if $d(PATHCPP)
.PATH.CPP = $(PATHCPP)
.PATH.C   = $(PATHCPP)
!endif

!if $d(PATHPAS)
.PATH.PAS = $(PATHPAS)
!endif

!if $d(PATHASM)
.PATH.ASM = $(PATHASM)
!endif

!if $d(PATHRC)
.PATH.RC  = $(PATHRC)
!endif
# ---------------------------------------------------------------------------
$(PROJECT): $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) -L$(LIBPATH) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES)
!
# ---------------------------------------------------------------------------
.pas.hpp:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.pas.obj:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.cpp.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.cpp.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.asm.obj:
    $(BCB)\BIN\$(TASM32) $(AFLAGS) -i$(INCLUDEPATH:;= -i) $(AUSERDEFINES) -d$(SYSDEFINES:;= -d) $<, $@

.rc.res:
    $(BCB)\BIN\$(BRCC32) $(RFLAGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -fo$@ $<
# ---------------------------------------------------------------------------




