PACKAGENAME=acdk_net

all:

%::
	@target=$@; \
	if test "x$$OSTYPE" = "xcygwin"; then \
	  $(MAKE) -f $(PACKAGENAME).linux $$target; \
	fi; \
	if test "x$$OSTYPE" = "xlinux" -o "x$$OSTYPE" = "xlinux-gnu" ; then \
	  $(MAKE) -f $(PACKAGENAME).linux $$target; \
	fi; \
	if test "x$$OSTYPE" = "xsolaris"; then \
	  $(MAKE) -f $(PACKAGENAME).sunos-gcc $$target; \
	fi; \
	if test "x$$OS" = "xWindows_NT"; then \
		$(MAKE) -f $(PACKAGENAME).mingw $$target; \
	fi

ACDKHOME=/artefaktur/acdk
DISTRHOME=$(ACDKHOME)/../distrib

SOEXT=_d.dll
EXEEXT=_d.exe
CFGEXT=_d.cfg
acdk_net_srsync-win32-distrib::
	tar zcf $(DISTRHOME)/acdk_net_srsync.tgz \
    -C $(ACDKHOME)/bin acdk_core$(SOEXT) \
    -C $(ACDKHOME)/bin acdk_security$(SOEXT) \
    -C $(ACDKHOME)/bin acdk_net$(SOEXT) \
    -C $(ACDKHOME)/bin acdk_net_srfsys$(SOEXT) \
    -C $(ACDKHOME)/bin acdk_net_srsync$(EXEEXT) \
    -C $(ACDKHOME)/bin acdk_net_srsync$(CFGEXT) \
    -C $(ACDKHOME)/acdk_net/cfg srsync_server.cmd \
    -C $(ACDKHOME)/acdk_net/cfg srsync_client.cmd \
    
acdk_net_srsync-win32-distrib-no::
	zip -D $(DISTRHOME)/acdk_net_srsync.zip \
    $(ACDKHOME)/bin/acdk_core$(SOEXT) \
    $(ACDKHOME)/bin/acdk_security$(SOEXT) \
    $(ACDKHOME)/bin/acdk_net$(SOEXT) \
    $(ACDKHOME)/bin/acdk_net_srfsys$(SOEXT) \
    $(ACDKHOME)/bin/acdk_net_srsync$(EXEEXT) \
    $(ACDKHOME)/bin/acdk_net_srsync$(CFGEXT) \
    $(ACDKHOME)/acdk_net/cfg/srsync_server.cmd \
    $(ACDKHOME)/acdk_net/cfg/srsync_client.cmd \
    

#-c 192.168.120.254:7777 -sync -r h:\ourmp3\mp3rooot -l D:\mp3\mp3root    