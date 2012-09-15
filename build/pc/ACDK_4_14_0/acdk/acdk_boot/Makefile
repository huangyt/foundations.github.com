PACKAGENAME=acdk_boot

all:

%::
	@target=$@; \
	if test "x$$OSTYPE" = "xcygwin"; then \
	  $(MAKE) -f $(PACKAGENAME).linux $$target; \
	elif test "x$$OSTYPE" = "xlinux" -o "x$$OSTYPE" = "xlinux-gnu" ; then \
	  $(MAKE) -f $(PACKAGENAME).linux $$target; \
	elif test "x$$OSTYPE" = "xsolaris"; then \
	  $(MAKE) -f $(PACKAGENAME).sunos-gcc $$target; \
	elif test "x$$OS" = "xWindows_NT"; then \
		$(MAKE) -f $(PACKAGENAME).mingw $$target; \
	fi


