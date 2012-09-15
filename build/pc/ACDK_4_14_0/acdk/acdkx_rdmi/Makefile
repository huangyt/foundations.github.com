PACKAGENAME=acdkx_rdmi

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


