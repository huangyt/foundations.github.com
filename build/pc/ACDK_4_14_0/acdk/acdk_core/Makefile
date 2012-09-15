PACKAGENAME=acdk_core


all:

%::
	@target=$@; \
	if test "x$$OSTYPE" = "xcygwin"; then \
	  $(MAKE) -f $(PACKAGENAME).linux $$target; \
	elif test "x$$OSTYPE" = "xlinux" -o "x$$OSTYPE" = "xlinux-gnu" ; then \
	  $(MAKE) -f $(PACKAGENAME).linux $$target; \
	elif test "x$$OSTYPE" = "xsolaris"; then \
	  $(MAKE) -f $(PACKAGENAME).sunos-gcc $$target; \
	elif test "x$$OSTYPE" = "xfreebsd4.3"; then \
	  $(MAKE) -f $(PACKAGENAME).bsd $$target; \
	fi

