PACKAGENAME=aal

all:

%::
	target=$@; \
	if test "x$$OSTYPE" = "xcygwin"; then \
	  $(MAKE) -f $(PACKAGENAME).linux $$target; \
	fi; \
	if test "x$$OSTYPE" = "xlinux" -o "x$$OSTYPE" = "xlinux-gnu" ; then \
	  $(MAKE) -f $(PACKAGENAME).linux $$target; \
	fi; \
	if test "x$$OSTYPE" = "xsolaris"; then \
	  $(MAKE) -f $(PACKAGENAME).sunos-gcc $$target; \
	fi

