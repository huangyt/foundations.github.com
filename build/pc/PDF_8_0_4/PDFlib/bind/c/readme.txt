Notes on the PDFlib C binding
-----------------------------

Applications which use the PDFlib binding for C must be linked with
a C++ compiler since the PDFlib library includes some parts which are
implemented in C++. Using a C linker may result in unresolved externals
unless the application is explicitly linked against the required C++
support libraries.

Supported compiler/linker versions for all supported platforms are
listed in system-requirements.txt. This document also lists additional
libraries which may be required for linking PDFlib applications on some
platforms.
