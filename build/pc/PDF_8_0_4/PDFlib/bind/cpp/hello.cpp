// $Id: hello.cpp,v 1.24 2009/07/29 07:22:54 stm Exp $
//
// PDFlib client: hello example in C++
//

#include <iostream>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    try {
	int font;
	PDFlib p;

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	if (p.begin_document(L"hello.pdf", L"") == -1) {
	    wcerr << "Error: " << p.get_errmsg() << endl;
	    return 2;
	}

	p.set_info(L"Creator", L"hello.cpp");
	p.set_info(L"Author", L"Thomas Merz");
	p.set_info(L"Title", L"Hello, world (C++)!");

	p.begin_page_ext(a4_width, a4_height, L"");

	// Change "host" encoding to "winansi" or whatever you need!
	font = p.load_font(L"Helvetica-Bold", L"host", L"");
	if (font == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
	    return(2);
	}
	p.setfont(font, 24);
	p.set_text_pos(50, 700);
	p.show(L"Hello, world!");
	p.continue_text(L"(says C++)");
	p.end_page_ext(L"");

	p.end_document(L"");
    }
    catch (PDFlib::Exception &ex) {
	wcerr << L"PDFlib exception occurred in hello sample: " << endl
	      << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
	      << L": " << ex.get_errmsg() << endl;
	return 2;
    }

    return 0;
}
