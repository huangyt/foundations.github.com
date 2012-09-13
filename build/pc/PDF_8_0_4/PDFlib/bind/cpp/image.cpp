// $Id: image.cpp,v 1.24 2009/07/23 09:40:53 stm Exp $
//
// PDFlib client: image example in C++
//

#include <iostream>
#include <string>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    try {
	PDFlib p;
	int image;
	const wstring imagefile = L"nesrin.jpg";
	// This is where font/image/PDF input files live. Adjust as necessary. 
	const wstring searchpath = L"../data";

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	p.set_parameter(L"SearchPath", searchpath);

	if (p.begin_document(L"image.pdf", L"") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	p.set_info(L"Creator", L"image.cpp");
	p.set_info(L"Author", L"Thomas Merz");
	p.set_info(L"Title", L"image sample (C++)!");

	image = p.load_image(L"auto", imagefile, L"");

	if (image == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
	    return 3;
	}

	// dummy page size, will be adjusted by PDF_fit_image()
	p.begin_page_ext(10, 10, L"");
	p.fit_image(image, 0.0, 0.0, L"adjustpage");
	p.close_image(image);
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
