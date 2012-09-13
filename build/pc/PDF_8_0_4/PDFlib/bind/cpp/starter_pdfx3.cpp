/* $Id: starter_pdfx3.cpp,v 1.3 2009/09/09 08:18:06 stm Exp $
 *
 * PDF/X-3 starter:
 * Create PDF/X-3 conforming output
 *
 * Required software: PDFlib/PDFlib+PDI/PPS 8
 * Required data: font file, image file, ICC profile
 *                (see www.pdflib.com for ICC profiles)
 */


#include <iostream>
#include <sstream>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    try {
	/* This is where the data files are. Adjust as necessary.*/
	const wstring searchpath = L"../data";

	PDFlib p;
	const wstring imagefile = L"nesrin.jpg";
	int font, image, spot, icc;
	wostringstream optlist;

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	p.set_parameter(L"SearchPath", searchpath);

	if (p.begin_document(L"starter_pdfx3.pdf", L"pdfx=PDF/X-3:2003")
                                                                    == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	p.set_info(L"Creator", L"PDFlib starter sample");
	p.set_info(L"Title", L"starter_pdfx3");

	/*
	 * You can use one of the Standard output intents (e.g. for SWOP
	 * printing) which do not require an ICC profile:

	p.load_iccprofile(L"CGATS TR 001", L"usage=outputintent");

	 * However, if you use ICC or Lab color you must load an ICC
	 * profile as output intent:
	 */
	if (p.load_iccprofile(L"ISOcoated.icc", L"usage=outputintent") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
	    wcerr << L"Please install the ICC profile package from " <<
		L"www.pdflib.com to run the PDF/X starter sample." << endl;
	    return 2;
	}

	p.begin_page_ext(595, 842, L"");

	/* Font embedding is required for PDF/X */
	font = p.load_font(L"LuciduxSans-Oblique", L"winansi", L"embedding");
	if (font == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}
	p.setfont(font, 24);

	spot = p.makespotcolor(L"PANTONE 123 C");
	p.setcolor(L"fill", L"spot", spot, 1.0, 0.0, 0.0);
	p.fit_textline(L"PDF/X-3:2003 starter", 50, 700, L"");

	/* The RGB image below needs an ICC profile; we use sRGB. */
	icc = p.load_iccprofile(L"sRGB", L"");
        optlist.str(L"");
        optlist << L"iccprofile=" << icc;
	image = p.load_image(L"auto", imagefile, optlist.str());

	if (image == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	p.fit_image(image, 0.0, 0.0, L"scale=0.5");
	p.end_page_ext(L"");
	p.end_document(L"");
    }
    catch (PDFlib::Exception &ex) {
	wcerr << L"PDFlib exception occurred:" << endl
	      << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
	      << L": " << ex.get_errmsg() << endl;
	return 2;
    }

    return 0;
}
