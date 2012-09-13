/* $Id: starter_pdfa1b.cpp,v 1.2 2009/09/09 12:32:24 rjs Exp $
 *
 * PDF/A starter:
 * Create PDF/A-1b conforming output
 *
 * Required software: PDFlib/PDFlib+PDI/PPS 8
 * Required data: font file, image file
 */

#include <iostream>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    try {
	/* This is where the data files are. Adjust as necessary. */
	const wstring searchpath = L"../data";

	PDFlib p;
	const wstring imagefile = L"nesrin.jpg";

	int font;
	int image;

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	p.set_parameter(L"SearchPath", searchpath);

	if (p.begin_document(L"starter_pdfa1b.pdf",
                                        L"pdfa=PDF/A-1b:2005") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	/*
	 * We use sRGB as output intent since it allows the color
	 * spaces CIELab, ICC-based, grayscale, and RGB.
	 *
	 * If you need CMYK color you must use a CMYK output profile.
	 */

	p.load_iccprofile(L"sRGB", L"usage=outputintent");

	p.set_info(L"Creator", L"PDFlib starter sample");
	p.set_info(L"Title", L"starter_pdfa1b");

	p.begin_page_ext(595, 842, L"");

	/* Font embedding is required for PDF/A */
	font = p.load_font(L"LuciduxSans-Oblique", L"winansi", L"embedding");
	if (font == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}
	p.setfont(font, 24);

	p.fit_textline(L"PDF/A-1b:2005 starter", 50, 700, L"");

	/* We can use an RGB image since we already supplied an
	 * output intent profile.
	 */
	image = p.load_image(L"auto", imagefile, L"");

	if (image == -1){
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	/* Place the image at the bottom of the page */
	p.fit_image(image, 0.0, 0.0, L"scale=0.5");

	p.end_page_ext(L"");
	p.close_image(image);

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
