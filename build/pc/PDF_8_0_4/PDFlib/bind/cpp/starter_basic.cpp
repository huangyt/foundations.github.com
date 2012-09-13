// $Id: starter_basic.cpp,v 1.7.2.2 2011/11/08 11:30:15 rjs Exp $
//
// Basic starter:
// Create some simple text, vector graphics and image output
// 
// required software: PDFlib/PDFlib+PDI/PPS 8
// required data: none

#include <iostream>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    try {
	/*  This is where the data files are. Adjust as necessary. */
	const wstring searchpath = L"../data";

	PDFlib p;
	const wstring imagefile = L"nesrin.jpg";
	int image;

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	p.set_parameter(L"SearchPath", searchpath);

	if (p.begin_document(L"starter_basic.pdf", L"") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	p.set_info(L"Creator", L"PDFlib starter sample");
	p.set_info(L"Title", L"starter_basic");

	/* We load the image before the first page, and use it
	 * on all pages
	 */
	image = p.load_image(L"auto", imagefile, L"");

	if (image == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	/*  Page 1 */
	p.begin_page_ext(595, 842, L"");
    /* use DejaVuSerif font and unicode encoding for placing the text
	 * and demonstrate various options how to pass the unicode text to PDFlib
	 */
	wostringstream optlist;
	optlist.str(L"fontname={DejaVuSerif} encoding=unicode embedding fontsize=24");

    /* using plain 7 bit ASCII text */
	p.fit_textline(L"en: Hello!", 50, 700, optlist.str());
	/* using unicode escapes */
	p.fit_textline(L"gr: \u0393\u03B5\u03B9\u03AC!", 50, 650, optlist.str());
	p.fit_textline(L"ru: \u041F\u0440\u0438\u0432\u0435\u0442!", 50, 600, optlist.str());
	/* using PDFlib's character references */
	optlist << optlist.str() << L" charref=true";
	p.fit_textline(L"es: &#xA1;Hola!", 50, 550, optlist.str());


	p.fit_image(image, 0.0, 0.0, L"scale=0.25");

	p.end_page_ext(L"");

	/*  Page 2 */
	p.begin_page_ext(595, 842, L"");

	/*  red rectangle */
	p.setcolor(L"fill", L"rgb", 1.0, 0.0, 0.0, 0.0);
	p.rect(200, 200, 250, 150);
	p.fill();

	/*  blue circle */
	p.setcolor(L"fill", L"rgb", 0.0, 0.0, 1.0, 0.0);
	p.arc(400, 600, 100, 0, 360);
	p.fill();

	/*  thick gray line */
	p.setcolor(L"stroke", L"gray", 0.5, 0.0, 0.0, 0.0);
	p.setlinewidth(10);
	p.moveto(100, 500);
	p.lineto(300, 700);
	p.stroke();

	/*  Using the same image handle means the data will be copied
	    to the PDF only once, which saves space.
	 */	
	p.fit_image(image, 150.0, 25.0, L"scale=0.25");
	p.end_page_ext(L"");

	/*  Page 3 */
	p.begin_page_ext(595, 842, L"");

	/*  Fit the image to a box of predefined size (without distortion) */
	optlist.str(L"boxsize={400 400} position={center} fitmethod=meet");

	p.fit_image(image, 100, 200, optlist.str());

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
