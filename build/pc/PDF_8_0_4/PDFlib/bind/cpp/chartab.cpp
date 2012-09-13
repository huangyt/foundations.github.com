// $Id: chartab.cpp,v 1.19 2009/07/29 07:22:54 stm Exp $
//
// PDFlib client: chartab example in C++
//

#include <iostream>
#include <sstream>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    /* change these as required */
    const wchar_t *fontname = L"LuciduxSans-Oblique";

    /* This is where font/image/PDF input files live. Adjust as necessary. */
    const wchar_t *searchpath = L"../data";

    /* list of encodings to use */
    const wchar_t *encodings[] = { L"iso8859-1", L"iso8859-2", L"iso8859-15" };

    /* whether or not to embed the font */
    int embed = 1;

    double x, y;
    int row, col, font, page;

    const int ENCODINGS = sizeof(encodings) / sizeof(wchar_t *);
    const double FONTSIZE	= 16;
    const double TOP		= 700;
    const double LEFT		= 50;
    const double YINCR		= 2*FONTSIZE;
    const double XINCR		= 2*FONTSIZE;

    try {
	PDFlib p;

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	p.set_parameter(L"SearchPath", searchpath);

	if (p.begin_document(L"chartab.pdf",
		L"destination {type fitwindow page 1}") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
	    return(2);
	}

	p.set_info(L"Creator", L"chartab.c");
	p.set_info(L"Author", L"Thomas Merz");
	p.set_info(L"Title", L"Character table (C++)");

	/* loop over all encodings */
	for (page = 0; page < ENCODINGS; page++) {
	    p.begin_page_ext(a4_width, a4_height, L"");  /* start a new page */

	    /* print the heading and generate the bookmark */

	    // Change L"host" encoding to L"winansi" or whatever you need!
	    font = p.load_font(L"Helvetica", L"host", L"");
	    if (font == -1) {
		wcerr << L"Error: " << p.get_errmsg() << endl; return(2);
	    }
	    p.setfont(font, FONTSIZE);

            wostringstream buf;

            buf.str(L"");
            buf << fontname << " (" << encodings[page] << ") "
                << (embed ? "" : "not ") << "embedded";

	    p.show_xy(buf.str(), LEFT - XINCR, TOP + 3 * YINCR);
	    p.create_bookmark(buf.str(), L"");

	    /* print the row and column captions */
	    p.setfont(font, 2 * FONTSIZE/3);

	    for (row = 0; row < 16; row++) {
                buf.str(L"");
                buf << 'x' << hex << uppercase << row;
		p.show_xy(buf.str(), LEFT + row*XINCR, TOP + YINCR);

                buf.str(L"");
                buf << hex << uppercase << row << 'x';
		p.show_xy(buf.str(), LEFT - XINCR, TOP - row * YINCR);
	    }

	    /* print the character table */
	    font = p.load_font(fontname, encodings[page],
                                embed ? L"embedding" : L"");
	    if (font == -1) {
		wcerr << L"Error: " << p.get_errmsg() << endl;
		return(2);
	    }
	    p.setfont(font, FONTSIZE);

	    y = TOP;
	    x = LEFT;

	    for (row = 0; row < 16; row++) {
		for (col = 0; col < 16; col++) {
                    buf.str(L"");
                    buf << static_cast<wchar_t>(16*row + col);
		    p.show_xy(buf.str(), x, y);
		    x += XINCR;
		}
		x = LEFT;
		y -= YINCR;
	    }

	    p.end_page_ext(L"");
	}
	p.end_document(L"");
    }
    catch (PDFlib::Exception &ex) {
	wcerr << L"PDFlib exception occurred in chartab sample: " << endl
              << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
	      << L": " << ex.get_errmsg() << endl;
	return 2;
    }

    return 0;
}
