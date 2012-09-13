// $Id: quickreference.cpp,v 1.31 2009/07/29 07:22:54 stm Exp $
//
// PDFlib+PDI client: mini imposition demo
//

#include <iostream>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    try {
	PDFlib p;
	int manual, page;
	int font, row, col;
	const int maxrow = 2;
	const int maxcol = 2;
	wostringstream optlist;
	int endpage;
	const double width = 500, height = 770;
	int pageno;
	const wstring infile = L"reference.pdf";
	/* This is where font/image/PDF input files live. Adjust as necessary.*/
	const wstring searchpath = L"../data";

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	p.set_parameter(L"SearchPath", searchpath);

	if (p.begin_document(L"quickreference.pdf", L"") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	p.set_info(L"Creator", L"quickreference.cpp");
	p.set_info(L"Author", L"Thomas Merz");
	p.set_info(L"Title", L"mini imposition demo (C++)");

	manual = p.open_pdi_document(infile, L"");
	if (manual == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	row = 0;
	col = 0;

	p.set_parameter(L"topdown", L"true");

	endpage = static_cast<int>(p.pcos_get_number(manual, L"length:pages"));

        optlist.str(L"");
        optlist << L"scale " << (1.0 / maxrow);

	for (pageno = 1; pageno <= endpage; pageno++) {
	    if (row == 0 && col == 0) {
		p.begin_page_ext(width, height, L"");
		font = p.load_font(L"Helvetica-Bold", L"host", L"");
		if (font == -1) {
		    wcerr << L"Error: " << p.get_errmsg() << endl;
                    return 2;
		}
		p.setfont(font, 18);
		p.set_text_pos(24, 24);
		p.show(L"PDFlib Quick Reference");
	    }

	    page = p.open_pdi_page(manual, pageno, L"");

	    if (page == -1) {
		wcerr << L"Error: " << p.get_errmsg() << endl; return 2;
	    }

	    p.fit_pdi_page(page, width/maxcol*col,
			(row + 1) *  height/maxrow, optlist.str());
	    p.close_pdi_page(page);

	    col++;
	    if (col == maxcol) {
		col = 0;
		row++;
	    }
	    if (row == maxrow) {
		row = 0;
		p.end_page_ext(L"");
	    }
	}

	// finish the last partial page
	if (row != 0 || col != 0)
	    p.end_page_ext(L"");

	p.end_document(L"");
	p.close_pdi_document(manual);
    }

    catch (PDFlib::Exception &ex) {
	wcerr << L"PDFlib exception occurred in quickreference sample: " << endl
	      << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
	      << L": " << ex.get_errmsg() << endl;
	return 2;
    }

    return 0;
}
