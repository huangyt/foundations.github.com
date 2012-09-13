// $Id: invoice.cpp,v 1.23 2009/07/29 07:22:54 stm Exp $
//
// PDFlib client: invoice example in C++
//

#include <iostream>
#include <string>

#include <time.h>

#if !defined(WIN32) && !defined(MAC)
#include <unistd.h>
#endif

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    try {
	int         i, stationery, page, regularfont, boldfont;
	const wstring infile = L"stationery.pdf";
	// This is where font/image/PDF input files live. Adjust as necessary.
	wstring      searchpath = L"../data";
	const double left = 55;
	const double right = 530;
	time_t      timer;
	struct tm   ltime;
	double      fontsize = 12, leading, y;
	double      sum, total;
	double      pagewidth = 595, pageheight = 842;
	wostringstream buf;
	wostringstream optlist;
	const wstring baseopt =
	    L"ruler        {   30 45     275   375   475} "
	    L"tabalignment {right left right right right} "
	    L"hortabmethod ruler fontsize 12 ";
	int         textflow;
	PDFlib      p;
	const wstring closingtext =
	    L"Terms of payment: <fillcolor={rgb 1 0 0}>30 days net. "
	    L"<fillcolor={gray 0}>90 days warranty starting at the day "
	    L"of sale. This warranty covers defects in workmanship only. "
	    L"<fontname=Helvetica-BoldOblique encoding=host>Kraxi Systems, "
	    L"Inc. <resetfont>will, at its option, repair or replace the "
	    L"product under the warranty. This warranty is not transferable. "
	    L"No returns or exchanges will be accepted for wet products.";

	struct articledata {
	    articledata(wstring n, double pr, int q):
		name(n), price(pr), quantity(q) {}
	    wstring name;
	    double price;
	    int quantity;
	};

	const articledata data[] = {
	    articledata(L"Super Kite",         20,     2),
	    articledata(L"Turbo Flyer",        40,     5),
	    articledata(L"Giga Trash",         180,    1),
	    articledata(L"Bare Bone Kit",      50,     3),
	    articledata(L"Nitty Gritty",       20,     10),
	    articledata(L"Pretty Dark Flyer",  75,     1),
	    articledata(L"Free Gift",          0,      1),
	};

        const int ARTICLECOUNT = sizeof(data) / sizeof(data[0]);

	static const wstring months[] = {
	    L"January", L"February", L"March", L"April", L"May", L"June",
	    L"July", L"August", L"September", L"October", L"November", L"December"
	};


	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	p.set_parameter(L"SearchPath", searchpath);

        if (p.begin_document(L"invoice.pdf", L"") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl; return(2);
        }

        p.set_info(L"Creator", L"invoice.cpp");
        p.set_info(L"Author", L"Thomas Merz");
        p.set_info(L"Title", L"PDFlib invoice generation demo (C++)");

        stationery = p.open_pdi_document(infile, L"");
        if (stationery == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        page = p.open_pdi_page(stationery, 1, L"");
        if (page == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        boldfont = p.load_font(L"Helvetica-Bold", L"host", L"");
	if (boldfont == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}
        regularfont = p.load_font(L"Helvetica", L"host", L"");
	if (regularfont == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}
        leading = fontsize + 2;

        // Establish coordinates with the origin in the upper left corner.
        p.begin_page_ext(pagewidth, pageheight, L"topdown");

        p.fit_pdi_page(page, 0, pageheight, L"");
        p.close_pdi_page(page);

        p.setfont(regularfont, 12);

        // Print the address
        y = 170;
        p.set_value(L"leading", leading);

        p.show_xy(L"John Q. Doe", left, y);
        p.continue_text(L"255 Customer Lane");
        p.continue_text(L"Suite B");
        p.continue_text(L"12345 User Town");
        p.continue_text(L"Everland");

        // Print the header and date

        p.setfont(boldfont, 12);
        y = 300;
        p.show_xy(L"INVOICE", left, y);

        time(&timer);
        ltime = *localtime(&timer);

        buf.str(L"");
        buf << months[ltime.tm_mon].c_str() << " " << ltime.tm_mday
            << ", " << ltime.tm_year + 1900;

        p.fit_textline(buf.str(), right, y, L"position {100 0}");

        // Print the invoice header line
        p.setfont(boldfont, 12);

        // L"position {0 0}" is left-aligned, L"position {100 0}" right-aligned
        y = 370;
        optlist << baseopt << " font " << boldfont;
	textflow = p.create_textflow(
                L"\tITEM\tDESCRIPTION\tQUANTITY\tPRICE\tAMOUNT",
                optlist.str());
        if (textflow == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}
 
	p.fit_textflow(textflow, left, y-leading, right, y, L"");
	p.delete_textflow(textflow);

        // Print the article list

        p.setfont(regularfont, 12);
        y += 2*leading;
        total = 0;

        optlist.str(L"");
        optlist << baseopt << " font " << regularfont;

        for (i = 0; i < (int)ARTICLECOUNT; i++) {
            sum = data[i].price * data[i].quantity;

            buf.str(L"");
            buf << L"\t" << (i + 1) << L"\t" << data[i].name << L"\t"
                << data[i].quantity << L"\t";
            buf.setf(ios_base::fixed, ios_base::floatfield);
            buf.precision(2);
            buf << data[i].price << L"\t" << sum;

	    textflow = p.create_textflow(buf.str(), optlist.str());
	    if (textflow == -1) {
		wcerr << L"Error: " << p.get_errmsg() << endl;
		return 2;
	    }
	    p.fit_textflow(textflow, left, y-leading, right, y, L"");
	    p.delete_textflow(textflow);

            y += leading;
            total += sum;
        }

        y += leading;
        p.setfont(boldfont, 12);
        buf.str(L"");
        buf.setf(ios_base::fixed, ios_base::floatfield);
        buf.precision(2);
        buf << total;
        p.fit_textline(buf.str(), right, y, L"position {100 0}");

        // Print the closing text

        y += 5*leading;
        optlist.str(L"");
        optlist << L"alignment=justify leading=120% "
                << L"fontname=Helvetica fontsize=12 encoding=host ";

	textflow = p.create_textflow(closingtext, optlist.str());
	if (textflow == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}
	p.fit_textflow(textflow, left, y+6*leading, right, y, L"");
	p.delete_textflow(textflow);

        p.end_page_ext(L"");
        p.end_document(L"");
        p.close_pdi_document(stationery);
    }

    catch (PDFlib::Exception &ex) {
	wcerr << L"PDFlib exception occurred in invoice sample: " << endl
	      << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
	      << L": " << ex.get_errmsg() << endl;
	return 2;
    }

    return 0;
}
