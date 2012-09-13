// $Id: businesscard.cpp,v 1.20 2009/07/23 09:40:53 stm Exp $
//
// PDFlib client: businesscard example in C++
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
	int         i, blockcontainer, page;
	const wstring infile = L"boilerplate.pdf";
        /* This is where font/image/PDF input files live. Adjust as necessary.
         *
         * Note that this directory must also contain the LuciduxSans font
         * outline and metrics files.
         */
	const wstring searchpath = L"../data";
	struct blockdata {
	    blockdata(wstring n, wstring v): name(n), value(v) {}
	    wstring name;
	    wstring value;
	};

	const blockdata data[] = {
	   blockdata(L"name",                   L"Victor Kraxi"),
	   blockdata(L"business.title",         L"Chief Paper Officer"),
	   blockdata(L"business.address.line1", L"17, Aviation Road"),
	   blockdata(L"business.address.city",  L"Paperfield"),
	   blockdata(L"business.telephone.voice",L"phone +1 234 567-89"),
	   blockdata(L"business.telephone.fax", L"fax +1 234 567-98"),
	   blockdata(L"business.email",         L"victor@kraxi.com"),
	   blockdata(L"business.homepage",      L"www.kraxi.com"),
	};

        const int BLOCKCOUNT = sizeof(data) / sizeof(blockdata);

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	// Set the search path for fonts and PDF files
	p.set_parameter(L"SearchPath", searchpath);

        if (p.begin_document(L"businesscard.pdf", L"") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return(2);
        }

        p.set_info(L"Creator", L"businesscard.cpp");
        p.set_info(L"Author", L"Thomas Merz");
        p.set_info(L"Title", L"PDFlib block processing sample (C++)");

        blockcontainer = p.open_pdi_document(infile, L"");
        if (blockcontainer == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl; return(2);
        }

        page = p.open_pdi_page(blockcontainer, 1, L"");
        if (page == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl; return(2);
        }

        p.begin_page_ext(20, 20, L"");              // dummy page size

        // This will adjust the page size to the block container's size.
        p.fit_pdi_page(page, 0, 0, L"adjustpage");

        // Fill all text blocks with dynamic data 
        for (i = 0; i < (int) BLOCKCOUNT; i++) {
            if (p.fill_textblock(page, data[i].name, data[i].value,
		L"embedding encoding=host") == -1) {
		wcerr << L"Error: " << p.get_errmsg() << endl;
            }
        }

        p.end_page_ext(L"");
	p.close_pdi_page(page);

        p.end_document(L"");
	p.close_pdi_document(blockcontainer);
    }
    catch (PDFlib::Exception &ex) {
	wcerr << L"PDFlib exception occurred in businesscard sample: " << endl;
	wcerr << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
	      << L": " << ex.get_errmsg() << endl;
	return 99;
    }

    return 0;
}
