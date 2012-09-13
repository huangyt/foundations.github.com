/* $Id: starter_pdfmerge.cpp,v 1.6 2009/07/29 07:22:54 stm Exp $
 *
 * PDF merge starter:
 * Merge pages from multiple PDF documents; interactive elements (e.g. 
 * bookmarks) will be dropped.
 *
 * required software: PDFlib+PDI/PPS 8
 * required data: PDF documents
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
	const wstring pdffiles[] =
	{
		L"PDFlib-real-world.pdf",
		L"PDFlib-datasheet.pdf",
		L"TET-datasheet.pdf",
		L"PLOP-datasheet.pdf",
		L"pCOS-datasheet.pdf"
	};
        const int filecount = sizeof(pdffiles) / sizeof(pdffiles[0]);
	int i;

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	p.set_parameter(L"SearchPath", searchpath);

	if (p.begin_document(L"starter_pdfmerge.pdf", L"") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}
	p.set_info(L"Creator", L"PDFlib starter sample");
	p.set_info(L"Title", L"starter_pdfmerge");

	for (i = 0; i < filecount; i++) {
	    int indoc, endpage, pageno, page; 

	    /* Open the input PDF */
	    indoc = p.open_pdi_document(pdffiles[i], L"");
	    if (indoc == -1) {
		wcerr << L"Error: " << p.get_errmsg() << endl;
                return 2;
	    }

	    endpage = static_cast<int>(p.pcos_get_number(indoc, L"length:pages"));


	    /* Loop over all pages of the input document */
	    for (pageno = 1; pageno <= endpage; pageno++)
	    {
		page = p.open_pdi_page(indoc, pageno, L"");
		if (page == -1) {
		    wcerr << L"Error: " << p.get_errmsg() << endl;
		    continue;
		}
		/* Dummy page size; will be adjusted later */
		p.begin_page_ext(10, 10, L"");

		/* Create a bookmark with the file name */
		if (pageno == 1)
		    p.create_bookmark(pdffiles[i], L"");

		/* Place the imported page on the output page, and
		 * adjust the page size
		 */
		p.fit_pdi_page(page, 0, 0, L"adjustpage");
		p.close_pdi_page(page);

		p.end_page_ext(L"");
	    }
	    p.close_pdi_document(indoc);
	}

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
