/* $Id: starter_tagged.cpp,v 1.6 2009/07/29 07:22:54 stm Exp $
 *
 * Tagged PDF starter:
 * Create document with structure information for reflow and accessibility
 *
 * required software: PDFlib/PDFlib+PDI/PPS 8
 * required data: none (dummy text created in program)
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
	int id, id2, id_artifact, font;

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	p.set_parameter(L"SearchPath", searchpath);

	if (p.begin_document(L"starter_tagged.pdf",
					L"tagged=true lang=en") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}
	p.set_info(L"Creator", L"PDFlib starter sample");
	p.set_info(L"Title", L"starter_tagged");

	/* Automatically create spaces between chunks of text */
	p.set_parameter(L"autospace", L"true");

	/* open the first structure element as a child of the document
	 * structure root (=0)
	 */
        id = p.begin_item(L"Document",
		L"Title = {Starter sample for Tagged PDF}");

        p.begin_page_ext(0, 0, 
	    L"width=a4.width height=a4.height taborder=structure");

	p.create_bookmark(L"Section 1", L"");

	font = p.load_font(L"Helvetica", L"winansi", L"");
	if (font == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return(2);
	}
	p.setfont(font, 24.0);

        id2 = p.begin_item(L"H1", L"Title = {Introduction}");
        p.show_xy(L"1 Introduction", 50, 700);
        p.end_item(id2);

        id2 = p.begin_item(L"P", L"Title = {Simple paragraph}");
        p.setfont(font, 12.0);
        p.continue_text(L"This PDF has a very simple document structure ");
        p.continue_text(L"which demonstrates basic Tagged PDF features ");
        p.continue_text(L"for accessibility.");
 
        p.end_item(id2);

	/* The page number is created as an artifact; it will be
	 * ignored when reflowing the page in Acrobat.
	 */
	id_artifact = p.begin_item(L"Artifact", L"");
	p.show_xy(L"Page 1", 250, 100);
	p.end_item(id_artifact);

	p.end_item(id);
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
