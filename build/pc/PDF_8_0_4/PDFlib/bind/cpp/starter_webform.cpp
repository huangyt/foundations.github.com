/* $Id: starter_webform.cpp,v 1.5 2009/07/29 07:22:54 stm Exp $
 *
 * Webform starter:
 * Create a linearized PDF (for fast delivery over the Web, also known
 * as "fast Web view") which is encrypted and contains some form fields.
 * A few lines of JavaScript are inserted as "page open" action to
 * automatically populate the date field with the current date.
 *
 * required software: PDFlib/PDFlib+PDI/PPS 8
 * required data: none
 */

#include <iostream>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    try {
	PDFlib p;
	wostringstream optlist;
	int font, action;
	double llx=150, lly=550, urx=350, ury=575;

	/* JavaScript for automatically filling the date into a form field */
	const wstring js =
	    L"var d = util.printd(\"mm/dd/yyyy\", new Date());" 
	    L"var date = this.getField(\"date\");" 
	    L"date.value = d;";

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	if (p.begin_document(L"starter_webform.pdf",
                                L"linearize masterpassword=pdflib "
                                L"permissions={nomodify}") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
	    return 2;
	}

	p.set_info(L"Creator", L"PDFlib starter sample");
	p.set_info(L"Title", L"starter_webform");

        optlist.str(L"");
        optlist << L"script={ " << js <<  L"}";
	action = p.create_action(L"JavaScript", optlist.str());

        optlist.str(L"");
        optlist << L"action={open=" << action << L"}";
	p.begin_page_ext(595, 842, optlist.str());

	font = p.load_font(L"Helvetica", L"winansi", L"");
	if (font == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
	    return 2;
	}
	p.setfont(font, 24);

	p.fit_textline(L"Date: ", 125, lly+5, L"position={right bottom}");

	/* The tooltip will be used as rollover text for the field */
        optlist.str(L"");
        optlist << L"tooltip={Date (will be filled automatically)} "
                   L"bordercolor={gray 0} font=" << font;
	p.create_field(llx, lly, urx, ury, L"date", L"textfield",
                        optlist.str());

	lly-=100; ury-=100;
	p.fit_textline(L"Name: ", 125, lly+5, L"position={right bottom}");

        optlist.str(L"");
        optlist << L"tooltip={Enter your name here} "
                   L"bordercolor={gray 0} font=" << font;
	p.create_field(llx, lly, urx, ury, L"name", L"textfield",
                        optlist.str());

	p.end_page_ext(L"");

	p.end_document(L"");
    }
    catch (PDFlib::Exception &ex) {
	wcerr << L"PDFlib exception occurred: " << endl
	      << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
	      << L": " << ex.get_errmsg() << endl;
	return 2;
    }
    return 0;
}
