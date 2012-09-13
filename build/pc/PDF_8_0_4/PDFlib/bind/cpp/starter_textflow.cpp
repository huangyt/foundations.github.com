/* $Id: starter_textflow.cpp,v 1.9 2009/07/29 07:22:54 stm Exp $
 *
 * Textflow starter:
 * Create multi-column text output which may span multiple pages
 *
 * required software: PDFlib/PDFlib+PDI/PPS 8 or above
 * required data: none
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
	PDFlib p;
	int i, tf = -1;
	wstring result;
	double llx1= 50, lly1=50, urx1=250, ury1=800;
	double llx2=300, lly2=50, urx2=500, ury2=800;

	/* Repeat the dummy text to produce more contents */
	int count = 50;

	const wstring optlist1 =
	    L"fontname=Helvetica fontsize=10.5 encoding=winansi " 
	    L"fillcolor={gray 0} alignment=justify";

	const wstring optlist2 =
	    L"fontname=Helvetica-Bold fontsize=14 encoding=winansi " 
	    L"fillcolor={rgb 1 0 0} charref";

	/* Dummy text for filling the columns. Soft hyphens are marked with
	 * the character reference L"&shy;" (character references are
	 * enabled by the charref option).
	 */
	wstring text= 
L"Lorem ipsum dolor sit amet, consectetur adi&shy;pi&shy;sicing elit, "
L"sed do eius&shy;mod tempor incidi&shy;dunt ut labore et dolore magna "
L"ali&shy;qua. Ut enim ad minim ve&shy;niam, quis nostrud exer&shy;citation "
L"ull&shy;amco la&shy;bo&shy;ris nisi ut ali&shy;quip ex ea commodo "
L"con&shy;sequat. Duis aute irure dolor in repre&shy;henderit in voluptate "
L"velit esse cillum dolore eu fugiat nulla pari&shy;atur. Excep&shy;teur "
L"sint occae&shy;cat cupi&shy;datat non proident, sunt in culpa qui officia "
L"dese&shy;runt mollit anim id est laborum. ";

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	if (p.begin_document(L"starter_textflow.pdf", L"") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	p.set_info(L"Creator", L"PDFlib starter sample");
	p.set_info(L"Title", L"starter_textflow");

	/* Create some amount of dummy text and feed it to a Textflow
	 * object with alternating options. 
	 */
	for (i = 1; i <= count; i++) {
            wostringstream num;

            num.str(L"");
            num << i << L" ";

	    tf = p.add_textflow(tf, num.str(), optlist2);
	    if (tf == -1) {
		wcerr << L"Error: " << p.get_errmsg() << endl;
                return 2;
	    }

	    tf = p.add_textflow(tf, text, optlist1);
	    if (tf == -1) {
		wcerr << L"Error: " << p.get_errmsg() << endl;
                return 2;
	    }
	}

	/* Loop until all of the text is placed; create new pages
	 * as long as more text needs to be placed. Two columns will
	 * be created on all pages.
	 */
	do {
	    /* Add "showborder to visualize the fitbox borders */
	    const wstring optlist =
		L"verticalalign=justify linespreadlimit=120% ";

	    p.begin_page_ext(0, 0, L"width=a4.width height=a4.height");

	    /* Fill the first column */
	    result = p.fit_textflow(tf, llx1, lly1, urx1, ury1, optlist);

	    /* Fill the second column if we have more text*/
	    if (result != L"_stop")
		result = p.fit_textflow(tf, llx2, lly2, urx2, ury2, optlist);

	    p.end_page_ext(L"");

	    /* "_boxfull" means we must continue because there is more text;
	     * "_nextpage" is interpreted as "start new column"
	     */
	} while (result == L"_boxfull" || result == L"_nextpage");

	/* Check for errors */
	if (result != L"_stop") {
	    /* "_boxempty" happens if the box is very small and doesn't
	     * hold any text at all.
	     */
	    if (result == L"_boxempty") {
		wcerr << L"Error: Textflow box too small" << endl;
		return 2;
	    } else {
		/* Any other return value is a user exit caused by
		 * the "return" option; this requires dedicated code to
		 * deal with.
		 */
		wcerr << L"User return '" << result << L"' found in Tetxtflow"  
		    << endl;
		return 2;
	    }
	}

	p.delete_textflow(tf);

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
