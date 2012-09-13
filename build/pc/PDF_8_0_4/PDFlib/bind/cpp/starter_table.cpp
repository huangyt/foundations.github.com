/* $Id: starter_table.cpp,v 1.10 2009/09/09 06:34:51 stm Exp $
 *
 * Table starter:
 * Create table which may span multiple pages
 *
 * required software: PDFlib/PDFlib+PDI/PPS 8
 * required data: image file (dummy text created within the program)
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

	const wstring imagefile = L"nesrin.jpg";

	int row, col, font, image, tf=-1, tbl=-1;
	int rowmax=50, colmax=5;
	PDFlib p;
	double llx= 50, lly=50, urx=550, ury=800;
	const wstring headertext =
                                L"Table header (centered across all columns)";
	wstring result;
	wostringstream optlist;

	/* Dummy text for filling a cell with multi-line Textflow */
	const wstring tf_text = 
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

	p.set_parameter(L"SearchPath", searchpath);

	if (p.begin_document(L"starter_table.pdf", L"") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}
	p.set_info(L"Creator", L"PDFlib starter sample");
	p.set_info(L"Title", L"starter_table");

	/* -------------------- Add table cells -------------------- */

	/* ---------- Row 1: table header (spans all columns) */
	row = 1; col = 1;
	font = p.load_font(L"Times-Bold", L"winansi", L"");
	if (font == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return(2);
	}

        optlist.str(L"");
        optlist << L"fittextline={position=center font=" << font
                << L" fontsize=14} colspan=" << colmax;

	tbl = p.add_table_cell(tbl, col, row, headertext, optlist.str());

	/* ---------- Row 2: various kinds of content */
	/* ----- Simple text cell */
	row++; col=1;

        optlist.str(L"");
        optlist << L"fittextline={font=" << font
                << L" fontsize=10 orientate=west}";

	tbl = p.add_table_cell(tbl, col, row, L"vertical line", optlist.str());
	if (tbl == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	/* ----- Colorized background */
	col++;

        optlist.str(L"");
        optlist << L"fittextline={font=" << font
                << L" fontsize=10} matchbox={fillcolor={rgb 0.9 0.5 0}}";

	tbl = p.add_table_cell(tbl, col, row, L"some color", optlist.str()); 

	/* ----- Multi-line text with Textflow */
	col++;
	font = p.load_font(L"Times-Roman", L"winansi", L"");
	if (font == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl; return(2);
	}

	tf = p.add_textflow(tf, tf_text,
                L"charref fontname=Times-Roman encoding=winansi fontsize=8");
	if (tf == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl; return(2);
	}

        optlist.str(L"");
        optlist << L"marginleft=2 marginright=2 margintop=2 marginbottom=2 "
                    L"textflow=" << tf;

	tbl = p.add_table_cell(tbl, col, row, L"", optlist.str());
	if (tbl == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	/* ----- Rotated image */
	col++;

	image = p.load_image(L"auto", imagefile, L"");
	if (image == -1) {
	    wcerr << L"Couldn't load image: " << p.get_errmsg() << endl;
            return 2;
	}

        optlist.str(L"");
        optlist << L"image=" << image << L" fitimage={orientate=west}";

	tbl = p.add_table_cell(tbl, col, row, L"", optlist.str());
	if (tbl == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	/* ----- Diagonal stamp */
	col++;

        optlist.str(L"");
        optlist << L"fittextline={font=" << font
                << L" fontsize=10 stamp=ll2ur}";

	tbl = p.add_table_cell(tbl, col, row, L"entry void", optlist.str());
	if (tbl == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
	}

	/* ---------- Fill row 3 and above with their numbers */
	for (row++; row <= rowmax; row++) {
	    for (col = 1; col <= colmax; col++) {
		wostringstream num;

                num << L"Col " << col << L"/Row " << row;

                optlist.str(L"");
                optlist << L"colwidth=20% fittextline={font=" << font
                        <<  L" fontsize=10}";

		tbl = p.add_table_cell(tbl, col, row, num.str(), optlist.str());
	    }
	}

	/* ---------- Place the table on one or more pages ---------- */

	/*
	 * Loop until all of the table is placed; create new pages
	 * as long as more table instances need to be placed.
	 */
	do {
	    p.begin_page_ext(0, 0, L"width=a4.width height=a4.height");

	    /* Shade every other row; draw lines for all table cells.
	     * Add "showcells showborder" to visualize cell borders 
	     */

	    /* Place the table instance */
	    result = p.fit_table(tbl, llx, lly, urx, ury, 
                        L"header=1 fill={{area=rowodd fillcolor={gray 0.9}}} " 
                        L"stroke={{line=other}}");
	    if (result == L"_error") {
		wcerr << L"Couldn't place table: " << p.get_errmsg() << endl;
                return 2;
	    }

	    p.end_page_ext(L"");

	} while (result == L"_boxfull");

	/* Check the result; "_stop" means all is ok. */
	if (result != L"_stop") {
	    if (result == L"_error") {
		wcerr << L"Error when placing table: " << p.get_errmsg()
                        << endl;
                return 2;
	    }
            else {
		/* Any other return value is a user exit caused by
		 * the "return" option; this requires dedicated code to
		 * deal with.
		 */
		wcerr << L"User return found in Textflow" << endl;
                return 2;
	    }
	}
	/* This will also delete Textflow handles used in the table */
	p.delete_table(tbl, L"");

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
