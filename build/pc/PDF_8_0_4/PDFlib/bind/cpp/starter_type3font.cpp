/* $Id: starter_type3font.cpp,v 1.5 2009/09/09 06:34:51 stm Exp $
 *
 * Type 3 font starter:
 * Create a simple Type 3 font from vector data
 *
 * Define a type 3 font with the glyphs "l" and "space" and output text with
 * that font. In addition the glyph ".notdef" is defined which any undefined
 * character will be mapped to.
 *
 * Required software: PDFlib/PDFlib+PDI/PPS 8
 * Required data: none
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
        const wstring outfile = L"starter_type3font.pdf";

        wostringstream buf;
        PDFlib p;
        int font;
        double x, y;

        p.set_parameter(L"SearchPath", searchpath);

        /* This means we must check return values of load_font() etc. */
        p.set_parameter(L"errorpolicy", L"return");

        if (p.begin_document(outfile, L"") == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        p.set_info(L"Creator", L"PDFlib starter sample");
        p.set_info(L"Title", L"starter_type3font");

        /* Create the font "SimpleFont" containing the glyph L"l",
         * the glyph "space" for spaces and the glyph ".notdef" for any
         * undefined character
         */
        p.begin_font(L"SimpleFont", 0.001, 0.0, 0.0, 0.001, 0.0, 0.0, L"");
        p.begin_glyph(L".notdef", 266, 0, 0, 0, 0);
        p.end_glyph();
        p.begin_glyph(L"space", 266, 0, 0, 0, 0);
        p.end_glyph();
        p.begin_glyph(L"l", 266, 0, 0, 266, 570);
        p.setlinewidth(20);
        p.setdash(0, 0);
        x = 197;
        y = 10;
        p.moveto(x, y);
        y += 530;
        p.lineto(x, y);
        x -= 64;
        p.lineto(x, y);
        y -= 530;
        p.moveto(x, y);
        x += 128;
        p.lineto(x, y);

        p.stroke();
        p.end_glyph();

        p.end_font();

        /* Start page */
        p.begin_page_ext(0, 0, L"width=300 height=200");

        /* Load the new L"SimpleFont" font */
        font = p.load_font(L"SimpleFont", L"winansi", L"");

        if (font == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl; return 2;
        }

        /* Output the characters L"l" and L"space" of the L"SimpleFont" font.
         * The character L"x" is undefined and will be mapped to L".notdef"
         */
        buf.str(L"");
        buf << L"font=" << font << L" fontsize=40";
        p.fit_textline(L"lll lllxlll", 100, 100, buf.str());

        p.end_page_ext(L"");

        p.end_document(L"");
    }
    catch (PDFlib::Exception &ex) {
        wcerr << L"PDFlib exception occurred in invoice sample: " << endl
              << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
              << L": " << ex.get_errmsg() << endl;
        return 2;
    }

    return 0;
}
