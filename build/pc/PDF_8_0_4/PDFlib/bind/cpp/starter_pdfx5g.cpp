/* $Id: starter_pdfx5g.cpp,v 1.1 2009/09/09 07:51:02 stm Exp $
 *
 * PDF/X-5g starter:
 * Create PDF/X-5g conforming output with a reference to an external page
 *
 * The external document from which a page is referenced must conform to
 * one of the following standards:
 * PDF/X-1a:2003, PDF/X-3:2002, PDF/X-4, PDF/X-4p, PDF/X-5g, or PDF/X-5pg
 *
 * In order to properly display and print the referenced target page with
 * Acrobat you must configure Acrobat appropriately (see PDFlib Tutorial),
 * and the target PDF must be available to Acrobat.
 *
 * Required software: PDFlib/PDFlib+PDI/PPS 8
 * Required data: font file, external PDF/X target, ICC output intent profile
 *                (see www.pdflib.com for ICC profiles)
 */

#include <iostream>
#include <string>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    /* This is where the data files are. Adjust as necessary.*/
    const wstring searchpath = L"../data";
    const wstring targetname = L"x5target.pdf";

    PDFlib p;
    wostringstream optlist;

    int font, proxy;
    const double linewidth = 2;
    double width, height;

    try {
        /* This means we must check return values of load_font() etc. */
        p.set_parameter(L"errorpolicy", L"return");

        p.set_parameter(L"SearchPath", searchpath);

        if (p.begin_document(L"starter_pdfx5g.pdf", L"pdfx=PDF/X-5g") == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        p.set_info(L"Creator", L"PDFlib starter sample");
        p.set_info(L"Title", L"starter_pdfx5g");

        /* Open the output intent profile */
        if (p.load_iccprofile(L"ISOcoated.icc", L"usage=outputintent") == -1)
        {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            wcerr << L"Please install the ICC profile package from "
                     L"www.pdflib.com to run the PDF/X-4 starter sample."
                    << endl;
            return 2;
        }

        /* Font embedding is required for PDF/X */
        font = p.load_font(L"LuciduxSans-Oblique", L"winansi", L"embedding");

        if (font == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        /* Create a template which will serve as proxy. The referenced
         * page (the target) is attached to the proxy.
	 * The template width and height will be determined automatically,
	 * so we don't have to supply them.
         */
        optlist.str(L"");
        optlist << L"reference={filename=" << targetname << L" pagenumber=1}";
        proxy = p.begin_template_ext(0, 0, optlist.str());

        if (proxy == -1)
        {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        width  = p.info_image(proxy, L"imagewidth", L"");
	height = p.info_image(proxy, L"imageheight", L"");

        /* Draw a crossed-out rectangle to visualize the proxy.
         * Attention: if we use the exact corner points, one half of the
         * linewidth would end up outside the template, and therefore be
         * clipped.
         */
        p.setlinewidth(linewidth);
        p.moveto(linewidth/2, linewidth/2);
        p.lineto(width-linewidth/2, linewidth/2);
        p.lineto(width-linewidth/2, height-linewidth/2);
        p.lineto(linewidth/2, height-linewidth/2);
        p.lineto(linewidth/2, linewidth/2);
        p.lineto(width-linewidth/2, height-linewidth/2);

        p.moveto(width-linewidth/2, linewidth/2);
        p.lineto(linewidth/2, height-linewidth/2);
        p.stroke();

        p.setfont(font, 24);

        optlist.str(L"");
        optlist << L"fitmethod=auto position=center boxsize={"
            << width << L" " << height << L"}";
        p.fit_textline(L"Proxy replaces target here", 0, 0, optlist.str());

        p.end_template_ext(0, 0);


        /* Create the page */
        p.begin_page_ext(595, 842, L"");

        p.setfont(font, 18);

        p.fit_textline(
            L"PDF/X-5 starter sample with reference to an external page",
            50, 700, L"");

        /* Place the proxy on the page */
        p.fit_image(proxy, 50, 50, L"boxsize={500 500} fitmethod=meet");

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
