/* $Id: starter_pdfx5g.c,v 1.1.2.5 2011/11/30 10:42:51 rjs Exp $
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

#include <stdio.h>
#include <stdlib.h>

#include "pdflib.h"


int
main(void)
{
    /* This is where the data files are. Adjust as necessary.*/
    const char * searchpath = "../data";
    const char *targetname = "x5target.pdf";

    PDF *p;
    char optlist[1024];

    int font, proxy;
    double linewidth=2;
    double width, height;

    /* create a new PDFlib object */
    if ((p = PDF_new()) == (PDF *) 0) {
        printf("Couldn't create PDFlib object (out of memory)!\n");
        return(2);
    }

    PDF_TRY(p) {
        /* This means we must check return values of load_font() etc. */
        PDF_set_parameter(p, "errorpolicy", "return");

        PDF_set_parameter(p, "SearchPath", searchpath);

        if (PDF_begin_document(p, "starter_pdfx5g.pdf", 0, "pdfx=PDF/X-5g")
                == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_set_info(p, "Creator", "PDFlib starter sample");
        PDF_set_info(p, "Title", "starter_pdfx5g");

        /* Open the output intent profile */
        if (PDF_load_iccprofile(p, "ISOcoated.icc", 0,
                "usage=outputintent") == -1)
        {
            printf("Error: %s\n", PDF_get_errmsg(p));
            printf("Please install the ICC profile package from "
                   "www.pdflib.com to run the PDF/X-5g starter sample.\n");
            PDF_delete(p);
            return(2);
        }

        /* Font embedding is required for PDF/X */
        font = PDF_load_font(p, "LuciduxSans-Oblique", 0,
                "winansi", "embedding");

        if (font == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        /* Create a template which will serve as proxy. The referenced
         * page (the target) is attached to the proxy.
	 * The template width and height will be determined automatically,
	 * so we don't have to supply them.
         */
        sprintf(optlist, "reference={filename=%s pagenumber=1}", targetname);
        proxy = PDF_begin_template_ext(p, 0, 0, optlist);

        if (proxy == -1)
        {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

	width  = PDF_info_image(p, proxy, "imagewidth", "");
	height = PDF_info_image(p, proxy, "imageheight", "");

        /* Draw a crossed-out rectangle to visualize the proxy.
         * Attention: if we use the exact corner points, one half of the
         * linewidth would end up outside the template, and therefore be
         * clipped.
         */
        PDF_setlinewidth(p, linewidth);
        PDF_moveto(p, linewidth/2, linewidth/2);
        PDF_lineto(p, width-linewidth/2, linewidth/2);
        PDF_lineto(p, width-linewidth/2, height-linewidth/2);
        PDF_lineto(p, linewidth/2, height-linewidth/2);
        PDF_lineto(p, linewidth/2, linewidth/2);
        PDF_lineto(p, width-linewidth/2, height-linewidth/2);

        PDF_moveto(p, width-linewidth/2, linewidth/2);
        PDF_lineto(p, linewidth/2, height-linewidth/2);
        PDF_stroke(p);

        PDF_setfont(p, font, 24);

        sprintf(optlist, "fitmethod=auto position=center boxsize={%f %f}",
            width, height);
        PDF_fit_textline(p, "Proxy replaces target here", 0,
            0, 0, optlist);

	PDF_end_template_ext(p, 0, 0);


        /* Create the page */
        PDF_begin_page_ext(p, 595, 842, "");

        PDF_setfont(p, font, 18);

        PDF_fit_textline(p,
            "PDF/X-5 starter sample with reference to an external page", 0,
            50, 700, "");

        /* Place the proxy on the page */
        PDF_fit_image(p, proxy, 50, 50, "boxsize={500 500} fitmethod=meet");

        PDF_end_page_ext(p, "");
        PDF_end_document(p, "");
    }

    PDF_CATCH(p) {
        printf("PDFlib exception occurred:\n");
        printf("[%d] %s: %s\n",
            PDF_get_errnum(p), PDF_get_apiname(p), PDF_get_errmsg(p));
        PDF_delete(p);
        return(2);
    }

    PDF_delete(p);

    return 0;
}
