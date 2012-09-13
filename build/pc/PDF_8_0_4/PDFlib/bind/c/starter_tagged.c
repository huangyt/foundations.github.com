/* $Id: starter_tagged.c,v 1.5.2.2 2011/07/11 09:24:24 rjs Exp $
 *
 * Tagged PDF starter:
 * Create document with structure information for reflow and accessibility
 *
 * Required software: PDFlib/PDFlib+PDI/PPS 7
 * Required data: none (dummy text created in program)
 */

#include <stdio.h>
#include <stdlib.h>

#include "pdflib.h"


int
main(void)
{
    /* This is where the data files are. Adjust as necessary. */
    const char* searchpath = "../data";

    PDF * p;
    int id, id2, id_artifact, font;

    /* create a new PDFlib object */
    if ((p = PDF_new()) == (PDF *) 0) {
        printf("Couldn't create PDFlib object (out of memory)!\n");
        return(2);
    }

    PDF_TRY(p) {

        /* This means we must check return values of load_font() etc. */
        PDF_set_parameter(p, "errorpolicy", "return");

        PDF_set_parameter(p, "SearchPath", searchpath);

        if (PDF_begin_document(p, "starter_tagged.pdf", 0,
                                            "tagged=true lang=en") == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_set_info(p, "Creator", "PDFlib starter sample");
        PDF_set_info(p, "Title", "starter_tagged");

        /* Automatically create spaces between chunks of text */
        PDF_set_parameter(p, "autospace", "true");

        /* open the first structure element as a child of the document
         * structure root (=0)
         */
        id = PDF_begin_item(p, "Document",
		"Title = {Starter sample for Tagged PDF}");

        PDF_begin_page_ext(p, 0, 0, 
	    "width=a4.width height=a4.height taborder=structure");

	PDF_create_bookmark(p, "Section 1", 0, "");

        font = PDF_load_font(p, "Helvetica", 0, "winansi", "");

        if (font == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_setfont(p, font, 24.0);

        id2 = PDF_begin_item(p, "H1", "Title = {Introduction}");
        PDF_show_xy(p, "1 Introduction", 50, 700);
        PDF_end_item(p, id2);

        id2 = PDF_begin_item(p, "P",
	    "Title = {Simple paragraph}");
        PDF_setfont(p, font, 12.0);
        PDF_continue_text(p, "This PDF has a very simple document structure ");
        PDF_continue_text(p, "which demonstrates basic Tagged PDF features ");
        PDF_continue_text(p, "for accessibility.");
 
        PDF_end_item(p, id2);

        /* The page number is created as an artifact; it will be
         * ignored when reflowing the page in Acrobat.
         */
        id_artifact = PDF_begin_item(p, "Artifact", "");
        PDF_show_xy(p, "Page 1", 250, 100);
        PDF_end_item(p, id_artifact);

        PDF_end_item(p, id);
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
