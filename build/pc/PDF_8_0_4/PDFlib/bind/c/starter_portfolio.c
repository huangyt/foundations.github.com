/* $Id: starter_portfolio.c,v 1.3.2.3 2010/01/28 12:27:51 rjs Exp $
 *
 * PDF portfolio starter:
 * Package multiple PDF and other documents into a PDF portfolio
 * The generated PDF portfolio requires Acrobat 9 for proper
 * viewing. The documents in the Portfolio will be assigned predefined
 * and custom metadata fields; for the custom fields a schema description
 * is created.
 *
 * Acrobat 8 will only display a "PDF package" with a flat list of documents
 * without any folder structure.
 *
 * Required software: PDFlib/PDFlib+PDI/PPS 8
 * Required data: PDF and other input documents
 */

#include <string.h>

#include "pdflib.h"


int
main(void)
{

    /* This is where the data files are. Adjust as necessary. */
    const char *searchpath = "../data";
    char optlist[1024], *target;
    int i, font, folder;
    PDF * p;

    typedef struct
    {
	const char *filename;
	const char *description;
	const char *status;
	int id;
    } portfolio;

    /* The documents for the Portfolio along with description and metadata */
    static const portfolio pf[] =
    {
    {
        "TIR_____.AFM",
        "Metrics for Times-Roman",
        "internal",
        200
    },
    {
        "nesrin.jpg",
        "Zabrisky point",
        "archived",
        300
    },
    {
	"PDFlib-real-world.pdf",
	"PDFlib in the real world",
	"published",
	100
    },
    {
	"PDFlib-datasheet.pdf",
	"Generate PDF on the fly",
	"published",
	101
    },
    {
	"TET-datasheet.pdf",
	"Extract text and images from PDF",
	"published",
	102
    },
    {
	"PLOP-datasheet.pdf",
	"PDF Linearization, Optimization, Protection",
	"published",
	103
    },
    {
	"pCOS-datasheet.pdf",
	"PDF Information Retrieval Tool",
	"published",
	104
    }
    };
    const int n_files = sizeof(pf) / sizeof(pf[0]);

    /* create a new PDFlib object */
    if ((p = PDF_new()) == (PDF *) 0)
    {
        printf("Couldn't create PDFlib object (out of memory)!\n");
        return(2);
    }

    PDF_TRY(p)
    {
        PDF_set_parameter(p, "SearchPath", searchpath);

        /* This means we must check return values of load_font() etc. */
        PDF_set_parameter(p, "errorpolicy", "return");

        if (PDF_begin_document(p, "starter_portfolio.pdf", 0,
		"compatibility=1.7ext3") == -1)
        {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_set_info(p, "Creator", "PDFlib starter sample");
        PDF_set_info(p, "Title", "starter_portfolio");

        /* Insert all files for the root folder along with their description
         * and the following custom fields:
         * status   string describing the document status
         * id       numerical identifier, prefixed with "PHX"
         */
        for (i = 0; i < 2; i++)
        {
            sprintf(optlist,
                "description={%s} "
                "fieldlist={ "
                        "{key=status value=%s} "
                        "{key=id value=%d prefix=PHX type=text} "
                "}",
                pf[i].description, pf[i].status, pf[i].id);

            /* -1 means root folder */
            PDF_add_portfolio_file(p, -1, pf[i].filename, 0, optlist);
        }

        /* Create the "datasheets" folder in the root folder */
        folder = PDF_add_portfolio_folder(p, -1, "datasheets", 0, "");

	/* Insert documents in the "datasheets" folder along with
	 * description and custom fields
	 */
	for (i = 2; i < n_files; i++)
	{
	    sprintf(optlist,
		"description={%s} "
		"fieldlist={ "
			"{key=status value=%s} "
			"{key=id value=%d prefix=PHX type=text} "
		"}",
		pf[i].description, pf[i].status, pf[i].id);

	    /* Add the file to the "datasheets" folder */
            PDF_add_portfolio_file(p, folder, pf[i].filename, 0, optlist);
	}

	/* Create a single-page document as cover sheet */
	PDF_begin_page_ext(p, 0, 0, "width=a4.width height=a4.height");

	font = PDF_load_font(p, "Helvetica", 0, "winansi", "");
        if (font == -1)
        {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_setfont(p, font, 24);
        PDF_fit_textline(p, "Welcome to the PDFlib Portfolio sample!", 0,
                50, 700, "");

	PDF_end_page_ext(p, "");

	/* Set options for Portfolio display */
	target = optlist;
	sprintf(target, "portfolio={initialview=detail ");
	target += strlen(target);

	/* Add schema definition for Portfolio metadata */
	sprintf(target,
	"schema={ "
	/* Some predefined fields are included here to make them visible. */
	"{order=1 label=Name key=_filename visible editable} "
	"{order=2 label=Description key=_description visible} "
	"{order=3 label=Size key=_size visible} "
	"{order=4 label={Last edited} key=_moddate visible} "

	/* User-defined fields */
	"{order=5 label=Status key=status type=text editable} "
	"{order=6 label=ID key=id type=text editable} ");

	target += strlen(target);
	sprintf(target, "}}");

        PDF_end_document(p, optlist);
    }

    PDF_CATCH(p)
    {
        printf("PDFlib exception occurred:\n");
        printf("[%d] %s: %s\n",
            PDF_get_errnum(p), PDF_get_apiname(p), PDF_get_errmsg(p));
        PDF_delete(p);
        return(2);
    }

    PDF_delete(p);

    return 0;
}
