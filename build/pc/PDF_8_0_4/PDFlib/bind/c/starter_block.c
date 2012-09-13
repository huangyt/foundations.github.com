/* $Id: starter_block.c,v 1.8.2.2 2010/01/28 12:27:51 rjs Exp $
 *
 * Block starter:
 * Import a PDF page containing blocks and fill text and image
 * blocks with some data. For each addressee of the simulated
 * mailing a separate page with personalized information is
 * generated.
 * A real-world application would of course fill the blocks with data
 * retrieved from some external data source.
 *
 * Required software: PPS 8 or above
 * Required data: input PDF, image
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "pdflib.h"


int
main(void)
{
    /* This is where the data files are. Adjust as necessary. */
    const char *searchpath = "../data";
    const char *outfile = "starter_block.pdf";
    const char *infile = "block_template.pdf";
    const char *imagefile = "new.jpg";

    PDF *p;
    int i, j, inpage, indoc, image;
    char optlist[1024], buf[1024];

    /* Names of the person-related blocks contained on the imported page */
    const char *addressblocks[] = {
        "name", "street", "city"
    };

    /* number of address blocks */
    const int nblocks = sizeof(addressblocks) / sizeof(addressblocks[0]);

    /* Data related to various persons used for personalization */
    const char *persons[][3] = {
        {"Mr Maurizio Moroni", "Strada Provinciale 124", "Reggio Emilia"},
        {"Ms Dominique Perrier", "25, rue Lauriston", "Paris"},
        {"Mr Liu Wong", "55 Grizzly Peak Rd.", "Butte"}
    };

    /* number of persons */
    const int npersons = sizeof(persons) / sizeof(persons[0]);

    /* Static text simulates database-driven variable contents */
    const char *intro = "Dear";
    const char *goodbye = "Yours sincerely,\nVictor Kraxi";
    const char *announcement =
        "Our <fillcolor=red>BEST PRICE OFFER<fillcolor=black> includes today:"
        "\n\n"
        "Long Distance Glider\nWith this paper rocket you can send all your "
        "messages even when sitting in a hall or in the cinema pretty near "
        "the back.\n\n"
        "Giant Wing\nAn unbelievable sailplane! It is amazingly robust and "
        "can even do aerobatics. But it is best suited to gliding.\n\n"
        "Cone Head Rocket\nThis paper arrow can be thrown with big swing. "
        "We launched it from the roof of a hotel. It stayed in the air a "
        "long time and covered a considerable distance.\n\n"
        "Super Dart\nThe super dart can fly giant loops with a radius of 4 "
        "or 5 meters and cover very long distances. Its heavy cone point is "
        "slightly bowed upwards to get the lift required for loops.\n\n"
        "Visit us on our Web site at www.kraxi.com!";

    /* create a new PDFlib object */
    if ((p = PDF_new()) == (PDF *) 0) {
        printf("Couldn't create PDFlib object (out of memory)!\n");
        return(2);
    }

    PDF_TRY(p) {
        PDF_set_parameter(p, "SearchPath", searchpath);

        /* This means we must check return values of load_font() etc. */
        PDF_set_parameter(p, "errorpolicy", "return");

        if (PDF_begin_document(p, outfile, 0,
                "destination={type=fitwindow} pagelayout=singlepage") == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_set_info(p, "Creator", "PDFlib starter sample");
        PDF_set_info(p, "Title", "starter_block");

        /* Open the Block template which contains PDFlib Blocks */
        indoc = PDF_open_pdi_document(p, infile, 0, "");
        if (indoc == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        /* Open the first page and clone the page size */
        inpage = PDF_open_pdi_page(p, indoc, 1, "cloneboxes");
        if (inpage == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        image = PDF_load_image(p, "auto", imagefile, 0, "");

        if (image == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            return(3);
        }

        /* Based on the imported page generate several pages with the blocks
         * being filled with data related to different persons
         */
        for (i = 0; i < npersons; i++)
        {
            /* Start the output page with a dummy size */
            PDF_begin_page_ext(p, 10, 10, "");

            /* Place the imported page on the output page, and clone all
             * page boxes which are present in the input page; this will
             * override the dummy size used in begin_page_ext().
             */
            PDF_fit_pdi_page(p, inpage, 0, 0, "cloneboxes");

            /* Option list for text blocks */
            strcpy(optlist, "encoding=winansi embedding");

            /* Loop over all person-related blocks. Fill the j-th block with the
             * corresponding entry of the persons array.
             */
            for (j = 0; j < nblocks; j++) {
                if (PDF_fill_textblock(p, inpage, addressblocks[j],
                        persons[i][j], 0, optlist) == -1)
                    printf("Warning: %s\n", PDF_get_errmsg(p));
            }

            /* Fill the "intro" block */
            sprintf(buf, "%s %s,", intro, persons[i][0]);
            if (PDF_fill_textblock(p, inpage, "intro", buf, 0, optlist) == -1)
                printf("Warning: %s\n", PDF_get_errmsg(p));

            /* Fill the "announcement" block */
            if (PDF_fill_textblock(p, inpage, "announcement", announcement, 0,
                    optlist) == -1)
                printf("Warning: %s\n", PDF_get_errmsg(p));

            /* Fill the "goodbye" block */
            if (PDF_fill_textblock(p, inpage, "goodbye", goodbye, 0,
                    optlist) == -1)
                printf("Warning: %s\n", PDF_get_errmsg(p));

            /* Fill the image block */
            strcpy(optlist, "");       /* Option list */
            if (PDF_fill_imageblock(p, inpage, "icon", image, optlist) == -1)
                printf("Warning: %s\n", PDF_get_errmsg(p));

            PDF_end_page_ext(p, "");
        }

        PDF_close_pdi_page(p, inpage);
        PDF_close_pdi_document(p, indoc);
        PDF_close_image(p, image);

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
