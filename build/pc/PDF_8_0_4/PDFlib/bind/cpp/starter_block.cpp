/* $Id: starter_block.cpp,v 1.9 2009/09/09 08:15:53 stm Exp $
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

#include <iostream>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    /* This is where the data files are. Adjust as necessary. */
    const wstring searchpath = L"../data";
    const wstring outfile = L"starter_block.pdf";
    const wstring infile = L"block_template.pdf";
    const wstring imagefile = L"new.jpg";

    PDFlib p;
    int i, j, inpage, indoc, image;

    /* Names of the person-related blocks contained on the imported page */
    const wstring addressblocks[] = {
        L"name", L"street", L"city"
    };

    /* number of address blocks */
    const int nblocks = sizeof(addressblocks) / sizeof(addressblocks[0]);

    /* Data related to various persons used for personalization */
    const wstring persons[][3] = {
        {L"Mr Maurizio Moroni", L"Strada Provinciale 124", L"Reggio Emilia"},
        {L"Ms Dominique Perrier", L"25, rue Lauriston", L"Paris"},
        {L"Mr Liu Wong", L"55 Grizzly Peak Rd.", L"Butte"}
    };

    /* number of persons */
    const int npersons = sizeof(persons) / sizeof(persons[0]);

    /* Static text simulates database-driven variable contents */
    const wstring intro = L"Dear";
    const wstring goodbye = L"Yours sincerely,\nVictor Kraxi";
    const wstring announcement =
        L"Our <fillcolor=red>BEST PRICE OFFER<fillcolor=black> includes today:"
        L"\n\n"
        L"Long Distance Glider\nWith this paper rocket you can send all your "
        L"messages even when sitting in a hall or in the cinema pretty near "
        L"the back.\n\n"
        L"Giant Wing\nAn unbelievable sailplane! It is amazingly robust and "
        L"can even do aerobatics. But it is best suited to gliding.\n\n"
        L"Cone Head Rocket\nThis paper arrow can be thrown with big swing. "
        L"We launched it from the roof of a hotel. It stayed in the air a "
        L"long time and covered a considerable distance.\n\n"
        L"Super Dart\nThe super dart can fly giant loops with a radius of 4 "
        L"or 5 meters and cover very long distances. Its heavy cone point is "
        L"slightly bowed upwards to get the lift required for loops.\n\n"
        L"Visit us on our Web site at www.kraxi.com!";

    try {
        p.set_parameter(L"SearchPath", searchpath);

        /* This means we must check return values of load_font() etc. */
        p.set_parameter(L"errorpolicy", L"return");

        if (p.begin_document(outfile,
                L"destination={type=fitwindow} pagelayout=singlepage") == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        p.set_info(L"Creator", L"PDFlib starter sample");
        p.set_info(L"Title", L"starter_block");

        /* Open the Block template which contains PDFlib Blocks */
        indoc = p.open_pdi_document(infile, L"");
        if (indoc == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        /* Open the first page and clone the page size */
        inpage = p.open_pdi_page(indoc, 1, L"cloneboxes");
        if (inpage == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        image = p.load_image(L"auto", imagefile, L"");

        if (image == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 3;
        }

        /* Based on the imported page generate several pages with the blocks
         * being filled with data related to different persons
         */
        for (i = 0; i < npersons; i++)
        {
            /* Start the output page with a dummy size */
            p.begin_page_ext(10, 10, L"");

            /* Place the imported page on the output page, and clone all
             * page boxes which are present in the input page; this will
             * override the dummy size used in begin_page_ext().
             */
            p.fit_pdi_page(inpage, 0, 0, L"cloneboxes");

            /* Option list for text blocks */
            const wstring optlist = L"encoding=winansi embedding";

            /* Loop over all person-related blocks. Fill the j-th block with the
             * corresponding entry of the persons array.
             */
            for (j = 0; j < nblocks; j++) {
                if (p.fill_textblock(inpage, addressblocks[j],
                        persons[i][j], optlist) == -1)
                    wcerr << L"Warning: " << p.get_errmsg() << endl;
            }

            /* Fill the "intro" block */
            wostringstream buf;
            buf << intro << L" " << persons[i][0] << L",";
            if (p.fill_textblock(inpage, L"intro", buf.str(), optlist) == -1)
                wcerr << L"Warning: " << p.get_errmsg() << endl;

            /* Fill the "announcement" block */
            if (p.fill_textblock(inpage, L"announcement", announcement,
                    optlist) == -1)
                wcerr << L"Warning: " << p.get_errmsg() << endl;

            /* Fill the "goodbye" block */
            if (p.fill_textblock(inpage, L"goodbye", goodbye,
                    optlist) == -1)
                wcerr << L"Warning: " << p.get_errmsg() << endl;

            /* Fill the image block */
            if (p.fill_imageblock(inpage, L"icon", image, L"") == -1)
                wcerr << L"Warning: " << p.get_errmsg() << endl;

            p.end_page_ext(L"");
        }

        p.close_pdi_page(inpage);
        p.close_pdi_document(indoc);
        p.close_image(image);

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
