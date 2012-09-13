/* $Id: starter_opentype.c,v 1.1.2.5 2010/01/28 12:55:34 rjs Exp $
 * Starter sample for OpenType font features
 *
 * Demonstrate various typographic OpenType features after checking
 * whether a particular feature is supported in a font.
 *
 * Required software: PDFlib/PDFlib+PDI/PPS 8
 * Required data: suitable fonts with OpenType feature tables
 *
 * This sample uses a default font which includes a few features.
 * For better results you should replace the default font with a suitable
 * commercial font. Depending on the implementation of the features you
 * may also have to replace the sample text below.
 *
 * Some ideas for suitable test fonts:
 * Palatino Linotype: standard Windows font with many OpenType features
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pdflib.h"

int
main(void)
{

    /* This is where the data files are. Adjust as necessary. */
    const char* searchpath = "../data";
    const char* outfile = "starter_opentype.pdf";

    char optlist[1024];
    PDF *p;
    int row, col, table, test, font;
    const double llx= 50, lly=50, urx=800, ury=550;
    const char *result;

    /* This font will be used unless another one is specified in the table */
    const char *defaulttestfont = "DejaVuSerif";

    const char *header[] = {
        "OpenType feature",
        "Option list",
        "Font name",
        "Raw input (feature disabled)",
        "Feature enabled"
    };
    const int MAXCOL = sizeof(header) / sizeof(header[0]);

    typedef struct {
        const char *description;
        /* the default test font above will be used if this string is empty */
        const char *fontname;
        const char *feature;
        const char *text;
    } testcase;

    static const testcase testcases[] = {
    {
      "ligatures",
      "",
      "liga",
       "ff fi fl ffi ffl"
    },
    {
      "discretionary ligatures",
      "",
      "dlig",
       "st c/o"
    },
    {
      "historical ligatures",
      "",
      "hlig",
       "&.longs;b &.longs;t"
    },
    {
      "small capitals",
      "",
      "smcp",
       "PostScript"
    },
    {
      "ordinals",
      "",
      "ordn",
       "1o 2a 3o"
    },
    {
      "fractions",
      "",
      "frac",
       "1/2 1/4 3/4"
    },
    {
      "alternate fractions",
      "",
      "afrc",
       "1/2 1/4 3/4"
    },
    {
      "slashed zero",
      "",
      "zero",
       "0"
    },
    {
      "historical forms",
      "",
      "hist",
       "s"
    },
    {
      "proportional figures",
      "",
      "pnum",
       "0123456789"
    },
    {
      "old-style figures",
      "",
      "onum",
       "0123456789"
    },
    {
      "lining figures",
      "",
      "lnum",
       "0123456789"
    },
    {
      "superscript",
      "",
      "sups",
       "0123456789"
    },
    };
    const int n_testcases = sizeof(testcases) / sizeof(testcases[0]);

    /* create a new PDFlib object */
    if ((p = PDF_new()) == (PDF *) 0) {
        printf("Couldn't create PDFlib object (out of memory)!\n");
        return(2);
    }

    PDF_TRY(p) {
        PDF_set_parameter(p, "SearchPath", searchpath);
        PDF_set_parameter(p, "textformat", "bytes");
        PDF_set_parameter(p, "charref", "true");

        /* This means that formatting and other errors will raise an
         * exception. This simplifies our sample code, but is not
         * recommended for production code.
         */
        PDF_set_parameter(p, "errorpolicy", "exception");

        /* Set an output path according to the name of the topic */
        if (PDF_begin_document(p, outfile, 0, "") == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_set_info(p, "Creator", "PDFlib starter sample");
        PDF_set_info(p, "Title", "starter_opentype");

        /* Start Page */
        PDF_begin_page_ext(p, 0, 0, "width=a4.height height=a4.width");

        table = -1;

        /* Table header */
        for (row=1, col=1; col <= MAXCOL; col++)
        {
            sprintf(optlist,
           "fittextline={fontname=Helvetica-Bold encoding=unicode fontsize=12} "
           "margin=4"
           );
            table = PDF_add_table_cell(p, table, col, row, header[col-1], 0,
                    optlist);
        }

        /* Create a table with feature samples, one feature per table row */
        for (row=2, test=0; test < n_testcases; row++, test++)
        {
            const char *testfont;
            char buf[1024];

            /* Use the entry in the test table if available, and the
             * default test font otherwise. This way we can easily check
             * a font for all features, as well as insert suitable fonts
             * for individual features.
             */
            if (testcases[test].fontname && *testcases[test].fontname)
                testfont = testcases[test].fontname;
            else
                testfont = defaulttestfont;

            col=1;

            /* Common option list for columns 1-3 */
            sprintf(optlist,
            "fittextline={fontname=Helvetica encoding=unicode fontsize=12} "
            "margin=4");

            /* Column 1: feature description */
            table = PDF_add_table_cell(p, table, col++, row,
                testcases[test].description, 0, optlist);

            /* Column 2: option list */
            sprintf(buf, "features={%s}", testcases[test].feature);
            table = PDF_add_table_cell(p, table, col++, row, buf, 0, optlist);

            /* Column 3: font name */
            table = PDF_add_table_cell(p, table, col++, row, testfont, 0,
                    optlist);

            /* Column 4: raw input text with  feature disabled */
            sprintf(optlist,
                 "fittextline={fontname={%s} encoding=unicode fontsize=12 "
                 "embedding} margin=4", testfont);
            table = PDF_add_table_cell(p, table, col++, row,
                    testcases[test].text, 0, optlist);

            /* Column 5: text with enabled feature, or warning if the
             * feature is not available in the font
             */
            font = PDF_load_font(p, testfont, 0,
                    "unicode", "embedding");

            /* Check whether font contains the required feature table */
            sprintf(optlist, "name=%s", testcases[test].feature);
            if (PDF_info_font(p, font, "feature", optlist) == 1)
            {
                /* feature is available: apply it to the text */
                sprintf(optlist,
                     "fittextline={fontname={%s} encoding=unicode fontsize=12 "
                     "embedding features={%s}} margin=4",
                     testfont, testcases[test].feature);
                table = PDF_add_table_cell(p, table, col++, row,
                   testcases[test].text, 0, optlist);
            }
            else
            {
                /* feature is not available: emit a warning */
                sprintf(optlist,
                     "fittextline={fontname=Helvetica encoding=unicode "
                     "fontsize=12 fillcolor=red} margin=4");
                table = PDF_add_table_cell(p, table, col++, row,
                        "(feature not available in this font)", 0, optlist);
            }

        }

        /* Place the table */
        sprintf(optlist, "header=1 fill={{area=rowodd "
            "fillcolor={gray 0.9}}} stroke={{line=other}} ");
        result = PDF_fit_table(p, table, llx, lly, urx, ury, optlist);

        if (!strcmp(result, "_error"))
        {
            printf("Couldn't place table: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

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
