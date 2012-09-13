/* $Id: starter_opentype.cpp,v 1.1 2009/09/08 14:45:30 stm Exp $
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

#include <iostream>
#include <string>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    /* This is where the data files are. Adjust as necessary. */
    const wstring searchpath = L"../data";
    const wstring outfile = L"starter_opentype.pdf";

    wostringstream optlist;
    PDFlib p;
    int row, col, table, test, font;
    const double llx=50, lly=50, urx=800, ury=550;
    wstring result;

    /* This font will be used unless another one is specified in the table */
    const wstring defaulttestfont = L"DejaVuSerif";

    const wstring header[] = {
        L"OpenType feature",
        L"Option list",
        L"Font name",
        L"Raw input (feature disabled)",
        L"Feature enabled"
    };
    const int MAXCOL = sizeof(header) / sizeof(header[0]);

    typedef struct {
        const wstring description;
        /* the default test font above will be used if this string is empty */
        const wstring fontname;
        const wstring feature;
        const wstring text;
    } testcase;

    static const testcase testcases[] = {
    {
      L"ligatures",
      L"",
      L"liga",
       L"ff fi fl ffi ffl"
    },
    {
      L"discretionary ligatures",
      L"",
      L"dlig",
       L"st c/o"
    },
    {
      L"historical ligatures",
      L"",
      L"hlig",
       L"&.longs;b &.longs;t"
    },
    {
      L"small capitals",
      L"",
      L"smcp",
       L"PostScript"
    },
    {
      L"ordinals",
      L"",
      L"ordn",
       L"1o 2a 3o"
    },
    {
      L"fractions",
      L"",
      L"frac",
       L"1/2 1/4 3/4"
    },
    {
      L"alternate fractions",
      L"",
      L"afrc",
       L"1/2 1/4 3/4"
    },
    {
      L"slashed zero",
      L"",
      L"zero",
       L"0"
    },
    {
      L"historical forms",
      L"",
      L"hist",
       L"s"
    },
    {
      L"proportional figures",
      L"",
      L"pnum",
       L"0123456789"
    },
    {
      L"old-style figures",
      L"",
      L"onum",
       L"0123456789"
    },
    {
      L"lining figures",
      L"",
      L"lnum",
       L"0123456789"
    },
    {
      L"superscript",
      L"",
      L"sups",
       L"0123456789"
    },
    };
    const int n_testcases = sizeof(testcases) / sizeof(testcases[0]);

    try {
        p.set_parameter(L"SearchPath", searchpath);
        p.set_parameter(L"charref", L"true");

        /* This means that formatting and other errors will raise an
         * exception. This simplifies our sample code, but is not
         * recommended for production code.
         */
        p.set_parameter(L"errorpolicy", L"exception");

        /* Set an output path according to the name of the topic */
        if (p.begin_document(outfile, L"") == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        p.set_info(L"Creator", L"PDFlib starter sample");
        p.set_info(L"Title", L"starter_opentype");

        /* Start Page */
        p.begin_page_ext(0, 0, L"width=a4.height height=a4.width");

        table = -1;

        /* Table header */
        for (row=1, col=1; col <= MAXCOL; col++)
        {
            optlist.str(L"");
            optlist << L"fittextline={fontname=Helvetica-Bold encoding=unicode "
                       L"fontsize=12} margin=4";
            table = p.add_table_cell(table, col, row, header[col-1],
                                        optlist.str());
        }

        /* Create a table with feature samples, one feature per table row */
        for (row=2, test=0; test < n_testcases; row++, test++)
        {
            wstring testfont;
            wostringstream buf;

            /* Use the entry in the test table if available, and the
             * default test font otherwise. This way we can easily check
             * a font for all features, as well as insert suitable fonts
             * for individual features.
             */
            if (testcases[test].fontname.length() > 0)
                testfont = testcases[test].fontname;
            else
                testfont = defaulttestfont;

            col = 1;

            /* Common option list for columns 1-3 */
            optlist.str(L"");
            optlist << L"fittextline={fontname=Helvetica encoding=unicode "
                       L"fontsize=12} margin=4";

            /* Column 1: feature description */
            table = p.add_table_cell(table, col++, row,
                testcases[test].description,optlist.str());

            /* Column 2: option list */
            buf << L"features={" << testcases[test].feature << L"}";
            table = p.add_table_cell(table, col++, row, buf.str(),
                                        optlist.str());

            /* Column 3: font name */
            table = p.add_table_cell(table, col++, row, testfont,
                                        optlist.str());

            /* Column 4: raw input text with  feature disabled */
            optlist.str(L"");
            optlist << L"fittextline={fontname={" << testfont << L"} "
                       L"encoding=unicode fontsize=12 embedding} margin=4";
            table = p.add_table_cell(table, col++, row,
                    testcases[test].text, optlist.str());

            /* Column 5: text with enabled feature, or warning if the
             * feature is not available in the font
             */
            font = p.load_font(testfont, L"unicode", L"embedding");

            /* Check whether font contains the required feature table */
            optlist.str(L"");
            optlist << L"name=" << testcases[test].feature;
            if (p.info_font(font, L"feature", optlist.str()) == 1)
            {
                /* feature is available: apply it to the text */                optlist.clear();
                optlist.str(L"");
                optlist << L"fittextline={fontname={" << testfont << L"} "
                    << L"encoding=unicode fontsize=12 embedding features={"
                    << testcases[test].feature << L"}} margin=4";
                table = p.add_table_cell(table, col++, row,
                   testcases[test].text, optlist.str());
            }
            else
            {
                /* feature is not available: emit a warning */
                optlist.str(L"");
                optlist << L"fittextline={fontname=Helvetica encoding=unicode "
                           L"fontsize=12 fillcolor=red} margin=4";
                table = p.add_table_cell(table, col++, row,
                        L"(feature not available in this font)", optlist.str());
            }
        }

        /* Place the table */
        optlist.str(L"");
        optlist << L"header=1 fill={{area=rowodd fillcolor={gray 0.9}}} "
                   L"stroke={{line=other}}";
        result = p.fit_table(table, llx, lly, urx, ury, optlist.str());

        if (result == L"_error")
        {
            wcerr << L"Couldn't place table: " << p.get_errmsg() << endl;
            return 2;
        }

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
