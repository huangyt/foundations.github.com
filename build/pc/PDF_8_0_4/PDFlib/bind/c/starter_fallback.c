/* $Id: starter_fallback.c,v 1.1.2.6 2010/01/28 20:47:15 rjs Exp $
 * Starter sample for fallback fonts
 *
 * Required software: PDFlib/PDFlib+PDI/PPS 8
 * Required data: suitable fonts, Japanese CMaps
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
    const char* outfile = "starter_fallback.pdf";

    char optlist[1024];
    PDF *p;
    int row, col, table, test;
    double llx= 50, lly=50, urx=800, ury=550;
    const char *result;

    const char *header[] = {
        "Use case",
        "Option list for the 'fallbackfonts' option",
        "Base font",
        "With fallback font"
    };
    int MAXCOL = sizeof(header)/sizeof(header[0]);

    typedef struct {
        const char *usecase;
        const char *fontname;
        const char *encoding;
        const char *fallbackoptions;
        const char *text;
    } testcase;

    static const testcase testcases[] = {
    { /* Add Euro glyph to an encoding which doesn't support it */
      "Extend 8-bit encoding",
      "Helvetica",
      "iso8859-1",
      "{fontname=Helvetica encoding=unicode forcechars=euro}",
      /* Reference Euro glyph by name (since it is missing from the encoding) */
       "123&euro;"
    },
    {
      "Use Euro glyph from another font",
      "Courier",
      "winansi",
      "{fontname=Helvetica encoding=unicode forcechars=euro textrise=-5%}",
       "123&euro;"
    },
    {
      "Enlarge all glyphs in a font",
      "Times-Italic",
      "winansi",
      /* Enlarge all glyphs to better match other fonts of the same point size
       */
      "{fontname=Times-Italic encoding=unicode forcechars={U+0020-U+00FF} "
      "fontsize=120%}",
       "font size"
    },
    {
      "Add enlarged pictogram",
      "Times-Roman",
      "unicode",
      /* pointing hand pictogram */
      "{fontname=ZapfDingbats encoding=unicode forcechars=.a12 fontsize=150% "
      "textrise=-15%}",
       "Bullet symbol: &.a12;"
    },
    {
      "Add enlarged symbol glyph",
      "Times-Roman",
      "unicode",
      "{fontname=Symbol encoding=unicode forcechars=U+2663 fontsize=125%}",
       "Club symbol: &#x2663;"
    },
    { /* Greek characters missing in the font will be pulled from Symbol font */
      "Add Greek characters to Latin font",
      "Times-Roman",
      "unicode",
      "{fontname=Symbol encoding=unicode}",
       "Greek text: &#x039B;&#x039F;&#x0393;&#x039F;&#x03A3;"
    },
    { /* Font with end-user defined character (EUDC) */
      "Gaiji with EUDC font",
      "KozMinProVI-Regular",
      "unicode",
      "{fontname=EUDC encoding=unicode forcechars=U+E000 fontsize=140% "
      "textrise=-20%}",
       "Gaiji: &#xE000;"
    },
    { /* SING fontlet containing a single gaiji character */
      "Gaiji with SING font",
      "KozMinProVI-Regular",
      "unicode",
      "{fontname=PDFlibWing encoding=unicode forcechars=gaiji}",
       "Gaiji: &#xE000;"
    },
    { "Replace Latin characters in CJK font",
      "KozMinProVI-Regular",
      "unicode",
      "{fontname=Courier-Bold encoding=unicode forcechars={U+0020-U+007E}}",
       "Latin and &#x65E5;&#x672C;&#x8A9E;"
    },
    /* Requires "Unicode BMP Fallback SIL" font in fallback.ttf */
    { /* Identify missing glyphs caused by workflow problems */
      "Identify missing glyphs",
      "Times-Roman",
      "unicode",
      "{fontname=fallback encoding=unicode}",
      /* deliberately use characters which are not available in the base font */
       "Missing glyphs: &#x1234; &#x672C; &#x8A9E;"
    },
    };
    int n_testcases = sizeof(testcases)/sizeof(testcases[0]);

    /* create a new PDFlib object */
    if ((p = PDF_new()) == (PDF *) 0) {
        printf("Couldn't create PDFlib object (out of memory)!\n");
        return(2);
    }

    PDF_TRY(p) {
        PDF_set_parameter(p, "SearchPath", searchpath);
        PDF_set_parameter(p, "textformat", "bytes");
        PDF_set_parameter(p, "charref", "true");
        PDF_set_parameter(p, "glyphcheck", "replace");

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
        PDF_set_info(p, "Title", "starter_fallback");

        /* Start Page */
        PDF_begin_page_ext(p, 0, 0, "width=a4.height height=a4.width");

        table = -1;

        /* Table header */
        for (row=1, col=1; col <= MAXCOL; col++)
        {
            sprintf(optlist,
           "fittextline={fontname=Helvetica-Bold encoding=unicode fontsize=11} "
           "margin=4"
           );
            table = PDF_add_table_cell(p, table, col, row, header[col-1], 0,
                    optlist);
        }

        /* Create fallback samples, one use case per row */
        for (row=2, test=0; test < n_testcases; row++, test++)
        {
            col=1;

            /* Column 1: description of the use case */
            sprintf(optlist,
            "fittextline={fontname=Helvetica encoding=unicode fontsize=11} "
            "margin=4");
            table = PDF_add_table_cell(p, table, col++, row,
                testcases[test].usecase, 0, optlist);

            /* Column 2: reproduce option list literally */
            sprintf(optlist,
            "fittextline={fontname=Helvetica encoding=unicode fontsize=10} "
            "margin=4");
            table = PDF_add_table_cell(p, table, col++, row,
                testcases[test].fallbackoptions, 0, optlist);

            /* Column 3: text with base font */
            sprintf(optlist,
                 "fittextline={fontname=%s encoding=%s fontsize=11 "
                 "replacementchar=? } margin=4",
                 testcases[test].fontname, testcases[test].encoding);
            table = PDF_add_table_cell(p, table, col++, row,
                    testcases[test].text, 0, optlist);

            /* Column 4: text with base font and fallback fonts */
            sprintf(optlist,
                 "fittextline={fontname=%s encoding=%s "
                 "fontsize=11 fallbackfonts={%s}} margin=4",
                 testcases[test].fontname,
                 testcases[test].encoding,
                 testcases[test].fallbackoptions);
            table = PDF_add_table_cell(p, table, col++, row,
               testcases[test].text, 0, optlist);
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
