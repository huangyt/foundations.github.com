/* $Id: starter_geospatial.c,v 1.3 2009/09/02 19:18:29 tm Exp $
 * Starter for georeferenced PDF:
 * Import an image with a map and add geospatial reference information
 *
 * Sample map and coordinates:
 * We use a map from www.openstreetmap.com; the geospatial coordinates of the
 * image edges were also provided by that Web site.
 * The coordinate system is WGS84 which is also used for GPS.
 *
 * Required software: PDFlib/PDFlib+PDI/PPS 8
 * Required data: image file and associated geospatial reference information
 */

#include <iostream>
#include <string>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    /* This is where the data files are. Adjust if necessary. */
    const wstring searchpath = L"../data";
    const wstring outfile = L"starter_geospatial.pdf";

    wostringstream georefoptlist;
    PDFlib p;
    int image;
    const wstring imagefile = L"munich.png";

    /* WKT for plain latitude/longitude values in WGS84 */
    const wstring georef =
    L"worldsystem={type=geographic wkt={"
        L"GEOGCS[\"WGS 84\","
        L"  DATUM[\"WGS_1984\", SPHEROID[\"WGS 84\", 6378137,298.257223563]],"
        L"  PRIMEM[\"Greenwich\", 0.0],"
        L"  UNIT[\"Degree\", 0.01745329251994328]]"
    L"}} linearunit=M areaunit=SQM angularunit=degree";

    /* world coordinates of the image (in degrees) */
    const double worldpoints[] =
    {
        48.145, /* latitude of top edge */
        11.565, /* longitude of left edge */
        11.59,  /* longitude of right edge */
        48.13   /* latitude of bottom edge */
    };

    try {
        p.set_parameter(L"SearchPath", searchpath);

        /* This means we must check return values of load_font() etc. */
        p.set_parameter(L"errorpolicy", L"return");

        /* Start the document */
        if (p.begin_document(outfile, L"compatibility=1.7ext3") == -1)
        {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        p.set_info(L"Creator", L"PDFlib starter sample");
        p.set_info(L"Title", L"starter_geospatial");

        /* Generate georeference option list. Use the four corners as reference
         * points; (0,0)=lower left etc.
         */
        georefoptlist << L"georeference={" << georef
            << L" mappoints={0 0  1 0  1 1  0 1} worldpoints={";

        /* lower left corner */
        georefoptlist << worldpoints[3] << L" " << worldpoints[1] << L" ";
        /* lower right corner */
        georefoptlist << worldpoints[3] << L" " << worldpoints[2] << L" ";
        /* upper right corner */
        georefoptlist << worldpoints[0] << L" " << worldpoints[2] << L" ";
        /* upper left corner */
        georefoptlist << worldpoints[0] << L" " << worldpoints[1] << L" ";

        georefoptlist << L"} }";

        /* Load the image with geospatial reference attached */
        image = p.load_image(L"auto", imagefile, georefoptlist.str());
        if (image == -1)
        {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        p.begin_page_ext(0, 0, L"width=a4.width height=a4.height");

        /* Create caption */
        p.fit_textline(L"Map with geospatial reference information",
            50, 700,
            L"fontname=LuciduxSans-Oblique encoding=winansi fontsize=18");

        /* Place the map on the page */
        p.fit_image(image, 50, 50, L"boxsize={500 600} fitmethod=meet");

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
