/* $Id: starter_geospatial.c,v 1.1.2.3 2010/01/27 20:52:49 rjs Exp $
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

#include <string.h>

#include "pdflib.h"

int
main(void)
{
    /* This is where the data files are. Adjust if necessary. */
    const char* searchpath = "../data";
    const char* outfile = "starter_geospatial.pdf";

    char georefoptlist[2048], *target;
    PDF *p;
    int image;
    const char *imagefile = "munich.png";

    /* WKT for plain latitude/longitude values in WGS84 */
    const char *georef =
    "worldsystem={type=geographic wkt={"
        "GEOGCS[\"WGS 84\","
        "  DATUM[\"WGS_1984\", SPHEROID[\"WGS 84\", 6378137,298.257223563]],"
        "  PRIMEM[\"Greenwich\", 0.0],"
        "  UNIT[\"Degree\", 0.01745329251994328]]"
    "}} linearunit=M areaunit=SQM angularunit=degree";

    /* world coordinates of the image (in degrees) */
    double worldpoints[] =
    {
        48.145, /* latitude of top edge */
        11.565, /* longitude of left edge */
        11.59,  /* longitude of right edge */
        48.13   /* latitude of bottom edge */
    };

    /* create a new PDFlib object */
    if ((p = PDF_new()) == (PDF *) 0) {
        printf("Couldn't create PDFlib object (out of memory)!\n");
        return(2);
    }

    PDF_TRY(p) {
        PDF_set_parameter(p, "SearchPath", searchpath);

        /* This means we must check return values of load_font() etc. */
        PDF_set_parameter(p, "errorpolicy", "return");

        /* Start the document */
        if (PDF_begin_document(p, outfile, 0, "compatibility=1.7ext3") == -1)
        {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_set_info(p, "Creator", "PDFlib starter sample");
        PDF_set_info(p, "Title", "starter_geospatial");

        /* Generate georeference option list */
        target=georefoptlist;

        /* Use the four corners as reference points; (0,0)=lower left etc. */
        sprintf(target, "georeference={%s mappoints={0 0  1 0  1 1  0 1} ",
                georef);
        target += strlen(target);

        sprintf(target, "worldpoints={");
        target += strlen(target);

        /* lower left corner */
        sprintf(target, "%g %g ", worldpoints[3], worldpoints[1]);
        target += strlen(target);
        /* lower right corner */
        sprintf(target, "%g %g ", worldpoints[3], worldpoints[2]);
        target += strlen(target);
        /* upper right corner */
        sprintf(target, "%g %g ", worldpoints[0], worldpoints[2]);
        target += strlen(target);
        /* upper left corner */
        sprintf(target, "%g %g ", worldpoints[0], worldpoints[1]);
        target += strlen(target);

        sprintf(target, "} }");
        target += strlen(target);

        /* Load the image with geospatial reference attached */
        image = PDF_load_image(p, "auto", imagefile, 0, georefoptlist);
        if (image == -1)
        {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_begin_page_ext(p, 0, 0, "width=a4.width height=a4.height");

        /* Create caption */
        PDF_fit_textline(p, "Map with geospatial reference information", 0,
            50, 700,
            "fontname=LuciduxSans-Oblique encoding=winansi fontsize=18");

        /* Place the map on the page */
        PDF_fit_image(p, image, 50, 50, "boxsize={500 600} fitmethod=meet");

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
