/* $Id: starter_pdfx4.c,v 1.2.2.4 2011/07/10 19:19:20 rjs Exp $
 *
 * PDF/X-4 starter:
 * Create PDF/X-4 conforming output with layer variants and transparency
 *
 * A low-level layer is created for each of several languages, as well
 * as an image layer. Each of the language layers together with the
 * image layer forms a "layer variant" according to PDF/X-4 (in Acrobat
 * layer variants are called "configurations").
 * This ensures that low-level layers cannot be enabled/disabled individually,
 * but only via the corresponding layer variant. This prevents accidental
 * printing of a language layer without the required image layer.
 *
 * The document contains transparent text which is allowed in
 * PDF/X-4, but not earlier PDF/X standards.
 *
 * Required software: PDFlib/PDFlib+PDI/PPS 8
 * Required data: font file, image file, ICC output intent profile
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

    PDF *p;
    const char * imagefile = "zebra.tif";
    char optlist[1024];

    int font, image, icc, gstate;
    int layer_english, layer_german, layer_french, layer_image;
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

        if (PDF_begin_document(p, "starter_pdfx4.pdf", 0, "pdfx=PDF/X-4")
                == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_set_info(p, "Creator", "PDFlib starter sample");
        PDF_set_info(p, "Title", "starter_pdfx4");


        if (PDF_load_iccprofile(p, "ISOcoated.icc", 0,
                "usage=outputintent") == -1)
        {
            printf("Error: %s\n", PDF_get_errmsg(p));
            printf("Please install the ICC profile package from "
                   "www.pdflib.com to run the PDF/X-4 starter sample.\n");
            PDF_delete(p);
            return(2);
        }

        /* Define the low-level layers. These cannot be controlled directly
         * in Acrobat's layer pane.
         */

        layer_english = PDF_define_layer(p, "English text", 0, "");
        layer_german  = PDF_define_layer(p, "German text", 0, "");
        layer_french  = PDF_define_layer(p, "French text", 0, "");
        layer_image   = PDF_define_layer(p, "Images", 0, "");

	/* Define a radio button relationship for the language layers.
	 * Individual layers will only be visible in Acrobat X (but
	 * not Acrobat 9).
	 */
        sprintf(optlist,
        	"group={%d %d %d}",
        	layer_english, layer_german, layer_french);
        PDF_set_layer_dependency(p, "Radiobtn", optlist);


        /* Define the layer combinations for document variants. The variants
         * control the low-level layers, and can be activated in Acrobat 9's
         * layer pane. Using layer variants we can make sure that the image
         * layer cannot accidentally be disabled; it will always accompany
         * the text regardless of the selected language.
         */

        sprintf(optlist,
        	"variantname={English variant} includelayers={%d %d} "
        	"defaultvariant=true createorderlist",
        	layer_english, layer_image);
        PDF_set_layer_dependency(p, "Variant", optlist);

        sprintf(optlist,
        	"variantname={German variant} includelayers={%d %d}",
        	layer_german, layer_image);
        PDF_set_layer_dependency(p, "Variant", optlist);

        sprintf(optlist,
        	"variantname={French variant} includelayers={%d %d}",
        	layer_french, layer_image);
        PDF_set_layer_dependency(p, "Variant", optlist);


        PDF_begin_page_ext(p, 595, 842, "");

        /* Font embedding is required for PDF/X */
        font = PDF_load_font(p, "LuciduxSans-Oblique", 0,
                "winansi", "embedding");

        if (font == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        PDF_setfont(p, font, 24);

        PDF_begin_layer(p, layer_english);

        PDF_fit_textline(p, "PDF/X-4 starter sample with layers",
			0, 50, 700, "");

        PDF_begin_layer(p, layer_german);
        PDF_fit_textline(p, "PDF/X-4 Starter-Beispiel mit Ebenen",
			0, 50, 700, "");

        PDF_begin_layer(p, layer_french);
        PDF_fit_textline(p, "PDF/X-4 Starter exemple avec des calques",
			0, 50, 700, "");

        PDF_begin_layer(p, layer_image);

        PDF_setfont(p, font, 48);

        /* The RGB image needs an ICC profile; we use sRGB. */
        icc = PDF_load_iccprofile(p, "sRGB", 0, "");
        sprintf(optlist, "iccprofile=%d", icc);
        image = PDF_load_image(p, "auto", imagefile, 0, optlist);

        if (image == -1) {
            printf("Error: %s\n", PDF_get_errmsg(p));
            PDF_delete(p);
            return(2);
        }

        /* Place a diagonal stamp across the image area */
        width = PDF_info_image(p, image, "width", "");
        height = PDF_info_image(p, image, "height", "");

        sprintf(optlist, "boxsize={%f %f} stamp=ll2ur", width, height);
        PDF_fit_textline(p, "Zebra", 0, 0, 0, optlist);

        /* Set transparency in the graphics state */
        gstate = PDF_create_gstate(p, "opacityfill=0.5");
        PDF_set_gstate(p, gstate);

        /* Place the image on the page and close it */
        PDF_fit_image(p, image, (double) 0.0, (double) 0.0, "");
        PDF_close_image(p, image);

        /* Close all layers */
        PDF_end_layer(p);

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
