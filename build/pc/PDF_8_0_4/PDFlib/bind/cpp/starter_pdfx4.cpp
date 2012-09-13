/* $Id: starter_pdfx4.cpp,v 1.1 2009/09/09 07:51:02 stm Exp $
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

#include <iostream>
#include <string>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    /* This is where the data files are. Adjust as necessary.*/
    const wstring searchpath = L"../data";

    const wstring imagefile = L"zebra.tif";
    wostringstream optlist;
    PDFlib p;
    int font, image, icc, gstate;
    int layer_english, layer_german, layer_french, layer_image;
    double width, height;

    try {
        /* This means we must check return values of load_font() etc. */
        p.set_parameter(L"errorpolicy", L"return");

        p.set_parameter(L"SearchPath", searchpath);

        if (p.begin_document(L"starter_pdfx4.pdf", L"pdfx=PDF/X-4") == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        p.set_info(L"Creator", L"PDFlib starter sample");
        p.set_info(L"Title", L"starter_pdfx4");


        if (p.load_iccprofile(L"ISOcoated.icc", L"usage=outputintent") == -1)
        {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            wcerr << L"Please install the ICC profile package from "
                     L"www.pdflib.com to run the PDF/X-4 starter sample."
                    << endl;
            return 2;
        }

        /* Define the low-level layers. These cannot be controlled directly
         * in Acrobat's layer pane.
         */

        layer_english = p.define_layer(L"English text", L"");
        layer_german  = p.define_layer(L"German text", L"");
        layer_french  = p.define_layer(L"French text", L"");
        layer_image   = p.define_layer(L"Images", L"");

	/* Define a radio button relationship for the language layers.
	 * Individual layers will only be visible in Acrobat X (but
	 * not Acrobat 9).
	 */
        optlist.str(L"");
        optlist << L"group={" << layer_english << L" " << layer_german
		<< L" " << layer_french << L"}";
        p.set_layer_dependency(L"Radiobtn", optlist.str());


        /* Define the layer combinations for document variants. The variants
         * control the low-level layers, and can be activated in Acrobat 9's
         * layer pane. Using layer variants we can make sure that the image
         * layer cannot accidentally be disabled; it will always accompany
         * the text regardless of the selected language.
         */

        optlist.str(L"");
        optlist << L"variantname={English variant} includelayers={"
            << layer_english << L" " << layer_image
	    << L"} defaultvariant=true createorderlist";
        p.set_layer_dependency(L"Variant", optlist.str());

        optlist.str(L"");
        optlist << L"variantname={German variant} includelayers={"
            << layer_german << L" " << layer_image << L"}";
        p.set_layer_dependency(L"Variant", optlist.str());

        optlist.str(L"");
        optlist << L"variantname={French variant} includelayers={"
            << layer_french << L" " << layer_image << L"}";
        p.set_layer_dependency(L"Variant", optlist.str());


        p.begin_page_ext(595, 842, L"");

        /* Font embedding is required for PDF/X */
        font = p.load_font(L"LuciduxSans-Oblique", L"winansi", L"embedding");

        if (font == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        p.setfont(font, 24);

        p.begin_layer(layer_english);

        p.fit_textline(L"PDF/X-4 starter sample with layers", 50, 700, L"");

        p.begin_layer(layer_german);
        p.fit_textline(L"PDF/X-4 Starter-Beispiel mit Ebenen", 50, 700, L"");

        p.begin_layer(layer_french);
        p.fit_textline(L"PDF/X-4 Starter exemple avec des calques", 50, 700,
                        L"");

        p.begin_layer(layer_image);

        p.setfont(font, 48);

        /* The RGB image needs an ICC profile; we use sRGB. */
        icc = p.load_iccprofile(L"sRGB", L"");
        optlist.str(L"");
        optlist << L"iccprofile=" << icc;
        image = p.load_image(L"auto", imagefile, optlist.str());

        if (image == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        /* Place a diagonal stamp across the image area */
        width = p.info_image(image, L"width", L"");
        height = p.info_image(image, L"height", L"");

        optlist.str(L"");
        optlist << L"boxsize={" << width << L" " << height << L"} stamp=ll2ur";
        p.fit_textline(L"Zebra", 0, 0, optlist.str());

        /* Set transparency in the graphics state */
        gstate = p.create_gstate(L"opacityfill=0.5");
        p.set_gstate(gstate);

        /* Place the image on the page and close it */
        p.fit_image(image, 0.0, 0.0, L"");
        p.close_image(image);

        /* Close all layers */
        p.end_layer();

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
