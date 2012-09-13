// $Id: starter_3d.cpp,v 1.6.2.2 2011/06/06 11:32:09 rjs Exp $
// 3D Starter:
// Load a 3D model and create a 3D annotation from it.
//
// Define a 3D view and load some 3D data with the view defined. Then create
// an annotation containing the loaded 3D data with the defined 3D view as the
// initial view.
//
// Required software: PDFlib/PDFlib+PDI/PPS 8.0.3
// Required data: PRC data file
//

#include <iostream>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    try {
	// This is where the data files are. Adjust if necessary.
	const wstring searchpath = L"../data";
	const wstring outfile = L"starter_3d.pdf";

	wostringstream buf;
	wstring optlist;
	int font, view, data;
	PDFlib p;

        p.set_parameter(L"SearchPath", searchpath);

        // This means we must check return values of load_font() etc.
        p.set_parameter(L"errorpolicy", L"return");

        // Start the document
        if (p.begin_document(outfile, L"compatibility=1.7ext3") == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        p.set_info(L"Creator", L"PDFlib Cookbook");
        p.set_info(L"Title", L"starter_3d");

        font = p.load_font(L"Helvetica", L"winansi", L"");
        if (font == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        p.begin_page_ext(0, 0, L"width=a4.width height=a4.height");

        // Define a 3D view which shows the model from the top 
        optlist = L"type=PRC name=FirstView background={fillcolor=Lavender} "
	    L"camera2world={-1 0 0 0 1 0 0 0 -1 0.5 0 300}";
        if ((view = p.create_3dview(L"First view", optlist)) == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        // Load some 3D data with the view defined above
        buf.str(L"");
        buf << L"type=PRC views={" << view << L"}";
        if ((data = p.load_3ddata(L"riemann.prc", buf.str())) == -1) {
            wcerr << L"Error: " << p.get_errmsg() << endl;
            return 2;
        }

        // Create an annotation containing the loaded 3D data with the
        // defined 3D view as the initial view
        //
        buf.str(L"");
        buf << L"name=annot usercoordinates contents=PRC 3Ddata=" << data
            << L" 3Dactivate={enable=open} 3Dinitialview=" << view;
        p.create_annotation(116, 200, 447, 580, L"3D", buf.str());

        p.end_page_ext(L"");

        p.end_document(L"");
    }
    catch (PDFlib::Exception &ex) {
        wcerr << L"PDFlib exception occurred in hello sample: " << endl
              << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
              << L": " << ex.get_errmsg() << endl;
        return 2;
    }

    return 0;
}
