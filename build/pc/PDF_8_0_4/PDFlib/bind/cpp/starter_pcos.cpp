/* $Id: starter_pcos.cpp,v 1.10 2009/10/06 12:55:15 stm Exp $
 *
 * pCOS starter:
 * Dump information from an existing PDF document
 *
 * required software: PDFlib+PDI/PPS 8
 * required data: PDF input file
 */


#include <iostream>
#include <iomanip>

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

int
main(void)
{
    try {
	/* This is where the data files are. Adjust as necessary. */
	const wstring searchpath = L"../data";

	PDFlib p;
	const wstring pdfinput = L"TET-datasheet.pdf";

	const wstring	docoptlist = L"requiredmode=minimum";
	int	count, pcosmode;
	int	i, doc;
	wstring	objtype;

	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	p.set_parameter(L"SearchPath", searchpath);

	/* We do not create any output document, so no call to
	 * begin_document() is required.
	 */

	/* Open the input document */
	if ((doc = p.open_pdi_document(pdfinput, docoptlist)) == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl;
	    return 2;
	}

	/* --------- general information (always available) */

	pcosmode = static_cast<int>(p.pcos_get_number(doc, L"pcosmode"));

	wcout << L"   File name: " <<
	    p.pcos_get_string(doc,L"filename") << endl;

	wcout << L" PDF version: " <<
	    p.pcos_get_string(doc, L"pdfversionstring") << endl;

	wcout << L"  Encryption: " <<
	    p.pcos_get_string(doc, L"encrypt/description") << endl;

	wcout << L"   Master pw: " <<
	    ((p.pcos_get_number(doc, L"encrypt/master") != 0)
	    ? L"yes":L"no") << endl;

	wcout << L"     User pw: " <<
	    ((p.pcos_get_number(doc, L"encrypt/user") != 0)
	    ? L"yes" : L"no") << endl;

	wcout << L"Text copying: " <<
	    ((p.pcos_get_number(doc, L"encrypt/nocopy") != 0)
	    ? L"no":L"yes") << endl;

	wcout << L"  Linearized: " <<
	    ((p.pcos_get_number(doc, L"linearized") != 0)
	    ? L"yes" : L"no") << endl;

	if (pcosmode == 0) {
	    wcout << L"Minimum mode: no more information available" << endl;
	    return 0;
	}
	wcout << endl;

	/* --------- more details (requires at least user password) */
	wcout << L"PDF/X status: " << p.pcos_get_string(doc, L"pdfx") << endl;

	wcout << L"PDF/A status: " << p.pcos_get_string(doc, L"pdfa") << endl;

        bool xfa_present =
            p.pcos_get_string(doc, L"type:/Root/AcroForm/XFA") != L"null";
        wcout << L"    XFA data: " << (xfa_present ? L"yes" : L"no") << endl;

	wcout << L"  Tagged PDF: " <<
            ((p.pcos_get_number(doc, L"tagged") != 0) ? L"yes" : L"no") << endl;

	wcout << L"No. of pages: " <<
	    static_cast<int>(p.pcos_get_number(doc, L"length:pages")) << endl;

	wcout << L" Page 1 size: width=" << fixed << setprecision(3) <<
	     p.pcos_get_number(doc, L"pages[0]/width") << L", height=" <<
	     p.pcos_get_number(doc, L"pages[0]/height") << endl;
	/* reset formatting to default */
	wcout << resetiosflags(ios::floatfield) << setprecision(6);

	count = static_cast<int>(p.pcos_get_number(doc, L"length:fonts"));
	wcout << L"No. of fonts: " <<  count << endl;

	for (i = 0; i < count; i++) {
	    wostringstream fontstr;

            fontstr.str(L"");
            fontstr << L"fonts[" << i << L"]";
            const wstring& font = fontstr.str();

	    if (p.pcos_get_number(doc, font + L"/embedded") != 0)
		wcout << L"embedded ";
	    else
		wcout << L"unembedded ";

	    wcout << p.pcos_get_string(doc, font + L"/type") << L" font ";
	    wcout << p.pcos_get_string(doc, font + L"/name") << endl;
	}

	wcout << endl;

	const bool plainmetadata =
	        p.pcos_get_number(doc, L"encrypt/plainmetadata") != 0;

	if (pcosmode == 1 && !plainmetadata
	            && p.pcos_get_number(doc, L"encrypt/nocopy") != 0) {
	    wcout << L"Restricted mode: no more information available" << endl;
	    return 0;
	}

	/* ----- document info keys and XMP metadata (requires master pw) */

	count = (int) p.pcos_get_number(doc, L"length:/Info");

	for (i=0; i < count; i++) {
	    wostringstream infostr;
	    wstring key;

            infostr.str(L"");
            infostr << L"/Info[" << i << L"]";
            const wstring& info = infostr.str();

	    objtype = p.pcos_get_string(doc, L"type:" + info);

	    key = p.pcos_get_string(doc, info + L".key");
	    wcout.width(12);
	    wcout << key << L": ";

	    /* Info entries can be stored as string or name objects */
	    if (objtype == L"name" || objtype == L"string") {
		wcout << L"'" + p.pcos_get_string(doc, info) << L"'" << endl;
	    }
            else {
		wcout << L"(" + objtype << L" object)" << endl;
	    }
	}

	wcout << endl << L"XMP metadata: ";

	objtype = p.pcos_get_string(doc, L"type:/Root/Metadata");
	if (objtype == L"stream") {
	    const unsigned char *contents;
	    int len;

	    contents = p.pcos_get_stream(doc, &len, L"", L"/Root/Metadata");
	    wcout << len << L" bytes ";
	}
        else {
	    wcout << L"not present";
	}
	wcout << endl;

	p.close_pdi_document(doc);
    }
    catch (PDFlib::Exception &ex) {
	wcerr << L"PDFlib exception occurred: " << endl
	      << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
	      << L": " << ex.get_errmsg() << endl;
	return 2;
    }

    return 0;
}
