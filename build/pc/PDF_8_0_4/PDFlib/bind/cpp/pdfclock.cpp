// $Id: pdfclock.cpp,v 1.25 2009/07/23 09:40:53 stm Exp $
//
// A little PDFlib application to draw an analog clock.
//

#include <iostream>

#include <time.h>

#if !defined(WIN32) && !defined(MAC)
#include <unistd.h>
#endif

#include "pdflib.hpp"

using namespace std;
using namespace pdflib;

#define RADIUS		200.0f
#define MARGIN		20.0f

int
main()
{
    try {
	PDFlib		p;
	double		alpha;
	time_t		timer;
	struct tm	ltime;
	
	//  This means we must check return values of load_font() etc.
	p.set_parameter(L"errorpolicy", L"return");

	// Open new PDF file
	if (p.begin_document(L"pdfclock.pdf", L"") == -1) {
	    wcerr << L"Error: " << p.get_errmsg() << endl; return 2;
	}

	p.set_info(L"Creator", L"pdfclock.cpp");
	p.set_info(L"Author", L"Thomas Merz");
	p.set_info(L"Title", L"PDF clock (C++)");

	p.begin_page_ext((unsigned int) (2 * (RADIUS + MARGIN)),
			  (unsigned int) (2 * (RADIUS + MARGIN)), L"");
	
	p.translate(RADIUS + MARGIN, RADIUS + MARGIN);
	p.setcolor(L"fillstroke", L"rgb", 0, 0, 1, 0);
	p.save();

	// minute strokes
	p.setlinewidth(2);
	for (alpha = 0; alpha < 360; alpha += 6) {
	    p.rotate(6);
	    p.moveto(RADIUS, 0);
	    p.lineto( (RADIUS-MARGIN/3), 0);
	    p.stroke();
	}

	p.restore();
	p.save();

	// 5 minute strokes
	p.setlinewidth(3);
	for (alpha = 0; alpha < 360; alpha += 30) {
	    p.rotate(30);
	    p.moveto(RADIUS, 0);
	    p.lineto(RADIUS-MARGIN, 0);
	    p.stroke();
	}

	time(&timer);
	ltime = *localtime(&timer);

	// draw hour hand
	p.save();
	p.rotate(
		(-((ltime.tm_min/60.0) + ltime.tm_hour - 3.0) * 30.0));
	p.moveto(-RADIUS/10, -RADIUS/20);
	p.lineto(RADIUS/2, 0);
	p.lineto(-RADIUS/10, RADIUS/20);
	p.closepath();
	p.fill();
	p.restore();

	// draw minute hand
	p.save();
	p.rotate( (-((ltime.tm_sec/60.0) + ltime.tm_min - 15.0) * 6.0));
	p.moveto(-RADIUS/10, -RADIUS/20);
	p.lineto(RADIUS * 0.8, 0);
	p.lineto(-RADIUS/10, RADIUS/20);
	p.closepath();
	p.fill();
	p.restore();

	// draw second hand
	p.setcolor(L"fillstroke", L"rgb", 1, 0, 0, 0);
	p.setlinewidth(2);
	p.save();
	p.rotate( -((ltime.tm_sec - 15) * 6));
	p.moveto(-RADIUS/5, 0);
	p.lineto(RADIUS, 0);
	p.stroke();
	p.restore();

	// draw little circle at center
	p.circle(0, 0, RADIUS/30);
	p.fill();

	p.restore();

	p.end_page_ext(L"");

	p.end_document(L"");
    }

    catch (PDFlib::Exception &ex) {
	wcerr << L"PDFlib exception occurred in pdfclock sample: " << endl
	      << L"[" << ex.get_errnum() << L"] " << ex.get_apiname()
	      << L": " << ex.get_errmsg() << endl;
	return 2;
    }

    return 0;
}
