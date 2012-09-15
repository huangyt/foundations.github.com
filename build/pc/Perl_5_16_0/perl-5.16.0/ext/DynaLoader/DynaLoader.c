/*
 * This file was generated automatically by ExtUtils::ParseXS version 3.16 from the
 * contents of DynaLoader.xs. Do not edit this file, edit DynaLoader.xs instead.
 *
 *    ANY CHANGES MADE HERE WILL BE LOST!
 *
 */

#line 1 "DynaLoader.xs"
/* dl_win32.xs
 * 
 * Platform:	Win32 (Windows NT/Windows 95)
 * Author:	Wei-Yuen Tan (wyt@hip.com)
 * Created:	A warm day in June, 1995
 *
 * Modified:
 *    August 23rd 1995 - rewritten after losing everything when I
 *                       wiped off my NT partition (eek!)
 */

/* Porting notes:

I merely took Paul's dl_dlopen.xs, took out extraneous stuff and
replaced the appropriate SunOS calls with the corresponding Win32
calls.

*/

#define WIN32_LEAN_AND_MEAN
#ifdef __GNUC__
#define Win32_Winsock
#endif
#include <windows.h>
#include <string.h>

#define PERL_NO_GET_CONTEXT

#include "EXTERN.h"
#include "perl.h"
#include "win32.h"

#include "XSUB.h"

typedef struct {
    SV *	x_error_sv;
} my_cxtx_t;		/* this *must* be named my_cxtx_t */

#define DL_CXT_EXTRA	/* ask for dl_cxtx to be defined in dlutils.c */
#include "dlutils.c"	/* SaveError() etc	*/

#define dl_error_sv	(dl_cxtx.x_error_sv)

static char *
OS_Error_String(pTHX)
{
    dMY_CXT;
    DWORD err = GetLastError();
    STRLEN len;
    if (!dl_error_sv)
	dl_error_sv = newSVpvn("",0);
    PerlProc_GetOSError(dl_error_sv,err);
    return SvPV(dl_error_sv,len);
}

static void
dl_private_init(pTHX)
{
    (void)dl_generic_private_init(aTHX);
}

/* 
    This function assumes the list staticlinkmodules
    will be formed from package names with '::' replaced
    with '/'. Thus Win32::OLE is in the list as Win32/OLE
*/
static int
dl_static_linked(char *filename)
{
    char **p;
    char *ptr, *hptr;
    static char subStr[] = "/auto/";
    char szBuffer[MAX_PATH];

    /* avoid buffer overflow when called with invalid filenames */
    if (strlen(filename) >= sizeof(szBuffer))
        return 0;

    /* change all the '\\' to '/' */
    strcpy(szBuffer, filename);
    for(ptr = szBuffer; ptr = strchr(ptr, '\\'); ++ptr)
	*ptr = '/';

    /* delete the file name */
    ptr = strrchr(szBuffer, '/');
    if(ptr != NULL)
	*ptr = '\0';

    /* remove leading lib path */
    ptr = strstr(szBuffer, subStr);
    if(ptr != NULL)
	ptr += sizeof(subStr)-1;
    else
	ptr = szBuffer;

    for (p = staticlinkmodules; *p;p++) {
	if (hptr = strstr(ptr, *p)) {
	    /* found substring, need more detailed check if module name match */
	    if (hptr==ptr) {
		return strcmp(ptr, *p)==0;
	    }
	    if (hptr[strlen(*p)] == 0)
		return hptr[-1]=='/';
	}
    };
    return 0;
}

#line 119 "DynaLoader.c"
#ifndef PERL_UNUSED_VAR
#  define PERL_UNUSED_VAR(var) if (0) var = var
#endif

#ifndef dVAR
#  define dVAR		dNOOP
#endif


/* This stuff is not part of the API! You have been warned. */
#ifndef PERL_VERSION_DECIMAL
#  define PERL_VERSION_DECIMAL(r,v,s) (r*1000000 + v*1000 + s)
#endif
#ifndef PERL_DECIMAL_VERSION
#  define PERL_DECIMAL_VERSION \
	  PERL_VERSION_DECIMAL(PERL_REVISION,PERL_VERSION,PERL_SUBVERSION)
#endif
#ifndef PERL_VERSION_GE
#  define PERL_VERSION_GE(r,v,s) \
	  (PERL_DECIMAL_VERSION >= PERL_VERSION_DECIMAL(r,v,s))
#endif
#ifndef PERL_VERSION_LE
#  define PERL_VERSION_LE(r,v,s) \
	  (PERL_DECIMAL_VERSION <= PERL_VERSION_DECIMAL(r,v,s))
#endif

/* XS_INTERNAL is the explicit static-linkage variant of the default
 * XS macro.
 *
 * XS_EXTERNAL is the same as XS_INTERNAL except it does not include
 * "STATIC", ie. it exports XSUB symbols. You probably don't want that
 * for anything but the BOOT XSUB.
 *
 * See XSUB.h in core!
 */


/* TODO: This might be compatible further back than 5.10.0. */
#if PERL_VERSION_GE(5, 10, 0) && PERL_VERSION_LE(5, 15, 1)
#  undef XS_EXTERNAL
#  undef XS_INTERNAL
#  if defined(__CYGWIN__) && defined(USE_DYNAMIC_LOADING)
#    define XS_EXTERNAL(name) __declspec(dllexport) XSPROTO(name)
#    define XS_INTERNAL(name) STATIC XSPROTO(name)
#  endif
#  if defined(__SYMBIAN32__)
#    define XS_EXTERNAL(name) EXPORT_C XSPROTO(name)
#    define XS_INTERNAL(name) EXPORT_C STATIC XSPROTO(name)
#  endif
#  ifndef XS_EXTERNAL
#    if defined(HASATTRIBUTE_UNUSED) && !defined(__cplusplus)
#      define XS_EXTERNAL(name) void name(pTHX_ CV* cv __attribute__unused__)
#      define XS_INTERNAL(name) STATIC void name(pTHX_ CV* cv __attribute__unused__)
#    else
#      ifdef __cplusplus
#        define XS_EXTERNAL(name) extern "C" XSPROTO(name)
#        define XS_INTERNAL(name) static XSPROTO(name)
#      else
#        define XS_EXTERNAL(name) XSPROTO(name)
#        define XS_INTERNAL(name) STATIC XSPROTO(name)
#      endif
#    endif
#  endif
#endif

/* perl >= 5.10.0 && perl <= 5.15.1 */


/* The XS_EXTERNAL macro is used for functions that must not be static
 * like the boot XSUB of a module. If perl didn't have an XS_EXTERNAL
 * macro defined, the best we can do is assume XS is the same.
 * Dito for XS_INTERNAL.
 */
#ifndef XS_EXTERNAL
#  define XS_EXTERNAL(name) XS(name)
#endif
#ifndef XS_INTERNAL
#  define XS_INTERNAL(name) XS(name)
#endif

/* Now, finally, after all this mess, we want an ExtUtils::ParseXS
 * internal macro that we're free to redefine for varying linkage due
 * to the EXPORT_XSUB_SYMBOLS XS keyword. This is internal, use
 * XS_EXTERNAL(name) or XS_INTERNAL(name) in your code if you need to!
 */

#undef XS_EUPXS
#if defined(PERL_EUPXS_ALWAYS_EXPORT)
#  define XS_EUPXS(name) XS_EXTERNAL(name)
#else
   /* default to internal */
#  define XS_EUPXS(name) XS_INTERNAL(name)
#endif

#ifndef PERL_ARGS_ASSERT_CROAK_XS_USAGE
#define PERL_ARGS_ASSERT_CROAK_XS_USAGE assert(cv); assert(params)

/* prototype to pass -Wmissing-prototypes */
STATIC void
S_croak_xs_usage(pTHX_ const CV *const cv, const char *const params);

STATIC void
S_croak_xs_usage(pTHX_ const CV *const cv, const char *const params)
{
    const GV *const gv = CvGV(cv);

    PERL_ARGS_ASSERT_CROAK_XS_USAGE;

    if (gv) {
        const char *const gvname = GvNAME(gv);
        const HV *const stash = GvSTASH(gv);
        const char *const hvname = stash ? HvNAME(stash) : NULL;

        if (hvname)
            Perl_croak(aTHX_ "Usage: %s::%s(%s)", hvname, gvname, params);
        else
            Perl_croak(aTHX_ "Usage: %s(%s)", gvname, params);
    } else {
        /* Pants. I don't think that it should be possible to get here. */
        Perl_croak(aTHX_ "Usage: CODE(0x%"UVxf")(%s)", PTR2UV(cv), params);
    }
}
#undef  PERL_ARGS_ASSERT_CROAK_XS_USAGE

#ifdef PERL_IMPLICIT_CONTEXT
#define croak_xs_usage(a,b)    S_croak_xs_usage(aTHX_ a,b)
#else
#define croak_xs_usage        S_croak_xs_usage
#endif

#endif

/* NOTE: the prototype of newXSproto() is different in versions of perls,
 * so we define a portable version of newXSproto()
 */
#ifdef newXS_flags
#define newXSproto_portable(name, c_impl, file, proto) newXS_flags(name, c_impl, file, proto, 0)
#else
#define newXSproto_portable(name, c_impl, file, proto) (PL_Sv=(SV*)newXS(name, c_impl, file), sv_setpv(PL_Sv, proto), (CV*)PL_Sv)
#endif /* !defined(newXS_flags) */

#line 261 "DynaLoader.c"

XS_EUPXS(XS_DynaLoader_dl_load_file); /* prototype to pass -Wmissing-prototypes */
XS_EUPXS(XS_DynaLoader_dl_load_file)
{
    dVAR; dXSARGS;
    if (items < 1 || items > 2)
       croak_xs_usage(cv,  "filename, flags=0");
    {
	char *	filename = (char *)SvPV_nolen(ST(0))
;
	int	flags;
#line 119 "DynaLoader.xs"
#line 274 "DynaLoader.c"
	void *	RETVAL;
	dXSTARG;

	if (items < 2)
	    flags = 0;
	else {
	    flags = (int)SvIV(ST(1))
;
	}
#line 120 "DynaLoader.xs"
  {
    DLDEBUG(1,PerlIO_printf(Perl_debug_log,"dl_load_file(%s):\n", filename));
    if (dl_static_linked(filename) == 0) {
	RETVAL = PerlProc_DynaLoad(filename);
    }
    else
	RETVAL = (void*) Win_GetModuleHandle(NULL);
    DLDEBUG(2,PerlIO_printf(Perl_debug_log," libref=%x\n", RETVAL));
    ST(0) = sv_newmortal() ;
    if (RETVAL == NULL)
	SaveError(aTHX_ "load_file:%s",
		  OS_Error_String(aTHX)) ;
    else
	sv_setiv( ST(0), (IV)RETVAL);
  }
#line 300 "DynaLoader.c"
    }
    XSRETURN(1);
}


XS_EUPXS(XS_DynaLoader_dl_unload_file); /* prototype to pass -Wmissing-prototypes */
XS_EUPXS(XS_DynaLoader_dl_unload_file)
{
    dVAR; dXSARGS;
    if (items != 1)
       croak_xs_usage(cv,  "libref");
    {
	void *	libref = INT2PTR(void *,SvIV(ST(0)))
;
	int	RETVAL;
	dXSTARG;
#line 140 "DynaLoader.xs"
    DLDEBUG(1,PerlIO_printf(Perl_debug_log, "dl_unload_file(%lx):\n", PTR2ul(libref)));
    RETVAL = FreeLibrary(libref);
    if (!RETVAL)
        SaveError(aTHX_ "unload_file:%s", OS_Error_String(aTHX)) ;
    DLDEBUG(2,PerlIO_printf(Perl_debug_log, " retval = %d\n", RETVAL));
#line 323 "DynaLoader.c"
	XSprePUSH; PUSHi((IV)RETVAL);
    }
    XSRETURN(1);
}


XS_EUPXS(XS_DynaLoader_dl_find_symbol); /* prototype to pass -Wmissing-prototypes */
XS_EUPXS(XS_DynaLoader_dl_find_symbol)
{
    dVAR; dXSARGS;
    if (items != 2)
       croak_xs_usage(cv,  "libhandle, symbolname");
    {
	void *	libhandle = INT2PTR(void *,SvIV(ST(0)))
;
	char *	symbolname = (char *)SvPV_nolen(ST(1))
;
	void *	RETVAL;
	dXSTARG;
#line 153 "DynaLoader.xs"
    DLDEBUG(2,PerlIO_printf(Perl_debug_log,"dl_find_symbol(handle=%x, symbol=%s)\n",
		      libhandle, symbolname));
    RETVAL = (void*) GetProcAddress((HINSTANCE) libhandle, symbolname);
    DLDEBUG(2,PerlIO_printf(Perl_debug_log,"  symbolref = %x\n", RETVAL));
    ST(0) = sv_newmortal() ;
    if (RETVAL == NULL)
	SaveError(aTHX_ "find_symbol:%s",
		  OS_Error_String(aTHX)) ;
    else
	sv_setiv( ST(0), (IV)RETVAL);
#line 354 "DynaLoader.c"
    }
    XSRETURN(1);
}


XS_EUPXS(XS_DynaLoader_dl_undef_symbols); /* prototype to pass -Wmissing-prototypes */
XS_EUPXS(XS_DynaLoader_dl_undef_symbols)
{
    dVAR; dXSARGS;
    if (items != 0)
       croak_xs_usage(cv,  "");
    PERL_UNUSED_VAR(ax); /* -Wall */
    SP -= items;
    {
#line 167 "DynaLoader.xs"
#line 370 "DynaLoader.c"
	PUTBACK;
	return;
    }
}


XS_EUPXS(XS_DynaLoader_dl_install_xsub); /* prototype to pass -Wmissing-prototypes */
XS_EUPXS(XS_DynaLoader_dl_install_xsub)
{
    dVAR; dXSARGS;
    if (items < 2 || items > 3)
       croak_xs_usage(cv,  "perl_name, symref, filename=\"$Package\"");
    {
	char *	perl_name = (char *)SvPV_nolen(ST(0))
;
	void *	symref = INT2PTR(void *,SvIV(ST(1)))
;
	char *	filename;

	if (items < 3)
	    filename = "DynaLoader";
	else {
	    filename = (char *)SvPV_nolen(ST(2))
;
	}
#line 179 "DynaLoader.xs"
    DLDEBUG(2,PerlIO_printf(Perl_debug_log,"dl_install_xsub(name=%s, symref=%x)\n",
		      perl_name, symref));
    ST(0) = sv_2mortal(newRV((SV*)newXS(perl_name,
					(void(*)(pTHX_ CV *))symref,
					filename)));
#line 402 "DynaLoader.c"
    }
    XSRETURN(1);
}


XS_EUPXS(XS_DynaLoader_dl_error); /* prototype to pass -Wmissing-prototypes */
XS_EUPXS(XS_DynaLoader_dl_error)
{
    dVAR; dXSARGS;
    if (items != 0)
       croak_xs_usage(cv,  "");
    {
	char *	RETVAL;
	dXSTARG;
#line 189 "DynaLoader.xs"
    dMY_CXT;
    RETVAL = dl_last_error;
#line 420 "DynaLoader.c"
	sv_setpv(TARG, RETVAL); XSprePUSH; PUSHTARG;
    }
    XSRETURN(1);
}

#if defined(USE_ITHREADS)
#define XSubPPtmpAAAA 1


XS_EUPXS(XS_DynaLoader_CLONE); /* prototype to pass -Wmissing-prototypes */
XS_EUPXS(XS_DynaLoader_CLONE)
{
    dVAR; dXSARGS;
    PERL_UNUSED_VAR(cv); /* -W */
    {
#line 199 "DynaLoader.xs"
    MY_CXT_CLONE;

    /* MY_CXT_CLONE just does a memcpy on the whole structure, so to avoid
     * using Perl variables that belong to another thread, we create our 
     * own for this thread.
     */
    MY_CXT.x_dl_last_error = newSVpvn("", 0);
#line 444 "DynaLoader.c"
    }
    XSRETURN_EMPTY;
}

#endif
#ifdef __cplusplus
extern "C"
#endif
XS_EXTERNAL(boot_DynaLoader); /* prototype to pass -Wmissing-prototypes */
XS_EXTERNAL(boot_DynaLoader)
{
    dVAR; dXSARGS;
#if (PERL_REVISION == 5 && PERL_VERSION < 9)
    char* file = __FILE__;
#else
    const char* file = __FILE__;
#endif

    PERL_UNUSED_VAR(cv); /* -W */
    PERL_UNUSED_VAR(items); /* -W */
#ifdef XS_APIVERSION_BOOTCHECK
    XS_APIVERSION_BOOTCHECK;
#endif
    XS_VERSION_BOOTCHECK;

        newXS("DynaLoader::dl_load_file", XS_DynaLoader_dl_load_file, file);
        newXS("DynaLoader::dl_unload_file", XS_DynaLoader_dl_unload_file, file);
        newXS("DynaLoader::dl_find_symbol", XS_DynaLoader_dl_find_symbol, file);
        newXS("DynaLoader::dl_undef_symbols", XS_DynaLoader_dl_undef_symbols, file);
        newXS("DynaLoader::dl_install_xsub", XS_DynaLoader_dl_install_xsub, file);
        newXS("DynaLoader::dl_error", XS_DynaLoader_dl_error, file);
#if XSubPPtmpAAAA
        newXS("DynaLoader::CLONE", XS_DynaLoader_CLONE, file);
#endif

    /* Initialisation Section */

#line 112 "DynaLoader.xs"
    (void)dl_private_init(aTHX);

#if XSubPPtmpAAAA
#endif
#line 487 "DynaLoader.c"

    /* End of Initialisation Section */

#if (PERL_REVISION == 5 && PERL_VERSION >= 9)
  if (PL_unitcheckav)
       call_list(PL_scopestack_ix, PL_unitcheckav);
#endif
    XSRETURN_YES;
}
