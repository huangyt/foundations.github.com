/* this ALWAYS GENERATED file contains the definitions for the interfaces */


/* File created by MIDL compiler version 5.01.0164 */
/* at Wed Nov 14 15:22:18 2001
 */
/* Compiler settings for IAcdkObject.idl:
    Os (OptLev=s), W1, Zp8, env=Win32, ms_ext, c_ext
    error checks: allocation ref bounds_check enum stub_data 
*/
//@@MIDL_FILE_HEADING(  )


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 440
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H*/

#ifndef __IAcdkObject_h__
#define __IAcdkObject_h__

#ifdef __cplusplus
extern "C"{
#endif 

/* Forward Declarations */ 

#ifndef __IAcdkObject_FWD_DEFINED__
#define __IAcdkObject_FWD_DEFINED__
typedef interface IAcdkObject IAcdkObject;
#endif 	/* __IAcdkObject_FWD_DEFINED__ */


#ifndef __AcdkObject_FWD_DEFINED__
#define __AcdkObject_FWD_DEFINED__

#ifdef __cplusplus
typedef class AcdkObject AcdkObject;
#else
typedef struct AcdkObject AcdkObject;
#endif /* __cplusplus */

#endif 	/* __AcdkObject_FWD_DEFINED__ */


/* header files for imported files */
#include "unknwn.h"
#if !defined(__BORLANDC__)
# include "dispex.h"
#endif 
void __RPC_FAR * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free( void __RPC_FAR * ); 

#ifndef __IAcdkObject_INTERFACE_DEFINED__
#define __IAcdkObject_INTERFACE_DEFINED__

/* interface IAcdkObject */
/* [dual][uuid][object] */ 


EXTERN_C const IID IID_IAcdkObject;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("9A8CA025-A6A0-4b8e-80C4-2F77266E8DDB")
    IAcdkObject : public IDispatch
    {
    public:
        virtual /* [id][vararg] */ HRESULT STDMETHODCALLTYPE New( 
            BSTR classname,
            /* [in] */ SAFEARRAY __RPC_FAR * __MIDL_0015,
            /* [retval][out] */ IDispatch __RPC_FAR *__RPC_FAR *retvalue) = 0;
        
        virtual /* [id][vararg] */ HRESULT STDMETHODCALLTYPE invoke( 
            BSTR methodname,
            /* [in] */ SAFEARRAY __RPC_FAR * args,
            /* [retval][out] */ VARIANT __RPC_FAR *retvalue) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE peek( 
            BSTR membername,
            /* [retval][out] */ VARIANT __RPC_FAR *retvalue) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE poke( 
            BSTR membername,
            /* [in] */ VARIANT value) = 0;
        
        virtual /* [id][vararg] */ HRESULT STDMETHODCALLTYPE invoke_static( 
            BSTR classname,
            BSTR methodname,
            /* [in] */ SAFEARRAY __RPC_FAR * args,
            /* [retval][out] */ VARIANT __RPC_FAR *retvalue) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE peek_static( 
            BSTR classname,
            BSTR membername,
            /* [retval][out] */ VARIANT __RPC_FAR *retvalue) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE poke_static( 
            BSTR classname,
            BSTR membername,
            /* [in] */ VARIANT value) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAcdkObjectVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *QueryInterface )( 
            IAcdkObject __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void __RPC_FAR *__RPC_FAR *ppvObject);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *AddRef )( 
            IAcdkObject __RPC_FAR * This);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *Release )( 
            IAcdkObject __RPC_FAR * This);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfoCount )( 
            IAcdkObject __RPC_FAR * This,
            /* [out] */ UINT __RPC_FAR *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfo )( 
            IAcdkObject __RPC_FAR * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetIDsOfNames )( 
            IAcdkObject __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR __RPC_FAR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID __RPC_FAR *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Invoke )( 
            IAcdkObject __RPC_FAR * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS __RPC_FAR *pDispParams,
            /* [out] */ VARIANT __RPC_FAR *pVarResult,
            /* [out] */ EXCEPINFO __RPC_FAR *pExcepInfo,
            /* [out] */ UINT __RPC_FAR *puArgErr);
        
        /* [id][vararg] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *New )( 
            IAcdkObject __RPC_FAR * This,
            BSTR classname,
            /* [in] */ SAFEARRAY __RPC_FAR * __MIDL_0015,
            /* [retval][out] */ IDispatch __RPC_FAR *__RPC_FAR *retvalue);
        
        /* [id][vararg] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *invoke )( 
            IAcdkObject __RPC_FAR * This,
            BSTR methodname,
            /* [in] */ SAFEARRAY __RPC_FAR * args,
            /* [retval][out] */ VARIANT __RPC_FAR *retvalue);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *peek )( 
            IAcdkObject __RPC_FAR * This,
            BSTR membername,
            /* [retval][out] */ VARIANT __RPC_FAR *retvalue);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *poke )( 
            IAcdkObject __RPC_FAR * This,
            BSTR membername,
            /* [in] */ VARIANT value);
        
        /* [id][vararg] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *invoke_static )( 
            IAcdkObject __RPC_FAR * This,
            BSTR classname,
            BSTR methodname,
            /* [in] */ SAFEARRAY __RPC_FAR * args,
            /* [retval][out] */ VARIANT __RPC_FAR *retvalue);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *peek_static )( 
            IAcdkObject __RPC_FAR * This,
            BSTR classname,
            BSTR membername,
            /* [retval][out] */ VARIANT __RPC_FAR *retvalue);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *poke_static )( 
            IAcdkObject __RPC_FAR * This,
            BSTR classname,
            BSTR membername,
            /* [in] */ VARIANT value);
        
        END_INTERFACE
    } IAcdkObjectVtbl;

    interface IAcdkObject
    {
        CONST_VTBL struct IAcdkObjectVtbl __RPC_FAR *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAcdkObject_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAcdkObject_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAcdkObject_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAcdkObject_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAcdkObject_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAcdkObject_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAcdkObject_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAcdkObject_New(This,classname,__MIDL_0015,retvalue)	\
    (This)->lpVtbl -> New(This,classname,__MIDL_0015,retvalue)

#define IAcdkObject_invoke(This,methodname,args,retvalue)	\
    (This)->lpVtbl -> invoke(This,methodname,args,retvalue)

#define IAcdkObject_peek(This,membername,retvalue)	\
    (This)->lpVtbl -> peek(This,membername,retvalue)

#define IAcdkObject_poke(This,membername,value)	\
    (This)->lpVtbl -> poke(This,membername,value)

#define IAcdkObject_invoke_static(This,classname,methodname,args,retvalue)	\
    (This)->lpVtbl -> invoke_static(This,classname,methodname,args,retvalue)

#define IAcdkObject_peek_static(This,classname,membername,retvalue)	\
    (This)->lpVtbl -> peek_static(This,classname,membername,retvalue)

#define IAcdkObject_poke_static(This,classname,membername,value)	\
    (This)->lpVtbl -> poke_static(This,classname,membername,value)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [id][vararg] */ HRESULT STDMETHODCALLTYPE IAcdkObject_New_Proxy( 
    IAcdkObject __RPC_FAR * This,
    BSTR classname,
    /* [in] */ SAFEARRAY __RPC_FAR * __MIDL_0015,
    /* [retval][out] */ IDispatch __RPC_FAR *__RPC_FAR *retvalue);


void __RPC_STUB IAcdkObject_New_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id][vararg] */ HRESULT STDMETHODCALLTYPE IAcdkObject_invoke_Proxy( 
    IAcdkObject __RPC_FAR * This,
    BSTR methodname,
    /* [in] */ SAFEARRAY __RPC_FAR * args,
    /* [retval][out] */ VARIANT __RPC_FAR *retvalue);


void __RPC_STUB IAcdkObject_invoke_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAcdkObject_peek_Proxy( 
    IAcdkObject __RPC_FAR * This,
    BSTR membername,
    /* [retval][out] */ VARIANT __RPC_FAR *retvalue);


void __RPC_STUB IAcdkObject_peek_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAcdkObject_poke_Proxy( 
    IAcdkObject __RPC_FAR * This,
    BSTR membername,
    /* [in] */ VARIANT value);


void __RPC_STUB IAcdkObject_poke_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id][vararg] */ HRESULT STDMETHODCALLTYPE IAcdkObject_invoke_static_Proxy( 
    IAcdkObject __RPC_FAR * This,
    BSTR classname,
    BSTR methodname,
    /* [in] */ SAFEARRAY __RPC_FAR * args,
    /* [retval][out] */ VARIANT __RPC_FAR *retvalue);


void __RPC_STUB IAcdkObject_invoke_static_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAcdkObject_peek_static_Proxy( 
    IAcdkObject __RPC_FAR * This,
    BSTR classname,
    BSTR membername,
    /* [retval][out] */ VARIANT __RPC_FAR *retvalue);


void __RPC_STUB IAcdkObject_peek_static_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAcdkObject_poke_static_Proxy( 
    IAcdkObject __RPC_FAR * This,
    BSTR classname,
    BSTR membername,
    /* [in] */ VARIANT value);


void __RPC_STUB IAcdkObject_poke_static_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAcdkObject_INTERFACE_DEFINED__ */



#ifndef __AcdkxCom_LIBRARY_DEFINED__
#define __AcdkxCom_LIBRARY_DEFINED__

/* library AcdkxCom */
/* [version][helpstring][uuid] */ 


EXTERN_C const IID LIBID_AcdkxCom;

EXTERN_C const CLSID CLSID_AcdkObject;

#ifdef __cplusplus

class DECLSPEC_UUID("E7C3A3CA-7198-4913-9405-ADA97F480DE7")
AcdkObject;
#endif
#endif /* __AcdkxCom_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

unsigned long             __RPC_USER  BSTR_UserSize(     unsigned long __RPC_FAR *, unsigned long            , BSTR __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserMarshal(  unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserUnmarshal(unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * ); 
void                      __RPC_USER  BSTR_UserFree(     unsigned long __RPC_FAR *, BSTR __RPC_FAR * ); 

unsigned long             __RPC_USER  LPSAFEARRAY_UserSize(     unsigned long __RPC_FAR *, unsigned long            , LPSAFEARRAY __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  LPSAFEARRAY_UserMarshal(  unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, LPSAFEARRAY __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  LPSAFEARRAY_UserUnmarshal(unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, LPSAFEARRAY __RPC_FAR * ); 
void                      __RPC_USER  LPSAFEARRAY_UserFree(     unsigned long __RPC_FAR *, LPSAFEARRAY __RPC_FAR * ); 

unsigned long             __RPC_USER  VARIANT_UserSize(     unsigned long __RPC_FAR *, unsigned long            , VARIANT __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  VARIANT_UserMarshal(  unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, VARIANT __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  VARIANT_UserUnmarshal(unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, VARIANT __RPC_FAR * ); 
void                      __RPC_USER  VARIANT_UserFree(     unsigned long __RPC_FAR *, VARIANT __RPC_FAR * ); 

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif
