http://acdk.sourceforge.net/
#VS2010 编译注意事项:  

1. intrin.h 注释掉:
   //__MACHINEI(long _InterlockedExchange(long volatile *, long))
   //__MACHINEI(long _InterlockedExchangeAdd(long volatile *, long))
   //__MACHINEI(long _InterlockedCompareExchange (long volatile *, long, long))
  这样才不报错，原因未知

2. 其中以下项目未能编译成功:

   acdk_aal  | acdk_sql_odbc | acdkx_com
   acdk_java | acdk_perl | acdk_python | acdk_tcl
   acdkjava  | acdkperl  | acdkpython  | acdktcl