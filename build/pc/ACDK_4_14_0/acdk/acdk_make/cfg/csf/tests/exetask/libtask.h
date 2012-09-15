
#if defined(_MSC_VER) || defined(__BORLANDC__)
# ifdef IN_ACDK_MAKE_LIB
#   define ACDK_LIBTASK_PUBLIC __declspec(dllexport)
# else
#   define ACDK_LIBTASK_PUBLIC __declspec(dllimport)
# endif
#else
# define ACDK_LIBTASK_PUBLIC
#endif

void ACDK_LIBTASK_PUBLIC doIt();
