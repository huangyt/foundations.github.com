#include <acdk/Platform.h>

#if defined(ACDK_64_BIT_PTR)
# define SQLITE_PTR_SZ 8
#else
# define SQLITE_PTR_SZ 4
#endif
