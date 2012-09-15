
#ifndef expat_config_h
#define expat_config_h

#define VERSION "1.95.8"

// modified rrk
#if defined(ACDK_OS_WINDOWS)
# define COMPILED_FROM_DSP
#endif

#define XML_STATIC 1

#if defined(ACDK_LITTLEENDIAN)
/* 1234 = LIL_ENDIAN, 4321 = BIGENDIAN */
#define BYTEORDER 1234
#else
#define BYTEORDER 4321
#endif

#if defined(ACDK_OS_LINUX)
/* Define to 1 if you have the `bcopy' function. */
#define HAVE_BCOPY 1
#endif

/* Define to 1 if you have the <check.h> header file. */
/* #undef HAVE_CHECK_H */
#if defined(ACDK_OS_LINUX)
/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1
#endif // defined(ACDK_OS_LINUX)

/* Define to 1 if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

#if defined(ACDK_OS_LINUX)
/* Define to 1 if you have a working `mmap' system call. */
#define HAVE_MMAP 1
#endif

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1


#endif //expat_config_h
