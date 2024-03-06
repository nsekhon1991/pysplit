/* config.h.  Generated from config.hin by configure.  */
#ifndef CONFIG_H_
#define CONFIG_H_
  #ifndef FALSE
    #define FALSE (1==0)
  #endif
  #ifndef TRUE
    #define TRUE (1==1)
  #endif

#ifndef XSUB
  #define XSUB(V) #V
  #define SUB(V) XSUB(V)
#endif

/* #undef HAVE_TYPE_SIZE_T */
/* #undef HAVE_TYPE_SSIZE_T */
#define HAVE_STRCHR 1
#define HAVE_STRRCHR 1
/* #undef WORDS_BIGENDIAN */

/* #undef DEBUGGER */

#ifndef DEBUG_OUT
  #ifdef DEBUGGER
    #define DEBUG_OUT(str) fprintf(stderr, PROGNAME " debug: " str);fflush(stderr)
    #define DEBUG_OUT1(str,v) fprintf(stderr, PROGNAME " debug: " str, v);fflush(stderr)
    #define DEBUG_OUT2(str,v,w) fprintf(stderr, PROGNAME " debug: " str, v, w);fflush(stderr)
    #define DEBUG_OUT3(str,v,w,x) fprintf(stderr, PROGNAME " debug: " str, v, w, x);fflush(stderr)
  #else
    #define DEBUG_OUT(str)
    #define DEBUG_OUT1(str,v)
    #define DEBUG_OUT2(str,v,w)
    #define DEBUG_OUT3(str,v,w,x)
  #endif
#endif

#endif /*CONFIG_H_*/