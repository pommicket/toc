
#define USE_COLORED_TEXT 1

#if USE_COLORED_TEXT
#define TEXT_ERROR(x) "\x1b[91m" x "\x1b[0m"
#define TEXT_IMPORTANT(x) "\x1b[1m" x "\x1b[0m"
#else
#define TEXT_ERROR(x) x
#define TEXT_IMPORTANT(x) x
#endif
